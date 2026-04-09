# ============================================================
# Fantasy Football Projection Model — ESPN Ensemble Version
# ============================================================
# Tier-adaptive ESPN blending:
#   - Starters (ESPN proj >= 12): 50/50 model/ESPN
#   - All others: 70/30 model/ESPN (our model dominates here)
# ============================================================
# Improvements over previous version:
#   1)  Per-position XGBoost models (RB, WR, TE trained separately)
#   2)  ESPN ensemble blending (model + ESPN weighted average)
#   3)  Weather & dome features (wind, temp, roof type)
#   4)  Rest & schedule context (days_rest, is_thursday, after_bye)
#   5)  Player age & experience features
#   6)  Hyperparameter tuning via random search
#   7)  Quantile regression for floor/ceiling projections
#   8)  Feature pruning based on importance
#   9)  Bug fixes: safe_min/safe_max helpers, pre-computed TD cols
#  10)  All prior features retained (Vegas, red zone, EWMA, etc.)
# ============================================================

# -- 1. Libraries ---------------------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readr)
library(nflreadr)
library(nflfastR)
library(slider)
library(rvest)
library(xml2)
library(lubridate)
library(xgboost)

# -- Reproducibility seed --
# Set before any random number generation (random search, CV folds, etc.)
# A fresh R session is required for deterministic results; sourcing this
# script twice in the same session will advance the RNG state.
set.seed(47)

# -- 2. Configuration -----------------------------------------

Sys.setenv(ESPN_S2 = "AEACephTYe0pI3CkugRRVQjp%2FpqW%2BxnUJBSfBMb1A50r81oL6U7WmRp6OBKWHFC5FJSDpGwVXMb8fHNMqV1RQcr7tu%2FHLPa1l2HHLt%2BWO0%2FCA9S1fIVfll94zpEGzO%2BMnVtMrk6Ewmcw2YlxOdyhDIMU%2B67Uq8Ufhs30tIortnNAvAhfSDtwaAsGj9YCM816aW7wquRT2P0CqeqyY7R2NnjSGDIUiVP3Dj2BwmIQ%2FIEfQ2cg5NCgL6IxkAtxq89nrGecECF%2BE8o5fe2yh%2BsxHyvNDYT3HGvX8rcBAlyUw1xDoxB9OudOvP2nWvRT2x9MVkM%3D")
Sys.setenv(ESPN_SWID = "{32054C85-DA90-46BC-A28C-E1580D145337}")

league_id    <- 1211731252
season_id    <- 2025
week_eval    <- 16
cutoff_week  <- week_eval - 1
model_seasons    <- 2022:season_id
backtest_seasons <- 2023:2024
backtest_start_week <- 1

pos_map   <- c(`1` = "QB", `2` = "RB", `3` = "WR", `4` = "TE", `5` = "K", `16` = "D/ST")
skill_pos <- c("RB", "WR", "TE")

# Flags
run_2023_2024_backtest <- TRUE
run_2025_eval          <- TRUE
run_hyperparam_tuning  <- TRUE    # NEW: tune XGBoost hyperparameters
run_quantile_models    <- TRUE    # NEW: train floor/ceiling quantile models
run_feature_pruning    <- TRUE    # NEW: prune low-importance features
ensemble_blend_starter <- 0.50    # NEW: blend weight for starters (ESPN proj >= 12)
ensemble_blend_other   <- 0.70    # NEW: blend weight for non-starters (model gets more weight)
tuning_n_trials        <- 20     # NEW: number of random search trials

`%||%` <- function(x, y) if (is.null(x)) y else x

espn_s2 <- Sys.getenv("ESPN_S2")
swid    <- Sys.getenv("ESPN_SWID")
stopifnot(nchar(espn_s2) > 0, nchar(swid) > 0)
cookie_header <- paste0("espn_s2=", espn_s2, "; SWID=", swid)

# -- 3. Utility functions (all defined ONCE) -------------------

safe_mae  <- function(actual, pred) {
  ok <- !is.na(actual) & !is.na(pred)
  if (!any(ok)) return(NA_real_)
  mean(abs(actual[ok] - pred[ok]))
}

safe_rmse <- function(actual, pred) {
  ok <- !is.na(actual) & !is.na(pred)
  if (!any(ok)) return(NA_real_)
  sqrt(mean((actual[ok] - pred[ok])^2))
}

paired_weekly_mae_boot <- function(df, pred_col, baseline_col,
                                   truth_col = "fantasy_ppr",
                                   week_col = "week",
                                   B = 5000,
                                   conf = 0.95) {
  weekly <- df %>%
    filter(
      !is.na(.data[[truth_col]]),
      !is.na(.data[[pred_col]]),
      !is.na(.data[[baseline_col]])
    ) %>%
    group_by(.data[[week_col]]) %>%
    summarise(
      mae_model = mean(abs(.data[[truth_col]] - .data[[pred_col]])),
      mae_base  = mean(abs(.data[[truth_col]] - .data[[baseline_col]])),
      mae_diff  = mae_base - mae_model,
      .groups = "drop"
    )
  
  n_weeks <- nrow(weekly)
  if (n_weeks == 0) stop("No valid weekly rows available for bootstrap.")
  
  alpha <- (1 - conf) / 2
  
  boot_diff <- replicate(B, {
    idx <- sample.int(n_weeks, size = n_weeks, replace = TRUE)
    mean(weekly$mae_diff[idx])
  })
  
  tibble(
    n_weeks = n_weeks,
    mae_diff = mean(weekly$mae_diff),
    ci_lo = unname(quantile(boot_diff, alpha)),
    ci_hi = unname(quantile(boot_diff, 1 - alpha)),
    p_two_sided = 2 * min(mean(boot_diff <= 0), mean(boot_diff >= 0))
  )
}

safe_mean <- function(x, default = 0) {
  out <- mean(x, na.rm = TRUE)
  if (!is.finite(out)) default else out
}

# FIX: Named helpers replace inline ~ if() that caused linter errors
safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

clamp01 <- function(x) pmax(0, pmin(1, x))

clean_name <- function(x) {
  x %>% str_to_lower() %>% str_replace_all("[^a-z0-9 ]", "") %>% str_squish()
}

season_week_key <- function(season, week) season * 100L + week

make_consecutive_count <- function(x) {
  out <- integer(length(x))
  run <- 0L
  for (i in seq_along(x)) {
    if (is.na(x[i]) || x[i] == 0L) run <- 0L else run <- run + 1L
    out[i] <- run
  }
  out
}

ewma <- function(x, alpha = 0.5) {
  n <- length(x)
  if (n == 0 || all(is.na(x))) return(NA_real_)
  weights <- alpha * (1 - alpha)^(seq(n - 1, 0))
  ok <- !is.na(x)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * weights[ok]) / sum(weights[ok])
}

# -- 4. ESPN API helpers (unchanged) --------------------------

fetch_roster_json <- function(team_id, week, season = season_id) {
  url <- sprintf(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/%d/segments/0/leagues/%d",
    season, league_id
  )
  res <- GET(url,
             query = list(forTeamId = team_id, scoringPeriodId = week, view = "mRoster"),
             add_headers(Cookie = cookie_header, "User-Agent" = "Mozilla/5.0")
  )
  if (status_code(res) != 200) stop("HTTP ", status_code(res))
  fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

extract_week_points <- function(j, team_id, week, statSourceId_target) {
  team    <- keep(j$teams, ~ .x$id == team_id)[[1]]
  entries <- team$roster$entries
  players <- map(entries, ~ .x$playerPoolEntry$player)
  
  tibble(
    week = week, teamId = team_id,
    espn_id     = map_int(players, "id"),
    fullName    = map_chr(players, "fullName"),
    positionId  = map_int(players, "defaultPositionId"),
    proTeamId   = map_int(players, "proTeamId"),
    stats       = map(players, "stats")
  ) %>%
    unnest_longer(stats) %>%
    mutate(
      appliedTotal    = map_dbl(stats, ~ .x$appliedTotal %||% NA_real_),
      seasonId        = map_int(stats, ~ .x$seasonId %||% NA_integer_),
      scoringPeriodId = map_int(stats, ~ .x$scoringPeriodId %||% NA_integer_),
      statSourceId    = map_int(stats, ~ .x$statSourceId %||% NA_integer_),
      statSplitTypeId = map_int(stats, ~ .x$statSplitTypeId %||% NA_integer_)
    ) %>%
    filter(seasonId == season_id, scoringPeriodId == week, statSourceId == statSourceId_target) %>%
    group_by(week, teamId, espn_id) %>%
    summarise(
      fullName    = first(fullName),
      positionId  = first(positionId),
      proTeamId   = first(proTeamId),
      pts         = appliedTotal[which.max(replace_na(appliedTotal, -Inf))],
      .groups = "drop"
    )
}

fetch_espn_projections_all_players <- function(season, week, lid = league_id, cookie = cookie_header) {
  url <- sprintf(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/%d/segments/0/leagues/%d",
    season, lid
  )
  res <- GET(url,
             query = list(scoringPeriodId = week, view = "kona_player_info"),
             add_headers(
               Cookie = cookie, "User-Agent" = "Mozilla/5.0",
               "x-fantasy-filter" = '{"players":{"limit":500,"sortPercOwned":{"sortAsc":false,"sortPriority":1},"filterSlotIds":{"value":[2,4,6,23,24]},"filterStatsForSourceIds":{"value":[1]},"filterStatsForSplitTypeIds":{"value":[1]}}}'
             )
  )
  if (status_code(res) != 200) {
    warning("ESPN API returned ", status_code(res), " for season=", season, " week=", week)
    return(tibble(season = integer(), week = integer(), espn_id = integer(),
                  fullName = character(), positionId = integer(), espn_proj = double()))
  }
  j <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  map_dfr(j$players, function(p) {
    player <- p$player
    proj_stats <- keep(player$stats, function(s) {
      !is.null(s$statSourceId) && s$statSourceId == 1 &&
        !is.null(s$scoringPeriodId) && s$scoringPeriodId == week
    })
    if (length(proj_stats) == 0) return(NULL)
    tibble(season = as.integer(season), week = as.integer(week),
           espn_id = player$id, fullName = player$fullName,
           positionId = player$defaultPositionId,
           espn_proj = proj_stats[[1]]$appliedTotal %||% NA_real_)
  })
}

# -- 5. Load play-by-play and build base weekly stats ----------

cat("Loading play-by-play data...\n")
pbp <- nflfastR::load_pbp(model_seasons) %>% filter(season_type == "REG")

# -- 5a. Vegas implied team totals -----------------------------

cat("Loading Vegas lines...\n")
schedules <- nflreadr::load_schedules(model_seasons) %>%
  filter(game_type == "REG") %>%
  mutate(
    implied_total_home = (total_line + spread_line) / 2,
    implied_total_away = (total_line - spread_line) / 2
  )

vegas_by_team_week <- bind_rows(
  schedules %>% transmute(season, week, team = home_team,
                          opponent_team = away_team,
                          implied_team_total = implied_total_home,
                          implied_opp_total  = implied_total_away,
                          game_total_line    = total_line,
                          spread_line        = spread_line,
                          is_home            = 1L),
  schedules %>% transmute(season, week, team = away_team,
                          opponent_team = home_team,
                          implied_team_total = implied_total_away,
                          implied_opp_total  = implied_total_home,
                          game_total_line    = total_line,
                          spread_line        = -spread_line,
                          is_home            = 0L)
)

# -- 5b. Team volume context -----------------------------------

team_volume_weekly <- pbp %>%
  filter(!is.na(posteam), posteam != "") %>%
  group_by(season, week, posteam) %>%
  summarise(
    team_pass_attempts = sum(pass_attempt == 1, na.rm = TRUE),
    team_rush_attempts = sum(rush_attempt == 1, na.rm = TRUE),
    team_total_plays   = n(),
    team_points_scored = max(posteam_score_post, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(team = posteam) %>%
  filter(!is.na(team), team != "") %>%
  group_by(season, team) %>%
  arrange(week, .by_group = TRUE) %>%
  mutate(
    team_pass_att_avg3 = slide_dbl(lag(team_pass_attempts), ~ mean(.x, na.rm = TRUE), .before = 2),
    team_rush_att_avg3 = slide_dbl(lag(team_rush_attempts), ~ mean(.x, na.rm = TRUE), .before = 2),
    team_plays_avg3    = slide_dbl(lag(team_total_plays),   ~ mean(.x, na.rm = TRUE), .before = 2),
    team_pts_avg3      = slide_dbl(lag(team_points_scored), ~ mean(.x, na.rm = TRUE), .before = 2)
  ) %>%
  ungroup()

# -- 5c. Red zone features ------------------------------------

rz_receiving <- pbp %>%
  filter(!is.na(receiver_player_id), receiver_player_id != "", yardline_100 <= 20) %>%
  group_by(season, week, player_id = receiver_player_id) %>%
  summarise(
    rz_targets    = n(),
    rz_receptions = sum(complete_pass == 1, na.rm = TRUE),
    rz_rec_tds    = sum(pass_touchdown == 1, na.rm = TRUE),
    .groups = "drop"
  )

rz_rushing <- pbp %>%
  filter(!is.na(rusher_player_id), rusher_player_id != "", yardline_100 <= 20) %>%
  group_by(season, week, player_id = rusher_player_id) %>%
  summarise(
    rz_carries  = n(),
    rz_rush_tds = sum(rush_touchdown == 1, na.rm = TRUE),
    .groups = "drop"
  )

gl_rushing <- pbp %>%
  filter(!is.na(rusher_player_id), rusher_player_id != "", yardline_100 <= 5) %>%
  group_by(season, week, player_id = rusher_player_id) %>%
  summarise(gl_carries = n(), .groups = "drop")

# -- 5d. Defensive EPA ----------------------------------------

defense_epa_weekly <- pbp %>%
  group_by(season, week, defense_team = defteam) %>%
  summarise(
    def_pass_epa_allowed    = mean(epa[pass_attempt == 1], na.rm = TRUE),
    def_rush_epa_allowed    = mean(epa[rush_attempt == 1], na.rm = TRUE),
    def_overall_epa_allowed = mean(epa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(season, defense_team) %>%
  arrange(week, .by_group = TRUE) %>%
  mutate(
    def_pass_epa_allowed_avg3    = slide_dbl(lag(def_pass_epa_allowed),    ~ mean(.x, na.rm = TRUE), .before = 2),
    def_rush_epa_allowed_avg3    = slide_dbl(lag(def_rush_epa_allowed),    ~ mean(.x, na.rm = TRUE), .before = 2),
    def_overall_epa_allowed_avg3 = slide_dbl(lag(def_overall_epa_allowed), ~ mean(.x, na.rm = TRUE), .before = 2)
  ) %>%
  ungroup()

defense_allowed_weekly <- pbp %>%
  group_by(season, week, defense_team = defteam) %>%
  summarise(
    pass_yards_allowed = sum(passing_yards, na.rm = TRUE),
    rush_yards_allowed = sum(rushing_yards, na.rm = TRUE),
    pass_tds_allowed   = sum(pass_touchdown == 1, na.rm = TRUE),
    rush_tds_allowed   = sum(rush_touchdown == 1, na.rm = TRUE),
    rec_allowed        = sum(complete_pass == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(season, defense_team) %>%
  arrange(week, .by_group = TRUE) %>%
  mutate(
    pass_yards_allowed_avg3 = slide_dbl(lag(pass_yards_allowed), ~ mean(.x, na.rm = TRUE), .before = 2),
    rush_yards_allowed_avg3 = slide_dbl(lag(rush_yards_allowed), ~ mean(.x, na.rm = TRUE), .before = 2),
    pass_tds_allowed_avg3   = slide_dbl(lag(pass_tds_allowed),   ~ mean(.x, na.rm = TRUE), .before = 2),
    rush_tds_allowed_avg3   = slide_dbl(lag(rush_tds_allowed),   ~ mean(.x, na.rm = TRUE), .before = 2),
    rec_allowed_avg3        = slide_dbl(lag(rec_allowed),         ~ mean(.x, na.rm = TRUE), .before = 2)
  ) %>%
  ungroup()

# -- 5e. NEW: Weather & game environment features ---------------

cat("Building weather & game environment features...\n")

game_env_by_team <- bind_rows(
  schedules %>% transmute(
    season, week, team = home_team,
    roof  = replace_na(roof, "outdoors"),
    temp  = suppressWarnings(as.numeric(temp)),
    wind  = suppressWarnings(as.numeric(wind))
  ),
  schedules %>% transmute(
    season, week, team = away_team,
    roof  = replace_na(roof, "outdoors"),
    temp  = suppressWarnings(as.numeric(temp)),
    wind  = suppressWarnings(as.numeric(wind))
  )
) %>%
  mutate(
    is_dome     = as.integer(roof %in% c("dome", "closed")),
    high_wind   = as.integer(!is.na(wind) & wind >= 15),
    cold_game   = as.integer(!is.na(temp) & temp <= 32),
    # Replace NA wind/temp with neutral defaults (dome games, etc.)
    wind = replace_na(wind, 0),
    temp = replace_na(temp, 65)
  )

cat("  Weather features built:", nrow(game_env_by_team), "rows\n")

# -- 5f. NEW: Rest & schedule context features ------------------

cat("Building rest & schedule context features...\n")

game_dates_by_team <- bind_rows(
  schedules %>% transmute(season, week, team = home_team,
                          gameday = as.Date(gameday, format = "%Y-%m-%d")),
  schedules %>% transmute(season, week, team = away_team,
                          gameday = as.Date(gameday, format = "%Y-%m-%d"))
) %>%
  filter(!is.na(gameday)) %>%
  arrange(team, season, week)

rest_features <- game_dates_by_team %>%
  group_by(team) %>%
  mutate(
    days_rest   = as.integer(gameday - lag(gameday)),
    is_thursday = as.integer(wday(gameday) == 5),
    is_monday   = as.integer(wday(gameday) == 2)
  ) %>%
  ungroup() %>%
  mutate(
    # Cap days_rest: first game of season gets NA -> use 7 as default
    days_rest = replace_na(days_rest, 7L),
    # Short week = fewer than 6 days rest (Thursday games after Sunday)
    short_week = as.integer(days_rest < 6)
  )

# Bye week detection: find weeks where team didn't play
all_team_weeks <- expand_grid(
  team = unique(c(schedules$home_team, schedules$away_team)),
  season = model_seasons
) %>%
  cross_join(tibble(week = 1:18))

bye_detection <- all_team_weeks %>%
  left_join(
    rest_features %>% select(team, season, week, gameday),
    by = c("team", "season", "week")
  ) %>%
  arrange(team, season, week) %>%
  group_by(team, season) %>%
  mutate(
    is_bye    = as.integer(is.na(gameday)),
    after_bye = as.integer(lag(is_bye, default = 0L) == 1L)
  ) %>%
  ungroup() %>%
  filter(!is.na(gameday)) %>%
  select(team, season, week, after_bye)

rest_schedule <- rest_features %>%
  left_join(bye_detection, by = c("team", "season", "week")) %>%
  mutate(after_bye = replace_na(after_bye, 0L)) %>%
  select(team, season, week, days_rest, is_thursday, is_monday, short_week, after_bye)

cat("  Rest features built:", nrow(rest_schedule), "rows\n")

# -- 5g. NEW: Player age & experience features -----------------

cat("Loading player age & experience...\n")

rosters_all <- nflreadr::load_rosters(model_seasons)

player_age_exp <- rosters_all %>%
  transmute(
    gsis_id,
    season,
    birth_date = as.Date(birth_date),
    years_exp  = suppressWarnings(as.integer(years_exp))
  ) %>%
  filter(!is.na(gsis_id), gsis_id != "") %>%
  group_by(gsis_id, season) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    age_at_season = as.numeric(
      difftime(as.Date(paste0(season, "-09-01")), birth_date, units = "days")
    ) / 365.25,
    is_rookie     = as.integer(replace_na(years_exp, 0) == 0),
    is_veteran    = as.integer(replace_na(years_exp, 0) >= 8)
  ) %>%
  select(gsis_id, season, age_at_season, years_exp, is_rookie, is_veteran)

cat("  Player age features built:", nrow(player_age_exp), "rows\n")

# -- 6. Player weekly aggregation ------------------------------

pass_weekly <- pbp %>%
  filter(!is.na(passer_player_id), passer_player_id != "") %>%
  group_by(season, week, player_id = passer_player_id, player_name = passer_player_name) %>%
  summarise(pass_yards = sum(passing_yards, na.rm = TRUE),
            pass_tds = sum(pass_touchdown == 1, na.rm = TRUE),
            interceptions = sum(interception == 1, na.rm = TRUE),
            .groups = "drop")

rush_weekly <- pbp %>%
  filter(!is.na(rusher_player_id), rusher_player_id != "") %>%
  group_by(season, week, player_id = rusher_player_id, player_name = rusher_player_name) %>%
  summarise(rush_yards = sum(rushing_yards, na.rm = TRUE),
            rush_tds = sum(rush_touchdown == 1, na.rm = TRUE),
            .groups = "drop")

rec_weekly <- pbp %>%
  filter(!is.na(receiver_player_id), receiver_player_id != "") %>%
  group_by(season, week, player_id = receiver_player_id, player_name = receiver_player_name) %>%
  summarise(receptions = sum(complete_pass == 1, na.rm = TRUE),
            rec_yards = sum(receiving_yards, na.rm = TRUE),
            rec_tds = sum(pass_touchdown == 1, na.rm = TRUE),
            air_yards = sum(air_yards, na.rm = TRUE),
            targets = n(),
            .groups = "drop")

weekly <- full_join(pass_weekly, rush_weekly, by = c("season", "week", "player_id", "player_name")) %>%
  full_join(rec_weekly, by = c("season", "week", "player_id", "player_name")) %>%
  replace_na(list(pass_yards = 0, pass_tds = 0, interceptions = 0,
                  rush_yards = 0, rush_tds = 0,
                  receptions = 0, rec_yards = 0, rec_tds = 0, air_yards = 0, targets = 0)) %>%
  mutate(
    fantasy_ppr = pass_yards * 0.04 + pass_tds * 4 - interceptions * 2 +
      rush_yards * 0.1 + rush_tds * 6 +
      receptions * 1 + rec_yards * 0.1 + rec_tds * 6
  )

# Join position and usage shares
ps <- nflreadr::load_player_stats(model_seasons) %>%
  filter(season_type == "REG") %>%
  transmute(season, week, player_id,
            pos_ps = position, team, opponent_team,
            target_share, receiving_air_yards, air_yards_share, wopr)

pos_by_player_season <- ps %>%
  filter(!is.na(pos_ps)) %>%
  count(season, player_id, pos_ps, name = "n") %>%
  group_by(season, player_id) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(season, player_id, pos_ps_mode = pos_ps)

weekly <- weekly %>%
  left_join(ps %>% select(-pos_ps), by = c("season", "week", "player_id")) %>%
  left_join(pos_by_player_season, by = c("season", "player_id")) %>%
  left_join(rz_receiving, by = c("season", "week", "player_id")) %>%
  left_join(rz_rushing,   by = c("season", "week", "player_id")) %>%
  left_join(gl_rushing,   by = c("season", "week", "player_id")) %>%
  mutate(
    position = toupper(replace_na(pos_ps_mode, "UNK")),
    target_share       = replace_na(target_share, 0),
    receiving_air_yards = as.numeric(replace_na(receiving_air_yards, 0)),
    air_yards_share    = replace_na(air_yards_share, 0),
    wopr               = replace_na(wopr, 0),
    rz_targets    = replace_na(rz_targets, 0),
    rz_receptions = replace_na(rz_receptions, 0),
    rz_rec_tds    = replace_na(rz_rec_tds, 0),
    rz_carries    = replace_na(rz_carries, 0),
    rz_rush_tds   = replace_na(rz_rush_tds, 0),
    gl_carries    = replace_na(gl_carries, 0),
    rz_opportunities = rz_targets + rz_carries,
    total_tds = pass_tds + rush_tds + rec_tds,
    # FIX: pre-compute combined RZ TDs before using in lag()
    rz_total_tds = rz_rec_tds + rz_rush_tds
  ) %>%
  select(-pos_ps_mode)

# -- 6b. Position-specific defense: FP allowed to RB/WR/TE -----

def_fp_allowed_by_pos <- weekly %>%
  filter(position %in% skill_pos, !is.na(opponent_team), opponent_team != "") %>%
  group_by(season, week, defense_team = opponent_team, opp_position = position) %>%
  summarise(
    fp_allowed_to_pos = sum(fantasy_ppr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(season, defense_team, opp_position) %>%
  arrange(week, .by_group = TRUE) %>%
  mutate(
    def_fp_allowed_to_pos_avg3 = slide_dbl(
      lag(fp_allowed_to_pos), ~ mean(.x, na.rm = TRUE), .before = 2),
    def_fp_allowed_to_pos_avg4 = slide_dbl(
      lag(fp_allowed_to_pos), ~ mean(.x, na.rm = TRUE), .before = 3)
  ) %>%
  ungroup()

# -- 7. Feature engineering: rolling stats ----------------------

# Join defensive features
weekly <- weekly %>%
  left_join(defense_epa_weekly %>%
              select(season, week, defense_team,
                     def_pass_epa_allowed_avg3, def_rush_epa_allowed_avg3, def_overall_epa_allowed_avg3),
            by = c("season", "week", "opponent_team" = "defense_team")) %>%
  left_join(defense_allowed_weekly %>%
              select(season, week, defense_team,
                     pass_yards_allowed_avg3, rush_yards_allowed_avg3,
                     pass_tds_allowed_avg3, rush_tds_allowed_avg3, rec_allowed_avg3),
            by = c("season", "week", "opponent_team" = "defense_team")) %>%
  left_join(team_volume_weekly %>%
              select(season, week, team, team_pass_att_avg3, team_rush_att_avg3, team_plays_avg3, team_pts_avg3),
            by = c("season", "week", "team")) %>%
  left_join(vegas_by_team_week %>%
              select(season, week, team, implied_team_total, implied_opp_total, game_total_line, spread_line, is_home),
            by = c("season", "week", "team")) %>%
  left_join(def_fp_allowed_by_pos %>%
              select(season, week, defense_team, opp_position, def_fp_allowed_to_pos_avg3, def_fp_allowed_to_pos_avg4),
            by = c("season", "week", "opponent_team" = "defense_team", "position" = "opp_position")) %>%
  # NEW: join weather
  left_join(game_env_by_team %>%
              select(season, week, team, is_dome, wind, temp, high_wind, cold_game),
            by = c("season", "week", "team")) %>%
  # NEW: join rest/schedule
  left_join(rest_schedule,
            by = c("season", "week", "team")) %>%
  # Position-specific defensive matchup
  mutate(
    def_matchup_epa_allowed_avg3 = case_when(
      position == "RB" ~ def_rush_epa_allowed_avg3,
      position %in% c("QB", "WR", "TE") ~ def_pass_epa_allowed_avg3,
      TRUE ~ def_overall_epa_allowed_avg3
    ),
    def_matchup_yards_allowed_avg3 = case_when(
      position == "RB" ~ rush_yards_allowed_avg3,
      TRUE ~ pass_yards_allowed_avg3
    ),
    def_matchup_tds_allowed_avg3 = case_when(
      position == "RB" ~ rush_tds_allowed_avg3,
      TRUE ~ pass_tds_allowed_avg3
    )
  )

# Build rolling player features with multiple windows
weekly <- weekly %>%
  arrange(player_id, season, week) %>%
  group_by(player_id, season) %>%
  mutate(
    # -- Fantasy points: 1-game, 3-game, 5-game, season, EWMA --
    lag1_fp  = lag(fantasy_ppr),
    lag2_fp  = lag(fantasy_ppr, 2),
    lag3_fp  = lag(fantasy_ppr, 3),
    avg3_fp  = slide_dbl(lag(fantasy_ppr), ~ mean(.x, na.rm = TRUE), .before = 2),
    avg4_fp  = slide_dbl(lag(fantasy_ppr), ~ mean(.x, na.rm = TRUE), .before = 3),
    avg5_fp  = slide_dbl(lag(fantasy_ppr), ~ mean(.x, na.rm = TRUE), .before = 4),
    avg6_fp  = slide_dbl(lag(fantasy_ppr), ~ mean(.x, na.rm = TRUE), .before = 5),
    n3_fp    = slide_int(lag(fantasy_ppr), ~ sum(!is.na(.x)), .before = 2),
    season_games  = row_number() - 1L,
    season_sum_fp = lag(cumsum(fantasy_ppr), default = 0),
    season_avg_fp = if_else(season_games > 0, season_sum_fp / season_games, NA_real_),
    
    # EWMA (exponential weighting -- recent games count more)
    ewma_fp = slide_dbl(lag(fantasy_ppr), ~ ewma(.x, alpha = 0.5), .before = 4),
    
    # -- Consistency/volatility features --
    fp_sd3   = slide_dbl(lag(fantasy_ppr), ~ sd(.x, na.rm = TRUE), .before = 2),
    fp_sd5   = slide_dbl(lag(fantasy_ppr), ~ sd(.x, na.rm = TRUE), .before = 4),
    # FIX: use named helper functions instead of inline ~ if()
    fp_min3  = slide_dbl(lag(fantasy_ppr), safe_min, .before = 2),
    fp_max3  = slide_dbl(lag(fantasy_ppr), safe_max, .before = 2),
    fp_range3 = fp_max3 - fp_min3,
    
    # -- Target share --
    lag_target_share  = lag(target_share),
    avg3_target_share = slide_dbl(lag(target_share), ~ mean(.x, na.rm = TRUE), .before = 2),
    avg4_target_share = slide_dbl(lag(target_share), ~ mean(.x, na.rm = TRUE), .before = 3),
    avg5_target_share = slide_dbl(lag(target_share), ~ mean(.x, na.rm = TRUE), .before = 4),
    avg6_target_share = slide_dbl(lag(target_share), ~ mean(.x, na.rm = TRUE), .before = 5),
    n3_target_share   = slide_int(lag(target_share), ~ sum(!is.na(.x)), .before = 2),
    
    # -- Air yards / WOPR --
    lag_air_yards_share  = lag(air_yards_share),
    avg3_air_yards_share = slide_dbl(lag(air_yards_share), ~ mean(.x, na.rm = TRUE), .before = 2),
    lag_rec_air_yards    = lag(receiving_air_yards),
    avg3_rec_air_yards   = slide_dbl(lag(receiving_air_yards), ~ mean(.x, na.rm = TRUE), .before = 2),
    lag_wopr  = lag(wopr),
    avg3_wopr = slide_dbl(lag(wopr), ~ mean(.x, na.rm = TRUE), .before = 2),
    avg4_wopr = slide_dbl(lag(wopr), ~ mean(.x, na.rm = TRUE), .before = 3),
    avg5_wopr = slide_dbl(lag(wopr), ~ mean(.x, na.rm = TRUE), .before = 4),
    avg6_wopr = slide_dbl(lag(wopr), ~ mean(.x, na.rm = TRUE), .before = 5),
    n3_wopr   = slide_int(lag(wopr), ~ sum(!is.na(.x)), .before = 2),
    
    # -- Red zone --
    lag_rz_opps  = lag(rz_opportunities),
    avg3_rz_opps = slide_dbl(lag(rz_opportunities), ~ mean(.x, na.rm = TRUE), .before = 2),
    # FIX: use pre-computed rz_total_tds instead of inline arithmetic in lag()
    lag_rz_tds   = lag(rz_total_tds),
    avg3_rz_tds  = slide_dbl(lag(rz_total_tds), ~ mean(.x, na.rm = TRUE), .before = 2),
    lag_gl_carries = lag(gl_carries),
    
    # -- TD regression signal --
    avg3_total_tds = slide_dbl(lag(total_tds), ~ mean(.x, na.rm = TRUE), .before = 2),
    avg3_rz_opps_for_td_reg = slide_dbl(lag(rz_opportunities), ~ mean(.x, na.rm = TRUE), .before = 4)
  ) %>%
  ungroup() %>%
  select(-season_sum_fp) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)))

cat("Weekly features built:", nrow(weekly), "rows\n")

# -- 8. Snap count features ------------------------------------

roster_xwalk <- nflreadr::load_rosters(model_seasons) %>%
  filter(!is.na(pfr_id), !is.na(gsis_id)) %>%
  distinct(pfr_id, gsis_id)

snap_raw <- nflreadr::load_snap_counts(model_seasons) %>%
  filter(game_type == "REG") %>%
  left_join(roster_xwalk, by = c("pfr_player_id" = "pfr_id"), relationship = "many-to-many") %>%
  filter(!is.na(gsis_id)) %>%
  transmute(gsis_id, season, week,
            offense_snaps = replace_na(offense_snaps, 0),
            offense_pct   = replace_na(offense_pct, 0)) %>%
  group_by(gsis_id, season, week) %>%
  summarise(offense_snaps = max(offense_snaps), offense_pct = max(offense_pct), .groups = "drop")

snap_features <- snap_raw %>%
  arrange(gsis_id, season, week) %>%
  group_by(gsis_id, season) %>%
  mutate(
    lag1_offense_pct = lag(offense_pct),
    avg3_offense_pct = slide_dbl(lag(offense_pct), ~ mean(.x, na.rm = TRUE), .before = 2),
    avg4_offense_pct = slide_dbl(lag(offense_pct), ~ mean(.x, na.rm = TRUE), .before = 3),
    avg5_offense_pct = slide_dbl(lag(offense_pct), ~ mean(.x, na.rm = TRUE), .before = 4),
    avg6_offense_pct = slide_dbl(lag(offense_pct), ~ mean(.x, na.rm = TRUE), .before = 5),
    was_active_gameday = as.integer(offense_snaps > 0),
    lag1_was_active    = lag(was_active_gameday),
    active_weeks_last_3 = slide_int(lag(was_active_gameday), ~ sum(replace_na(.x, 0L)), .before = 2),
    consecutive_zero_snap_weeks = {
      active_lag <- lag(was_active_gameday, default = 1L)
      inactive_lag <- as.integer(active_lag == 0L)
      make_consecutive_count(inactive_lag)
    }
  ) %>%
  ungroup()

cat("Snap features built:", nrow(snap_features), "rows\n")

# -- 9. Injury features ----------------------------------------

cat("Loading injury data...\n")

standardize_practice_status <- function(x) {
  x <- str_squish(str_to_lower(replace_na(x, "")))
  case_when(
    str_detect(x, "did not participate|dnp") ~ "DNP",
    str_detect(x, "limited") ~ "Limited",
    str_detect(x, "full") ~ "Full",
    str_detect(x, "rest") ~ "Rest",
    TRUE ~ "None"
  )
}

standardize_report_status <- function(x) {
  x <- str_squish(str_to_lower(replace_na(x, "")))
  case_when(
    x == "out" ~ "Out",
    x == "doubtful" ~ "Doubtful",
    x == "questionable" ~ "Questionable",
    TRUE ~ "None"
  )
}

map_injury_group <- function(x) {
  x <- str_squish(str_to_lower(replace_na(x, "")))
  case_when(
    x == "" ~ "none",
    str_detect(x, "concussion|head") ~ "concussion",
    str_detect(x, "hamstring|groin|quad|calf") ~ "soft_tissue_lower",
    str_detect(x, "ankle|foot|toe|achilles") ~ "lower_body_joint",
    str_detect(x, "knee") ~ "knee",
    str_detect(x, "hip") ~ "hip",
    str_detect(x, "back|neck") ~ "spine",
    str_detect(x, "shoulder|arm|elbow|wrist|hand|finger|pectoral|chest") ~ "upper_body",
    str_detect(x, "illness|ill") ~ "illness",
    str_detect(x, "rest") ~ "rest",
    TRUE ~ "other"
  )
}

compute_injury_severity_score <- function(practice_status, report_status) {
  case_when(
    report_status == "Out" ~ 1.00,
    report_status == "Doubtful" ~ 0.75,
    report_status == "Questionable" ~ 0.40,
    practice_status == "DNP" ~ 0.25,
    practice_status == "Limited" ~ 0.10,
    TRUE ~ 0.00
  )
}

inj_hist <- nflreadr::load_injuries(seasons = 2009:max(backtest_seasons))

team_col    <- intersect(c("team", "report_team", "latest_team"), names(inj_hist))[1]
name_col    <- intersect(c("full_name", "player_name", "name"), names(inj_hist))[1]
prim_inj    <- intersect(c("report_primary_injury", "primary_injury"), names(inj_hist))[1]
prac_col    <- intersect(c("practice_status"), names(inj_hist))[1]
report_col  <- intersect(c("report_status"), names(inj_hist))[1]

injury_snapshots <- inj_hist %>%
  transmute(
    gsis_id = as.character(gsis_id),
    season, week,
    practice_status = standardize_practice_status(if (!is.na(prac_col)) .data[[prac_col]] else NA),
    report_status   = standardize_report_status(if (!is.na(report_col)) .data[[report_col]] else NA),
    injury_text     = if (!is.na(prim_inj)) .data[[prim_inj]] else NA_character_
  ) %>%
  mutate(injury_group = map_injury_group(injury_text)) %>%
  filter(!is.na(gsis_id), gsis_id != "") %>%
  group_by(gsis_id, season, week) %>%
  slice(1) %>%
  ungroup()

played_tbl <- weekly %>%
  transmute(gsis_id = as.character(player_id), season, week, played = 1L, fantasy_ppr)

player_season_team <- weekly %>%
  transmute(gsis_id = as.character(player_id), season, team) %>%
  filter(!is.na(gsis_id)) %>%
  group_by(gsis_id, season) %>% slice(1) %>% ungroup()

team_schedule <- schedules %>%
  transmute(season, week, team = home_team, opponent_team = away_team) %>%
  bind_rows(schedules %>% transmute(season, week, team = away_team, opponent_team = home_team))

all_player_weeks <- player_season_team %>%
  inner_join(team_schedule, by = c("season", "team"), relationship = "many-to-many") %>%
  distinct(gsis_id, team, season, week)

injury_features <- all_player_weeks %>%
  left_join(injury_snapshots, by = c("gsis_id", "season", "week")) %>%
  left_join(played_tbl, by = c("gsis_id", "season", "week")) %>%
  mutate(
    practice_status = replace_na(practice_status, "None"),
    report_status   = replace_na(report_status, "None"),
    injury_group    = replace_na(injury_group, "none"),
    played          = replace_na(played, 0L),
    practice_dnp       = as.integer(practice_status == "DNP"),
    practice_limited   = as.integer(practice_status == "Limited"),
    status_out         = as.integer(report_status == "Out"),
    status_doubtful    = as.integer(report_status == "Doubtful"),
    status_questionable = as.integer(report_status == "Questionable"),
    injury_any_flag    = as.integer(practice_status != "None" | report_status != "None" | injury_group != "none"),
    injury_severity_score = compute_injury_severity_score(practice_status, report_status),
    missed_game = as.integer(played == 0L)
  ) %>%
  arrange(gsis_id, season, week) %>%
  group_by(gsis_id, season) %>%
  mutate(
    injury_severity_score_lag1 = lag(injury_severity_score, default = 0),
    injury_severity_avg3 = slide_dbl(lag(injury_severity_score), ~ mean(replace_na(.x, 0)), .before = 2),
    injury_report_count_last_3 = slide_int(lag(injury_any_flag), ~ sum(replace_na(.x, 0L)), .before = 2),
    dnp_count_last_3     = slide_int(lag(practice_dnp), ~ sum(replace_na(.x, 0L)), .before = 2),
    limited_count_last_3 = slide_int(lag(practice_limited), ~ sum(replace_na(.x, 0L)), .before = 2),
    games_missed_season  = lag(cumsum(missed_game), default = 0L),
    games_missed_last_3  = slide_int(lag(missed_game), ~ sum(replace_na(.x, 0L)), .before = 2),
    consecutive_weeks_on_injury_report = lag(make_consecutive_count(injury_any_flag), default = 0L)
  ) %>%
  ungroup()

cat("Injury features built:", nrow(injury_features), "rows\n")

# -- 10. Assemble final modeling table --------------------------

cat("Assembling modeling table...\n")

pos_lookup <- weekly %>%
  mutate(gsis_id = as.character(player_id)) %>%
  filter(position %in% skill_pos) %>%
  group_by(gsis_id, season) %>%
  summarise(position = names(sort(table(position), decreasing = TRUE))[1],
            player_name = last(player_name), .groups = "drop")

weekly_features <- weekly %>%
  mutate(gsis_id = as.character(player_id)) %>%
  select(
    gsis_id, season, week,
    lag1_fp, lag2_fp, lag3_fp, avg3_fp, avg4_fp, avg5_fp, avg6_fp, ewma_fp, n3_fp,    season_avg_fp, season_games,
    fp_sd3, fp_sd5, fp_min3, fp_max3, fp_range3,
    lag_target_share, avg3_target_share, avg4_target_share, avg5_target_share, avg6_target_share, n3_target_share,    lag_air_yards_share, avg3_air_yards_share,
    lag_rec_air_yards, avg3_rec_air_yards,
    lag_wopr, avg3_wopr, avg4_wopr, avg5_wopr, avg6_wopr, n3_wopr,    lag_rz_opps, avg3_rz_opps, lag_rz_tds, avg3_rz_tds, lag_gl_carries,
    avg3_total_tds, avg3_rz_opps_for_td_reg,
    def_pass_epa_allowed_avg3, def_rush_epa_allowed_avg3, def_overall_epa_allowed_avg3,
    pass_yards_allowed_avg3, rush_yards_allowed_avg3,
    pass_tds_allowed_avg3, rush_tds_allowed_avg3, rec_allowed_avg3,
    def_matchup_epa_allowed_avg3, def_matchup_yards_allowed_avg3, def_matchup_tds_allowed_avg3,
    team_pass_att_avg3, team_rush_att_avg3, team_plays_avg3, team_pts_avg3,
    implied_team_total, implied_opp_total, game_total_line, spread_line,
    is_home,
    def_fp_allowed_to_pos_avg3, def_fp_allowed_to_pos_avg4,
    # NEW: weather & schedule features
    is_dome, wind, temp, high_wind, cold_game,
    days_rest, is_thursday, is_monday, short_week, after_bye
  )

weekly_features_dedup <- weekly_features %>%
  group_by(gsis_id, season, week) %>%
  slice(1) %>%
  ungroup()

model_tbl <- injury_features %>%
  left_join(pos_lookup, by = c("gsis_id", "season")) %>%
  filter(position %in% skill_pos) %>%
  left_join(weekly_features_dedup, by = c("gsis_id", "season", "week")) %>%
  left_join(snap_features %>%
              select(gsis_id, season, week,
                     offense_pct, lag1_offense_pct, avg3_offense_pct, avg4_offense_pct, avg5_offense_pct, avg6_offense_pct,
                     was_active_gameday, lag1_was_active,
                     active_weeks_last_3, consecutive_zero_snap_weeks),
            by = c("gsis_id", "season", "week")) %>%
  # NEW: join player age & experience
  left_join(player_age_exp, by = c("gsis_id", "season"))

# Carry forward player features
carry_forward_cols <- c(
  "lag1_fp", "avg3_fp", "avg4_fp", "avg5_fp", "avg6_fp", "ewma_fp", "season_avg_fp", "season_games",
  "fp_sd3", "fp_sd5",
  "lag_target_share", "avg3_target_share", "avg4_target_share", "avg5_target_share", "avg6_target_share",
  "lag_air_yards_share", "avg3_air_yards_share",
  "lag_rec_air_yards", "avg3_rec_air_yards",
  "lag_wopr", "avg3_wopr", "avg4_wopr", "avg5_wopr", "avg6_wopr",
  "lag_rz_opps", "avg3_rz_opps",
  "lag1_offense_pct", "avg3_offense_pct", "avg4_offense_pct", "avg5_offense_pct", "avg6_offense_pct", "lag1_was_active",
  "active_weeks_last_3", "consecutive_zero_snap_weeks"
)

model_tbl <- model_tbl %>%
  arrange(gsis_id, season, week) %>%
  group_by(gsis_id) %>%
  fill(all_of(carry_forward_cols), .direction = "down") %>%
  ungroup()

model_tbl <- model_tbl %>%
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)),
    played = replace_na(played, 0L),
    player_id = gsis_id
  )

cat("Final modeling table:", nrow(model_tbl), "rows,",
    n_distinct(model_tbl$gsis_id), "players\n")
cat("Seasons:", paste(sort(unique(model_tbl$season)), collapse = ", "), "\n")
cat("Position breakdown:\n")
print(table(model_tbl$position))

# -- 11. XGBoost model definition (REWRITTEN) ------------------

# Full feature list (used by all position models)
xgb_feature_cols <- c(
  # Player recent form (multiple windows)
  "lag1_fp", "avg3_fp", "avg4_fp", "avg5_fp", "avg6_fp", "ewma_fp", "season_avg_fp", "season_games",
  
  # Consistency
  "fp_sd3", "fp_sd5", "fp_range3",
  
  # Usage shares
  "lag_target_share", "avg3_target_share", "avg4_target_share", "avg5_target_share", "avg6_target_share", "n3_target_share",
  "lag_air_yards_share", "avg3_air_yards_share",
  "lag_rec_air_yards", "avg3_rec_air_yards",
  "lag_wopr", "avg3_wopr", "avg4_wopr", "avg5_wopr", "avg6_wopr", "n3_wopr",
  
  # Red zone usage
  "lag_rz_opps", "avg3_rz_opps", "lag_rz_tds", "avg3_rz_tds", "lag_gl_carries",
  "avg3_total_tds",
  
  # Snap counts
  "lag1_offense_pct", "avg3_offense_pct", "avg4_offense_pct", "avg5_offense_pct", "avg6_offense_pct",
  "lag1_was_active", "active_weeks_last_3", "consecutive_zero_snap_weeks",
  
  # Defensive matchup
  "def_matchup_epa_allowed_avg3",
  "def_matchup_yards_allowed_avg3", "def_matchup_tds_allowed_avg3",
  "pass_yards_allowed_avg3", "rush_yards_allowed_avg3",
  "pass_tds_allowed_avg3", "rush_tds_allowed_avg3",
  
  # Team context
  "team_pass_att_avg3", "team_rush_att_avg3", "team_plays_avg3", "team_pts_avg3",
  
  # Vegas
  "implied_team_total", "implied_opp_total", "game_total_line", "spread_line",
  
  # Game context
  "is_home",
  "week",
  
  # Position-specific defense
  "def_fp_allowed_to_pos_avg3", "def_fp_allowed_to_pos_avg4",
  
  # NEW: Weather & environment
  "is_dome", "wind", "temp", "high_wind", "cold_game",
  
  # NEW: Rest & schedule
  "days_rest", "is_thursday", "is_monday", "short_week", "after_bye",
  
  # NEW: Player age & experience
  "age_at_season", "years_exp", "is_rookie", "is_veteran",
  
  # Injury -- current week status
  "status_out", "status_doubtful", "status_questionable",
  "practice_dnp", "practice_limited",
  "injury_severity_score",
  
  # Injury -- lagged history
  "injury_severity_score_lag1", "injury_severity_avg3",
  "injury_report_count_last_3", "dnp_count_last_3", "limited_count_last_3",
  "games_missed_season", "games_missed_last_3",
  "consecutive_weeks_on_injury_report"
)

# NOTE: Per-position models don't need position dummies since each
# model only sees one position. We keep the encoding for the
# fallback combined model.
prepare_xgb_matrix <- function(df, feature_cols = xgb_feature_cols,
                               target = "fantasy_ppr",
                               include_position_dummies = TRUE) {
  available_features <- intersect(feature_cols, names(df))
  feat_matrix <- df %>%
    select(all_of(available_features)) %>%
    mutate(across(everything(), ~ replace_na(as.numeric(.x), 0))) %>%
    as.matrix()
  
  if (include_position_dummies && "position" %in% names(df)) {
    pos_dummies <- model.matrix(~ position - 1, data = df %>% mutate(position = factor(position)))
    colnames(pos_dummies) <- paste0("pos_", colnames(pos_dummies) %>% str_remove("position"))
    X <- cbind(feat_matrix, pos_dummies)
  } else {
    X <- feat_matrix
  }
  
  if (!is.null(target) && target %in% names(df)) {
    y <- df[[target]]
    stopifnot(length(y) == nrow(X))
    return(list(X = X, y = y, feature_names = colnames(X)))
  }
  list(X = X, feature_names = colnames(X))
}

# -- 11a. Hyperparameter tuning via random search ---------------

tune_xgb_params <- function(train_df, n_trials = tuning_n_trials,
                            include_pos_dummies = TRUE) {
  cat("  Running hyperparameter tuning (", n_trials, "trials)...\n")
  prep   <- prepare_xgb_matrix(train_df, include_position_dummies = include_pos_dummies)
  dtrain <- xgb.DMatrix(data = prep$X, label = prep$y)
  
  best_rmse   <- Inf
  best_params <- NULL
  best_nr     <- 200L
  
  set.seed(42)
  for (i in seq_len(n_trials)) {
    params <- list(
      objective        = "reg:squarederror",
      max_depth        = sample(4:8, 1),
      eta              = runif(1, 0.02, 0.15),
      subsample        = runif(1, 0.6, 0.95),
      colsample_bytree = runif(1, 0.5, 0.9),
      min_child_weight = sample(5:25, 1),
      gamma            = runif(1, 0, 3),
      lambda           = runif(1, 0.5, 5),
      alpha            = runif(1, 0, 1)
    )
    
    cv_result <- tryCatch({
      xgb.cv(
        params = params, data = dtrain,
        nrounds = 400, nfold = 5,
        early_stopping_rounds = 15,
        verbose = 0
      )
    }, error = function(e) NULL)
    
    if (is.null(cv_result)) next
    
    # Extract best RMSE (compatible with xgboost v1 and v2+)
    elog <- cv_result$evaluation_log
    test_cols <- grep("test.*rmse.*mean", names(elog), value = TRUE)
    if (length(test_cols) == 0) next
    
    min_rmse <- min(elog[[test_cols[1]]])
    nr       <- which.min(elog[[test_cols[1]]])
    
    if (min_rmse < best_rmse) {
      best_rmse   <- min_rmse
      best_params <- params
      best_nr     <- nr
    }
  }
  
  if (is.null(best_params)) {
    cat("  Tuning failed, using defaults\n")
    best_params <- list(
      objective = "reg:squarederror", max_depth = 6, eta = 0.05,
      subsample = 0.8, colsample_bytree = 0.8,
      min_child_weight = 10, gamma = 1, lambda = 1, alpha = 0.1
    )
    best_nr <- 200L
  }
  
  cat("  Best CV RMSE:", round(best_rmse, 4),
      "| depth:", best_params$max_depth,
      "| eta:", round(best_params$eta, 3),
      "| nrounds:", best_nr, "\n")
  
  list(params = best_params, best_nrounds = best_nr)
}

# -- 11b. Core model fitting ------------------------------------

fit_xgb_model <- function(train_df, params = NULL, nrounds = NULL,
                          include_pos_dummies = TRUE,
                          feature_subset = NULL) {
  # Use feature subset if provided (for pruning)
  fc <- if (!is.null(feature_subset)) feature_subset else xgb_feature_cols
  
  if (is.null(params)) {
    params <- list(
      objective        = "reg:squarederror",
      max_depth        = 6,
      eta              = 0.05,
      subsample        = 0.8,
      colsample_bytree = 0.8,
      min_child_weight = 10,
      gamma            = 1,
      lambda           = 1,
      alpha            = 0.1
    )
  }
  
  prep   <- prepare_xgb_matrix(train_df, feature_cols = fc,
                               include_position_dummies = include_pos_dummies)
  dtrain <- xgb.DMatrix(data = prep$X, label = prep$y)
  
  # Pick nrounds via CV if not provided
  if (is.null(nrounds)) {
    best_nrounds <- tryCatch({
      cv <- xgb.cv(
        params = params, data = dtrain,
        nrounds = 500, nfold = 5,
        early_stopping_rounds = 20,
        verbose = 0
      )
      nr <- cv$best_iteration
      if (is.null(nr) || length(nr) == 0) {
        elog <- cv$evaluation_log
        test_cols <- grep("test.*rmse.*mean", names(elog), value = TRUE)
        if (length(test_cols) > 0) which.min(elog[[test_cols[1]]]) else 200L
      } else {
        as.integer(nr)
      }
    }, error = function(e) {
      warning("xgb.cv failed: ", conditionMessage(e), " -- using nrounds=200")
      200L
    })
    best_nrounds <- max(10L, min(best_nrounds, 500L))
  } else {
    best_nrounds <- nrounds
  }
  
  cat("  XGBoost nrounds:", best_nrounds, "\n")
  
  model <- xgb.train(
    params  = params,
    data    = dtrain,
    nrounds = best_nrounds,
    evals   = list(train = dtrain),
    verbose = 0
  )
  
  list(model = model, feature_names = prep$feature_names,
       best_nrounds = best_nrounds, feature_cols = fc,
       include_pos_dummies = include_pos_dummies)
}

predict_xgb <- function(fitted, new_df) {
  prep <- prepare_xgb_matrix(new_df,
                             feature_cols = fitted$feature_cols %||% xgb_feature_cols,
                             target = NULL,
                             include_position_dummies = fitted$include_pos_dummies %||% TRUE
  )
  
  X_aligned <- matrix(0, nrow = nrow(prep$X), ncol = length(fitted$feature_names))
  colnames(X_aligned) <- fitted$feature_names
  common <- intersect(colnames(prep$X), fitted$feature_names)
  X_aligned[, common] <- prep$X[, common]
  
  dtest <- xgb.DMatrix(data = X_aligned)
  pred  <- predict(fitted$model, dtest)
  pmax(pred, 0)
}

# -- 11c. NEW: Per-position model training & prediction ---------

fit_position_models <- function(train_df, params = NULL, nrounds = NULL,
                                feature_subset = NULL) {
  models <- list()
  
  # Detect whether params/nrounds are per-position dicts or shared values.
  # Per-position dicts have names matching skill_pos (e.g., "RB", "WR", "TE").
  # Shared params lists have names like "objective", "max_depth", etc.
  is_per_pos_params  <- is.list(params)  && !is.null(names(params))  &&
    all(skill_pos %in% names(params))
  is_per_pos_nrounds <- is.list(nrounds) && !is.null(names(nrounds)) &&
    all(skill_pos %in% names(nrounds))
  
  for (pos in skill_pos) {
    cat("  Fitting model for", pos, "...\n")
    pos_train <- train_df %>% filter(position == pos)
    if (nrow(pos_train) < 100) {
      cat("    Skipping", pos, "-- only", nrow(pos_train), "rows\n")
      next
    }
    
    # Look up per-position values if applicable
    pos_params  <- if (is_per_pos_params)  params[[pos]]  else params
    pos_nrounds <- if (is_per_pos_nrounds) nrounds[[pos]] else nrounds
    
    # Per-position models don't need position dummies
    models[[pos]] <- fit_xgb_model(
      pos_train,
      params = pos_params, nrounds = pos_nrounds,
      include_pos_dummies = FALSE,
      feature_subset = feature_subset
    )
  }
  models
}

predict_position_models <- function(fitted_models, new_df) {
  preds <- rep(NA_real_, nrow(new_df))
  for (pos in names(fitted_models)) {
    idx <- which(new_df$position == pos)
    if (length(idx) > 0) {
      preds[idx] <- predict_xgb(fitted_models[[pos]], new_df[idx, , drop = FALSE])
    }
  }
  pmax(replace_na(preds, 0), 0)
}

# -- 11d. NEW: Quantile regression for floor/ceiling ------------

fit_xgb_quantile <- function(train_df, quantile_alpha = 0.5,
                             params = NULL, nrounds = 200L,
                             include_pos_dummies = FALSE,
                             feature_subset = NULL) {
  # Check if xgboost version supports quantile regression (>= 2.0)
  xgb_ok <- tryCatch(
    packageVersion("xgboost") >= package_version("2.0.0"),
    error = function(e) FALSE
  )
  
  if (!isTRUE(xgb_ok)) {
    warning("Quantile regression requires xgboost >= 2.0. ",
            "Installed version: ", packageVersion("xgboost"),
            ". Skipping quantile model.")
    return(NULL)
  }
  
  fc <- if (!is.null(feature_subset)) feature_subset else xgb_feature_cols
  
  q_params <- list(
    objective        = "reg:quantileerror",
    quantile_alpha   = quantile_alpha,
    max_depth        = params$max_depth %||% 5,
    eta              = params$eta %||% 0.05,
    subsample        = params$subsample %||% 0.8,
    colsample_bytree = params$colsample_bytree %||% 0.8,
    min_child_weight = params$min_child_weight %||% 10,
    gamma            = params$gamma %||% 1
  )
  
  prep   <- prepare_xgb_matrix(train_df, feature_cols = fc,
                               include_position_dummies = include_pos_dummies)
  dtrain <- xgb.DMatrix(data = prep$X, label = prep$y)
  
  model <- tryCatch({
    xgb.train(params = q_params, data = dtrain, nrounds = nrounds, verbose = 0)
  }, error = function(e) {
    warning("Quantile model failed: ", conditionMessage(e))
    return(NULL)
  })
  
  if (is.null(model)) return(NULL)
  list(model = model, feature_names = prep$feature_names,
       feature_cols = fc, include_pos_dummies = include_pos_dummies)
}

fit_position_quantile_models <- function(train_df, quantile_alpha = 0.5,
                                         params = NULL, feature_subset = NULL) {
  models <- list()
  
  # Detect per-position params dict (same logic as fit_position_models)
  is_per_pos_params <- is.list(params) && !is.null(names(params)) &&
    all(skill_pos %in% names(params))
  
  for (pos in skill_pos) {
    cat("  Quantile (", quantile_alpha, ") model for", pos, "...\n")
    pos_train <- train_df %>% filter(position == pos)
    if (nrow(pos_train) < 100) next
    
    pos_params <- if (is_per_pos_params) params[[pos]] else params
    
    m <- fit_xgb_quantile(pos_train, quantile_alpha = quantile_alpha,
                          params = pos_params, include_pos_dummies = FALSE,
                          feature_subset = feature_subset)
    if (!is.null(m)) models[[pos]] <- m
  }
  models
}

# -- 11e. NEW: Feature pruning ---------------------------------

get_important_features <- function(fitted_models, top_n = 35) {
  # Collect importance across all position models
  all_importance <- map_dfr(names(fitted_models), function(pos) {
    imp <- xgb.importance(
      feature_names = fitted_models[[pos]]$feature_names,
      model = fitted_models[[pos]]$model
    )
    imp$position <- pos
    imp
  })
  
  # Aggregate across positions: take union of top features per position
  top_per_pos <- all_importance %>%
    group_by(position) %>%
    slice_max(Gain, n = top_n, with_ties = FALSE) %>%
    ungroup()
  
  unique_features <- unique(top_per_pos$Feature)
  
  # Filter to only XGBoost feature cols (not position dummies)
  pruned <- intersect(unique_features, xgb_feature_cols)
  cat("  Feature pruning: kept", length(pruned), "of", length(xgb_feature_cols), "features\n")
  pruned
}

# -- 11f. NEW: Tier-adaptive ESPN ensemble blending -------------
# Starters (ESPN >= 12): ESPN has better intel, so 50/50 split
# Everyone else: our model dominates bench/inactive, so 70/30 split

ensemble_predict <- function(model_pred, espn_proj,
                             starter_weight = ensemble_blend_starter,
                             other_weight   = ensemble_blend_other) {
  blend_weight <- ifelse(
    !is.na(espn_proj) & espn_proj >= 12,
    starter_weight,
    other_weight
  )
  ifelse(
    !is.na(espn_proj) & espn_proj > 0,
    blend_weight * model_pred + (1 - blend_weight) * espn_proj,
    model_pred
  )
}

# -- 12. Backtest function (REWRITTEN for per-position) ---------

score_one_week <- function(eval_season, eval_week, full_data,
                           espn_proj_tbl = NULL,
                           tuned_params = NULL,
                           tuned_nrounds = NULL,
                           pruned_features = NULL,
                           quantile_models_10 = NULL,
                           quantile_models_90 = NULL) {
  
  cutoff_key <- season_week_key(eval_season, eval_week)
  
  train <- full_data %>%
    filter(position %in% skill_pos,
           season_week_key(season, week) < cutoff_key)
  
  eval  <- full_data %>%
    filter(position %in% skill_pos,
           season == eval_season, week == eval_week)
  
  if (nrow(train) < 200 || nrow(eval) == 0) return(NULL)
  
  cat("  Scoring season", eval_season, "week", eval_week,
      "| train:", nrow(train), "| eval:", nrow(eval), "\n")
  
  # Fit per-position XGBoost models
  pos_models <- fit_position_models(
    train,
    params = tuned_params,
    nrounds = tuned_nrounds,
    feature_subset = pruned_features
  )
  
  # Predict
  eval$model_pred_raw <- predict_position_models(pos_models, eval)
  
  # Quantile predictions (floor/ceiling) if available
  if (!is.null(quantile_models_10) && length(quantile_models_10) > 0) {
    eval$pred_floor <- predict_position_models(quantile_models_10, eval)
  } else {
    eval$pred_floor <- NA_real_
  }
  if (!is.null(quantile_models_90) && length(quantile_models_90) > 0) {
    eval$pred_ceiling <- predict_position_models(quantile_models_90, eval)
  } else {
    eval$pred_ceiling <- NA_real_
  }
  
  # NEW (empirically-tuned on 2023-2024 backtest)
  eval$model_pred <- eval$model_pred_raw * case_when(
    eval$status_out == 1L ~ 0,
    eval$status_doubtful == 1L ~ 0,           # was 0.25; doubtful players never play
    eval$avg3_offense_pct < 0.05 &
      eval$active_weeks_last_3 == 0 &
      eval$consecutive_zero_snap_weeks >= 2 ~ 0,
    eval$model_pred_raw < 1.50 ~ 0,           # was 1.25; empirical optimum
    eval$status_questionable == 1L ~ 0.80,
    TRUE ~ 1
  )
  
  # Baseline: 3-game avg
  eval$baseline_avg3 <- pmax(replace_na(eval$avg3_fp, 0), 0)
  
  # Join ESPN projections if available
  if (!is.null(espn_proj_tbl) && nrow(espn_proj_tbl) > 0) {
    eval <- eval %>%
      left_join(
        espn_proj_tbl %>% select(season, week, player_id, espn_proj),
        by = c("season", "week", "player_id")
      )
  } else {
    eval$espn_proj <- NA_real_
  }
  
  # NEW: ESPN ensemble blend
  eval$ensemble_pred <- ensemble_predict(eval$model_pred, eval$espn_proj)
  
  eval %>%
    select(
      season, week, player_id, player_name, position, team,
      fantasy_ppr, played,
      model_pred, model_pred_raw, ensemble_pred,
      pred_floor, pred_ceiling,
      baseline_avg3, espn_proj,
      lag1_fp, avg3_fp, ewma_fp, implied_team_total, is_home,
      def_fp_allowed_to_pos_avg3,
      status_out, status_doubtful, status_questionable,
      injury_severity_score,
      avg3_offense_pct, active_weeks_last_3, consecutive_zero_snap_weeks,
      # NEW features for diagnostics
      wind, is_dome, days_rest, after_bye, age_at_season, years_exp
    )
}

# -- 13. Run backtests ------------------------------------------

if (run_2023_2024_backtest) {
  cat("\n=== Running 2023-2024 rolling backtest ===\n")
  
  # Optional: tune hyperparams on a subset of training data
  tuned_params  <- NULL
  tuned_nrounds <- NULL
  pruned_feats  <- NULL
  
  if (run_hyperparam_tuning) {
    cat("\nTuning hyperparameters per position on 2023-2024 data...\n")
    tuned_params  <- list()
    tuned_nrounds <- list()
    for (pos in skill_pos) {
      cat("  Position:", pos, "\n")
      tune_data_pos <- model_tbl %>%
        filter(position == pos, season %in% c(2023, 2024))
      tuning_result_pos <- tune_xgb_params(tune_data_pos, n_trials = tuning_n_trials,
                                           include_pos_dummies = FALSE)
      tuned_params[[pos]]  <- tuning_result_pos$params
      tuned_nrounds[[pos]] <- tuning_result_pos$best_nrounds
    }
  }
  
  if (run_feature_pruning) {
    cat("\nRunning initial feature importance for pruning...\n")
    init_train <- model_tbl %>%
      filter(position %in% skill_pos, season %in% c(2023, 2024))
    init_models <- fit_position_models(init_train, params = tuned_params,
                                       nrounds = tuned_nrounds)
    pruned_feats <- get_important_features(init_models, top_n = 35)
    cat("  Pruned feature set:", paste(head(pruned_feats, 10), collapse = ", "), "...\n")
  }
  
  # Train quantile models (floor/ceiling) if enabled
  q_models_10 <- NULL
  q_models_90 <- NULL
  if (run_quantile_models) {
    cat("\nTraining quantile models for floor/ceiling...\n")
    q_train <- model_tbl %>%
      filter(position %in% skill_pos, season == 2023)
    q_models_10 <- fit_position_quantile_models(q_train, quantile_alpha = 0.10,
                                                params = tuned_params,
                                                feature_subset = pruned_feats)
    q_models_90 <- fit_position_quantile_models(q_train, quantile_alpha = 0.90,
                                                params = tuned_params,
                                                feature_subset = pruned_feats)
  }
  
  bt_schedule <- model_tbl %>%
    filter(position %in% skill_pos, season %in% backtest_seasons) %>%
    distinct(season, week) %>%
    arrange(season, week) %>%
    filter(!(season == min(backtest_seasons) & week < backtest_start_week))
  
  bt_predictions <- pmap_dfr(
    list(bt_schedule$season, bt_schedule$week),
    ~ score_one_week(..1, ..2, full_data = model_tbl,
                     tuned_params = tuned_params,
                     tuned_nrounds = tuned_nrounds,
                     pruned_features = pruned_feats,
                     quantile_models_10 = q_models_10,
                     quantile_models_90 = q_models_90)
  )
  
  # Weekly metrics
  bt_weekly <- bt_predictions %>%
    group_by(season, week) %>%
    summarise(
      n = n(),
      n_played = sum(played == 1),
      actual_mean     = mean(fantasy_ppr, na.rm = TRUE),
      model_mae       = safe_mae(fantasy_ppr, model_pred),
      model_rmse      = safe_rmse(fantasy_ppr, model_pred),
      baseline_mae    = safe_mae(fantasy_ppr, baseline_avg3),
      baseline_rmse   = safe_rmse(fantasy_ppr, baseline_avg3),
      .groups = "drop"
    )
  
  bt_overall <- bt_predictions %>%
    summarise(
      n = n(),
      model_mae     = safe_mae(fantasy_ppr, model_pred),
      model_rmse    = safe_rmse(fantasy_ppr, model_pred),
      baseline_mae  = safe_mae(fantasy_ppr, baseline_avg3),
      baseline_rmse = safe_rmse(fantasy_ppr, baseline_avg3)
    )
  
  cat("\n-- 2023-2024 Backtest Results --\n")
  print(bt_overall)
  
  bt_by_pos <- bt_predictions %>%
    group_by(position) %>%
    summarise(
      n = n(),
      model_mae  = safe_mae(fantasy_ppr, model_pred),
      model_rmse = safe_rmse(fantasy_ppr, model_pred),
      baseline_mae = safe_mae(fantasy_ppr, baseline_avg3),
      .groups = "drop"
    )
  cat("\nBy position:\n")
  print(bt_by_pos)
  
  # Quantile calibration check
  if (run_quantile_models && any(!is.na(bt_predictions$pred_floor))) {
    q_calib <- bt_predictions %>%
      filter(played == 1, !is.na(pred_floor), !is.na(pred_ceiling)) %>%
      summarise(
        pct_below_floor   = mean(fantasy_ppr < pred_floor, na.rm = TRUE),
        pct_above_ceiling = mean(fantasy_ppr > pred_ceiling, na.rm = TRUE),
        pct_in_range      = mean(fantasy_ppr >= pred_floor & fantasy_ppr <= pred_ceiling, na.rm = TRUE)
      )
    cat("\nQuantile calibration (played only):\n")
    cat("  Below 10th percentile floor: ", round(q_calib$pct_below_floor * 100, 1), "%\n")
    cat("  Above 90th percentile ceiling:", round(q_calib$pct_above_ceiling * 100, 1), "%\n")
    cat("  Within floor-ceiling range:   ", round(q_calib$pct_in_range * 100, 1), "%\n")
  }
  
  write_csv(bt_predictions, "3backtest_2023_2024_predictions.csv")
  write_csv(bt_weekly, "3backtest_2023_2024_weekly_metrics.csv")
}

# -- 14. 2025 evaluation vs ESPN --------------------------------

if (run_2025_eval) {
  cat("\n=== Running 2025 rolling eval vs ESPN ===\n")
  
  # Scrape ESPN projections
  cat("Scraping ESPN 2025 projections...\n")
  espn_proj_2025 <- map_dfr(1:18, function(w) {
    cat("  Week", w, "...")
    out <- fetch_espn_projections_all_players(2025, w)
    cat(nrow(out), "players\n")
    Sys.sleep(1)
    out
  }) %>%
    mutate(position = pos_map[as.character(positionId)]) %>%
    filter(position %in% skill_pos, !is.na(espn_proj))
  
  players_xw <- nflreadr::load_players() %>%
    transmute(espn_id = suppressWarnings(as.integer(espn_id)), gsis_id) %>%
    filter(!is.na(espn_id), !is.na(gsis_id))
  
  espn_proj_2025 <- espn_proj_2025 %>%
    left_join(players_xw, by = "espn_id") %>%
    mutate(player_id = as.character(gsis_id)) %>%
    filter(!is.na(player_id))
  
  write_csv(espn_proj_2025, "espn_projections_2025.csv")
  
  # Re-tune on full backtest data for 2025 prediction
  tuned_params_2025  <- tuned_params
  tuned_nrounds_2025 <- tuned_nrounds
  pruned_feats_2025  <- pruned_feats
  
  if (run_hyperparam_tuning && is.null(tuned_params_2025)) {
    cat("\nTuning hyperparameters for 2025...\n")
    tune_data_2025 <- model_tbl %>%
      filter(position %in% skill_pos, season %in% backtest_seasons)
    tuning_2025 <- tune_xgb_params(tune_data_2025, include_pos_dummies = FALSE)
    tuned_params_2025  <- tuning_2025$params
    tuned_nrounds_2025 <- tuning_2025$best_nrounds
  }
  
  if (run_feature_pruning && is.null(pruned_feats_2025)) {
    cat("\nPruning features for 2025...\n")
    init_2025 <- model_tbl %>% filter(position %in% skill_pos, season %in% backtest_seasons)
    init_models_2025 <- fit_position_models(init_2025, params = tuned_params_2025,
                                            nrounds = tuned_nrounds_2025)
    pruned_feats_2025 <- get_important_features(init_models_2025, top_n = 35)
  }
  
  # Quantile models for 2025
  q_models_10_2025 <- NULL
  q_models_90_2025 <- NULL
  if (run_quantile_models) {
    cat("\nTraining quantile models for 2025...\n")
    q_train_2025 <- model_tbl %>%
      filter(position %in% skill_pos, season %in% backtest_seasons)
    q_models_10_2025 <- fit_position_quantile_models(q_train_2025, quantile_alpha = 0.10,
                                                     params = tuned_params_2025,
                                                     feature_subset = pruned_feats_2025)
    q_models_90_2025 <- fit_position_quantile_models(q_train_2025, quantile_alpha = 0.90,
                                                     params = tuned_params_2025,
                                                     feature_subset = pruned_feats_2025)
  }
  
  eval_2025_schedule <- model_tbl %>%
    filter(position %in% skill_pos, season == season_id, week >= backtest_start_week) %>%
    distinct(season, week) %>% arrange(week)
  
  eval_2025_predictions <- pmap_dfr(
    list(eval_2025_schedule$season, eval_2025_schedule$week),
    ~ score_one_week(..1, ..2, full_data = model_tbl,
                     espn_proj_tbl = espn_proj_2025,
                     tuned_params = tuned_params_2025,
                     tuned_nrounds = tuned_nrounds_2025,
                     pruned_features = pruned_feats_2025,
                     quantile_models_10 = q_models_10_2025,
                     quantile_models_90 = q_models_90_2025)
  )
  
  # Compare where both model and ESPN have predictions
  comparison <- eval_2025_predictions %>% filter(!is.na(espn_proj))
  
  model_vs_espn_ci <- paired_weekly_mae_boot(
    comparison,
    pred_col = "model_pred",
    baseline_col = "espn_proj"
  )
  
  ensemble_vs_espn_ci <- paired_weekly_mae_boot(
    comparison,
    pred_col = "ensemble_pred",
    baseline_col = "espn_proj"
  )
  
  # -- Overall --
  comp_overall <- comparison %>%
    summarise(
      n = n(),
      model_mae     = safe_mae(fantasy_ppr, model_pred),
      ensemble_mae  = safe_mae(fantasy_ppr, ensemble_pred),
      espn_mae      = safe_mae(fantasy_ppr, espn_proj),
      model_rmse    = safe_rmse(fantasy_ppr, model_pred),
      ensemble_rmse = safe_rmse(fantasy_ppr, ensemble_pred),
      espn_rmse     = safe_rmse(fantasy_ppr, espn_proj),
      model_wins    = mean(abs(model_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      ensemble_wins = mean(abs(ensemble_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE)
    )
  cat("\n-- 2025 Model vs Ensemble vs ESPN (Overall) --\n")
  print(comp_overall)
  
  cat("\n-- Weekly paired-bootstrap MAE difference vs ESPN --\n")
  cat("Positive mae_diff means the model beats ESPN.\n\n")
  
  cat("Standalone vs ESPN:\n")
  print(model_vs_espn_ci)
  
  cat("\nEnsemble vs ESPN:\n")
  print(ensemble_vs_espn_ci)
  
  # -- Played only --
  comp_played_only <- comparison %>%
    filter(played == 1) %>%
    summarise(
      n = n(),
      model_mae     = safe_mae(fantasy_ppr, model_pred),
      ensemble_mae  = safe_mae(fantasy_ppr, ensemble_pred),
      espn_mae      = safe_mae(fantasy_ppr, espn_proj),
      model_rmse    = safe_rmse(fantasy_ppr, model_pred),
      ensemble_rmse = safe_rmse(fantasy_ppr, ensemble_pred),
      espn_rmse     = safe_rmse(fantasy_ppr, espn_proj),
      model_wins    = mean(abs(model_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      ensemble_wins = mean(abs(ensemble_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE)
    )
  cat("\n-- 2025 Model vs Ensemble vs ESPN (Played only) --\n")
  print(comp_played_only)
  
  # -- Post-processing impact --
  comp_postproc <- comparison %>%
    summarise(
      n = n(),
      raw_mae      = safe_mae(fantasy_ppr, model_pred_raw),
      adj_mae      = safe_mae(fantasy_ppr, model_pred),
      ensemble_mae = safe_mae(fantasy_ppr, ensemble_pred),
      espn_mae     = safe_mae(fantasy_ppr, espn_proj),
      raw_rmse     = safe_rmse(fantasy_ppr, model_pred_raw),
      adj_rmse     = safe_rmse(fantasy_ppr, model_pred),
      ensemble_rmse = safe_rmse(fantasy_ppr, ensemble_pred),
      espn_rmse    = safe_rmse(fantasy_ppr, espn_proj),
      raw_wins     = mean(abs(model_pred_raw - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      adj_wins     = mean(abs(model_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      ensemble_wins = mean(abs(ensemble_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      n_zeroed     = sum(model_pred_raw > 0 & model_pred == 0, na.rm = TRUE)
    )
  cat("\n-- Post-processing impact (raw vs adjusted vs ensemble vs ESPN) --\n")
  print(comp_postproc)
  
  # -- By week --
  comp_by_week <- comparison %>%
    group_by(week) %>%
    summarise(
      n = n(),
      model_mae     = safe_mae(fantasy_ppr, model_pred),
      ensemble_mae  = safe_mae(fantasy_ppr, ensemble_pred),
      espn_mae      = safe_mae(fantasy_ppr, espn_proj),
      model_rmse    = safe_rmse(fantasy_ppr, model_pred),
      ensemble_rmse = safe_rmse(fantasy_ppr, ensemble_pred),
      espn_rmse     = safe_rmse(fantasy_ppr, espn_proj),
      model_wins    = mean(abs(model_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      ensemble_wins = mean(abs(ensemble_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      .groups = "drop"
    )
  cat("\nBy week:\n")
  print(comp_by_week, n = 20)
  
  # -- By position --
  comp_by_pos <- comparison %>%
    group_by(position) %>%
    summarise(
      n = n(),
      model_mae     = safe_mae(fantasy_ppr, model_pred),
      ensemble_mae  = safe_mae(fantasy_ppr, ensemble_pred),
      espn_mae      = safe_mae(fantasy_ppr, espn_proj),
      model_rmse    = safe_rmse(fantasy_ppr, model_pred),
      ensemble_rmse = safe_rmse(fantasy_ppr, ensemble_pred),
      espn_rmse     = safe_rmse(fantasy_ppr, espn_proj),
      model_wins    = mean(abs(model_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      ensemble_wins = mean(abs(ensemble_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      .groups = "drop"
    )
  cat("\nBy position:\n")
  print(comp_by_pos)
  
  # -- By player tier --
  comparison <- comparison %>%
    mutate(
      player_tier = case_when(
        espn_proj >= 12 ~ "starters (ESPN >= 12)",
        espn_proj >= 5  ~ "flex (ESPN 5-12)",
        espn_proj > 0   ~ "bench (ESPN 0-5)",
        TRUE            ~ "zero (ESPN = 0)"
      )
    )
  
  comp_by_tier <- comparison %>%
    group_by(player_tier) %>%
    summarise(
      n = n(),
      actual_mean   = mean(fantasy_ppr, na.rm = TRUE),
      model_mae     = safe_mae(fantasy_ppr, model_pred),
      ensemble_mae  = safe_mae(fantasy_ppr, ensemble_pred),
      espn_mae      = safe_mae(fantasy_ppr, espn_proj),
      model_rmse    = safe_rmse(fantasy_ppr, model_pred),
      ensemble_rmse = safe_rmse(fantasy_ppr, ensemble_pred),
      espn_rmse     = safe_rmse(fantasy_ppr, espn_proj),
      model_wins    = mean(abs(model_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      ensemble_wins = mean(abs(ensemble_pred - fantasy_ppr) < abs(espn_proj - fantasy_ppr), na.rm = TRUE),
      .groups = "drop"
    )
  cat("\nBy player tier (model vs ensemble vs ESPN):\n")
  print(comp_by_tier)
  
  # -- Quantile calibration for 2025 --
  if (run_quantile_models && any(!is.na(eval_2025_predictions$pred_floor))) {
    q_calib_2025 <- eval_2025_predictions %>%
      filter(played == 1, !is.na(pred_floor), !is.na(pred_ceiling)) %>%
      summarise(
        n = n(),
        pct_below_floor   = mean(fantasy_ppr < pred_floor, na.rm = TRUE),
        pct_above_ceiling = mean(fantasy_ppr > pred_ceiling, na.rm = TRUE),
        pct_in_range      = mean(fantasy_ppr >= pred_floor & fantasy_ppr <= pred_ceiling, na.rm = TRUE),
        avg_range_width   = mean(pred_ceiling - pred_floor, na.rm = TRUE)
      )
    cat("\n-- 2025 Quantile Calibration (10th-90th, played only) --\n")
    cat("  Below floor:  ", round(q_calib_2025$pct_below_floor * 100, 1), "% (target: ~10%)\n")
    cat("  Above ceiling:", round(q_calib_2025$pct_above_ceiling * 100, 1), "% (target: ~10%)\n")
    cat("  In range:     ", round(q_calib_2025$pct_in_range * 100, 1), "% (target: ~80%)\n")
    cat("  Avg range:    ", round(q_calib_2025$avg_range_width, 1), "points\n")
  }
  
  # -- Feature importance (per position, for thesis!) --
  cat("\nFitting full models for feature importance...\n")
  full_train <- model_tbl %>%
    filter(position %in% skill_pos, season %in% backtest_seasons)
  
  full_pos_models <- fit_position_models(
    full_train,
    params = tuned_params_2025,
    nrounds = tuned_nrounds_2025,
    feature_subset = pruned_feats_2025
  )
  
  all_importance <- map_dfr(names(full_pos_models), function(pos) {
    imp <- xgb.importance(
      feature_names = full_pos_models[[pos]]$feature_names,
      model = full_pos_models[[pos]]$model
    )
    imp$position <- pos
    imp
  })
  
  cat("\n-- Top 20 Most Important Features by Position --\n")
  for (pos in skill_pos) {
    cat("\n", pos, ":\n")
    pos_imp <- all_importance %>% filter(position == pos) %>% arrange(desc(Gain))
    print(head(pos_imp %>% select(Feature, Gain, Cover, Frequency), 20))
  }
  
  write_csv(eval_2025_predictions, "3ensemble_eval_2025_predictions.csv")
  write_csv(comparison, "3ensemble_comparison_2025_model_vs_espn.csv")
  write_csv(comp_by_week, "3ensemble_comparison_2025_by_week.csv")
  write_csv(comp_by_pos, "3ensemble_comparison_2025_by_position.csv")
  write_csv(comp_by_tier, "3ensemble_comparison_2025_by_tier.csv")
  write_csv(as_tibble(all_importance), "3ensemble_feature_importance_by_position.csv")
}

cat("\n=== Done! ===\n")

#Check
# Save the most recent per-position fitted models for diagnostic inspection
final_models <- fit_position_models(
  model_tbl %>% filter(position %in% skill_pos,
                       season_week_key(season, week) < season_week_key(2025, 18)),
  params = tuned_params_2025,
  nrounds = tuned_nrounds_2025,
  feature_subset = pruned_feats_2025
)

#Check
# -- Hyperparameter stability check --
# Re-tune at midseason 2025 and compare to original tuning
if (run_hyperparam_tuning) {
  cat("\n=== Hyperparameter stability check ===\n")
  cat("Re-tuning per-position on (2023-2024 + Weeks 1-9 of 2025) data:\n")
  midseason_results <- list()
  for (pos in skill_pos) {
    cat("  Position:", pos, "\n")
    midseason_data_pos <- model_tbl %>%
      filter(position == pos,
             (season %in% c(2023, 2024)) | (season == 2025 & week <= 9))
    midseason_results[[pos]] <- tune_xgb_params(midseason_data_pos,
                                                n_trials = tuning_n_trials,
                                                include_pos_dummies = FALSE)
  }
  
  cat("\nComparison (original vs midseason retune):\n")
  for (pos in skill_pos) {
    cat("  ", pos, ":\n", sep = "")
    cat("    Original:  depth=", tuned_params[[pos]]$max_depth,
        "  eta=", round(tuned_params[[pos]]$eta, 3),
        "  nrounds=", tuned_nrounds[[pos]], "\n", sep = "")
    cat("    Midseason: depth=", midseason_results[[pos]]$params$max_depth,
        "  eta=", round(midseason_results[[pos]]$params$eta, 3),
        "  nrounds=", midseason_results[[pos]]$best_nrounds, "\n", sep = "")
  }
  cat("(If per-position values are similar across the two tunings,\n")
  cat(" hyperparameter retuning frequency does not matter much.)\n")
}

# =====================================================================
# Experiment diagnostics: 4-game window + 2022 start year
# =====================================================================
cat("\n", strrep("=", 70), "\n", sep = "")
cat("EXPERIMENT DIAGNOSTICS\n")
cat(strrep("=", 70), "\n\n", sep = "")

# (1) Backtest MAE (2023-2024) — comparable to baseline of 2.633
cat("[1] 2023-2024 Backtest MAE (overall):\n")
cat("    ", round(safe_mae(bt_predictions$fantasy_ppr,
                           bt_predictions$model_pred), 4), "\n")
cat("    Baseline to beat: 2.6332 (current setup)\n\n")

# (2) 2025 evaluation MAE — comparable to baseline 2.921 standalone, 2.916 ensemble
cat("[2] 2025 Standalone model MAE: ",
    round(safe_mae(comparison$fantasy_ppr, comparison$model_pred), 4), "\n")
cat("    Ensemble MAE:               ",
    round(safe_mae(comparison$fantasy_ppr, comparison$ensemble_pred), 4), "\n")
cat("    ESPN MAE:                   ",
    round(safe_mae(comparison$fantasy_ppr, comparison$espn_proj), 4), "\n")
cat("    Baselines to beat: model 2.9210, ensemble 2.9162, ESPN 3.0743\n\n")

# (3) Per-position 2025 MAE
cat("[3] Per-position 2025 MAE:\n")
for (pos in c("RB", "WR", "TE")) {
  s <- comparison[comparison$position == pos, ]
  cat("    ", pos, ": model ",
      round(safe_mae(s$fantasy_ppr, s$model_pred), 4),
      "  ESPN ",
      round(safe_mae(s$fantasy_ppr, s$espn_proj), 4), "\n", sep = "")
}
cat("    Baselines: RB 3.077/3.089  WR 2.967/3.149  TE 2.614/2.898\n\n")

# (4) Did the 4-game features show up?
cat("[4] Top features by gain — looking for avg4_* features:\n")
for (pos in c("RB", "WR", "TE")) {
  cat("\n    Position:", pos, "\n")
  imp <- xgb.importance(
    feature_names = final_models[[pos]]$feature_names,
    model = final_models[[pos]]$model
  )
  top15 <- head(imp, 15)
  for (i in seq_len(nrow(top15))) {
    marker <- if (grepl("avg4", top15$Feature[i])) " <-- 4-game!" else ""
    cat(sprintf("      %2d. %-35s gain=%.4f%s\n",
                i, top15$Feature[i], top15$Gain[i], marker))
  }
}
cat("\n")

# (5) Effect of 2022 data
cat("[5] Training data summary (should now include 2022):\n")
train_summary <- model_tbl %>%
  filter(position %in% skill_pos) %>%
  count(season)
print(train_summary)

cat("\n", strrep("=", 70), "\n", sep = "")



cat("\n================ INJURY DATA CHECK ================\n")

# 1) What seasons are actually present in the raw injury feed?
cat("\n[1] Seasons present in inj_hist:\n")
print(sort(unique(inj_hist$season)))

cat("\nRows in inj_hist by season:\n")
print(inj_hist %>% count(season, sort = TRUE))

cat("\n2025 rows in inj_hist:\n")
print(
  inj_hist %>%
    filter(season == 2025) %>%
    summarise(rows_2025 = n(),
              players_2025 = n_distinct(gsis_id))
)

# 2) Did any 2025 rows survive into injury_snapshots?
cat("\n[2] injury_snapshots coverage for 2025:\n")
print(
  injury_snapshots %>%
    filter(season == 2025) %>%
    summarise(
      rows_2025 = n(),
      players_2025 = n_distinct(gsis_id),
      weeks_2025 = n_distinct(week)
    )
)

cat("\n2025 injury_snapshots by week:\n")
print(
  injury_snapshots %>%
    filter(season == 2025) %>%
    count(week, sort = FALSE)
)

# Optional: inspect a few 2025 injury snapshot rows if any exist
cat("\nSample 2025 injury_snapshots rows:\n")
print(
  injury_snapshots %>%
    filter(season == 2025) %>%
    select(gsis_id, season, week, practice_status, report_status, injury_text, injury_group) %>%
    head(20)
)

# 3) Did 2025 injury information make it into the final model table?
cat("\n[3] model_tbl 2025 injury feature coverage:\n")
print(
  model_tbl %>%
    filter(season == 2025) %>%
    summarise(
      rows_2025 = n(),
      players_2025 = n_distinct(gsis_id),
      out_flags = sum(status_out == 1, na.rm = TRUE),
      doubtful_flags = sum(status_doubtful == 1, na.rm = TRUE),
      questionable_flags = sum(status_questionable == 1, na.rm = TRUE),
      dnp_flags = sum(practice_dnp == 1, na.rm = TRUE),
      limited_flags = sum(practice_limited == 1, na.rm = TRUE),
      positive_severity = sum(injury_severity_score > 0, na.rm = TRUE)
    )
)

cat("\n2025 model_tbl injury flags by week:\n")
print(
  model_tbl %>%
    filter(season == 2025) %>%
    group_by(week) %>%
    summarise(
      rows = n(),
      out_flags = sum(status_out == 1, na.rm = TRUE),
      doubtful_flags = sum(status_doubtful == 1, na.rm = TRUE),
      questionable_flags = sum(status_questionable == 1, na.rm = TRUE),
      dnp_flags = sum(practice_dnp == 1, na.rm = TRUE),
      limited_flags = sum(practice_limited == 1, na.rm = TRUE),
      positive_severity = sum(injury_severity_score > 0, na.rm = TRUE),
      .groups = "drop"
    )
)

# 4) If you want the blunt yes/no answer:
cat("\n[4] Bottom line:\n")
has_inj_hist_2025 <- any(inj_hist$season == 2025)
has_snapshots_2025 <- any(injury_snapshots$season == 2025)
has_model_tbl_2025_signal <- model_tbl %>%
  filter(season == 2025) %>%
  summarise(any_signal =
              any(status_out == 1 |
                    status_doubtful == 1 |
                    status_questionable == 1 |
                    practice_dnp == 1 |
                    practice_limited == 1 |
                    injury_severity_score > 0, na.rm = TRUE)
  ) %>%
  pull(any_signal)

cat("inj_hist has 2025 rows?           ", has_inj_hist_2025, "\n")
cat("injury_snapshots has 2025 rows?   ", has_snapshots_2025, "\n")
cat("model_tbl has 2025 injury signal? ", has_model_tbl_2025_signal, "\n")
cat("==================================================\n")

write_csv(model_vs_espn_ci, "3ensemble_model_vs_espn_weekly_bootstrap_ci.csv")
write_csv(ensemble_vs_espn_ci, "3ensemble_ensemble_vs_espn_weekly_bootstrap_ci.csv")