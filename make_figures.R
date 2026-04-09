# make_figures.R — generates all 6 thesis figures
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(tidytext)

dir.create("Figures", showWarnings = FALSE)

# Shared theme — clean, print-friendly, serif to match LaTeX body
thesis_theme <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey30", size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Consistent color mapping across figures
model_colors <- c("Standalone" = "#1f77b4", "Ensemble" = "#2ca02c", "ESPN" = "#d62728")

# ============================================================
# FIGURE 1 — Weekly MAE time series (2025)
# ============================================================
wk <- read.csv("3ensemble_comparison_2025_by_week.csv")
wk_long <- wk %>%
  select(week, Standalone = model_mae, Ensemble = ensemble_mae, ESPN = espn_mae) %>%
  pivot_longer(-week, names_to = "Model", values_to = "MAE")

p1 <- ggplot(wk_long, aes(x = week, y = MAE, color = Model, linetype = Model)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = model_colors) +
  scale_x_continuous(breaks = 1:18) +
  labs(x = "Week (2025 season)", y = "Mean Absolute Error (PPR points)",
       title = "Weekly projection error, 2025 season",
       subtitle = "Lower is better; all skill-position players with an ESPN projection") +
  thesis_theme
ggsave("Figures/fig1_weekly_mae.png", p1, width = 7, height = 4, dpi = 300)

# ============================================================
# FIGURE 2 — Top-10 feature importance by position
# ============================================================
fi <- read.csv("3ensemble_feature_importance_by_position.csv")
fi_top <- fi %>%
  group_by(position) %>%
  slice_max(Gain, n = 10) %>%
  ungroup() %>%
  mutate(position = factor(position, levels = c("RB", "WR", "TE")))

p2 <- ggplot(fi_top, aes(x = Gain, y = reorder_within(Feature, Gain, position))) +
  geom_col(fill = "#1f77b4", alpha = 0.85) +
  facet_wrap(~ position, scales = "free_y") +
  tidytext::scale_y_reordered() +
  labs(x = "Gain (fraction of total)", y = NULL,
       title = "Top-10 features by gain, per position",
       subtitle = "Ensemble run; Vegas implied totals dominate across all positions") +
  thesis_theme +
  theme(panel.grid.major.y = element_blank())
ggsave("Figures/fig2_feature_importance.png", p2, width = 8, height = 4.5, dpi = 300)

# ============================================================
# FIGURE 3 — Predicted vs actual (played subset, 2025)
# ============================================================
ev <- read.csv("3ensemble_comparison_2025_model_vs_espn.csv")
ev_played <- ev %>% filter(played == 1)
ev_scatter <- ev_played %>%
  select(position, fantasy_ppr, Standalone = model_pred, ESPN = espn_proj) %>%
  pivot_longer(c(Standalone, ESPN), names_to = "Model", values_to = "Prediction")

p3 <- ggplot(ev_scatter, aes(x = Prediction, y = fantasy_ppr)) +
  geom_point(alpha = 0.15, size = 0.7, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  facet_grid(position ~ Model) +
  coord_cartesian(xlim = c(0, 35), ylim = c(0, 50)) +
  labs(x = "Projected points", y = "Actual PPR points",
       title = "Predicted vs. actual fantasy points, 2025 (played only)",
       subtitle = "Dashed red = perfect calibration; solid black = OLS fit") +
  thesis_theme
ggsave("Figures/fig3_pred_vs_actual.png", p3, width = 7, height = 6.5, dpi = 300)

# ============================================================
# FIGURE 4 — Quantile calibration (80% interval coverage by bin)
# ============================================================
calib <- ev_played %>%
  filter(!is.na(pred_floor), !is.na(pred_ceiling), !is.na(model_pred)) %>%
  mutate(bin = cut(model_pred, breaks = c(-Inf, 2, 5, 8, 12, 16, 20, Inf),
                   labels = c("0–2","2–5","5–8","8–12","12–16","16–20","20+"))) %>%
  group_by(bin) %>%
  summarise(
    n = n(),
    below = mean(fantasy_ppr < pred_floor),
    above = mean(fantasy_ppr > pred_ceiling),
    inside = mean(fantasy_ppr >= pred_floor & fantasy_ppr <= pred_ceiling),
    .groups = "drop"
  ) %>%
  pivot_longer(c(below, inside, above), names_to = "region", values_to = "frac") %>%
  mutate(region = factor(region, levels = c("above","inside","below"),
                         labels = c("Above 90th","Inside 80% interval","Below 10th")))

p4 <- ggplot(calib, aes(x = bin, y = frac, fill = region)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = c(0.1, 0.9), linetype = "dashed", color = "grey20") +
  scale_fill_manual(values = c("Below 10th" = "#d62728",
                               "Inside 80% interval" = "#2ca02c",
                               "Above 90th" = "#ff7f0e")) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Predicted points (bin)", y = "Fraction of observations",
       title = "Quantile calibration of 80% prediction intervals",
       subtitle = "Target: 10% below, 80% inside, 10% above. Dashed lines mark ideal coverage.") +
  thesis_theme
ggsave("Figures/fig4_quantile_calibration.png", p4, width = 7, height = 4.2, dpi = 300)

# ============================================================
# FIGURE 5 — Week 18 case study: top-30 ranking comparison
# ============================================================
wk18 <- ev %>% filter(week == 18) %>%
  arrange(desc(fantasy_ppr)) %>%
  mutate(actual_rank = row_number()) %>%
  filter(actual_rank <= 30) %>%
  mutate(
    model_rank = rank(-model_pred, ties.method = "min"),
    espn_rank  = rank(-espn_proj,  ties.method = "min"),
    label = paste0(player_name, " (", position, ")")
  )

wk18_long <- wk18 %>%
  select(label, actual_rank, Standalone = model_pred, ESPN = espn_proj, fantasy_ppr) %>%
  pivot_longer(c(Standalone, ESPN), names_to = "Model", values_to = "Prediction")

p5 <- ggplot(wk18_long, aes(x = Prediction, y = reorder(label, -actual_rank))) +
  geom_point(aes(color = Model, shape = Model), size = 2.5) +
  geom_point(aes(x = fantasy_ppr), color = "black", shape = 4, size = 2.5, stroke = 1) +
  scale_color_manual(values = c("Standalone" = "#1f77b4", "ESPN" = "#d62728")) +
  labs(x = "Points (projected = colored, actual = ×)", y = NULL,
       title = "Week 18, 2025: top-30 actual scorers",
       subtitle = "Standalone MAE 2.545 vs. ESPN 3.088 — the largest weekly margin") +
  thesis_theme
ggsave("Figures/fig5_week18_case_study.png", p5, width = 7.5, height = 7.5, dpi = 300)

# ============================================================
# FIGURE 6 — Tier-level MAE with error bars (bootstrap 95% CI)
# ============================================================
set.seed(42)

boot_ci <- function(err, B = 1000) {
  err <- err[!is.na(err)]
  means <- replicate(B, mean(sample(err, size = length(err), replace = TRUE)))
  qs <- quantile(means, c(0.025, 0.975))
  c(lo = unname(qs[1]), hi = unname(qs[2]))
}

tier_levels <- c(
  "starter (ESPN 12+)",
  "flex (ESPN 5-12)",
  "bench (ESPN 0-5)"
)

tier_boot <- ev %>%
  mutate(
    tier = case_when(
      !is.na(espn_proj) & espn_proj >= 12 ~ "starter (ESPN 12+)",
      !is.na(espn_proj) & espn_proj >= 5  ~ "flex (ESPN 5-12)",
      !is.na(espn_proj) & espn_proj >= 0  ~ "bench (ESPN 0-5)",
      TRUE ~ NA_character_
    ),
    tier = factor(tier, levels = tier_levels)
  ) %>%
  filter(!is.na(tier)) %>%
  select(tier, fantasy_ppr, model_pred, ensemble_pred, espn_proj)

tier_summary <- tier_boot %>%
  group_by(tier) %>%
  group_modify(~{
    standalone_err <- abs(.x$fantasy_ppr - .x$model_pred)
    ensemble_err   <- abs(.x$fantasy_ppr - .x$ensemble_pred)
    espn_err       <- abs(.x$fantasy_ppr - .x$espn_proj)
    
    standalone_ci <- boot_ci(standalone_err)
    ensemble_ci   <- boot_ci(ensemble_err)
    espn_ci       <- boot_ci(espn_err)
    
    tibble(
      Model = c("Standalone", "Ensemble", "ESPN"),
      mae   = c(
        mean(standalone_err, na.rm = TRUE),
        mean(ensemble_err, na.rm = TRUE),
        mean(espn_err, na.rm = TRUE)
      ),
      lo    = c(standalone_ci["lo"], ensemble_ci["lo"], espn_ci["lo"]),
      hi    = c(standalone_ci["hi"], ensemble_ci["hi"], espn_ci["hi"])
    )
  }) %>%
  ungroup() %>%
  mutate(
    Model = factor(Model, levels = c("Standalone", "Ensemble", "ESPN")),
    tier = factor(tier, levels = tier_levels)
  )

p6 <- ggplot(tier_summary, aes(x = tier, y = mae, color = Model)) +
  geom_point(position = position_dodge(width = 0.55), size = 3) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    position = position_dodge(width = 0.55),
    width = 0.2,
    linewidth = 0.7
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_color_manual(values = model_colors) +
  labs(
    x = "Player tier (by ESPN projection)",
    y = "MAE (PPR points)",
    title = "2025 MAE by player tier, with 95% bootstrap intervals",
    subtitle = "Model gains over ESPN concentrate in different tiers"
  ) +
  thesis_theme

ggsave("Figures/fig6_tier_error_bars.png", p6, width = 7.5, height = 4.5, dpi = 300)

cat("Done. 6 PNGs written to Figures/\n")
