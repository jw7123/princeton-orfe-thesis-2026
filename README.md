# NFL Fantasy Football Projection: An XGBoost-Based Approach

This repository contains the code accompanying the senior thesis
**"[THESIS TITLE]"** by Jim [LAST NAME], submitted to the Department of
Operations Research and Financial Engineering at Princeton University in
[MONTH] 2026, advised by Professor Alain Kornhauser.

## Overview

The project builds per-position XGBoost models (RB, WR, TE) that predict
weekly point-per-reception (PPR) fantasy football scores, and benchmarks
them against ESPN's commercial weekly projections. The headline result on
the held-out 2025 NFL regular season is that the standalone XGBoost model
achieves a mean absolute error of **2.909** versus ESPN's **3.074** (a
5.4% reduction), and that a tier-adaptive ensemble of the model and ESPN
achieves the best MAE on played-only player-weeks. Vegas-derived
features — implied team total and game total line — are the top
predictors across all three positions.

The full methodology and results are documented in the thesis itself.
This README documents how to reproduce the headline numbers from a clean
checkout.

## Repository contents

```
.
├── README.md                  This file
├── model_espn_ensemble.R      Main model script. Builds the feature
│                              panel from nflreadr/nflfastR and ESPN,
│                              tunes per-position XGBoost models,
│                              produces both the standalone and the
│                              tier-adaptive ensemble forecasts, and
│                              writes all evaluation CSVs used in the
│                              thesis. ~8–9 minute run time.
├── model_played_only.R        Played-only ablation. Architecturally
│                              identical to the main script, but trains
│                              only on player-weeks with at least one
│                              offensive snap. Produces the ablation
│                              comparison CSVs. Standalone; safe to run
│                              independently.
├── make_figures.R             Generates the six figures used in the
│                              thesis from the CSV outputs of the two
│                              model scripts.
└── .gitignore                 Excludes generated CSVs, R session files,
                               and editor caches.
```

## Requirements

- **R** version 4.3 or later
- The following R packages, all available from CRAN:
  - `xgboost` — gradient-boosted regression trees
  - `nflreadr` — schedule, snap counts, weekly stats from nflverse
  - `nflfastR` — play-by-play data
  - `dplyr`, `tidyr`, `purrr`, `stringr`, `readr`, `lubridate` — tidyverse data manipulation
  - `slider` — rolling window helpers used in feature construction
  - `httr`, `jsonlite`, `rvest`, `xml2` — used by the ESPN projections fetcher

To install all dependencies in one step from an R console:

```r
install.packages(c(
  "xgboost", "nflreadr", "nflfastR",
  "dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate",
  "slider", "httr", "jsonlite", "rvest", "xml2"
))
```

No additional data files need to be downloaded manually. All data
(historical player stats, snap counts, schedules, Vegas lines, and
contemporaneous ESPN projections) is fetched at runtime by the scripts
themselves.

## How to reproduce the headline results

1. Clone or download this repository.
2. Open `model_espn_ensemble.R` in RStudio (or any R environment) and
   set the working directory to the repository root.
3. Source the script. The run takes approximately 8–9 minutes on a
   modern laptop, the bulk of which is spent on the per-position
   hyperparameter random search and the rolling backtest over the 2023
   and 2024 seasons.
4. The script writes the following CSV files to the working directory:
   - `3backtest_2023_2024_predictions.csv` — per player-week predictions for the 2023–2024 rolling backtest
   - `3backtest_2023_2024_weekly_metrics.csv` — weekly aggregate metrics for the rolling backtest
   - `espn_projections_2025.csv` — fetched ESPN weekly projections for 2025
   - `3ensemble_eval_2025_predictions.csv` — per player-week predictions on the 2025 forward evaluation
   - `3ensemble_comparison_2025_model_vs_espn.csv` — headline 2025 comparison numbers
   - `3ensemble_comparison_2025_by_week.csv` — 2025 comparison broken down by week
   - `3ensemble_comparison_2025_by_position.csv` — 2025 comparison broken down by position
   - `3ensemble_comparison_2025_by_tier.csv` — 2025 comparison broken down by ESPN-projection tier
   - `3ensemble_feature_importance_by_position.csv` — XGBoost gain-based feature importance for each per-position model

5. (Optional, for the played-only ablation in Section 6.4 of the thesis)
   Source `model_played_only.R`. This produces a parallel set of CSVs
   prefixed without the leading `3`. The ablation script is independent
   of the main script and can be run in either order.

6. (Optional) Source `make_figures.R` to regenerate the six figures used
   in the thesis from the CSV outputs above.

## A note on run-to-run variation

The hyperparameter random search uses R's pseudo-random number generator
without a fixed seed, so reported MAE values may vary by a few
thousandths between runs. The qualitative results (standalone beats
ESPN; ensemble beats both on played-only; Vegas features rank first) are
stable across runs.

## Data sources

- **Player statistics, schedules, snap counts:** the
  [nflverse](https://github.com/nflverse) project, accessed via the
  `nflreadr` and `nflfastR` R packages. nflverse aggregates official NFL
  Game Statistics and Information System (GSIS) data and is widely used
  in public NFL analytics.
- **Vegas lines:** point spreads and game totals are pulled from
  `nflreadr`'s schedule tables, which mirror closing lines from major
  sportsbooks.
- **ESPN weekly projections:** scraped at runtime from ESPN's public
  fantasy football endpoints by the helper functions at the top of
  `model_espn_ensemble.R`.

## Citation

If you use or extend this code, please cite the thesis:

> Jim [LAST NAME]. *[Thesis title]*. Senior thesis, Department of
> Operations Research and Financial Engineering, Princeton University,
> 2026.

## License

This code is released for academic use. See `LICENSE` (or contact the
author) for details.

## Contact

Questions and bug reports can be directed to the author at
[your Princeton email].
