# NFL Draft Prospect Model

Projecting the 2026 NFL Draft class at QB, RB, and WR using historical draft
outcomes (2010-2024), college production, and college team strength (SRS /
OSRS). For each prospect, the models estimate **where they'll be picked**
and **how likely they are to go in round 1**.

## Top projections (linear model)

| Position | Prospect | Projected pick | Round-1 probability |
|---|---|---|---|
| QB | Fernando Mendoza (Indiana) | 16.2 | 80% |
| RB | Jeremiyah Love (Notre Dame) | 65.6 | 59% |
| WR | Makai Lemon (USC) | 93.8 | 20% |

Full results for all 14 prospects per position are in
`output/results/*_Final_Draft_Results.csv`.

## Repo structure

```
data/
  train_test/      Historical (2010-2024) and 2026 prospect data per position
  team_strength/    College football team ratings (SRS/OSRS), 2009-2025 seasons
R/
  utils/            Shared team-strength feature engineering
  qb/               QB models and visuals
  rb/               RB models and visuals
  wr/               WR models and visuals
  archive/          Earlier model iterations, kept for reference
output/
  results/          Model predictions (CSV)
  plots/            Saved chart outputs
  logos/            College logos used in the rankings charts
docs/
  METHODOLOGY.md    Feature engineering and modeling approach
  DATA_DICTIONARY.md  Column definitions for all data files
  presentation.pdf  Slide deck with final results and visuals
```

## Running the models

Requires R with the following packages: `tidyverse`, `randomForest`,
`xgboost`, `ggforce`, `ggrepel`, `ggtext`, `ggpath`.

All scripts assume the working directory is the project root (open
`nfl-draft-model.Rproj` in RStudio, or run with `Rscript` from this
directory).

```bash
# Main models (write output/results/<POS>_Final_Draft_Results.csv)
Rscript R/qb/qb_model.R
Rscript R/rb/rb_model.R
Rscript R/wr/wr_model.R

# Historical-comp (KNN) models
Rscript R/qb/qb_knn_comparison.R
Rscript R/rb/rb_knn_comparison.R
Rscript R/wr/wr_knn_comparison.R

# WR-only XGBoost second opinion
Rscript R/wr/wr_xgboost_comparison.R

# Round-1 base-rate checks
Rscript R/rb/rb_round1_analysis.R
Rscript R/wr/wr_round1_analysis.R

# Visuals (run the corresponding *_model.R first)
Rscript R/qb/qb_rankings_plot.R
Rscript R/qb/qb_scatter_plot.R
Rscript R/qb/qb_voronoi_map.R
Rscript R/rb/rb_rankings_plot.R
Rscript R/rb/rb_scatter_plot.R
Rscript R/wr/wr_rankings_plot.R
Rscript R/wr/wr_scatter_plot.R
```

## Approach

See [docs/METHODOLOGY.md](docs/METHODOLOGY.md) for details on the feature
engineering (per-game stats, team-strength integration) and the models used
for each position, and [docs/DATA_DICTIONARY.md](docs/DATA_DICTIONARY.md)
for column definitions. The original [presentation](docs/presentation.pdf)
has the final headline results and visuals.

## Known issues

`qb_voronoi_map.R` prints a non-fatal `ggforce`/`deldir` compatibility
warning when run via `Rscript`. See [docs/METHODOLOGY.md](docs/METHODOLOGY.md#known-issues).
