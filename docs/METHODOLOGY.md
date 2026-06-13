# Methodology

This project projects the 2026 NFL Draft class at QB, RB, and WR using
historical draft outcomes (2010-2024) and each prospect's final college
season stats. For each position we build several models that all answer two
questions:

1. **Where will this player be picked?** (`pred_pick_*`, regression — lower
   is earlier)
2. **How likely is this player to go in round 1 (picks 1-32)?**
   (`prob_1st_round` / `prob_1st_round_knn`)

## Feature engineering

### 1. Per-game rate stats

The raw training data has season totals (e.g. `passing_yds`, `rushing_td`,
`receiving_rec`), which aren't directly comparable across players who played
different numbers of games. Each model converts the relevant counting stats
to **per-game rates** by dividing by games played, e.g.:

```r
passing_yds_pg = passing_yds / (regular_season_games + postseason_games)
```

> **Note on inconsistency:** the main models (`R/qb/qb_model.R`,
> `R/rb/rb_model.R`, `R/wr/wr_model.R`) divide by
> `regular_season_games + postseason_games`, while the KNN "historical comp"
> scripts (`*_knn_comparison.R`) divide by `regular_season_games` only. This
> matches the original analysis and is preserved rather than silently
> normalized — see each script's header comment for which denominator it
> uses.

### 2. College team strength (SRS / OSRS)

A prospect's raw stat line doesn't capture the quality of competition they
faced. To account for this, each player's final college season is enhanced
with their team's **Simple Rating System (SRS)** and **Offensive SRS
(OSRS)** from Sports-Reference, via `R/utils/team_strength.R`:

1. Look up `data/team_strength/R<season_before_draft>.csv` for the season the
   player's stats are drawn from.
2. These files have a two-row header (Sports-Reference groups columns into
   Overall/Offense/Defense sections), so they're read with `skip = 1`, and
   the `SRS` column is disambiguated from `SRS.1`/`SRS.2` (OSRS/DSRS repeats).
3. Join the player's `college_team` to the team ratings on `School` +
   `season_before_draft`.
4. **Fallback for unmatched schools:** if a player's school doesn't appear in
   that year's ratings (small-school transfers, FCS opponents, name
   mismatches), they're assigned the **10th-percentile SRS/OSRS** for that
   year — a "below replacement level" penalty — instead of `NA`, so they
   aren't dropped from the model.

This same `enhance_with_team_stats()` function is shared by all three
position models (`R/utils/team_strength.R`).

### 3. Round-1 label

For the classification-style outputs (`prob_1st_round`), a player is labeled
a "1st rounder" if `overall < 33` (picks 1-32).

## Models per position

Each position follows the same core pattern in its `*_model.R` script:

| Model | Type | Output column |
|---|---|---|
| Linear regression | `lm()` on `overall` | `pred_pick_lm` |
| Random forest | `randomForest()` regression on `overall` | `pred_pick_rf` |
| Logistic regression | `glm(..., family = binomial)` on the round-1 label | `prob_1st_round` |

All three are trained on the same feature set and run against the 2026 test
set in one pass (`run_qb_model()`, `run_rb_model()`, `run_wr_model()`).

### QB (`R/qb/`)

- **Features:** completion %, passing yards/TDs/INTs per game, height,
  weight, completions per game, yards per attempt, rushing yards per game,
  regular season wins, SRS, OSRS.
- **qb_knn_comparison.R** adds a "historical comp" approach: z-score a
  5-stat profile (pass yards/TDs per game, yards/attempt, SRS, OSRS) and
  find the nearest historical QBs by Euclidean distance.
  - `pred_pick_knn`: average overall pick of the 5 nearest comps.
  - `prob_1st_round_knn`: round-1 rate among the 21 nearest comps.
- **qb_voronoi_map.R**: a Voronoi "territory map" of the QB class on
  passing yards/game vs. passing TDs/game (see Known Issues below).

### RB (`R/rb/`)

- **Features:** height, weight, combine attendance, rushing yards/TDs per
  game, yards per carry, receiving yards per game, regular season wins, SRS,
  OSRS.
- **rb_knn_comparison.R**: a 6-stat comp profile (rushing yards/TDs per game,
  yards per carry, receiving yards per game, SRS, OSRS) — `pred_pick_knn` is
  the average overall pick of the 5 nearest comps.
- **rb_round1_analysis.R**: historical base-rate check — how many RBs go in
  round 1 per draft class, for sanity-checking `prob_1st_round`.

### WR (`R/wr/`)

- **Features:** height, weight, receptions/receiving yards/receiving TDs per
  game, yards per reception, regular season wins, combine attendance, SRS,
  OSRS.
- **wr_knn_comparison.R**: a 2-stat comp profile (receiving yards/game, team
  SRS).
  - `pred_pick_knn`: average overall pick of the 5 nearest comps
    (per-game stats include postseason games).
  - `prob_1st_round_knn`: round-1 rate among the 10 nearest comps (per-game
    stats use regular season games only — this difference is preserved from
    the original scripts).
- **wr_xgboost_comparison.R**: an XGBoost regression on the same feature set
  as `wr_model.R`, as a second opinion on `pred_pick_lm`/`pred_pick_rf`.
- **wr_round1_analysis.R**: historical base-rate check, same idea as the RB
  version.

## Visuals (`R/*/<*>_rankings_plot.R`, `<*>_scatter_plot.R`)

Each position has two charts:

- **Rankings plot** — a lollipop/draft-board chart of the top 14 prospects
  with college logos, ranked by one of the model's projected picks.
- **Scatter plot** — a 2D scatter of the top features from the random
  forest's importance ranking (e.g. completion % vs. INTs/game for QB,
  rushing yards/game vs. TDs/game for RB), colored by team SRS or height.

The rankings plots use a snapshot of model output (`pred_pick_*`) captured
at the time the chart was built, rather than re-reading the live results —
so the chart and the current `output/results/*_Final_Draft_Results.csv` may
differ slightly if the models are re-run.

## Known issues

- **Voronoi map warning:** `qb_voronoi_map.R` (`geom_voronoi_tile`) prints a
  non-fatal warning — `Computation failed in 'stat_voronoi_tile()'... The
  x-range of the points is zero` — when run via `Rscript`. This is a known
  `ggforce` 0.5.0 / `deldir` 2.0.4 compatibility issue, not specific to this
  code (it reproduces on the original script as well). The plot still
  renders. If it bothers you, try
  `remotes::install_version("deldir", "1.0-9")`.
