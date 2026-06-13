## qb_model.R
##
## Main QB draft model. For each 2026 QB prospect, predicts:
##   - pred_pick_lm    : projected overall pick (multiple linear regression)
##   - pred_pick_rf    : projected overall pick (random forest regression)
##   - prob_1st_round  : probability the player is picked in round 1 (logistic regression)
##
## Feature engineering:
##   1. Team strength (SRS/OSRS) is merged onto each player from
##      data/team_strength/R<season_before_draft>.csv via enhance_with_team_stats().
##   2. Raw season totals (passing yards, TDs, completions, etc.) are converted
##      to per-game rates using (regular_season_games + postseason_games), so
##      players from teams with longer playoff runs aren't penalized for
##      having higher season totals.
##
## Input:  data/train_test/QB_train.csv, data/train_test/QB_Test.csv
## Output: output/results/QB_Final_Draft_Results.csv

library(tidyverse)
library(randomForest)

source("R/utils/team_strength.R")

run_qb_model <- function(train_file, test_file, features) {
  train_raw <- enhance_with_team_stats(read.csv(train_file))
  test_raw <- enhance_with_team_stats(read.csv(test_file))

  add_per_game_stats <- function(df) {
    df %>% mutate(
      passing_yds_pg = passing_yds / (regular_season_games + postseason_games),
      passing_td_pg = passing_td / (regular_season_games + postseason_games),
      passing_completions_pg = passing_completions / (regular_season_games + postseason_games),
      rushing_yds_pg = rushing_yds / (regular_season_games + postseason_games),
      passing_int_pg = passing_int / (regular_season_games + postseason_games)
    )
  }

  train_raw <- add_per_game_stats(train_raw)
  test_raw <- add_per_game_stats(test_raw)

  # Training rows need every feature present, plus the round-1 label for the
  # logistic model (picks 1-32 = round 1).
  train_clean <- train_raw %>%
    select(overall, all_of(features)) %>%
    na.omit() %>%
    mutate(
      is_first_round = if_else(overall < 33, 1, 0),
      attended_combine = as.factor(attended_combine)
    )

  test_clean <- test_raw %>%
    mutate(attended_combine = factor(attended_combine, levels = levels(train_clean$attended_combine)))

  f_reg <- as.formula(paste("overall ~", paste(features, collapse = " + ")))
  f_cls <- as.formula(paste("is_first_round ~", paste(features, collapse = " + ")))

  lm_mod <- lm(f_reg, data = train_clean)
  rf_mod <- randomForest(f_reg, data = train_clean, importance = TRUE)
  logit_mod <- glm(f_cls, data = train_clean, family = binomial)

  test_raw <- test_raw %>%
    mutate(
      pred_pick_lm = predict(lm_mod, newdata = test_clean),
      pred_pick_rf = predict(rf_mod, newdata = test_clean),
      prob_1st_round = predict(logit_mod, newdata = test_clean, type = "response")
    )

  write.csv(test_raw, "output/results/QB_Final_Draft_Results.csv", row.names = FALSE)

  list(results = test_raw, lm = lm_mod, rf = rf_mod, logit = logit_mod)
}

qb_features <- c(
  "completion_pct", "passing_yds_pg", "passing_td_pg", "passing_int_pg",
  "height", "weight", "passing_completions_pg", "passing_ypa",
  "rushing_yds_pg", "regular_season_wins", "attended_combine", "SRS", "OSRS"
)

qb_model <- run_qb_model("data/train_test/QB_train.csv", "data/train_test/QB_Test.csv", qb_features)
qb_results <- qb_model$results

# Top prospects by projected overall pick (linear model)
qb_results %>%
  select(name, pred_pick_lm, pred_pick_rf, prob_1st_round) %>%
  arrange(pred_pick_lm) %>%
  head(14)

# What's driving the random forest's predictions
print(importance(qb_model$rf))
