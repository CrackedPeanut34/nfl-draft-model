## wr_model.R
##
## Main WR draft model. For each 2026 WR prospect, predicts:
##   - pred_pick_lm    : projected overall pick (multiple linear regression)
##   - pred_pick_rf    : projected overall pick (random forest regression)
##   - prob_1st_round  : probability the player is picked in round 1 (logistic regression)
##
## Feature engineering:
##   1. Team strength (SRS/OSRS) is merged onto each player from
##      data/team_strength/R<season_before_draft>.csv via enhance_with_team_stats().
##   2. Raw season totals (receptions, receiving yards, receiving TDs) are
##      converted to per-game rates using (regular_season_games + postseason_games).
##
## Input:  data/train_test/WR_train.csv, data/train_test/WR_Test.csv
## Output: output/results/WR_Final_Draft_Results.csv

library(tidyverse)
library(randomForest)

source("R/utils/team_strength.R")

run_wr_model <- function(train_file, test_file, features) {
  train_raw <- enhance_with_team_stats(read.csv(train_file))
  test_raw <- enhance_with_team_stats(read.csv(test_file))

  add_per_game_stats <- function(df) {
    df %>% mutate(
      receptions_pg = receiving_rec / (regular_season_games + postseason_games),
      receiving_yds_pg = receiving_yds / (regular_season_games + postseason_games),
      receiving_td_pg = receiving_td / (regular_season_games + postseason_games)
    )
  }

  train_raw <- add_per_game_stats(train_raw)
  test_raw <- add_per_game_stats(test_raw)

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

  write.csv(test_raw, "output/results/WR_Final_Draft_Results.csv", row.names = FALSE)

  list(results = test_raw, lm = lm_mod, rf = rf_mod, logit = logit_mod)
}

wr_features <- c(
  "height", "weight", "receptions_pg", "receiving_yds_pg",
  "receiving_ypr", "regular_season_wins", "attended_combine",
  "receiving_td_pg", "SRS", "OSRS"
)

wr_model <- run_wr_model("data/train_test/WR_train.csv", "data/train_test/WR_Test.csv", wr_features)
wr_results <- wr_model$results

# Top prospects by projected overall pick (linear model)
wr_results %>%
  select(name, prob_1st_round, pred_pick_rf, pred_pick_lm) %>%
  arrange(pred_pick_lm) %>%
  head(14)

# What's driving the random forest's predictions
print(importance(wr_model$rf))
