## rb_rf_classification.R (archived)
##
## Early iteration that modeled "1st round or not" as a random forest
## classification problem (factor target "Yes"/"No"), using the SRS/OSRS
## team strength features but per-game stats based on regular_season_games
## only (no postseason).
##
## Superseded by R/rb/rb_model.R, which uses a single logistic regression
## for round-1 probability. Kept for reference.

library(tidyverse)
library(randomForest)

source("R/utils/team_strength.R")

run_rb_model <- function(train_file, test_file, features) {
  train_raw <- enhance_with_team_stats(read.csv(train_file))
  test_raw <- enhance_with_team_stats(read.csv(test_file))

  mutate_eff <- function(df) {
    df %>% mutate(
      rushing_yds_pg = rushing_yds / regular_season_games,
      rushing_td_pg = rushing_td / regular_season_games,
      receiving_yds_pg = receiving_yds / regular_season_games
    )
  }

  train_raw <- mutate_eff(train_raw)
  test_raw <- mutate_eff(test_raw)

  train_clean <- train_raw %>%
    select(overall, all_of(features)) %>%
    na.omit() %>%
    mutate(
      is_first_round = as.factor(if_else(overall < 33, "Yes", "No")),
      attended_combine = as.factor(attended_combine)
    )

  test_clean <- test_raw %>%
    select(all_of(features)) %>%
    mutate(attended_combine = factor(attended_combine, levels = levels(train_clean$attended_combine))) %>%
    mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm = TRUE), .)))

  f_cls <- as.formula(paste("is_first_round ~", paste(features, collapse = " + ")))

  # ntree = 500 for a smoother probability estimate from the vote share
  rf_mod <- randomForest(f_cls, data = train_clean, ntree = 500, importance = TRUE)

  rf_probs <- predict(rf_mod, newdata = test_clean, type = "prob")
  test_raw$prob_1st_round_rf <- rf_probs[, "Yes"]

  test_raw
}

rb_features <- c("height", "weight", "attended_combine", "rushing_yds_pg",
                "rushing_td_pg", "rushing_ypc", "receiving_yds_pg",
                "regular_season_wins", "SRS", "OSRS")

rb_results <- run_rb_model("data/train_test/RB_train.csv", "data/train_test/RB_Test.csv", rb_features)

rb_results %>%
  select(name, prob_1st_round_rf) %>%
  arrange(desc(prob_1st_round_rf)) %>%
  head(14)
