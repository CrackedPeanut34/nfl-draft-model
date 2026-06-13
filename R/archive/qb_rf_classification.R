## qb_rf_classification.R (archived)
##
## Early iteration that modeled "1st round or not" as a random forest
## classification problem (factor target "Yes"/"No") rather than the
## logistic regression used in the final model. Per-game stats here use
## regular_season_games only (no postseason).
##
## Superseded by R/qb/qb_model.R, which uses a single logistic regression
## for round-1 probability. Kept for reference.

library(tidyverse)
library(randomForest)

source("R/utils/team_strength.R")

run_position_model <- function(pos, train_file, test_file, features) {
  train_raw <- enhance_with_team_stats(read.csv(train_file))
  test_raw <- enhance_with_team_stats(read.csv(test_file))

  mutate_qb_pg <- function(df) {
    df %>% mutate(
      passing_yds_pg = passing_yds / regular_season_games,
      passing_td_pg = passing_td / regular_season_games,
      passing_completions_pg = passing_completions / regular_season_games,
      rushing_yds_pg = rushing_yds / regular_season_games,
      passing_int_pg = passing_int / regular_season_games
    )
  }

  train_raw <- mutate_qb_pg(train_raw)
  test_raw <- mutate_qb_pg(test_raw)

  train_clean <- train_raw %>%
    select(overall, all_of(features)) %>%
    na.omit() %>%
    mutate(
      # Classification RF needs a factor target
      is_first_round = as.factor(if_else(overall < 33, "Yes", "No")),
      attended_combine = as.factor(attended_combine)
    )

  test_clean <- test_raw %>%
    select(all_of(features)) %>%
    mutate(attended_combine = factor(attended_combine, levels = levels(train_clean$attended_combine))) %>%
    mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm = TRUE), .)))

  f_reg <- as.formula(paste("overall ~", paste(features, collapse = " + ")))
  f_cls <- as.formula(paste("is_first_round ~", paste(features, collapse = " + ")))

  lm_mod <- lm(f_reg, data = train_clean %>% mutate(overall = as.numeric(overall)))
  rf_mod <- randomForest(f_cls, data = train_clean, importance = TRUE)
  logit_mod <- glm(f_cls, data = train_clean, family = binomial)

  rf_probs <- predict(rf_mod, newdata = test_clean, type = "prob")

  test_raw <- test_raw %>%
    mutate(
      pred_pick_lm = predict(lm_mod, newdata = test_clean),
      prob_1st_round_rf = rf_probs[, "Yes"],
      prob_1st_round_logit = predict(logit_mod, newdata = test_clean, type = "response")
    )

  write.csv(test_raw, file.path("output", "results", paste0(pos, "_RF_Classification_Experiment.csv")), row.names = FALSE)
  test_raw
}

qb_feats <- c("completion_pct", "passing_yds_pg", "passing_td_pg", "passing_int_pg",
             "height", "weight", "passing_completions_pg", "passing_ypa",
             "rushing_yds_pg", "regular_season_wins", "attended_combine", "SRS", "OSRS")

qb_results <- run_position_model("QB", "data/train_test/QB_train.csv", "data/train_test/QB_Test.csv", qb_feats)

qb_results %>%
  select(name, prob_1st_round_rf, prob_1st_round_logit, pred_pick_lm) %>%
  arrange(desc(prob_1st_round_rf)) %>%
  head(14)
