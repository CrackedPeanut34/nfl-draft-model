## wr_xgboost_comparison.R
##
## XGBoost regression as a second opinion alongside wr_model.R's linear/RF/
## logistic models. Same feature set as wr_model.R, but per-game stats here
## use regular_season_games only (not + postseason), preserved as-is from
## the original WRXQBoostModel.R.
##
## Input: data/train_test/WR_train.csv, data/train_test/WR_Test.csv

library(tidyverse)
library(xgboost)

source("R/utils/team_strength.R")

wr_features <- c(
  "height", "weight", "receptions_pg", "receiving_yds_pg", "receiving_ypr",
  "regular_season_wins", "attended_combine", "receiving_td_pg", "SRS", "OSRS"
)

add_per_game_stats <- function(df) {
  df %>% mutate(
    receptions_pg = receiving_rec / regular_season_games,
    receiving_yds_pg = receiving_yds / regular_season_games,
    receiving_td_pg = receiving_td / regular_season_games,
    attended_combine = as.numeric(as.factor(attended_combine))
  )
}

train_clean <- enhance_with_team_stats(read.csv("data/train_test/WR_train.csv")) %>%
  add_per_game_stats() %>%
  select(overall, all_of(wr_features)) %>%
  na.omit()

dtrain <- xgb.DMatrix(
  data = as.matrix(train_clean %>% select(all_of(wr_features))),
  label = train_clean$overall
)
xgb_params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 6)
xgb_mod <- xgb.train(params = xgb_params, data = dtrain, nrounds = 50)

test_processed <- enhance_with_team_stats(read.csv("data/train_test/WR_Test.csv")) %>%
  add_per_game_stats()

# Fill any missing test features with the training column average
test_matrix_data <- test_processed %>%
  select(all_of(wr_features)) %>%
  mutate(across(everything(), ~ if_else(is.na(.), mean(., na.rm = TRUE), .)))

dtest <- xgb.DMatrix(data = as.matrix(test_matrix_data))
test_processed$pred_pick_xgb <- predict(xgb_mod, dtest)

# Top prospects by projected overall pick (XGBoost)
test_processed %>%
  select(name, pred_pick_xgb) %>%
  arrange(pred_pick_xgb) %>%
  head(14)

importance_matrix <- xgb.importance(model = xgb_mod)
print(importance_matrix)

par(mar = c(5, 12, 4, 2))
xgb.plot.importance(importance_matrix, main = "WR Statistical Drivers (XGBoost)")
