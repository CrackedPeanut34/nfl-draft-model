## wr_model_primary.R (archived)
##
## Early iteration of the WR model: lm / random forest / logistic regression
## on per-game stats (regular_season_games only), before the SRS/OSRS team
## strength features were added.
##
## Superseded by R/wr/wr_model.R. Kept for reference to show how the feature
## set evolved. Produced the original output/results/WR_2026_Draft_Predictions.csv.

library(tidyverse)
library(randomForest)

wr_train <- read.csv("data/train_test/WR_train.csv", check.names = TRUE)
wr_test <- read.csv("data/train_test/WR_Test.csv", check.names = TRUE)

names(wr_train) <- make.names(names(wr_train), unique = TRUE)
names(wr_test) <- make.names(names(wr_test), unique = TRUE)

preprocess_wr_data <- function(df) {
  df %>%
    mutate(
      receptions_pg = receiving_rec / regular_season_games,
      receiving_yds_pg = receiving_yds / regular_season_games,
      receiving_td_pg = receiving_td / regular_season_games,
      attended_combine = as.factor(attended_combine)
    )
}

features <- c("height", "weight", "attended_combine", "receptions_pg",
              "receiving_yds_pg", "receiving_ypr", "regular_season_wins", "receiving_td_pg")

train_clean <- preprocess_wr_data(wr_train) %>%
  select(overall, all_of(features)) %>%
  na.omit() %>%
  mutate(is_first_round = if_else(overall <= 32, 1, 0))

test_clean <- preprocess_wr_data(wr_test) %>%
  select(name, all_of(features)) %>%
  mutate(attended_combine = factor(attended_combine, levels = levels(train_clean$attended_combine)))

formula_reg <- as.formula(paste("overall ~", paste(features, collapse = " + ")))
formula_class <- as.formula(paste("is_first_round ~", paste(features, collapse = " + ")))

lm_model_wr <- lm(formula_reg, data = train_clean)
rf_model_wr <- randomForest(formula_reg, data = train_clean, importance = TRUE)
logit_model_wr <- glm(formula_class, data = train_clean, family = binomial)

results_wr_2026 <- test_clean %>%
  mutate(
    pred_pick_lm = predict(lm_model_wr, newdata = test_clean),
    pred_pick_rf = predict(rf_model_wr, newdata = test_clean),
    prob_first_round = predict(logit_model_wr, newdata = test_clean, type = "response")
  )

results_wr_2026 %>%
  select(name, prob_first_round, pred_pick_lm, pred_pick_rf) %>%
  arrange(desc(prob_first_round)) %>%
  head(14)

results_wr_2026 %>%
  select(name, prob_first_round, pred_pick_lm, pred_pick_rf) %>%
  arrange(pred_pick_lm) %>%
  head(14)

results_wr_2026 %>%
  select(name, prob_first_round, pred_pick_lm, pred_pick_rf) %>%
  arrange(pred_pick_rf) %>%
  head(14)

write.csv(results_wr_2026, "output/results/WR_2026_Draft_Predictions.csv", row.names = FALSE)
