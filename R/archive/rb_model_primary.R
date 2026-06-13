## rb_model_primary.R (archived)
##
## Early iteration of the RB model: lm / random forest / logistic regression
## on per-game stats (regular_season_games only), before the SRS/OSRS team
## strength features were added.
##
## Superseded by R/rb/rb_model.R. Kept for reference to show how the feature
## set evolved. Produced the original output/results/RB_2026_Draft_Predictions.csv.

library(tidyverse)
library(randomForest)

rb_train <- read.csv("data/train_test/RB_train.csv", check.names = TRUE)
rb_test <- read.csv("data/train_test/RB_Test.csv", check.names = TRUE)

names(rb_train) <- make.names(names(rb_train), unique = TRUE)
names(rb_test) <- make.names(names(rb_test), unique = TRUE)

preprocess_rb_data <- function(df) {
  df %>%
    mutate(
      rushing_yds_pg = rushing_yds / regular_season_games,
      rushing_td_pg = rushing_td / regular_season_games,
      receiving_yds_pg = receiving_yds / regular_season_games,
      attended_combine = as.factor(attended_combine)
    )
}

features <- c("height", "weight", "attended_combine", "rushing_yds_pg",
              "rushing_td_pg", "rushing_ypc", "receiving_yds_pg", "regular_season_wins")

train_clean <- preprocess_rb_data(rb_train) %>%
  select(overall, all_of(features)) %>%
  na.omit() %>%
  mutate(is_first_round = if_else(overall <= 32, 1, 0))

test_clean <- preprocess_rb_data(rb_test) %>%
  select(name, all_of(features)) %>%
  mutate(attended_combine = factor(attended_combine, levels = levels(train_clean$attended_combine)))

formula_regression <- as.formula(paste("overall ~", paste(features, collapse = " + ")))
formula_classification <- as.formula(paste("is_first_round ~", paste(features, collapse = " + ")))

lm_model_rb <- lm(formula_regression, data = train_clean)
rf_model_rb <- randomForest(formula_regression, data = train_clean, importance = TRUE)
logit_model_rb <- glm(formula_classification, data = train_clean, family = binomial)

results_rb_2026 <- test_clean %>%
  mutate(
    pred_pick_lm = predict(lm_model_rb, newdata = test_clean),
    pred_pick_rf = predict(rf_model_rb, newdata = test_clean),
    prob_first_round = predict(logit_model_rb, newdata = test_clean, type = "response")
  )

results_rb_2026 %>%
  select(name, prob_first_round, pred_pick_lm, pred_pick_rf) %>%
  arrange(desc(prob_first_round)) %>%
  head(14)

write.csv(results_rb_2026, "output/results/RB_2026_Draft_Predictions.csv", row.names = FALSE)
