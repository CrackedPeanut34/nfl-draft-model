## qb_models_early.R (archived)
##
## Early iteration of the QB model: lm / random forest / logistic regression
## on raw season totals, before per-game normalization and the SRS/OSRS team
## strength features were added.
##
## Superseded by R/qb/qb_model.R. Kept for reference to show how the feature
## set evolved.

library(tidyverse)
library(randomForest)

qb_train <- read.csv("data/train_test/QB_train.csv", check.names = TRUE)
qb_test <- read.csv("data/train_test/QB_Test.csv", check.names = TRUE)

names(qb_train) <- make.names(names(qb_train), unique = TRUE)
names(qb_test) <- make.names(names(qb_test), unique = TRUE)

features <- c("completion_pct", "passing_td", "passing_int", "height",
              "weight", "passing_completions", "passing_ypa", "rushing_yds",
              "regular_season_wins", "attended_combine")

train_clean <- qb_train %>%
  select(overall, all_of(features)) %>%
  na.omit() %>%
  mutate(
    is_first_round = if_else(overall <= 32, 1, 0),
    attended_combine = as.factor(attended_combine)
  )

test_clean <- qb_test %>%
  select(name, all_of(features)) %>%
  mutate(attended_combine = factor(attended_combine, levels = levels(train_clean$attended_combine)))

formula_regression <- as.formula(paste("overall ~", paste(features, collapse = " + ")))
formula_classification <- as.formula(paste("is_first_round ~", paste(features, collapse = " + ")))

lm_model <- lm(formula_regression, data = train_clean)
rf_model <- randomForest(formula_regression, data = train_clean, importance = TRUE)
logit_model <- glm(formula_classification, data = train_clean, family = binomial)

results_2026 <- qb_test %>%
  mutate(
    pred_pick_lm = predict(lm_model, newdata = test_clean),
    pred_pick_rf = predict(rf_model, newdata = test_clean),
    prob_first_round = predict(logit_model, newdata = test_clean, type = "response")
  )

results_2026 %>%
  select(name, prob_first_round, pred_pick_lm, pred_pick_rf) %>%
  arrange(desc(prob_first_round)) %>%
  head(14)

results_2026 %>%
  select(name, prob_first_round, pred_pick_lm, pred_pick_rf) %>%
  arrange(pred_pick_lm) %>%
  head(14)

results_2026 %>%
  select(name, prob_first_round, pred_pick_lm, pred_pick_rf) %>%
  arrange(pred_pick_rf) %>%
  head(14)
