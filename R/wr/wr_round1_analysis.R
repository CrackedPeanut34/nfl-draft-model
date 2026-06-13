## wr_round1_analysis.R
##
## Quick historical check: how many WRs have gone in round 1 per draft class
## in the training data, used to sanity-check the round-1 probability output
## from wr_model.R against base rates.
##
## Input: data/train_test/WR_train.csv

library(tidyverse)

wr_train <- read.csv("data/train_test/WR_train.csv")

sum(wr_train$round == 1)

round_1_wrs <- wr_train %>%
  filter(round == 1)

# Distribution of 1st-round WR picks by year
hist_round1 <- ggplot(data = round_1_wrs, aes(x = year)) +
  geom_bar() +
  labs(
    title = "Historical 1st Round WR Volume (Training Set)",
    x = "Draft Year",
    y = "Number of WRs Selected"
  ) +
  theme_minimal()

print(hist_round1)

# Number of first-round WRs per draft year
wr_yearly_totals <- round_1_wrs %>%
  group_by(year) %>%
  summarise(total_1st_round_wrs = n())

print(wr_yearly_totals)

sd(wr_yearly_totals$total_1st_round_wrs)
