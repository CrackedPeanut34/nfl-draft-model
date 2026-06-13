## rb_round1_analysis.R
##
## Quick historical check: how many RBs have gone in round 1 per draft class
## in the training data, used to sanity-check the round-1 probability output
## from rb_model.R against base rates.
##
## Input: data/train_test/RB_train.csv

library(tidyverse)

rb_train <- read.csv("data/train_test/RB_train.csv")

# Number of first-round RBs per draft year
rb_yearly_summary <- rb_train %>%
  filter(round == 1) %>%
  group_by(year) %>%
  summarise(total_first_round_rbs = n())

print(rb_yearly_summary)

mean(rb_yearly_summary$total_first_round_rbs)
sd(rb_yearly_summary$total_first_round_rbs)

# Visualizing the selection volume by year
rb_plot <- ggplot(data = rb_yearly_summary, aes(x = year, y = total_first_round_rbs)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Historical 1st Round RB Volume (Training Set)",
    x = "Draft Year",
    y = "Number of RBs Selected"
  ) +
  theme_classic()

print(rb_plot)
