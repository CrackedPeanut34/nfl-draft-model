## wr_knn_comparison.R
##
## "Historical comp" approach for WRs, used alongside wr_model.R as a sanity
## check on the regression/RF results.
##
## For each 2026 prospect we build a 2-stat profile (receiving yards/game and
## team SRS), z-score it against the training set, and compare against
## historical WRs by Euclidean distance.
##
##   - run_wr_knn_regression()     -> pred_pick_knn: average overall pick of
##                                     the 5 nearest historical WRs. Per-game
##                                     stats use (regular_season_games + postseason_games).
##   - run_wr_knn_classification() -> prob_1st_round_knn: share of the 10
##                                     nearest historical WRs taken in round 1.
##                                     Per-game stats use regular_season_games only.
##
## The two functions use different per-game denominators, preserved as-is
## from the original WRKNNModel.R / WRKNNClass.R scripts.

library(tidyverse)

enhance_wr_comps <- function(player_file, include_postseason) {
  p_df <- read.csv(player_file, stringsAsFactors = FALSE)
  names(p_df) <- make.names(names(p_df), unique = TRUE)

  p_team_col <- which(grepl("college|team", names(p_df), ignore.case = TRUE))[1]
  if (!is.na(p_team_col)) names(p_df)[p_team_col] <- "college_team"

  unique_years <- unique(p_df$season_before_draft)
  all_teams <- data.frame()

  for (yr in unique_years) {
    f_name <- file.path("data", "team_strength", paste0("R", yr, ".csv"))
    if (!file.exists(f_name)) next

    t_raw <- read.csv(f_name, skip = 1, check.names = FALSE, stringsAsFactors = FALSE)
    names(t_raw) <- make.names(names(t_raw), unique = TRUE)

    s_idx <- which(grepl("School", names(t_raw), ignore.case = TRUE))[1]
    srs_idx <- which(names(t_raw) == "SRS" | names(t_raw) == "SRS.1")[1]

    if (!is.na(s_idx)) names(t_raw)[s_idx] <- "School"
    if (!is.na(srs_idx)) names(t_raw)[srs_idx] <- "SRS" else t_raw$SRS <- 0

    all_teams <- bind_rows(all_teams, t_raw %>%
      mutate(season_before_draft = yr, SRS = as.numeric(SRS)) %>%
      select(School, SRS, season_before_draft))
  }

  games <- if (include_postseason) {
    p_df$regular_season_games + p_df$postseason_games
  } else {
    p_df$regular_season_games
  }

  p_df %>%
    left_join(all_teams, by = c("college_team" = "School", "season_before_draft")) %>%
    mutate(
      receiving_yds_pg = receiving_yds / games,
      z_yds = (receiving_yds_pg - mean(receiving_yds_pg, na.rm = TRUE)) / sd(receiving_yds_pg, na.rm = TRUE),
      z_srs = (SRS - mean(SRS, na.rm = TRUE)) / sd(SRS, na.rm = TRUE)
    )
}

knn_distance <- function(train, test_row) {
  sqrt(
    (train$z_yds - test_row$z_yds)^2 +
      (train$z_srs - test_row$z_srs)^2
  )
}

# Average overall pick of the 5 closest historical comps
run_wr_knn_regression <- function(train_file, test_file) {
  train <- enhance_wr_comps(train_file, include_postseason = TRUE) %>% na.omit()
  test <- enhance_wr_comps(test_file, include_postseason = TRUE)

  test$pred_pick_knn <- sapply(seq_len(nrow(test)), function(i) {
    dist <- knn_distance(train, test[i, ])
    closest <- order(dist)[1:5]
    mean(train$overall[closest], na.rm = TRUE)
  })

  test
}

# Share of the 10 closest historical comps taken in round 1 (picks 1-32)
run_wr_knn_classification <- function(train_file, test_file) {
  train <- enhance_wr_comps(train_file, include_postseason = FALSE) %>% na.omit()
  test <- enhance_wr_comps(test_file, include_postseason = FALSE)
  train$is_first_round <- if_else(train$overall < 33, 1, 0)

  test$prob_1st_round_knn <- sapply(seq_len(nrow(test)), function(i) {
    dist <- knn_distance(train, test[i, ])
    closest <- order(dist)[1:10]
    mean(train$is_first_round[closest], na.rm = TRUE)
  })

  test
}

wr_knn_reg <- run_wr_knn_regression("data/train_test/WR_train.csv", "data/train_test/WR_Test.csv")
wr_knn_class <- run_wr_knn_classification("data/train_test/WR_train.csv", "data/train_test/WR_Test.csv")

# Projected pick by historical comp
wr_knn_reg %>%
  select(name, college_team, pred_pick_knn) %>%
  arrange(pred_pick_knn) %>%
  head(14)

# Round-1 rate among the 10 closest historical comps
wr_knn_class %>%
  select(name, college_team, prob_1st_round_knn) %>%
  arrange(desc(prob_1st_round_knn)) %>%
  head(14)
