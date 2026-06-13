## qb_knn_comparison.R
##
## "Historical comp" approach for QBs, used alongside qb_model.R as a sanity
## check on the regression/RF/logistic results.
##
## For each 2026 prospect we build a 5-stat profile (passing yards/game,
## passing TDs/game, yards per attempt, and the team's SRS/OSRS), z-score it
## against the training set, and find the nearest historical QBs by Euclidean
## distance.
##
##   - run_qb_knn_regression()     -> pred_pick_knn: average overall pick of
##                                     the 5 nearest historical QBs
##   - run_qb_knn_classification() -> prob_1st_round_knn: share of the 21
##                                     nearest historical QBs taken in round 1
##
## Note: per-game stats here use regular_season_games only (not + postseason),
## which differs from qb_model.R's per-game calculation.

library(tidyverse)

enhance_qb_comps <- function(player_file) {
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
    srs_idx <- which(grepl("^SRS$|\\.SRS$|^SRS\\.", names(t_raw), ignore.case = TRUE))[1]
    osr_idx <- which(grepl("OSRS", names(t_raw), ignore.case = TRUE))[1]

    if (!is.na(s_idx)) names(t_raw)[s_idx] <- "School"
    if (!is.na(srs_idx)) names(t_raw)[srs_idx] <- "SRS" else t_raw$SRS <- 0
    if (!is.na(osr_idx)) names(t_raw)[osr_idx] <- "OSRS" else t_raw$OSRS <- 0

    t_clean <- t_raw %>%
      mutate(season_before_draft = yr, SRS = as.numeric(SRS), OSRS = as.numeric(OSRS)) %>%
      filter(!is.na(School) & School != "School") %>%
      select(School, SRS, OSRS, season_before_draft)

    all_teams <- bind_rows(all_teams, t_clean)
  }

  merged <- p_df %>%
    left_join(all_teams, by = c("college_team" = "School", "season_before_draft")) %>%
    mutate(
      pass_yds_pg = passing_yds / regular_season_games,
      pass_td_pg = passing_td / regular_season_games,
      pass_com_pg = passing_completions / regular_season_games,
      rush_yds_pg = rushing_yds / regular_season_games,
      pass_int_pg = passing_int / regular_season_games
    )

  # Mean-impute any prospect missing a comparison stat so they still get a comp
  for (col in c("pass_yds_pg", "pass_td_pg", "passing_ypa", "SRS", "OSRS")) {
    avg_val <- mean(merged[[col]], na.rm = TRUE)
    merged[[col]][is.na(merged[[col]])] <- avg_val
  }

  merged %>% mutate(
    z_yds = (pass_yds_pg - mean(pass_yds_pg)) / sd(pass_yds_pg),
    z_td = (pass_td_pg - mean(pass_td_pg)) / sd(pass_td_pg),
    z_ypa = (passing_ypa - mean(passing_ypa)) / sd(passing_ypa),
    z_srs = (SRS - mean(SRS)) / sd(SRS),
    z_osrs = (OSRS - mean(OSRS)) / sd(OSRS)
  )
}

knn_distance <- function(train, test_row) {
  sqrt(
    (train$z_yds - test_row$z_yds)^2 +
      (train$z_td - test_row$z_td)^2 +
      (train$z_ypa - test_row$z_ypa)^2 +
      (train$z_srs - test_row$z_srs)^2 +
      (train$z_osrs - test_row$z_osrs)^2
  )
}

# Average overall pick of the 5 closest historical comps
run_qb_knn_regression <- function(train_file, test_file) {
  train <- enhance_qb_comps(train_file)
  test <- enhance_qb_comps(test_file)

  test$pred_pick_knn <- sapply(seq_len(nrow(test)), function(i) {
    dist <- knn_distance(train, test[i, ])
    closest <- order(dist)[1:5]
    mean(train$overall[closest], na.rm = TRUE)
  })

  test
}

# Share of the 21 closest historical comps taken in round 1 (picks 1-32)
run_qb_knn_classification <- function(train_file, test_file) {
  train <- enhance_qb_comps(train_file)
  test <- enhance_qb_comps(test_file)
  train$is_first_round <- if_else(train$overall < 33, 1, 0)

  test$prob_1st_round_knn <- sapply(seq_len(nrow(test)), function(i) {
    dist <- knn_distance(train, test[i, ])
    closest <- order(dist)[1:21]
    mean(train$is_first_round[closest], na.rm = TRUE)
  })

  test
}

qb_knn_reg <- run_qb_knn_regression("data/train_test/QB_train.csv", "data/train_test/QB_Test.csv")
qb_knn_class <- run_qb_knn_classification("data/train_test/QB_train.csv", "data/train_test/QB_Test.csv")

# Projected pick by historical comp
qb_knn_reg %>%
  select(name, college_team, pred_pick_knn) %>%
  arrange(pred_pick_knn) %>%
  head(14)

# Round-1 rate among the 21 closest historical comps
qb_knn_class %>%
  select(name, college_team, prob_1st_round_knn) %>%
  arrange(desc(prob_1st_round_knn)) %>%
  head(14)
