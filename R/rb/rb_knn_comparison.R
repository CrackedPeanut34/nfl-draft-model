## rb_knn_comparison.R
##
## "Historical comp" approach for RBs, used alongside rb_model.R as a sanity
## check on the regression/RF results.
##
## For each 2026 prospect we build a 6-stat profile (rushing yards/game,
## rushing TDs/game, yards per carry, receiving yards/game, and the team's
## SRS/OSRS), z-score it against the training set, and average the overall
## pick of the 5 nearest historical RBs by Euclidean distance.
##
## Note: per-game stats here use regular_season_games only (not + postseason),
## which differs from rb_model.R's per-game calculation.

library(tidyverse)

enhance_rb_comps <- function(player_file) {
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

    all_teams <- bind_rows(all_teams, t_raw %>%
      mutate(season_before_draft = yr, SRS = as.numeric(SRS), OSRS = as.numeric(OSRS)) %>%
      select(School, SRS, OSRS, season_before_draft))
  }

  merged <- p_df %>%
    left_join(all_teams, by = c("college_team" = "School", "season_before_draft")) %>%
    mutate(
      rush_yds_pg = rushing_yds / regular_season_games,
      rush_td_pg = rushing_td / regular_season_games,
      rec_yds_pg = receiving_yds / regular_season_games
    )

  # Mean-impute any prospect missing a comparison stat so they still get a comp
  for (col in c("rush_yds_pg", "rush_td_pg", "rushing_ypc", "rec_yds_pg", "SRS", "OSRS")) {
    avg_val <- mean(merged[[col]], na.rm = TRUE)
    merged[[col]][is.na(merged[[col]])] <- avg_val
  }

  merged %>% mutate(
    z_rush_yds = (rush_yds_pg - mean(rush_yds_pg)) / sd(rush_yds_pg),
    z_rush_td = (rush_td_pg - mean(rush_td_pg)) / sd(rush_td_pg),
    z_ypc = (rushing_ypc - mean(rushing_ypc)) / sd(rushing_ypc),
    z_rec = (rec_yds_pg - mean(rec_yds_pg)) / sd(rec_yds_pg),
    z_srs = (SRS - mean(SRS)) / sd(SRS),
    z_osrs = (OSRS - mean(OSRS)) / sd(OSRS)
  )
}

run_rb_knn_model <- function(train_file, test_file) {
  train <- enhance_rb_comps(train_file)
  test <- enhance_rb_comps(test_file)

  test$pred_pick_knn <- sapply(seq_len(nrow(test)), function(i) {
    dist <- sqrt(
      (train$z_rush_yds - test$z_rush_yds[i])^2 +
        (train$z_rush_td - test$z_rush_td[i])^2 +
        (train$z_ypc - test$z_ypc[i])^2 +
        (train$z_rec - test$z_rec[i])^2 +
        (train$z_srs - test$z_srs[i])^2 +
        (train$z_osrs - test$z_osrs[i])^2
    )
    closest <- order(dist)[1:5]
    mean(train$overall[closest], na.rm = TRUE)
  })

  test
}

rb_knn_results <- run_rb_knn_model("data/train_test/RB_train.csv", "data/train_test/RB_Test.csv")

# Projected pick by historical comp
rb_knn_results %>%
  select(name, college_team, pred_pick_knn) %>%
  arrange(pred_pick_knn) %>%
  head(14)
