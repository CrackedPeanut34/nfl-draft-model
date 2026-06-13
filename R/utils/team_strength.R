## team_strength.R
##
## Shared helper used by the QB, RB, and WR draft models to merge each
## prospect's college team strength (SRS / OSRS, from Sports-Reference team
## ratings) onto their individual stat line for the season before the draft.
##
## Used by:
##   R/qb/qb_model.R
##   R/rb/rb_model.R
##   R/wr/wr_model.R
##
## Expects team rating files at data/team_strength/R<year>.csv, e.g.
## data/team_strength/R2025.csv. Each file is a Sports-Reference export with
## two header rows (a category row, then the real column names), so it must
## be read with skip = 1.

library(dplyr)

enhance_with_team_stats <- function(player_df) {
  names(player_df) <- make.names(names(player_df), unique = TRUE)

  unique_years <- unique(player_df$season_before_draft)
  all_team_history <- data.frame()

  for (yr in unique_years) {
    file_name <- file.path("data", "team_strength", paste0("R", yr, ".csv"))
    if (!file.exists(file_name)) next

    # Skip the category row so the real column names (School, SRS, OSRS, ...) load
    team_yr_raw <- read.csv(file_name, skip = 1, check.names = FALSE, stringsAsFactors = FALSE)
    names(team_yr_raw) <- make.names(names(team_yr_raw), unique = TRUE)

    # The Sports-Reference layout repeats "SRS" across Overall/Offense/Defense
    # sections, so the team-level SRS column can land as SRS, SRS.1, or SRS.2.
    sch_idx <- which(grepl("School", names(team_yr_raw), ignore.case = TRUE))[1]
    srs_idx <- which(names(team_yr_raw) %in% c("SRS", "SRS.1", "SRS.2"))[1]
    osr_idx <- which(grepl("OSRS", names(team_yr_raw), ignore.case = TRUE))[1]

    if (!is.na(sch_idx)) names(team_yr_raw)[sch_idx] <- "School" else team_yr_raw$School <- NA
    if (!is.na(srs_idx)) names(team_yr_raw)[srs_idx] <- "SRS"    else team_yr_raw$SRS    <- NA
    if (!is.na(osr_idx)) names(team_yr_raw)[osr_idx] <- "OSRS"   else team_yr_raw$OSRS   <- NA

    team_yr_raw <- team_yr_raw %>%
      filter(School != "School" & !is.na(School)) %>%
      mutate(
        SRS = as.numeric(SRS),
        OSRS = as.numeric(OSRS),
        season_before_draft = yr
      )

    all_team_history <- bind_rows(all_team_history, team_yr_raw)
  }

  merged_df <- player_df %>%
    left_join(all_team_history, by = c("college_team" = "School", "season_before_draft"))

  # Players whose school isn't in the ratings file (small-school transfers,
  # name mismatches, etc.) get the bottom-10th-percentile SRS/OSRS for that
  # year as a "below replacement level" stand-in rather than NA.
  for (yr in unique_years) {
    year_stats <- all_team_history %>% filter(season_before_draft == yr)
    if (nrow(year_stats) == 0) next

    srs_p10 <- quantile(year_stats$SRS, 0.1, na.rm = TRUE)
    osrs_p10 <- quantile(year_stats$OSRS, 0.1, na.rm = TRUE)

    merged_df <- merged_df %>%
      mutate(
        SRS = if_else(season_before_draft == yr & is.na(SRS), srs_p10, SRS),
        OSRS = if_else(season_before_draft == yr & is.na(OSRS), osrs_p10, OSRS)
      )
  }

  return(merged_df)
}
