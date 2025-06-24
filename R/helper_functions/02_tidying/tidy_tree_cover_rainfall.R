# tidy_tree_cover_rainfall_data.R
#
# Purpose: Provide a reusable function to tidy and summarize tree cover rainfall data
#          from 1990, 2013, 2016, and 2019 across Mokupuni.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/02_processing/).
#   2. Source the file and call `tidy_tree_cover_rainfall_data()` with the raw data inputs.
#   3. Pass in:
#        - `raw_tree_cover_rainfall_90_13_19_df`: rainfall data with years 1990, 2013, 2019.
#        - `raw_tree_cover_rainfall_16_df`: rainfall data for 2016.
#   4. Returns a named list of cleaned and derived data frames.
#
# =============================================================================

# # 1a. Dependency checks
# required_pkgs <- c("dplyr", "tidyr", "janitor")
# base::invisible(base::lapply(required_pkgs, function(pkg) {
#   if (!base::requireNamespace(pkg, quietly = TRUE)) {
#     base::stop(base::sprintf("Please install package '%s' to use tidy_tree_cover_rainfall_data()", pkg), call. = FALSE)
#   }
# }))

#' Tidy and summarize tree cover rainfall data
#'
#' @param raw_tree_cover_rainfall_90_13_19_df A data frame with 1990, 2013, and 2019 rainfall.
#' @param raw_tree_cover_rainfall_16_df A data frame with 2016 rainfall.
#'
#' @return A named list of data frames:
#'   - `mean_max_tree_cover_rainfall`: Cleaned full dataset.
#'   - `baseline_change_tree_cover_rainfall`: Differences from 1990 baseline.
#'   - `change_summary_tree_cover_rainfall`: Year-over-year rainfall changes and direction.
#'
#' @examples
#' data <- tidy_tree_cover_rainfall_data(df_90_13_19, df_16)
#' data$mean_max_tree_cover_rainfall
#' data$baseline_change_tree_cover_rainfall
#' data$change_summary_tree_cover_rainfall
tidy_tree_cover_rainfall_data <- function(raw_tree_cover_rainfall_90_13_19_df, raw_tree_cover_rainfall_16_df) {
  
  # 1. Clean and standardize datasets
  tidied_tree_cover_rainfall_90_13_19 <- raw_tree_cover_rainfall_90_13_19_df |>
    janitor::clean_names() |>
    dplyr::rename(
      mean_rainfall = rain_avg,
      max_rainfall = rain_max
    ) |>
    dplyr::select(-c(x, x_1319))
  
  tidied_tree_cover_rainfall_16 <- raw_tree_cover_rainfall_16_df |>
    janitor::clean_names() |>
    dplyr::rename(name2 = moku) |>
    dplyr::mutate(year = 2016)
  
  # 2. Combine and clean merged dataset
  mean_max_tree_cover_rainfall <- dplyr::bind_rows(
    tidied_tree_cover_rainfall_90_13_19,
    tidied_tree_cover_rainfall_16
  ) |>
    tidyr::drop_na()
  
  # 3. Compute difference from 1990 baseline
  baseline_change_tree_cover_rainfall <- mean_max_tree_cover_rainfall |>
    dplyr::select(-max_rainfall) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = mean_rainfall,
      names_prefix = "mean_rainfall_"
    ) |>
    dplyr::mutate(
      # Difference between 1990 baseline mean rainfall (mm) and the mean rainfall
      # (mm) in recent years (2013, 2016, and 2019) per moku
      three_yr_avg = (mean_rainfall_2013 + mean_rainfall_2016 + mean_rainfall_2019) / 3,
      baseline_diff = three_yr_avg - mean_rainfall_1990
    )
  
  # 4. Compute year-over-year rainfall changes
  change_summary_tree_cover_rainfall <- mean_max_tree_cover_rainfall |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = mean_rainfall,
      names_prefix = "mean_rainfall_"
    ) |>
    dplyr::mutate(
      # Difference in mean rainfall (mm) between two years per each moku
      change_13_19 = mean_rainfall_2019 - mean_rainfall_2013,
      change_13_16 = mean_rainfall_2016 - mean_rainfall_2013,
      change_16_19 = mean_rainfall_2019 - mean_rainfall_2016,
      change_direction = dplyr::case_when(
        change_13_19 > 0 ~ "Positive",
        change_13_19 < 0 ~ "Negative",
        TRUE ~ "No Change"
      )
    )
  
  # 5. Return named list of tibbles
  return(list(
    mean_max_tree_cover_rainfall = mean_max_tree_cover_rainfall,
    baseline_change_tree_cover_rainfall = baseline_change_tree_cover_rainfall,
    change_summary_tree_cover_rainfall = change_summary_tree_cover_rainfall
  ))
}