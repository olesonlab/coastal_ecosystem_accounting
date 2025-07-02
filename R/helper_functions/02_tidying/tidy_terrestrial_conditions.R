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
#' @param raw_terrestrial_rainfall_90_13_19_df A data frame with 1990, 2013, and 2019 rainfall.
#' @param raw_terrestrial_rainfall_16_df A data frame with 2016 rainfall.
#'
#' @return A named list of data frames:
#'   - `mean_max_tree_cover_rainfall`: Cleaned full dataset.
#'   - `baseline_change_tree_cover_rainfall`: Differences from 1990 baseline.
#'   - `change_summary_tree_cover_rainfall`: Year-over-year rainfall changes and direction.
#'
#' @examples
#' data <- tidy_terrestrial_conditions(raw_terrestrial_rainfall_90_13_19_df, raw_terrestrial_rainfall_16_df)
#' data$mean_max_tree_cover_rainfall
#' data$baseline_change_tree_cover_rainfall
#' data$change_summary_tree_cover_rainfall
tidy_terrestrial_conditions <- function(raw_terrestrial_rainfall_90_13_19_df, raw_terrestrial_rainfall_16_df) {
  
  # 1. Clean and standardize datasets
  tidied_terrestrial_rainfall_90_13_19_df <- raw_terrestrial_rainfall_90_13_19_df |>
    janitor::clean_names() |>
    dplyr::rename(
      mean_rainfall = rain_avg,
      max_rainfall = rain_max
    ) |>
    dplyr::select(-c(x, x_1319))
  
  tidied_terrestrial_rainfall_16_df <- raw_terrestrial_rainfall_16_df |>
    janitor::clean_names() |>
    dplyr::rename(name2 = moku) |>
    dplyr::mutate(year = 2016)
  
  merged_terrestrial_rainfall_df <- dplyr::bind_rows(
    tidied_terrestrial_rainfall_90_13_19_df,
    tidied_terrestrial_rainfall_16_df
  ) |> 
    dplyr::rename(
      "mean_rainfall_mm" = mean_rainfall,
      "max_rainfall_mm" = max_rainfall
    ) |> 
    dplyr::mutate(year = as.integer(year))
  
  # 2. Combine and clean merged dataset
  mean_max_terrestrial_rainfall_df <- merged_terrestrial_rainfall_df |> 
    tidyr::pivot_wider(
      names_from = year,
      values_from = c("mean_rainfall_mm", "max_rainfall_mm")
    ) |>
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    ) 
  
  # 3. Compute difference from 1990 baseline
  baseline_change_terrestrial_rainfall_df <- merged_terrestrial_rainfall_df |>
    dplyr::select(-max_rainfall_mm) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = mean_rainfall_mm,
      names_prefix = "mean_rainfall_mm_"
    ) |>
    dplyr::mutate(
      # Difference between 1990 baseline mean rainfall (mm) and the mean rainfall
      # (mm) in recent years (2013, 2016, and 2019) per moku
      three_yr_avg_mm = (mean_rainfall_mm_2013 + mean_rainfall_mm_2016 + mean_rainfall_mm_2019) / 3,
      baseline_diff_mm = three_yr_avg_mm - mean_rainfall_mm_1990
    ) |> 
    dplyr::select(-starts_with("mean")) |>
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    )
  
  # 4. Compute year-over-year rainfall changes
  change_summary_terrestrial_rainfall_df <- 
    merged_terrestrial_rainfall_df |>
    # drop the precomputed max rainfall column
    dplyr::select(-max_rainfall_mm) |> 
    # pivot years into wide format
    tidyr::pivot_wider(
      names_from   = year,
      values_from  = mean_rainfall_mm,
      names_prefix = "mean_rainfall_mm_"
    ) |>
    # Maybe not as important to include here; included in baseline summaries
    dplyr::select(-mean_rainfall_mm_1990) |>
    dplyr::mutate(
      # absolute changes in mean rainfall (mm) for each interval
      change_mm_2013_2016 = mean_rainfall_mm_2016 - mean_rainfall_mm_2013,
      change_mm_2013_2019 = mean_rainfall_mm_2019 - mean_rainfall_mm_2013,
      change_mm_2016_2019 = mean_rainfall_mm_2019 - mean_rainfall_mm_2016,
      # change directions: -1 = decrease, 0 = no change, +1 = increase
      dir_2013_2016 = sign(change_mm_2013_2016),
      dir_2013_2019 = sign(change_mm_2013_2019),
      dir_2016_2019 = sign(change_mm_2016_2019),
      # percent changes relative to the earlier year
      pct_change_2013_2016 = (change_mm_2013_2016 / mean_rainfall_mm_2013) * 100,
      pct_change_2013_2019 = (change_mm_2013_2019 / mean_rainfall_mm_2013) * 100,
      pct_change_2016_2019 = (change_mm_2016_2019 / mean_rainfall_mm_2016) * 100
    ) |>
    dplyr::select(-c(mean_rainfall_mm_2019, mean_rainfall_mm_2016, mean_rainfall_mm_2013)) |> 
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    )
  
  summarized_terrestrial_rainfall_df <- dplyr::bind_rows(
    mean_max_terrestrial_rainfall_df,
    baseline_change_terrestrial_rainfall_df,
    change_summary_terrestrial_rainfall_df
  ) |>
    # Tree cover: No data for Kahoolawe and Niihau (n = 98)
    # Freshwater wetlands: No data for Honuaula and Kahikinui mokus and Kahoolawe and Niihau islands
    # (n = 110)
    dplyr::filter(
      value != 0, 
      !is.infinite(value)
    ) |> 
    tidyr::drop_na()

  return(summarized_terrestrial_rainfall_df)
}
