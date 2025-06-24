# tidy_marine_extents.R
#
# Purpose: Provide a reusable function to clean and expand marine extent data by
#          standardizing column names, aligning them with terrestrial extent structure,
#          and replicating the data across analysis years (2013, 2016, 2019).
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/02_processing/).
#   2. Source the file or call `tidy_marine_extents()` directly after loading your raw marine extents data.
#   3. Pass in the raw marine extents data frame (e.g., `extent_dfs$marine_extents`).
#
# =============================================================================

#' Tidy marine extent data for visualization and merging
#'
#' @param raw_df A data frame or tibble containing raw marine extent data.
#'
#' @return A tibble with cleaned and standardized columns:
#'         name2 (moku), area (km²), value, realm ("Marine"), and year (replicated across 2013, 2016, 2019).
#'
#' @examples
#' raw_marine_df <- tibble::tibble(
#'   moku = c("Kona", "Hilo"),
#'   area_km_2 = c(10.5, 12.3),
#'   value = c(0.8, 0.9)
#' )
#' tidy_marine_extents(raw_marine_df)

tidy_marine_extents <- function(raw_df) {
  # Validate input
  if (!inherits(raw_df, "data.frame")) {
    stop("Input must be a data.frame or tibble.", call. = FALSE)
  }
  
  raw_df |>
    # Clean column names (e.g., snake_case)
    janitor::clean_names() |>
    # Standardize for alignment with terrestrial extents
    dplyr::select(
      "ecosystem_type" = value,
      name2 = moku,
      area = area_km_2
    ) |>
    # Replicate rows for target analysis years
    tidyr::crossing(year = c(2013, 2016, 2019)) |>
    dplyr::mutate(
      realm = "Marine"
    )
}
