# tidy_terrestrial_extents.R
#
# Purpose: Provide a reusable function to clean terrestrial extent data by
#          standardizing column names, pivoting ecosystem type columns into long format,
#          formatting values for clarity, and tagging data as terrestrial.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/02_processing/).
#   2. Source the file or call `tidy_terrestrial_extents()` directly after loading your raw terrestrial extents data.
#   3. Pass in the raw terrestrial extents data frame (e.g., `extent_dfs$terrestrial_extents`).
#
# =============================================================================

#' Tidy terrestrial extent data for analysis and merging
#'
#' @param raw_df A data frame or tibble containing raw terrestrial extent data.
#'
#' @return A tibble with cleaned and standardized columns:
#'         name2 (moku), year, ecosystem_type (long format), area (km²), and realm ("Terrestrial").
#'
#' @examples
#' raw_terrestrial_df <- tibble::tibble(
#'   name2 = c("Kona", "Hilo"),
#'   year = c(2013, 2013),
#'   developed = c(15.1, 12.8),
#'   grass_shrub = c(8.0, 9.5),
#'   beaches_dunes = c(0.5, 0.6)
#' )
#' tidy_terrestrial_extents(raw_terrestrial_df)

tidy_terrestrial_extents <- function(raw_df) {
  # Validate input
  if (!inherits(raw_df, "data.frame")) {
    stop("Input must be a data.frame or tibble.", call. = FALSE)
  }
  
  raw_df |> 
    janitor::clean_names() |> 
    # Convert wide ecosystem type columns to long format
    tidyr::pivot_longer(
      cols = -c(name2, year),
      names_to = "ecosystem_type",
      values_to = "area"
    ) |> 
    dplyr::mutate(
      # Normalize some special cases
      ecosystem_type = dplyr::case_when(
        ecosystem_type == "grass_shrub" ~ "Grass/Shrub",
        ecosystem_type == "beaches_dunes" ~ "Beaches/Dunes",
        TRUE ~ ecosystem_type
      ),
      # Replace underscores and apply title case
      ecosystem_type = stringr::str_replace_all(ecosystem_type, "_", " "),
      ecosystem_type = stringr::str_to_title(ecosystem_type),
      realm = "Terrestrial"
    ) 
}
