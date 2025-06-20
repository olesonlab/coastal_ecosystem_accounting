# tidy_marine_mhi_mokus_sf.R
#
# Purpose: Clean and standardize the raw marine MHI moku sf object for analysis and visualization.
#
# Usage:
#   1. Place this script in your project (e.g. R/helper_functions/02_processing/ directory).
#   2. Provide a raw sf object (e.g. from import_multiple_shapefiles()).
#   3. Call `tidy_marine_mhi_mokus_sf(raw_sf)` to return the cleaned tibble/sf object.
#
# =============================================================================

# # 1. Check for and load dependencies
# required_pkgs <- c("dplyr", "janitor", "tidyr", "stringr")
# base::invisible(base::lapply(required_pkgs, function(pkg) {
#   if (!base::requireNamespace(pkg, quietly = TRUE)) {
#     base::stop(base::sprintf("Please install the '%s' package to proceed.", pkg), call. = FALSE)
#   }
# }))

# 2. Function to clean and tidy the marine MHI moku sf object
#' Clean and standardize the raw marine MHI moku sf object
#'
#' @param raw_sf An sf object as read in from the raw shapefile.
#' @return A cleaned tibble/sf object with standardized and informative columns.
#' @examples
#' raw_marine_mhi_mokus_sf <- import_shapefiles(...)$himarc_mhi_marine_mokus
#' tidied_marine_mhi_mokus_sf <- tidy_marine_mhi_mokus_sf(raw_marine_mhi_mokus_sf)
tidy_marine_mhi_mokus_sf <- function(raw_sf) {
  raw_sf |>
    # Standardize column names
    janitor::clean_names() |>
    # Subset and rename columns
    dplyr::select(
      name2, island, moku_olelo, geometry
    ) |>
    # Separate `moku_olelo` into `island_olelo` and `moku_olelo` by first space
    tidyr::separate(
      moku_olelo,
      into = c("island_olelo", "moku_olelo"),
      sep = " ",
      remove = TRUE
    ) |>
    # Remove rows missing `name2`
    dplyr::filter(!is.na(name2)) |>
    # Assign or standardize values for special cases
    dplyr::mutate(
      island_olelo = dplyr::case_when(
        name2 == "MANA" ~ "Kauaʻi",
        name2 == "KAUPO" ~ "Maui",
        name2 == "KAHIKINUI" ~ "Maui",
        TRUE ~ island_olelo
      ),
      moku_olelo = dplyr::case_when(
        name2 == "MANA" ~ stringr::str_to_title(name2),
        name2 == "KAUPO" ~ "Kaupō",
        name2 == "KAHIKINUI" ~ stringr::str_to_title(name2),
        TRUE ~ moku_olelo
      ),
      # Create cleaned `moku` column from `name2`, standardize, and remove from space onward
      moku = stringr::str_replace(stringr::str_to_title(name2), " .*$", "")
    ) |>
    # Rearrange columns
    dplyr::select(moku, island, moku_olelo, island_olelo, name2, geometry)
}
