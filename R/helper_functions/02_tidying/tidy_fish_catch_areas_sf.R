# tidy_fish_catch_areas_sf.R
#
# Purpose: Clean and standardize the raw fish catch areas sf object for analysis and visualization.
#
# Usage:
#   1. Place this script in your project (e.g. R/helper_functions/02_processing/ directory).
#   2. Provide a raw sf object (e.g. from import_shapefiles()).
#   3. Call `tidy_fish_catch_areas_sf(raw_sf)` to return the cleaned tibble/sf object.
#
# =============================================================================

# 1. Check for and load dependencies
required_pkgs <- c("dplyr", "janitor")
base::invisible(base::lapply(required_pkgs, function(pkg) {
  if (!base::requireNamespace(pkg, quietly = TRUE)) {
    base::stop(base::sprintf("Please install the '%s' package to proceed.", pkg), call. = FALSE)
  }
}))

# 2. Function to clean and tidy the fish catch areas sf object
#' Clean and standardize the raw fish catch areas sf object
#'
#' @param raw_sf An sf object as read in from the raw shapefile.
#' @return A cleaned tibble/sf object with standardized and informative columns.
#' @examples
#' tidied_fish_catch_areas_sf <- tidy_fish_catch_areas_sf(raw_fish_catch_areas_sf)
tidy_fish_catch_areas_sf <- function(raw_sf) {
  raw_sf |>
    # Standardize column names
    janitor::clean_names() |>
    # Subset columns
    dplyr::select(island, area_id, type, geometry) |>
    # Filter to desired area types and islands (including NAs)
    dplyr::filter(
      type %in% c("Island", "MHI Inshore", "MHI Coastal"),
      island %in% c(
        "Kaho'olawe", "Lana'i", "Maui", "O'ahu", "Ni'ihau", "Kauai", "Hawai'i", "Moloka'i"
      ) | is.na(island),
      # Small islands near Oahu
      !(type == "Island" & is.na(island))
    )
}
