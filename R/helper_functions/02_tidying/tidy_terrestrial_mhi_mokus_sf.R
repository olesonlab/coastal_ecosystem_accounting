# tidy_terrestrial_mhi_mokus_sf.R
#
# Purpose: Clean and standardize the raw terrestrial MHI moku sf object for analysis and visualization.
#
# Usage:
#   1. Place this script in your project (e.g. R/helper_functions/02_processing/ directory).
#   2. Provide a raw sf object (e.g. from import_shapefiles()).
#   3. Call `tidy_terrestrial_mhi_mokus_sf(raw_sf)` to return the cleaned tibble/sf object.
#
# =============================================================================

# # 1. Check for and load dependencies
# required_pkgs <- c("dplyr", "janitor")
# base::invisible(base::lapply(required_pkgs, function(pkg) {
#   if (!base::requireNamespace(pkg, quietly = TRUE)) {
#     base::stop(base::sprintf("Please install the '%s' package to proceed.", pkg), call. = FALSE)
#   }
# }))

# 2. Function to clean and tidy the terrestrial MHI moku sf object
#' Clean and standardize the raw terrestrial MHI moku sf object
#'
#' @param raw_sf An sf object as read in from the raw shapefile.
#' @return A cleaned tibble/sf object with standardized and informative columns.
#' @examples
#' raw_terrestrial_mhi_mokus_sf <- import_multiple_shapefiles(...)$himarc_mhi_terrestrial_mokus
#' tidied_terrestrial_mhi_mokus_sf <- tidy_terrestrial_mhi_mokus_sf(raw_terrestrial_mhi_mokus_sf)
tidy_terrestrial_mhi_mokus_sf <- function(raw_sf) {
  raw_sf |>
    # Standardize column names
    janitor::clean_names() |>
    # Subset and order columns
    dplyr::select(name2, island, geometry)
}
