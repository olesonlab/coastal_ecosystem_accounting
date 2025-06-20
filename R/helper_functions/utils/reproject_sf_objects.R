# reproject_sf_object.R
#
# Purpose: Reproject an sf object to a specified CRS for accurate spatial analysis and mapping.
#
# Usage:
#   1. Place this script in your project (e.g. R/helper_functions/02_processing/ directory).
#   2. Provide an sf object and the target CRS (EPSG code or PROJ string).
#   3. Call `reproject_sf_object(your_sf, target_crs)` to get a reprojected sf object.
#
# =============================================================================

# # 1. Check for and load dependencies
# required_pkgs <- c("sf")
# base::invisible(base::lapply(required_pkgs, function(pkg) {
#   if (!base::requireNamespace(pkg, quietly = TRUE)) {
#     base::stop(base::sprintf("Please install the '%s' package to proceed.", pkg), call. = FALSE)
#   }
# }))

# 2. Function to reproject an sf object
#' Reproject an sf object to a specified CRS
#'
#' @param sf_object An sf object to reproject.
#' @param target_crs The target CRS (can be an EPSG code, PROJ string, or another CRS object).
#' @return The reprojected sf object.
#' @examples
#' reprojected_sf <- reproject_sf_object(my_sf, 3563) # To Hawaii Albers Equal Area
#' reprojected_sf <- reproject_sf_object(my_sf, 4326) # To WGS 84 (EPSG:4326) for visualizations
reproject_sf_object <- function(sf_object, target_crs) {
  sf::st_transform(sf_object, crs = target_crs)
}
