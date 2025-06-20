# export_to_gpkg.R
#
# Purpose: Export an sf object to a GeoPackage (.gpkg) file with a specified filename and directory.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/utils/ directory).
#   2. Source the file or call `export_to_gpkg()` after loading your sf object.
#   3. Pass in your sf object, desired file name (without extension), and output directory.
#
# =============================================================================

# 1. Dependency checks
required_pkgs <- c("sf", "glue")
invisible(lapply(required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Please install the '%s' package.", pkg), call. = FALSE)
  }
}))

#' Export an sf object to a GeoPackage file
#'
#' Saves a given sf object as a .gpkg file in the specified directory.
#'
#' @param sf        An sf object to export.
#' @param file_name File name to use (without extension).
#' @param dir       Directory path where the file will be saved.
#'
#' @return NULL. Writes the file as a side effect.
#' @examples
#' \dontrun{
#' export_to_gpkg(
#'   sf        = my_sf_object,
#'   file_name = "my_data",
#'   dir       = "data/outputs"
#' )
#' }# export_to_gpkg.R
#
# Purpose: Export an sf object to a GeoPackage (.gpkg) file with a specified filename and directory.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/utils/ directory).
#   2. Source the file or call `export_to_gpkg()` after loading your sf object.
#   3. Pass in your sf object, desired file name (without extension), and output directory.
#
# =============================================================================

# 1. Dependency checks
required_pkgs <- c("sf", "glue")
invisible(lapply(required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Please install the '%s' package.", pkg), call. = FALSE)
  }
}))

#' Export an sf object to a GeoPackage file
#'
#' Saves a given sf object as a .gpkg file in the specified directory.
#'
#' @param sf        An sf object to export.
#' @param file_name File name to use (without extension).
#' @param dir       Directory path where the file will be saved.
#'
#' @return NULL. Writes the file as a side effect.
#' @examples
#' export_to_gpkg(
#'   sf        = my_sf_object,
#'   file_name = "my_data",
#'   dir       = "data/outputs"
#' )
export_to_gpkg <- function(sf, file_name, dir) {
  # Use glue to safely combine directory and file name
  file_path <- glue::glue("{dir}/{file_name}.gpkg")
  
  # Write the sf object to a GeoPackage, overwriting if exists
  sf::st_write(sf, file_path, delete_dsn = TRUE)
  
  invisible(NULL)
}

export_to_gpkg <- function(sf, file_name, dir) {
  # Use glue to safely combine directory and file name
  file_path <- glue::glue("{dir}/{file_name}.gpkg")
  
  # Write the sf object to a GeoPackage, overwriting if exists
  sf::st_write(sf, file_path, delete_dsn = TRUE)
  
  invisible(NULL)
}