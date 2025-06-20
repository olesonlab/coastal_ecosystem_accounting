# import_shapefiles.R
#
# Purpose: Load multiple shapefiles (each in its own subfolder) from a specified directory
#          into a named list of sf objects for downstream spatial analysis.
#
# Usage:
#   1. Place this script in your project (e.g. R/helper_functions/01_importing/ directory).
#   2. Adjust `root_dir` and provide `shapefile_names` (vector of base folder/file names).
#   3. Call `import_shapefiles(root_dir, shapefile_names)` to read in all shapefiles.
#
# =============================================================================

# # 1. Check for and load dependencies
# required_pkgs <- c("sf", "fs", "here")
# base::invisible(base::lapply(required_pkgs, function(pkg) {
#   if (!base::requireNamespace(pkg, quietly = TRUE)) {
#     base::stop(base::sprintf("Please install the '%s' package to proceed.", pkg), call. = FALSE)
#   }
# }))

# 2. Function to load multiple shapefiles from a directory of subfolders
#' Load multiple shapefiles into a named list of sf objects
#'
#' @param root_dir Character. Path to the directory containing shapefile folders.
#' @param shapefile_names Character vector. Base names for shapefiles (e.g. "foo" for "foo/foo.shp").
#' @return A named list of sf objects, one per shapefile.
#' @examples
#' root_dir <- here::here("data", "raw", "mhi_mokus")
#' shapefile_names <- c("himarc_mhi_marine_mokus", "himarc_mhi_terrestrial_mokus")
#' mhi_mokus_sf <- import_shapefiles(root_dir, shapefile_names)
import_shapefiles <- function(root_dir, shapefile_names) {
  # Check root directory exists
  if (!fs::dir_exists(root_dir)) {
    base::stop("Directory not found: ", root_dir, call. = FALSE)
  }
  
  # Build expected shapefile paths (subfolder/base/base.shp)
  shapefile_paths <- base::file.path(
    root_dir,
    shapefile_names,
    paste0(shapefile_names, ".shp")
  )
  
  # Check each .shp file exists
  missing <- !fs::file_exists(shapefile_paths)
  if (base::any(missing)) {
    base::stop("File(s) not found: ", paste(shapefile_paths[missing], collapse = ", "), call. = FALSE)
  }
  
  # Read in each shapefile as sf object (suppress messages)
  sf_list <- base::lapply(shapefile_paths, function(path) {
    sf::st_read(path, quiet = TRUE)
  })
  
  # Name the list elements by base name
  base::names(sf_list) <- shapefile_names
  sf_list
}