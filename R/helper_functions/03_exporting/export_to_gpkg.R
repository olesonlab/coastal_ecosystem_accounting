# export_to_gpkg.R
#
# Purpose: Export one or more sf objects to GeoPackage (.gpkg) files.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/utils/).
#   2. Call `export_to_gpkg()` after loading your sf objects.
#   3. Pass in an sf object or a list of sf objects, optional file names, and output directory.
#
# ──────────────────────────────────────────────────────────────────────────────
# Imports
box::use(
  purrr[walk2],
  sf[st_write]
)
# ──────────────────────────────────────────────────────────────────────────────

#' Export sf object(s) to one or more GeoPackage files
#'
#' Writes a single sf or a list of sf objects to .gpkg files in the specified directory.
#'
#' @param layers      An sf object or a list of sf objects to export.
#' @param file_names  Character vector of file names (without extension). If `layers` is a named list,
#'                    names(layers) will be used when `file_names` is NULL.
#' @param dir         Directory path where the file(s) will be saved.
#'
#' @return NULL. Files are written as a side effect.
#' @export
export_to_gpkg <- function(layers, file_names = NULL, dir) {
  # Ensure output directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # Single sf object
  if (inherits(layers, "sf")) {
    if (!is.character(file_names) || length(file_names) != 1) {
      stop(
        "When exporting a single sf, 'file_names' must be a single string.",
        call. = FALSE
      )
    }
    out_path <- file.path(dir, paste0(file_names, ".gpkg"))
    sf::st_write(layers, out_path, delete_dsn = TRUE)
    
    # List of sf objects
  } else if (
    is.list(layers) &&
    all(vapply(layers, inherits, logical(1), what = "sf"))
  ) {
    # Determine file names
    if (is.null(file_names)) {
      if (is.null(names(layers)) || any(names(layers) == "")) {
        stop(
          "For multiple layers, supply 'file_names' or a named list of 'layers'.",
          call. = FALSE
        )
      }
      file_names <- names(layers)
    }
    if (length(file_names) != length(layers)) {
      stop(
        "Length of 'file_names' must match number of sf objects.",
        call. = FALSE
      )
    }
    
    # Export each layer
    purrr::walk2(
      layers, file_names,
      function(sf_obj, fn) {
        out_path <- file.path(dir, paste0(fn, ".gpkg"))
        sf::st_write(sf_obj, out_path, delete_dsn = TRUE)
      }
    )
    
  } else {
    stop(
      "'layers' must be either a single sf object or a list of sf objects.",
      call. = FALSE
    )
  }
  
  invisible(NULL)
}