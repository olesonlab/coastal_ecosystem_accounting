# fill_gaps_between_sfs.R
#
# Purpose: Fill small spatial gaps between two adjacent sf polygon layers (e.g., marine and terrestrial)
#          by buffering, unioning, and regrouping polygons by a specified shared column.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/utils/ directory).
#   2. Source the file or call `fill_gaps_between_sfs()` after loading your sf objects.
#   3. Pass in the marine and terrestrial sf objects, buffer distance (in CRS units), and the grouping column name.
#
# =============================================================================

# 1. Dependency checks
required_pkgs <- c("sf", "dplyr")
invisible(lapply(required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Please install the '%s' package.", pkg), call. = FALSE)
  }
}))

#' Fill gaps between two sf polygon layers by grouping attribute.
#'
#' Buffers, unions, and regroups two adjacent polygon layers, closing small spatial gaps
#' along their shared boundaries, by grouping on a shared column.
#'
#' @param marine_sf      An sf object of marine polygons.
#' @param terrestrial_sf An sf object of terrestrial polygons.
#' @param buffer_dist    Numeric. Buffer distance in CRS units (meters for most projected CRSs).
#' @param group_col      Character. Name of the grouping column (shared in both layers).
#'
#' @return An sf object with polygons grouped and unioned by \code{group_col}, with gaps filled.
#' @examples
#' # Example usage:
#' filled_sf <- fill_gaps_between_sfs(
#'   marine_sf = marine_layer,
#'   terrestrial_sf = terrestrial_layer,
#'   buffer_dist = 30, # meters
#'   group_col = "name2" # grouping column present in both sf objects
#' )
#'
#' # Visualize result
#' mapview::mapview(filled_sf, zcol = "name2")
fill_gaps_between_sfs <- function(marine_sf, terrestrial_sf, buffer_dist, group_col) {
  # Buffer marine polygons outward to close small spatial gaps
  marine_buffer <- sf::st_buffer(marine_sf, buffer_dist)
  
  # Buffer terrestrial polygons outward to close small spatial gaps
  terrestrial_buffer <- sf::st_buffer(terrestrial_sf, buffer_dist)
  
  # Combine both buffered layers into a single sf object
  combined_buffers <- dplyr::bind_rows(marine_buffer, terrestrial_buffer)
  
  # Group by the specified column and union all geometries in each group
  # This merges overlapping polygons for each group (e.g., each moku)
  filled <- combined_buffers |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      geometry = sf::st_union(geometry),
      dplyr::across(dplyr::everything(),  # for every non‐geometry column
             ~ dplyr::first(.x)),   # grab its first value in the group
      .groups = "drop"
    ) |> 
    # Buffer inward to restore polygons to original size, closing the gaps
    sf::st_buffer(-buffer_dist) |>
    # Ensure valid geometry output
    sf::st_make_valid()
  
  # Return the gap-filled, grouped polygons
  return(filled)
}

