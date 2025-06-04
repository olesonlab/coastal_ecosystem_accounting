# export_to_svg.R
#
# Purpose: Provide a reusable function to export an interactive HTML widget
#          (e.g., visNetwork, plotly, leaflet) as a standalone HTML file.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/ directory).
#   2. Source the file and call `export_to_svg(widget, "output.html")`.
#
# =============================================================================

# # 1. Dependency checks
# required_pkgs <- c("DiagrammeR")
# invisible(lapply(required_pkgs, function(pkg) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     stop(sprintf("Please install the '%s' package.", pkg), call. = FALSE)
#   }
# }))

#' 2. Export an HTML widget as a standalone HTML file
#'
#' @param widget The HTML widget object (e.g., from visNetwork, plotly, etc.).
#' @param file_path The file path (character string) for the HTML file to save.
#' @return Invisibly returns the file name.
#' @examples
#' widget <- generate_targets_diagram()
#' export_to_svg(widget, "targets_pipeline.html")
#'
export_to_svg <- function(widget, file_path) {
  htmlwidgets::saveWidget(
    widget, file_path, selfcontained = TRUE
  )
  invisible(file_path)
}
