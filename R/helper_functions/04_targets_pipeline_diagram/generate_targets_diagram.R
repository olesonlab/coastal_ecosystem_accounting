# generate_targets_diagram.R
#
# Purpose: Provide a reusable function to create an interactive diagram
#          of your {targets} pipeline using tar_visnetwork().
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/ directory).
#   2. Source the file and call `generate_targets_diagram()`.
#   3. Use the returned widget in Shiny, R Markdown, Quarto, or save as HTML.
#
# =============================================================================

# # 1. Dependency checks
# required_pkgs <- c("targets", "visNetwork")
# invisible(lapply(required_pkgs, function(pkg) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     stop(sprintf("Please install the '%s' package.", pkg), call. = FALSE)
#   }
# }))

#' 2. Generate interactive {targets} pipeline diagram
#'
#' @param ... Additional arguments passed to `targets::tar_visnetwork()`
#' @return A visNetwork HTML widget of the pipeline diagram.
#' @examples
#' # Simple usage
#' diagram <- generate_targets_diagram()
generate_targets_diagram <- function(...) {
  tar_visnetwork(targets_only = TRUE, callr_function = NULL, ...)
}
