# create_file_path.R
#
# Purpose: Create a full file path using directory, file name, and extension,
#          using `here` and `glue` for cross-platform safe paths.
#
# Usage:
#   1. Place this script in your project (e.g., R/utils/).
#   2. Source the file or use tar_source() on your helpers folder.
#   3. Call create_file_path(dir, file_name, ext)
#
# =============================================================================

# # 1. Dependency checks
# required_pkgs <- c("here", "glue")
# invisible(lapply(required_pkgs, function(pkg) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     stop(sprintf("Please install the '%s' package to use create_file_path()", pkg), call. = FALSE)
#   }
# }))

#' Create a full file path from directory, file name, and extension
#'
#' @param dir Character. Directory (relative to project root).
#' @param file_name Character. File name (without extension).
#' @param ext Character. File extension (without the dot).
#'
#' @return Character. The full file path.
#' @examples
#' create_file_path("data/fisheries_valuation/processed/tabular", "20250527_tidied_comm_ev", "csv")
create_file_path <- function(dir, file_name, ext) {
  here::here(glue::glue("{dir}/{file_name}.{ext}"))
}
