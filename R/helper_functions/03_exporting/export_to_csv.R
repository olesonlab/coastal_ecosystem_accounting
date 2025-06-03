# export_to_csv.R
#
# Purpose: Export a data frame to a CSV file at a user-specified directory and file name,
#          using a helper to construct the file path.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/03_exporting/ directory).
#   2. Source this file and the `create_file_path.R` helper.
#   3. Call export_to_csv(df, dir, file_name).
#
# =============================================================================

# # 1. Dependency checks
# required_pkgs <- c("utils", "here", "glue")
# invisible(lapply(required_pkgs, function(pkg) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     stop(sprintf("Please install the '%s' package to use export_to_csv()", pkg), call. = FALSE)
#   }
# }))

# 2. Source the create_file_path() helper
source(here::here("R/helper_functions/utils/create_file_path.R"))

#' Export a data frame to CSV in a specified directory with a custom file name.
#'
#' @param df A data.frame or tibble to write.
#' @param dir Character. The directory to save the file (relative to project root).
#' @param file_name Character. The file name without the .csv extension.
#'
#' @return The file path of the exported CSV (invisibly).
#' @examples
#' export_to_csv(
#'   tidied_noncomm_ev_df,
#'   dir = "data/fisheries_valuation/processed/tabular/",
#'   file_name = paste0(format(Sys.Date(), "%Y%m%d"), "_tidied_noncomm_ev")
#' )
export_to_csv <- function(df, dir, file_name) {
  file_path <- create_file_path(dir = dir, file_name = file_name, ext = "csv")
  utils::write.csv(df, file = file_path, row.names = FALSE)
  invisible(file_path)
}
