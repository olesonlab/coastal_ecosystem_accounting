# extract_single_xlsx_sheet.R
#
# Purpose: Extract a specific sheet (by name) from a named list of Excel sheets.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/01_importing/.
#   2. Source the file or use tar_source() on your helpers folder.
#   3. Call extract_single_xlsx_sheet(sheet_list, sheet_name)
#
# =============================================================================

# # 1. Dependency checks (for consistency)
# required_pkgs <- c()
# invisible(lapply(required_pkgs, function(pkg) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     stop(sprintf("Please install package '%s' to use extract_single_xlsx_sheet()", pkg), call. = FALSE)
#   }
# }))

#' Extract a single sheet by name from a named list of Excel sheets
#'
#' @param sheets A named list of data frames as returned by import_xlsx_sheets().
#' @param sheet_name Character. Name of the sheet to extract.
#'
#' @return The specified sheet as a data frame/tibble.
#' @examples
#' # Assuming `sheets` is a named list:
#' # raw_comm_ev <- extract_single_xlsx_sheet(sheets, "Comm")
extract_single_xlsx_sheet <- function(sheets, sheet_name) {
  if (!is.list(sheets)) {
    stop("`sheets` must be a named list of data frames.", call. = FALSE)
  }
  if (!sheet_name %in% names(sheets)) {
    stop(sprintf("Sheet '%s' not found in the Excel file.", sheet_name), call. = FALSE)
  }
  sheets[[sheet_name]]
}