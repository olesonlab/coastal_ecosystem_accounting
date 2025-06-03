# import_xlsx_sheets.R
#
# Purpose: Load every worksheet from a specified Excel (.xlsx) file
#          into a named list of data frames for downstream analysis.
#
# Usage:
#   1. Place this script in your project (e.g. R/helper_functions/01_importing/ directory).
#   2. Adjust the `xlsx_path` to point to your workbook.
#   3. Call `import_xlsx_sheets(xlsx_path)` to read in all sheets.
#
# =============================================================================

# # 1. Check for and load dependencies
# required_pkgs <- c("readxl", "fs", "here")
# base::invisible(base::lapply(required_pkgs, function(pkg) {
#   if (!base::requireNamespace(pkg, quietly = TRUE)) {
#     base::stop(base::sprintf("Please install the '%s' package to proceed.", pkg), call. = FALSE)
#   }
# }))

# 2. Function to load all sheets from an Excel file
#' Load all worksheets from an .xlsx file into a named list of data frames
#'
#' @param file_path Character. Path to the .xlsx file.
#' @return A named list of data frames, one per sheet.
#' @examples
#' file_path <- here::here("data", "fisheries_valuation", "raw", "mydata.xlsx")
#' raw_df <- import_xlsx_sheets(file_path)
import_xlsx_sheets <- function(file_path) {
  # Ensure the file exists
  if (!fs::file_exists(file_path)) {
    base::stop("File not found: ", file_path, call. = FALSE)
  }
  
  # Discover sheet names
  sheet_names <- readxl::excel_sheets(file_path)
  
  # Read each sheet into a data frame
  data_list <- base::lapply(sheet_names, function(sheet) {
    readxl::read_excel(
      path  = file_path,
      sheet = sheet
    )
  })
  
  # Name the list elements by sheet
  base::names(data_list) <- sheet_names
  data_list
}