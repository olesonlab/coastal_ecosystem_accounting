# import_csv.R
#
# Purpose: Provide a reusable function for quietly importing CSV files using `readr::read_csv`,
#          with column type messages and progress bars suppressed for cleaner logs.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/01_importing/).
#   2. Source the file or call `import_csv()` directly with a valid file path.
#   3. Integrate it into `targets` pipelines, data prep scripts, or ad hoc analyses.
#
# =============================================================================

#' Import a CSV file quietly
#'
#' @param file_path A character string specifying the full path to the CSV file.
#'
#' @return A tibble containing the data from the CSV.
#'
#' @examples
#' df <- import_csv("data/raw/extents/marine_et_areas_per_moku.csv")
#'
#' @export
import_csv <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }
  
  readr::read_csv(
    file = file_path,
    show_col_types = FALSE,
    progress = FALSE
  )
}
