# get_todays_date.R
#
# Purpose: Get today's date in YYYYMMDD format as a character string.
#
# Usage:
#   1. Place this script in your project (e.g., R/utils/global_helpers/).
#   2. Source the file or use tar_source() on your helpers folder.
#   3. Call get_todays_date() anywhere in your code.
#
# =============================================================================

# # 1. Dependency checks (not strictly needed for base functions, but for style consistency)
# required_pkgs <- c("base")
# invisible(lapply(required_pkgs, function(pkg) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     stop(sprintf("Please install the '%s' package to use get_todays_date()", pkg), call. = FALSE)
#   }
# }))

#' Get today's date in YYYYMMDD format
#'
#' @return Character string with today's date as YYYYMMDD.
#' @examples
#' get_todays_date()
get_todays_date <- function() {
  base::format(base::Sys.Date(), "%Y%m%d")
}