# pivot_prefix_cols_longer.R
#
# Purpose: Convert wide-format columns with specified prefixes into a long-format table,
#          with customizable name and value column names.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/02_processessing/ directory).
#   2. Source the file or call `pivot_prefix_cols_longer()` after loading your data frame.
#   3. Pass in the data frame and optional prefixes (default: "ev").
#
# =============================================================================

# # 1. Dependency checks
# required_pkgs <- c("dplyr", "tidyr", "stringr")
# invisible(lapply(required_pkgs, function(pkg) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     stop(sprintf("Please install the '%s' package.", pkg), call. = FALSE)
#   }
# }))

#' Pivot columns starting with specified prefixes into a long format.
#'
#' @param df A data.frame or tibble.
#' @param prefixes Character vector of prefixes to match (e.g. c("ev", "total")). Default: "ev".
#' @param names_to  Name for the new column of pivoted column names. Default: "key".
#' @param values_to Name for the new column of pivoted values. Default: "value".
#' @return A tibble in long format containing the pivoted data.
#' @examples
#' df <- tibble::tibble(
#'   id           = 1:3,
#'   ev_a         = c(10, 20, 30),
#'   ev_b         = c(1, 2, 3),
#'   total_other  = c(5, 5, 5)
#' )
#' # Pivot columns starting with "ev"
#' long1 <- pivot_prefix_cols_longer(df, prefixes = "ev", names_to = "type", values_to = "value")
#' # Pivot columns starting with "ev" or "total"
#' long2 <- pivot_prefix_cols_longer(df, prefixes = c("ev", "total"), names_to = "type", values_to = "value")
pivot_prefix_cols_longer <- function(df,
                                     prefixes = "ev",
                                     names_to  = "key",
                                     values_to = "value") {
  if (!base::inherits(df, "data.frame")) {
    base::stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }
  if (!base::is.character(prefixes) || base::length(prefixes) < 1) {
    base::stop("`prefixes` must be a non-empty character vector.", call. = FALSE)
  }
  
  pattern <- base::paste0("^(?:", base::paste(prefixes, collapse = "|"), ")")
  
  df |> 
    tidyr::pivot_longer(
      cols      = tidyselect::matches(pattern),
      names_to  = names_to,
      values_to = values_to
    )
}