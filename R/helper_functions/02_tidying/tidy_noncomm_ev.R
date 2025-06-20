# tidy_noncomm_ev.R
#
# Purpose: Provide a reusable function to clean non-commercial exchange value data
#          by standardizing column names, reshaping, filtering, and formatting species groups.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/02_processessing/ directory).
#   2. Source the file or call `tidy_noncomm_ev()` directly after loading your raw data frame.
#   3. Pass in the raw non-commercial exchange value data (e.g., `raw_noncomm_ev_df`).
#   4. Adjust prefixes or recoding logic as needed.
#
# =============================================================================

# # 1a. Dependency checks
# required_pkgs <- c("dplyr", "tidyr", "janitor", "stringr")
# base::invisible(base::lapply(required_pkgs, function(pkg) {
#   if (!base::requireNamespace(pkg, quietly = TRUE)) {
#     base::stop(base::sprintf("Please install package '%s' to clean exchange value data.", pkg), call. = FALSE)
#   }
# }))

# 1b. Utility functions
# base::source(here::here("R/helper_functions/02_processing/pivot_prefix_cols_longer.R"))

#' Tidy non-commercial exchange value data
#'
#' @param df A data frame or tibble containing raw non-commercial exchange value columns.
#' @return A cleaned tibble with columns: island, ecosystem_type, exchange_value, and any other originals.
#' @examples
#' raw_noncomm_data <- tibble::tibble(
#'   area = c("A", "B"),
#'   ev_reef = c(10, 0),
#'   ev_oo = c(5, 2),
#'   total = c(15, 2)
#' )
#' tidied_noncomm_ev <- tidy_noncomm_ev(raw_noncomm_ev)
tidy_noncomm_ev <- function(df) {
  # Validate input
  if (!base::inherits(df, "data.frame")) {
    base::stop("Input must be a data.frame or tibble.", call. = FALSE)
  }
  
  PREFIXES <- c("ev")
  
  df |>
    # Clean column names (e.g., Use lowercase and snakecase)
    janitor::clean_names() |> 
    # Convert from wide to long format
    pivot_prefix_cols_longer(
      # Specify which columns to pivot using partial strings in PREFIXES constant above
      prefixes = PREFIXES,
      # Change column names converted to column to `ecosystem_type`
      names_to = "ecosystem_type",
      # Change column names converted to column to `exchange_value`
      values_to = "exchange_value"
    ) |>
    dplyr::mutate(
      # Format to match moku spatial df
      island = stringr::str_to_title(island),
      # Remove unnecessary partial string
      ecosystem_type = stringr::str_remove(ecosystem_type, "^ev_total_"),
      # Format string values
      ecosystem_type = stringr::str_to_title(ecosystem_type),
      ecosystem_type = dplyr::if_else(
        ecosystem_type == "Oo", "Open Ocean", ecosystem_type
      ),
      ecosystem_type = dplyr::if_else(
        ecosystem_type == "Ev_total", "All Ecosystems", ecosystem_type
      ),
      # Specify species group
      species_group = "Herbivore",
      # NAs withheld by DAR, zeros are meaningful
      withheld = is.na(exchange_value)
    ) |> 
    # Relocate columns
    dplyr::select(
      year, island, ecosystem_type, species_group, exchange_value, withheld
    ) 
}