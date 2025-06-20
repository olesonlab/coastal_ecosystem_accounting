# tidy_comm_ev.R
#
# Purpose: Provide a reusable function to tidy commercial exchange value data
#          by removing all-zero columns, pivoting specified prefixes to long,
#          splitting species and ecosystem types, and standardizing labels.
#
# Usage:
#   1. Place this script in your project (e.g., R/helper_functions/02_processessing/ directory).
#   2. Source the file and call `tidy_comm_ev()` with your raw dataframe.
#   3. Pass in the raw commercial exchange value data (e.g., `raw_comm_ev_df`).
#   4. Adjust prefixes or recoding logic as needed.
#
# =============================================================================

# # 1a. Dependency checks
# required_pkgs <- c("dplyr", "tidyr", "janitor", "stringr")
# base::invisible(base::lapply(required_pkgs, function(pkg) {
#   if (!base::requireNamespace(pkg, quietly = TRUE)) {
#     base::stop(base::sprintf("Please install package '%s' to use tidy_comm_ev()", pkg), call. = FALSE)
#   }
# }))

# 1b. Utility functions
# base::source(here::here("R/helper_functions/utils/pivot_prefix_cols_longer.R"))

#' Tidy commercial exchange value data
#'
#' @param df A data.frame or tibble containing raw commercial exchange value columns.
#' @param prefixes Character vector of prefixes to pivot (default: c("ev", "total")).
#' @return A tidied tibble with columns: area_id (if exists), species_group, ecosystem_type, exchange_value.
#' @examples
#' raw_comm_data <- tibble::tibble(
#'   area = c("A", "B"),
#'   ev_reef = c(10, 0),
#'   ev_oo = c(5, 2),
#'   total = c(15, 2)
#' )
#' tidied_comm_data <- tidy_comm_ev(raw_comm_data)
tidy_comm_ev <- function(df, prefixes = c("ev", "total")) {
  if (!base::inherits(df, "data.frame")) {
    base::stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }
  
  PREFIXES <- c("ev")
  
  df |>
    # Clean column names (e.g., Use lowercase and snakecase)
    janitor::clean_names() |>
    # Convert from wide to long format
    pivot_prefix_cols_longer(
      # Specify which columns to pivot using partial strings in PREFIXES constant above
      prefixes = PREFIXES,
      # Change column names converted to column to `species_group`
      names_to = "species_group",
      # Change values within pivoted columns to `exchange_value`
      values_to = "exchange_value"
    ) |>
    # Remove unnecessary partial string
    dplyr::mutate(species_group = stringr::str_remove(species_group, "^ev_")) |>
    # Split column's string values into separate columns
    tidyr::separate(
      col = "species_group",
      into = c("species_group", "ecosystem_type"),
      sep = "_",
      fill = "right"
    ) |>
    dplyr::mutate(
      # Rename column to match DAR catch spatial df
      "area_id" = area,
      # Format to match moku spatial df
      county = stringr::str_to_title(county),
      # Clean species group names
      species_group = dplyr::case_when(
        species_group == "deep7" ~ "Deep 7 Bottomfish",
        species_group == "sbf" ~ "Shallow Bottomfish",
        species_group == "pelagic" ~ "Pelagics",
        species_group == "reef" ~ "Reef-Associated",
        species_group == "total" ~ "All Species",
        TRUE ~ stringr::str_to_title(species_group)
      ),
      # Clean ecosystem type names
      ecosystem_type = dplyr::case_when(
        ecosystem_type == "oo" ~ "Open Ocean",
        ecosystem_type == "reef" ~ "Reef",
        ecosystem_type == "ev" ~ "All Ecosystems",
        base::is.na(ecosystem_type) ~ "All Ecosystems"
      ),
      # NAs withheld by DAR, zeros are meaningful
      withheld = is.na(exchange_value)
    ) |> 
    # Relocate columns
    dplyr::select(
      year, county, area_id, ecosystem_type, species_group, exchange_value, withheld
    )
}