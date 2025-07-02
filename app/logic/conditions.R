# app/logic/conditions.R
box::use(
  dplyr[filter, select, distinct, case_when, mutate, arrange, desc],
  stringr[str_replace, str_remove],
  tidyr[pivot_wider],
  reactable[reactable, colDef, colFormat]
)

box::use(
  app/logic/utils[
    add_formatted_moku_names, 
    fix_okinas, 
    filter_metric, 
    tidy_rainfall_data,
    prepare_change_data,
    create_generic_change_table
  ]
)

#' @export
fetch_mean_max_rainfall_data <- function(all_data) {
  data <- all_data$terrestrial_conditions_data |> 
    tidy_rainfall_data(
      moku_names_formatted_df = all_data$formatted_moku_names,  
      filter_metric = c("mean", "max"), 
      extract_year = TRUE
    )
}

#' @export
fetch_baseline_rainfall_changes_data <- function(all_data) {
  data <- all_data$terrestrial_conditions_data |> 
    tidy_rainfall_data(
      moku_names_formatted_df = all_data$formatted_moku_names,
      filter_metric = c("three", "baseline"), 
      extract_year = FALSE
    ) 
}

#' @export
fetch_rainfall_change_summary_data <- function(all_data) {
  data <- all_data$terrestrial_conditions_data |>
    tidyr::pivot_wider(
      names_from = metric,
      values_from = value
    ) |>
    add_formatted_moku_names(all_data$formatted_moku_names) |>
    dplyr::mutate(
      moku_olelo = fix_okinas(moku_olelo),
      island_olelo = fix_okinas(island_olelo)
    ) |>
    # Add absolute percentage change for ranking - handle NAs
    dplyr::mutate(
      abs_pct_2013_2016 = ifelse(is.na(pct_change_2013_2016), 0, abs(pct_change_2013_2016)),
      abs_pct_2013_2019 = ifelse(is.na(pct_change_2013_2019), 0, abs(pct_change_2013_2019)),
      abs_pct_2016_2019 = ifelse(is.na(pct_change_2016_2019), 0, abs(pct_change_2016_2019))
    ) |>
    # Sort by island, moku, then by highest absolute percentage change
    dplyr::arrange(island_olelo, moku_olelo, desc(abs_pct_2013_2019)) |>
    # Select only the columns we need for the table
    dplyr::select(
      island_olelo, moku_olelo, ecosystem_type,
      # Mean rainfall columns (if they exist)
      any_of(c("mean_rainfall_mm_2013", "mean_rainfall_mm_2016", "mean_rainfall_mm_2019")),
      # Max rainfall columns (if they exist)  
      any_of(c("max_rainfall_mm_2013", "max_rainfall_mm_2016", "max_rainfall_mm_2019")),
      # Change columns
      any_of(c("change_mm_2013_2016", "change_mm_2013_2019", "change_mm_2016_2019")),
      # Percentage change columns
      any_of(c("pct_change_2013_2016", "pct_change_2013_2019", "pct_change_2016_2019")),
      # Direction columns (needed for coloring)
      any_of(c("dir_2013_2016", "dir_2013_2019", "dir_2016_2019"))
    )
  
  return(data)
}
#' @export
rainfall_change_summary_table <- function(all_data) {
  df <- fetch_rainfall_change_summary_data(all_data)
  create_generic_change_table(df, title = "Mean Rainfall Change (mm & %)")
}

# # app/logic/conditions.R
# box::use(
#   dplyr[filter, select, distinct, case_when, mutate, arrange, desc],
#   stringr[str_replace, str_remove],
#   tidyr[pivot_wider],
#   reactable[reactable, colDef, colFormat]
# )

# box::use(
#   app/logic/utils[
#     add_formatted_moku_names, 
#     fix_okinas, 
#     filter_metric, 
#     tidy_rainfall_data,
#     prepare_change_data,
#     create_generic_change_table
#   ]
# )

# #' @export
# fetch_mean_max_rainfall_data <- function(all_data) {
#   data <- all_data$terrestrial_conditions_data |> 
#     tidy_rainfall_data(
#       moku_names_formatted_df = all_data$formatted_moku_names,  
#       filter_metric = c("mean", "max"), 
#       extract_year = TRUE
#     )
# }

# #' @export
# fetch_baseline_rainfall_changes_data <- function(all_data) {
#   data <- all_data$terrestrial_conditions_data |> 
#     tidy_rainfall_data(
#       moku_names_formatted_df = all_data$formatted_moku_names,
#       filter_metric = c("three", "baseline"), 
#       extract_year = FALSE
#     ) 
# }

# #' @export
# fetch_rainfall_change_data_simple <- function(all_data) {
#   data <- all_data$terrestrial_conditions_data |> 
#     tidyr::pivot_wider(
#       names_from = metric,
#       values_from = value
#     ) |> 
#     add_formatted_moku_names(all_data$formatted_moku_names) |> 
#     dplyr::mutate(
#       moku_olelo = fix_okinas(moku_olelo),
#       island_olelo = fix_okinas(island_olelo)
#     )
  
#   # Manual sorting and column selection (avoid prepare_change_data for now)
#   data |>
#     # Add absolute percentage change for ranking (handle missing columns)
#     dplyr::mutate(
#       abs_pct_2013_2016 = ifelse(is.na(pct_change_2013_2016), 0, abs(pct_change_2013_2016)),
#       abs_pct_2013_2019 = ifelse(is.na(pct_change_2013_2019), 0, abs(pct_change_2013_2019)),
#       abs_pct_2016_2019 = ifelse(is.na(pct_change_2016_2019), 0, abs(pct_change_2016_2019))
#     ) |>
#     # Sort by island, moku, then by highest absolute percentage change
#     dplyr::arrange(island_olelo, moku_olelo, desc(abs_pct_2013_2019))
# }

# #' @export
# rainfall_change_table_simple <- function(all_data) {
#   df <- fetch_rainfall_change_data_simple(all_data)
#   create_generic_change_table(df, title = "Mean Rainfall Change (mm & %)")
# }

# #' @export
# fetch_rainfall_change_data <- function(all_data) {
#   # Check if terrestrial_conditions_data exists and is not NULL
#   if (is.null(all_data$terrestrial_conditions_data)) {
#     stop("terrestrial_conditions_data is NULL in all_data. Available keys: ", 
#          paste(names(all_data), collapse = ", "))
#   }
  
#   data <- all_data$terrestrial_conditions_data |> 
#     tidyr::pivot_wider(
#       names_from = metric,
#       values_from = value
#     ) |> 
#     add_formatted_moku_names(all_data$formatted_moku_names) |> 
#     dplyr::mutate(
#       moku_olelo = fix_okinas(moku_olelo),
#       island_olelo = fix_okinas(island_olelo)
#     ) |>
#     prepare_change_data(group_by_cols = c("island_olelo", "moku_olelo")) 
  
#   return(data)
# }

# #' @export
# rainfall_change_table <- function(all_data) {
#   df <- fetch_rainfall_change_data(all_data)
#   create_generic_change_table(df, title = "Mean Rainfall Change (mm & %)")
# }

# #' @export
# fetch_rainfall_change_summary_data <- function(
#   all_data, period = NULL, island = NULL, ecosystem_type = NULL
# ) {
#   data <- all_data$terrestrial_conditions_data |>
#     add_formatted_moku_names(all_data$formatted_moku_names) |>
#     filter_metric(c("change", "dir", "pct")) |>
#     dplyr::mutate(
#       year_range = stringr::str_replace(metric, ".*_(\\d{4})_(\\d{4})$", "\\1-\\2"),
#       year_range = factor(year_range, levels = c("2013-2016","2013-2019","2016-2019")),
#       metric     = stringr::str_remove(metric, "_\\d{4}_\\d{4}$")
#     ) |>
#     dplyr::distinct(
#       name2, ecosystem_type, moku, island, moku_olelo, island_olelo, year_range, metric, value
#     ) |>
#     tidyr::pivot_wider(
#       id_cols     = c(name2, ecosystem_type, moku, island, moku_olelo, island_olelo, year_range),
#       names_from  = metric,
#       values_from = value
#     ) |> 
#     dplyr::mutate(
#       dir = dplyr::case_when(
#         dir == -1 ~ "Negative",
#         dir == 0 ~ "No Change",
#         dir == 1 ~ "Positive"
#       ),
#       moku_olelo = fix_okinas(moku_olelo),
#       island_olelo = fix_okinas(island_olelo)
#     )
# }

# #' @export
# rainfall_change_summary_table <- function(all_data) {
#   # Use the generic functions to create a table that looks like the extents table
#   df <- fetch_rainfall_change_data(all_data)
#   create_generic_change_table(df, title = "Mean Rainfall Change (mm & %)")
# }

# # app/logic/conditions.R
# box::use(
#   dplyr[filter, select, distinct, case_when, mutate, arrange, desc],
#   stringr[str_replace, str_remove],
#   tidyr[pivot_wider],
#   reactable[reactable, colDef, colFormat]
# )

# box::use(
#   app/logic/utils[
#     add_formatted_moku_names, fix_okinas, filter_metric, tidy_rainfall_data
#   ]
# )

# #' @export
# fetch_mean_max_rainfall_data <- function(all_data) {
#   data <- all_data$terrestrial_conditions_data |> 
#     tidy_rainfall_data(
#     moku_names_formatted_df = all_data$formatted_moku_names,  
#     filter_metric = c("mean", "max"), 
#     extract_year = TRUE
#     )
# }

# #' @export
# fetch_baseline_rainfall_changes_data <- function(all_data) {
#   data <- all_data$terrestrial_conditions_data |> 
#     tidy_rainfall_data(
#       moku_names_formatted_df = all_data$formatted_moku_names,
#       filter_metric = c("three", "baseline"), 
#       extract_year = FALSE
#     ) 
# }

# #' @export
# fetch_rainfall_change_summary_data <- function(
#   all_data, period = NULL, island = NULL, ecosystem_type = NULL
# ) {
#   data <- all_data$terrestrial_conditions_data |>
#     add_formatted_moku_names(all_data$formatted_moku_names) |>
#     filter_metric(c("change", "dir", "pct")) |>
#     dplyr::mutate(
#       year_range = stringr::str_replace(metric, ".*_(\\d{4})_(\\d{4})$", "\\1-\\2"),
#       year_range = factor(year_range, levels = c("2013-2016","2013-2019","2016-2019")),
#       metric     = stringr::str_remove(metric, "_\\d{4}_\\d{4}$")
#     ) |>
#     dplyr::distinct(
#       name2, ecosystem_type, moku, island, moku_olelo, island_olelo, year_range, metric, value
#     ) |>
#     tidyr::pivot_wider(
#       id_cols     = c(name2, ecosystem_type, moku, island, moku_olelo, island_olelo, year_range),
#       names_from  = metric,
#       values_from = value
#     ) |> 
#     dplyr::mutate(
#       dir = dplyr::case_when(
#         dir == -1 ~ "Negative",
#         dir == 0 ~ "No Change",
#         dir == 1 ~ "Positive"
#       ),
#       moku_olelo = fix_okinas(moku_olelo),
#       island_olelo = fix_okinas(island_olelo)
#     )
# }

# #' @export
# rainfall_change_summary_table <- function(all_data) {
#   df <- fetch_rainfall_change_summary_data(all_data) |>
#     mutate(abs_pct_change = abs(pct_change)) |>
#     arrange(
#       ecosystem_type,
#       desc(abs_pct_change)
#     ) |>
#     select(
#       ecosystem_type, year_range, island_olelo, moku_olelo,
#       change_mm, pct_change, abs_pct_change
#     )

#   reactable(
#     df,
#     groupBy = c("year_range", "ecosystem_type", "island_olelo"),
#     defaultSorted = "abs_pct_change",
#     defaultSortOrder = "desc",
#     columns = list(
#       ecosystem_type = colDef(name = "Ecosystem Type"),
#       year_range = colDef(name = "Period"),
#       island_olelo = colDef(name = "Island"),
#       moku_olelo = colDef(name = "Moku"),
#       change_mm = colDef(
#         name = "Δ Rainfall (mm)",
#         format = colFormat(digits = 2),
#         style = function(value) {
#           bg <- ifelse(
#             is.na(value),
#             NA_character_,
#             ifelse(value < 0, "#FFCCCC", "#CCFFCC")
#           )
#           list(background = bg)
#         }
#       ),
#       pct_change = colDef(
#         name = "Δ (%)",
#         format = colFormat(suffix = "%", digits = 2)
#       ),
#       abs_pct_change = colDef(
#         name = "│Δ│ (%)",
#         format = colFormat(suffix = "%", digits = 2),
#         show = FALSE
#       )
#     ),
#     searchable = TRUE,
#     filterable = TRUE,
#     sortable = TRUE,
#     pagination = TRUE,
#     defaultPageSize = 10,
#     compact = TRUE,
#     bordered = TRUE,
#     striped = TRUE,
#     highlight = TRUE,
#     defaultColDef = colDef(align = "center")
#   )
# }


# df <- conditions$fetch_rainfall_change_summary_data(all_data)
# reactable::reactable(
#   df |>
#     dplyr::mutate(
#       abs_pct_change = abs(pct_change)
#     ) |>
#     dplyr::arrange(
#       ecosystem_type,
#       island_olelo,
#       dplyr::desc(abs_pct_change)
#     ) |>
#     dplyr::select(
#       ecosystem_type, island_olelo, moku_olelo, year_range,
#       change_mm, pct_change, abs_pct_change
#     ),
#   groupBy = c("year_range", "ecosystem_type", "island_olelo"),
#   defaultSorted = "abs_pct_change",
#   defaultSortOrder = "desc",
#   columns = list(
#     ecosystem_type = reactable::colDef(name = "Ecosystem Type"),
#     island_olelo = reactable::colDef(name = "Island"),
#     moku_olelo = reactable::colDef(name = "Moku"),
#     year_range = reactable::colDef(name = "Period"),
#     change_mm = reactable::colDef(
#       name = "Δ Rainfall (mm)",
#       format = reactable::colFormat(digits = 2),
#       style  = function(value) {
#         color <- if (value < 0) "#FFCCCC" else "#CCFFCC"
#         list(background = color)
#       }
#     ),
#     pct_change = reactable::colDef(
#       name = "Δ (%)",
#       format = reactable::colFormat(suffix = "%", digits = 2)
#     ),
#     abs_pct_change = reactable::colDef(
#       name = "│Δ│ (%)",
#       format = reactable::colFormat(suffix = "%", digits = 2),
#       show = FALSE
#     )
#   ),
#   searchable = TRUE,
#   filterable = TRUE,
#   sortable = TRUE,
#   pagination = TRUE,
#   defaultPageSize = 10,
#   compact = TRUE,
#   bordered = TRUE,
#   striped = TRUE,
#   highlight = TRUE,
#   defaultColDef = reactable::colDef(align = "center")
# )
