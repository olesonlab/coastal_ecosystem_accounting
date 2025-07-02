# app/logic/utils.R
box::use(
  readr[read_csv],
  here[here],
  fs[file_exists],
  sf[st_read],
  qs[qread],
  dplyr[left_join],
  tidyr[extract, pivot_wider]
)

#' @export
import_csv <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }
  read_csv(
    file = here(file_path),
    show_col_types = FALSE,
    progress = FALSE
  )
}

#' @export
import_gpkg <- function(path) {
  full <- if (file_exists(path)) path else here(path)
  if (!file_exists(full)) stop("File not found: ", full)
  st_read(full, quiet = TRUE)
}

#' @export
import_qs <- function(path) {
  qread(here(path))
}

#' @export
add_formatted_moku_names <- function(df, moku_names_formatted_df) {
  df <- left_join(df, moku_names_formatted_df, by = "name2")
  return(df)
}

#' @export
filter_metric <- function(df, words) {
  # words: a character vector of prefixes, e.g. c("mean", "max")
  pattern <- paste0("^(", paste(words, collapse = "|"), ")")
  df |>
    dplyr::filter(stringr::str_detect(metric, pattern))
}

#' @export
expand_metric_col <- function(df, extract_year = TRUE) {
  if (extract_year) {
    df |>
      tidyr::extract(
        col     = metric,
        into    = c("metric", "year"),
        regex   = "^(.*)_(\\d{4})$",
        convert = TRUE
      ) |>
      tidyr::pivot_wider(
        names_from  = metric,
        values_from = value
      )
  } else {
    df |>
      tidyr::pivot_wider(
        names_from  = metric,
        values_from = value
      )
  }
}

#' @export
fix_okinas <- function(x) {
  # Replace curly single-quote (U+2018) with Hawaiian okina (U+02BB)
  stringr::str_replace_all(
    x,
    stringr::fixed("\u2018"),  # curly single‐quote
    "\u02BB"                    # true Hawaiian ʻokina
  )
}

#' @export
tidy_rainfall_data <- function(df, moku_names_formatted_df, filter_metric, extract_year = FALSE) {
  df |> 
    add_formatted_moku_names(moku_names_formatted_df) |> 
    filter_metric(filter_metric) |> 
    expand_metric_col(extract_year = extract_year) |> 
    dplyr::mutate(
      moku_olelo = fix_okinas(moku_olelo),
      island_olelo = fix_okinas(island_olelo)
    )
}

#' @export
create_change_cell <- function(change_value, direction, is_percentage = FALSE) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required but not installed.")
  }
  
  function(value, index) {
    dir_val <- direction[index]
    
    # Handle NA values and determine color based on direction
    color <- if (is.na(dir_val)) {
      "#6b7280"  # Gray for NA
    } else if (dir_val == 1) {
      "#22c55e"  # Green for positive
    } else if (dir_val == -1) {
      "#ef4444"  # Red for negative
    } else {
      "#6b7280"  # Gray for no change
    }
    
    # Format the value, handle NA
    formatted_value <- if (is.na(value)) {
      "NA"
    } else if (is_percentage) {
      paste0(sprintf("%.2f", value), "%")
    } else {
      sprintf("%.3f", value)
    }
    
    # Create colored cell
    htmltools::div(
      style = list(
        color = color,
        fontWeight = if (!is.na(value) && abs(value) > 0) "bold" else "normal"
      ),
      formatted_value
    )
  }
}

#' @export
prepare_change_data <- function(df, group_by_cols = c("island_olelo", "moku_olelo", "realm")) {
  df |>
    # Add absolute percentage change for ranking
    transform(
      abs_pct_2013_2016 = abs(pct_change_2013_2016),
      abs_pct_2013_2019 = abs(pct_change_2013_2019),
      abs_pct_2016_2019 = abs(pct_change_2016_2019)
    ) |>
    # Sort by grouping columns, then by highest absolute percentage change
    (\(x) {
      # Create order based on available columns
      order_cols <- c()
      for (col in group_by_cols) {
        if (col %in% names(x)) {
          order_cols <- c(order_cols, x[[col]])
        }
      }
      order_cols <- c(order_cols, -x$abs_pct_2013_2019)
      x[do.call(order, as.list(order_cols)), ]
    })() |>
    # Select relevant columns dynamically
    (\(x) {
      base_cols <- c("island_olelo", "moku_olelo", "ecosystem_type")
      # Add realm if it exists
      if ("realm" %in% names(x)) {
        base_cols <- c(base_cols[1:2], "realm", base_cols[3])
      }
      
      # Add metric columns based on what's available in the data
      metric_cols <- character(0)
      
      # Check for different metric patterns
      if (any(grepl("area_km2_", names(x)))) {
        metric_cols <- c(metric_cols, 
                        "area_km2_2013", "area_km2_2016", "area_km2_2019")
      }
      if (any(grepl("mean_rainfall_mm_", names(x)))) {
        metric_cols <- c(metric_cols, 
                        "mean_rainfall_mm_2013", "mean_rainfall_mm_2016", "mean_rainfall_mm_2019")
      }
      if (any(grepl("max_rainfall_mm_", names(x)))) {
        metric_cols <- c(metric_cols, 
                        "max_rainfall_mm_2013", "max_rainfall_mm_2016", "max_rainfall_mm_2019")
      }
      
      change_cols <- c(
        "change_mm_2013_2016", "change_mm_2013_2019", "change_mm_2016_2019",
        "change_km2_2013_2016", "change_km2_2013_2019", "change_km2_2016_2019",
        "pct_change_2013_2016", "pct_change_2013_2019", "pct_change_2016_2019",
        "dir_2013_2016", "dir_2013_2019", "dir_2016_2019"
      )
      
      # Select only existing columns
      all_cols <- c(base_cols, metric_cols, change_cols)
      existing_cols <- all_cols[all_cols %in% names(x)]
      
      x[, existing_cols]
    })()
}

#' @export
create_generic_change_table <- function(data, title = "Change Analysis") {
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop("Package 'reactable' is required but not installed.")
  }
  
  # Determine grouping columns based on what's available
  group_cols <- c("island_olelo", "moku_olelo")
  if ("realm" %in% names(data)) {
    group_cols <- c(group_cols, "realm")
  }
  
  # Base column definitions
  base_column_defs <- list(
    island_olelo = reactable::colDef(
      name = "Island",
      style = list(fontWeight = "bold")
    ),
    moku_olelo = reactable::colDef(
      name = "Moku",
      style = list(fontWeight = "bold")
    ),
    ecosystem_type = reactable::colDef(
      name = "Ecosystem Type"
    )
  )
  
  # Add realm if it exists
  if ("realm" %in% names(data)) {
    base_column_defs$realm <- reactable::colDef(
      name = "Realm",
      style = list(fontWeight = "bold")
    )
  }
  
  # Dynamic metric columns
  metric_column_defs <- list()
  
  # Area columns (for extents data)
  if ("area_km2_2013" %in% names(data)) {
    metric_column_defs <- c(metric_column_defs, list(
      area_km2_2013 = reactable::colDef(
        name = "2013 Area (km²)",
        format = reactable::colFormat(digits = 2)
      ),
      area_km2_2016 = reactable::colDef(
        name = "2016 Area (km²)",
        format = reactable::colFormat(digits = 2)
      ),
      area_km2_2019 = reactable::colDef(
        name = "2019 Area (km²)",
        format = reactable::colFormat(digits = 2)
      )
    ))
  }
  
  # Rainfall columns (for conditions data)
  if ("mean_rainfall_mm_2013" %in% names(data)) {
    metric_column_defs <- c(metric_column_defs, list(
      mean_rainfall_mm_2013 = reactable::colDef(
        name = "2013 Mean Rainfall (mm)",
        format = reactable::colFormat(digits = 2)
      ),
      mean_rainfall_mm_2016 = reactable::colDef(
        name = "2016 Mean Rainfall (mm)",
        format = reactable::colFormat(digits = 2)
      ),
      mean_rainfall_mm_2019 = reactable::colDef(
        name = "2019 Mean Rainfall (mm)",
        format = reactable::colFormat(digits = 2)
      )
    ))
  }
  
  if ("max_rainfall_mm_2013" %in% names(data)) {
    metric_column_defs <- c(metric_column_defs, list(
      max_rainfall_mm_2013 = reactable::colDef(
        name = "2013 Max Rainfall (mm)",
        format = reactable::colFormat(digits = 2)
      ),
      max_rainfall_mm_2016 = reactable::colDef(
        name = "2016 Max Rainfall (mm)",
        format = reactable::colFormat(digits = 2)
      ),
      max_rainfall_mm_2019 = reactable::colDef(
        name = "2019 Max Rainfall (mm)",
        format = reactable::colFormat(digits = 2)
      )
    ))
  }
  
  # Change columns
  change_column_defs <- list()
  
  # Area change columns
  if ("change_km2_2013_2016" %in% names(data)) {
    change_column_defs <- c(change_column_defs, list(
      change_km2_2013_2016 = reactable::colDef(
        name = "Δ 2013-2016 (km²)",
        cell = create_change_cell(data$change_km2_2013_2016, data$dir_2013_2016)
      ),
      change_km2_2013_2019 = reactable::colDef(
        name = "Δ 2013-2019 (km²)",
        cell = create_change_cell(data$change_km2_2013_2019, data$dir_2013_2019)
      ),
      change_km2_2016_2019 = reactable::colDef(
        name = "Δ 2016-2019 (km²)",
        cell = create_change_cell(data$change_km2_2016_2019, data$dir_2016_2019)
      )
    ))
  }
  
  # Rainfall change columns
  if ("change_mm_2013_2016" %in% names(data)) {
    change_column_defs <- c(change_column_defs, list(
      change_mm_2013_2016 = reactable::colDef(
        name = "Δ 2013-2016 (mm)",
        cell = create_change_cell(data$change_mm_2013_2016, data$dir_2013_2016)
      ),
      change_mm_2013_2019 = reactable::colDef(
        name = "Δ 2013-2019 (mm)",
        cell = create_change_cell(data$change_mm_2013_2019, data$dir_2013_2019)
      ),
      change_mm_2016_2019 = reactable::colDef(
        name = "Δ 2016-2019 (mm)",
        cell = create_change_cell(data$change_mm_2016_2019, data$dir_2016_2019)
      )
    ))
  }
  
  # Percentage change columns (common to both)
  pct_change_column_defs <- list(
    pct_change_2013_2016 = reactable::colDef(
      name = "% Δ 2013-2016",
      cell = create_change_cell(data$pct_change_2013_2016, data$dir_2013_2016, TRUE)
    ),
    pct_change_2013_2019 = reactable::colDef(
      name = "% Δ 2013-2019",
      cell = create_change_cell(data$pct_change_2013_2019, data$dir_2013_2019, TRUE)
    ),
    pct_change_2016_2019 = reactable::colDef(
      name = "% Δ 2016-2019",
      cell = create_change_cell(data$pct_change_2016_2019, data$dir_2016_2019, TRUE)
    ),
    # Hide direction columns
    dir_2013_2016 = reactable::colDef(show = FALSE),
    dir_2013_2019 = reactable::colDef(show = FALSE),
    dir_2016_2019 = reactable::colDef(show = FALSE)
  )
  
  # Combine all column definitions
  all_column_defs <- c(
    base_column_defs,
    metric_column_defs,
    change_column_defs,
    pct_change_column_defs
  )
  
  reactable::reactable(
    data,
    groupBy = group_cols,
    defaultExpanded = FALSE,
    defaultPageSize = 20,
    searchable = TRUE,
    filterable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    theme = reactable::reactableTheme(
      borderColor = "#ddd",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5ff",
      cellPadding = "8px 12px"
    ),
    columns = all_column_defs,
    defaultColDef = reactable::colDef(
      headerStyle = list(background = "#f7f7f7", fontWeight = "bold"),
      align = "center"
    )
  )
}

# # app/logic/extents.R
# box::use(
#   dplyr[mutate],
#   tidyr[pivot_wider]
# )

# box::use(
#   app/logic/utils[
#     add_formatted_moku_names,
#     fix_okinas,
#     prepare_change_data,
#     create_generic_change_table
#   ]
# )

# #' @export
# fetch_extents_data <- function(all_data) {
#   data <- all_data$extents_data |> 
#     tidyr::pivot_wider(
#       names_from = metric,
#       values_from = value
#     ) |> 
#     add_formatted_moku_names(all_data$formatted_moku_names) |> 
#     dplyr::mutate(
#       moku_olelo = fix_okinas(moku_olelo),
#       island_olelo = fix_okinas(island_olelo)
#     ) |>
#     prepare_change_data(group_by_cols = c("island_olelo", "moku_olelo", "realm"))
  
#   return(data)
# }

# #' @export
# extents_table <- function(all_data) {
#   df <- fetch_extents_data(all_data)
#   create_generic_change_table(df, title = "Ecosystem Extents Change Analysis")
# }

# box::use(
#   readr[read_csv],
#   here[here],
#   fs[file_exists],
#   sf[st_read],
#   qs[qread],
#   dplyr[left_join],
#   tidyr[extract, pivot_wider]
# )

# #' @export
# import_csv <- function(file_path) {
#   if (!is.character(file_path) || length(file_path) != 1) {
#     stop("`file_path` must be a single character string.", call. = FALSE)
#   }
#   read_csv(
#     file = here(file_path),
#     show_col_types = FALSE,
#     progress = FALSE
#   )
# }

# #' @export
# import_gpkg <- function(path) {
#   full <- if (file_exists(path)) path else here(path)
#   if (!file_exists(full)) stop("File not found: ", full)
#   st_read(full, quiet = TRUE)
# }

# #' @export
# import_qs <- function(path) {
#   qread(here(path))
# }

# #' @export
# add_formatted_moku_names <- function(df, moku_names_formatted_df) {
#   df <- left_join(df, moku_names_formatted_df, by = "name2")
#   return(df)
# }

# #' @export
# filter_metric <- function(df, words) {
#   # words: a character vector of prefixes, e.g. c("mean", "max")
#   pattern <- paste0("^(", paste(words, collapse = "|"), ")")
#   df |>
#     dplyr::filter(stringr::str_detect(metric, pattern))
# }

# #' @export
# expand_metric_col <- function(df, extract_year = TRUE) {
#   if (extract_year) {
#     df |>
#       tidyr::extract(
#         col     = metric,
#         into    = c("metric", "year"),
#         regex   = "^(.*)_(\\d{4})$",
#         convert = TRUE
#       ) |>
#       tidyr::pivot_wider(
#         names_from  = metric,
#         values_from = value
#       )
#   } else {
#     df |>
#       tidyr::pivot_wider(
#         names_from  = metric,
#         values_from = value
#       )
#   }
# }

# #' @export
# fix_okinas <- function(x) {
#   # Replace curly single-quote (U+2018) with Hawaiian okina (U+02BB)
#   stringr::str_replace_all(
#     x,
#     stringr::fixed("\u2018"),  # curly single‐quote
#     "\u02BB"                    # true Hawaiian ʻokina
#   )
# }

# #' @export
# tidy_rainfall_data <- function(df, moku_names_formatted_df, filter_metric, extract_year = FALSE) {
#   df |> 
#     add_formatted_moku_names(moku_names_formatted_df) |> 
#     filter_metric(filter_metric) |> 
#     expand_metric_col(extract_year = extract_year) |> 
#     dplyr::mutate(
#       moku_olelo = fix_okinas(moku_olelo),
#       island_olelo = fix_okinas(island_olelo)
#     )
# }

# # #' @export
# # prepare_change_data <- function(df, group_by_cols = c("island_olelo", "moku_olelo", "realm")) {
# #   df |>
# #     # Add absolute percentage change for ranking
# #     transform(
# #       abs_pct_2013_2016 = abs(pct_change_2013_2016),
# #       abs_pct_2013_2019 = abs(pct_change_2013_2019),
# #       abs_pct_2016_2019 = abs(pct_change_2016_2019)
# #     ) |>
# #     # Sort by grouping columns, then by highest absolute percentage change
# #     (\(x) {
# #       # Create order based on available columns
# #       order_cols <- c()
# #       for (col in group_by_cols) {
# #         if (col %in% names(x)) {
# #           order_cols <- c(order_cols, x[[col]])
# #         }
# #       }
# #       order_cols <- c(order_cols, -x$abs_pct_2013_2019)
# #       x[do.call(order, as.list(order_cols)), ]
# #     })() |>
# #     # Select relevant columns dynamically
# #     (\(x) {
# #       base_cols <- c("island_olelo", "moku_olelo", "ecosystem_type")
# #       # Add realm if it exists
# #       if ("realm" %in% names(x)) {
# #         base_cols <- c(base_cols[1:2], "realm", base_cols[3])
# #       }
      
# #       # Add metric columns based on what's available in the data
# #       metric_cols <- character(0)
      
# #       # Check for different metric patterns
# #       if (any(grepl("area_km2_", names(x)))) {
# #         metric_cols <- c(metric_cols, 
# #                         "area_km2_2013", "area_km2_2016", "area_km2_2019")
# #       }
# #       if (any(grepl("mean_rainfall_mm_", names(x)))) {
# #         metric_cols <- c(metric_cols, 
# #                         "mean_rainfall_mm_2013", "mean_rainfall_mm_2016", "mean_rainfall_mm_2019")
# #       }
# #       if (any(grepl("max_rainfall_mm_", names(x)))) {
# #         metric_cols <- c(metric_cols, 
# #                         "max_rainfall_mm_2013", "max_rainfall_mm_2016", "max_rainfall_mm_2019")
# #       }
      
# #       change_cols <- c(
# #         "change_mm_2013_2016", "change_mm_2013_2019", "change_mm_2016_2019",
# #         "change_km2_2013_2016", "change_km2_2013_2019", "change_km2_2016_2019",
# #         "pct_change_2013_2016", "pct_change_2013_2019", "pct_change_2016_2019",
# #         "dir_2013_2016", "dir_2013_2019", "dir_2016_2019"
# #       )
      
# #       # Select only existing columns
# #       all_cols <- c(base_cols, metric_cols, change_cols)
# #       existing_cols <- all_cols[all_cols %in% names(x)]
      
# #       x[, existing_cols]
# #     })()
# # }

# # #' @export
# # create_generic_change_table <- function(data, title = "Change Analysis") {
# #   if (!requireNamespace("reactable", quietly = TRUE)) {
# #     stop("Package 'reactable' is required but not installed.")
# #   }
  
# #   # Determine grouping columns based on what's available
# #   group_cols <- c("island_olelo", "moku_olelo")
# #   if ("realm" %in% names(data)) {
# #     group_cols <- c(group_cols, "realm")
# #   }
  
# #   # Base column definitions
# #   base_column_defs <- list(
# #     island_olelo = reactable::colDef(
# #       name = "Island",
# #       style = list(fontWeight = "bold")
# #     ),
# #     moku_olelo = reactable::colDef(
# #       name = "Moku",
# #       style = list(fontWeight = "bold")
# #     ),
# #     ecosystem_type = reactable::colDef(
# #       name = "Ecosystem Type"
# #     )
# #   )
  
# #   # Add realm if it exists
# #   if ("realm" %in% names(data)) {
# #     base_column_defs$realm <- reactable::colDef(
# #       name = "Realm",
# #       style = list(fontWeight = "bold")
# #     )
# #   }
  
# #   # Dynamic metric columns
# #   metric_column_defs <- list()
  
# #   # Area columns (for extents data)
# #   if ("area_km2_2013" %in% names(data)) {
# #     metric_column_defs <- c(metric_column_defs, list(
# #       area_km2_2013 = reactable::colDef(
# #         name = "2013 Area (km²)",
# #         format = reactable::colFormat(digits = 3)
# #       ),
# #       area_km2_2016 = reactable::colDef(
# #         name = "2016 Area (km²)",
# #         format = reactable::colFormat(digits = 3)
# #       ),
# #       area_km2_2019 = reactable::colDef(
# #         name = "2019 Area (km²)",
# #         format = reactable::colFormat(digits = 3)
# #       )
# #     ))
# #   }
  
# #   # Rainfall columns (for conditions data)
# #   if ("mean_rainfall_mm_2013" %in% names(data)) {
# #     metric_column_defs <- c(metric_column_defs, list(
# #       mean_rainfall_mm_2013 = reactable::colDef(
# #         name = "2013 Mean Rainfall (mm)",
# #         format = reactable::colFormat(digits = 2)
# #       ),
# #       mean_rainfall_mm_2016 = reactable::colDef(
# #         name = "2016 Mean Rainfall (mm)",
# #         format = reactable::colFormat(digits = 2)
# #       ),
# #       mean_rainfall_mm_2019 = reactable::colDef(
# #         name = "2019 Mean Rainfall (mm)",
# #         format = reactable::colFormat(digits = 2)
# #       )
# #     ))
# #   }
  
# #   if ("max_rainfall_mm_2013" %in% names(data)) {
# #     metric_column_defs <- c(metric_column_defs, list(
# #       max_rainfall_mm_2013 = reactable::colDef(
# #         name = "2013 Max Rainfall (mm)",
# #         format = reactable::colFormat(digits = 2)
# #       ),
# #       max_rainfall_mm_2016 = reactable::colDef(
# #         name = "2016 Max Rainfall (mm)",
# #         format = reactable::colFormat(digits = 2)
# #       ),
# #       max_rainfall_mm_2019 = reactable::colDef(
# #         name = "2019 Max Rainfall (mm)",
# #         format = reactable::colFormat(digits = 2)
# #       )
# #     ))
# #   }
  
# #   # Change columns
# #   change_column_defs <- list()
  
# #   # Area change columns
# #   if ("change_km2_2013_2016" %in% names(data)) {
# #     change_column_defs <- c(change_column_defs, list(
# #       change_km2_2013_2016 = reactable::colDef(
# #         name = "Change 2013-2016 (km²)",
# #         cell = create_change_cell(data$change_km2_2013_2016, data$dir_2013_2016)
# #       ),
# #       change_km2_2013_2019 = reactable::colDef(
# #         name = "Change 2013-2019 (km²)",
# #         cell = create_change_cell(data$change_km2_2013_2019, data$dir_2013_2019)
# #       ),
# #       change_km2_2016_2019 = reactable::colDef(
# #         name = "Change 2016-2019 (km²)",
# #         cell = create_change_cell(data$change_km2_2016_2019, data$dir_2016_2019)
# #       )
# #     ))
# #   }
  
# #   # Rainfall change columns
# #   if ("change_mm_2013_2016" %in% names(data)) {
# #     change_column_defs <- c(change_column_defs, list(
# #       change_mm_2013_2016 = reactable::colDef(
# #         name = "Change 2013-2016 (mm)",
# #         cell = create_change_cell(data$change_mm_2013_2016, data$dir_2013_2016)
# #       ),
# #       change_mm_2013_2019 = reactable::colDef(
# #         name = "Change 2013-2019 (mm)",
# #         cell = create_change_cell(data$change_mm_2013_2019, data$dir_2013_2019)
# #       ),
# #       change_mm_2016_2019 = reactable::colDef(
# #         name = "Change 2016-2019 (mm)",
# #         cell = create_change_cell(data$change_mm_2016_2019, data$dir_2016_2019)
# #       )
# #     ))
# #   }
  
# #   # Percentage change columns (common to both)
# #   pct_change_column_defs <- list(
# #     pct_change_2013_2016 = reactable::colDef(
# #       name = "% Change 2013-2016",
# #       cell = create_change_cell(data$pct_change_2013_2016, data$dir_2013_2016, TRUE)
# #     ),
# #     pct_change_2013_2019 = reactable::colDef(
# #       name = "% Change 2013-2019",
# #       cell = create_change_cell(data$pct_change_2013_2019, data$dir_2013_2019, TRUE)
# #     ),
# #     pct_change_2016_2019 = reactable::colDef(
# #       name = "% Change 2016-2019",
# #       cell = create_change_cell(data$pct_change_2016_2019, data$dir_2016_2019, TRUE)
# #     ),
# #     # Hide direction columns
# #     dir_2013_2016 = reactable::colDef(show = FALSE),
# #     dir_2013_2019 = reactable::colDef(show = FALSE),
# #     dir_2016_2019 = reactable::colDef(show = FALSE)
# #   )
  
# #   # Combine all column definitions
# #   all_column_defs <- c(
# #     base_column_defs,
# #     metric_column_defs,
# #     change_column_defs,
# #     pct_change_column_defs
# #   )
  
# #   reactable::reactable(
# #     data,
# #     groupBy = group_cols,
# #     defaultExpanded = FALSE,
# #     defaultPageSize = 20,
# #     searchable = TRUE,
# #     filterable = TRUE,
# #     striped = TRUE,
# #     highlight = TRUE,
# #     bordered = TRUE,
# #     theme = reactable::reactableTheme(
# #       borderColor = "#ddd",
# #       stripedColor = "#f6f8fa",
# #       highlightColor = "#f0f5ff",
# #       cellPadding = "8px 12px"
# #     ),
# #     columns = all_column_defs,
# #     defaultColDef = reactable::colDef(
# #       headerStyle = list(background = "#f7f7f7", fontWeight = "bold")
# #     )
# #   )
# # }