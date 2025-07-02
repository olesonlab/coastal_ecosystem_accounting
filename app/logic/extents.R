# app/logic/extents.R
box::use(
  dplyr[
    arrange, case_when, desc, distinct, filter, 
    mutate, select
  ],
  highcharter[
    highchart, hc_add_series, hc_chart, hc_plotOptions, 
    hc_title, hc_xAxis, hc_yAxis, hw_grid
  ],
  purrr[map],
  stringr[str_replace_all, str_remove],
  tidyr[pivot_longer, pivot_wider]
)

box::use(
  app/logic/utils[
    add_formatted_moku_names, create_generic_change_table,
    fix_okinas, prepare_change_data
  ]
)

#' @export
fetch_extents_data <- function(all_data) {
  data <- all_data$extents_data |>
    tidyr::pivot_wider(
      names_from = metric,
      values_from = value
    ) |>
    add_formatted_moku_names(all_data$formatted_moku_names) |>
    dplyr::mutate(
      moku_olelo = fix_okinas(moku_olelo),
      island_olelo = fix_okinas(island_olelo)
    ) |>
    # Add absolute percentage change for ranking
    transform(
      abs_pct_2013_2016 = abs(pct_change_2013_2016),
      abs_pct_2013_2019 = abs(pct_change_2013_2019),
      abs_pct_2016_2019 = abs(pct_change_2016_2019)
    ) |>
    # Sort by island, moku, realm, then by highest absolute percentage change
    (\(x) {
      x[order(x$island_olelo, x$moku_olelo, x$realm, -x$abs_pct_2013_2019), ]
    })() |>
    # Select relevant columns
    (\(x) {
      x[, c(
        "island_olelo",
        "moku_olelo",
        "realm",
        "ecosystem_type",
        "area_km2_2013",
        "area_km2_2016",
        "area_km2_2019",
        "change_km2_2013_2016",
        "change_km2_2013_2019",
        "change_km2_2016_2019",
        "pct_change_2013_2016",
        "pct_change_2013_2019",
        "pct_change_2016_2019",
        "dir_2013_2016",
        "dir_2013_2019",
        "dir_2016_2019"
      )]
    })()

  return(data)
}

#' @export
extents_table <- function(all_data) {
  df <- fetch_extents_data(all_data)
  create_generic_change_table(df, title = "Ecosystem Extents Change Analysis")
}

#' @export
fetch_extent_areas_data <- function(all_data) {
  data <- fetch_extents_data(all_data) |>
    dplyr::select(
      island_olelo,
      moku_olelo,
      ecosystem_type,
      realm,
      area_km2_2013,
      area_km2_2016,
      area_km2_2019
    ) |>
    tidyr::pivot_longer(
      cols = c(
        area_km2_2013,
        area_km2_2016,
        area_km2_2019
      ),
      names_to = "year",
      values_to = "area_km2"
    ) |>
    dplyr::mutate(
      year = as.integer(stringr::str_replace_all(year, "area_km2_", ""))
    )
  
  return(data)
}

# #' @export
extent_chart_hc <- function(all_data,
                            islands            = NULL,
                            mokus              = NULL,
                            realms             = NULL,
                            ecosystem_types    = NULL,
                            years              = NULL,
                            chart_title_prefix = "Ecosystem Areas by Moku") {

  # 1) Fetch & filter
  df <- fetch_extent_areas_data(all_data) |>
    dplyr::filter(
      is.null(islands)         | island_olelo   %in% islands,
      is.null(mokus)           | moku_olelo     %in% mokus,
      is.null(realms)          | realm          %in% realms,
      is.null(ecosystem_types) | ecosystem_type %in% ecosystem_types,
      is.null(years)           | year           %in% years
    )

  # 2) Early exit
  if (nrow(df) == 0) {
    return(
      highcharter::highchart() |>
        highcharter::hc_title(text = "No data matches the selected filters")
    )
  }

  # 3) Make one chart per island
  charts <- map(
    unique(df$island_olelo),
    function(isl) {
      sub <- df[df$island_olelo == isl, ]
      highcharter::highchart() |>
        highcharter::hc_chart(type = "column") |>
        highcharter::hc_title(text = paste(chart_title_prefix, "—", isl)) |>
        highcharter::hc_xAxis(categories = sub$moku_olelo, title = list(text = "Moku")) |>
        highcharter::hc_yAxis(title = list(text = "Area (km²)")) |>
        highcharter::hc_add_series(
          name = as.character(sub$year),
          data = sub$area_km2,
          colorByPoint = FALSE
        ) |>
        highcharter::hc_plotOptions(
          column = list(stacking = "normal")
        ) 
    }
  )

  # 4) Arrange in 2 columns
  highcharter::hw_grid(charts, ncol = 2)
}

# # app/logic/extents.R
# box::use(
#   dplyr[filter, select, distinct, case_when, mutate, arrange, desc],
#   stringr[str_replace, str_remove],
#   tidyr[pivot_wider]
# )

# box::use(
#   app /
#     logic /
#     utils[
#       add_formatted_moku_names,
#       fix_okinas,
#       create_change_cell,
#       prepare_change_data,
#       create_generic_change_table
#     ]
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
#     # Add absolute percentage change for ranking
#     transform(
#       abs_pct_2013_2016 = abs(pct_change_2013_2016),
#       abs_pct_2013_2019 = abs(pct_change_2013_2019),
#       abs_pct_2016_2019 = abs(pct_change_2016_2019)
#     ) |>
#     # Sort by island, moku, realm, then by highest absolute percentage change
#     (\(x) {
#       x[order(x$island_olelo, x$moku_olelo, x$realm, -x$abs_pct_2013_2019), ]
#     })() |>
#     # Select relevant columns
#     (\(x) {
#       x[, c(
#         "island_olelo",
#         "moku_olelo",
#         "realm",
#         "ecosystem_type",
#         "area_km2_2013",
#         "area_km2_2016",
#         "area_km2_2019",
#         "change_km2_2013_2016",
#         "change_km2_2013_2019",
#         "change_km2_2016_2019",
#         "pct_change_2013_2016",
#         "pct_change_2013_2019",
#         "pct_change_2016_2019",
#         "dir_2013_2016",
#         "dir_2013_2019",
#         "dir_2016_2019"
#       )]
#     })()

#   return(data)
# }

# #' @export
# fetch_extent_areas_data <- function(all_data) {
#   data <- fetch_extents_data(all_data) |>
#     dplyr::select(
#       island_olelo,
#       moku_olelo,
#       ecosystem_type,
#       area_km2_2013,
#       area_km2_2016,
#       area_km2_2019
#     ) |>
#     tidyr::pivot_longer(
#       cols = c(
#         area_km2_2013,
#         area_km2_2016,
#         area_km2_2019
#       ),
#       names_to = "year",
#       values_to = "area_km2"
#     ) |>
#     dplyr::mutate(
#       year = as.integer(stringr::str_replace_all(year, "area_km2_", ""))
#     )
# }

# #' @export
# extents_table <- function(all_data) {
#   df <- fetch_extents_data(all_data)
#   create_generic_change_table(df, title = "Ecosystem Extent Changes (km² & %)")
# }

# #' @export
# extents_table <- function(all_data) {
#   df <- fetch_extents_data(all_data)
#   create_generic_change_table(df, title = "Ecosystem Extent Changes (km² & %)")
# }