box::use(
  app/logic/utils[import_csv],
  reactable[reactable],
  dplyr[filter, rename, mutate],
  tidyr[pivot_wider],
  echarts4r[e_charts, e_bar, e_tooltip],
)

#' @export
fetch_data <- function() {
  import_csv("data/processed/conditions/20250624_tidied_tree_cover_rainfall_df.csv") |>
    filter(
      metric %in% c("change_2013_2019", "change_2013_2016", "change_2016_2019")
    ) |>
    mutate(value = round(value, 2)) |>
    pivot_wider(
      names_from = metric,
      values_from = value
    ) |>
    rename("moku" = name2)
}

#' @export
table <- function(data) {
  data |> 
    reactable()
}

#' @export
chart <- function(data) {
  data |>
    e_charts(moku) |>
    e_bar(change_2013_2016, name = "2013–2016") |>
    e_bar(change_2016_2019, name = "2016–2019") |>
    e_bar(change_2013_2019, name = "2013–2019") |>
    e_tooltip(trigger = "axis")
}