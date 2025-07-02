# app/logic/data_loader.R
box::use(
  purrr[map]
)

box::use(
  app/logic/utils[import_csv, import_qs]
)

#' @export
load_all_data <- function() {
  data_config <- list(
    formatted_moku_names = list(func = import_csv, path = "data/processed/shiny/moku_names_formatted.csv"),
    extents_data = list(func = import_csv, path = "data/processed/extents/20250625_extent_changes.csv"),
    terrestrial_conditions_data = list(func = import_qs, path = "data/processed/shiny/20250701_summarized_terrestrial_conditions.qs"),
    coral_conditions_data = list(func = import_qs, path = "data/processed/shiny/20250701_tidied_coral_condition_indicators.qs")
  )
  
  map(data_config, ~.x$func(.x$path))
}