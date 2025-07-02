tidy_coral_reef_condition_indicators <- function(raw_df) {
  coral_reef_conditions <- raw_df |> 
  janitor::clean_names() |>
  janitor::remove_empty(which = "cols")

  overall_change_pct <- coral_reef_conditions |>
    dplyr::filter(moku_1 == "Overall normalized change 
(weighted mean)") |> 
    dplyr::select(dplyr::starts_with("p_value_")) |>
    dplyr::select(dplyr::where(~ !any(is.na(.)))) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("p_value_"),
      names_to  = "metric",
      values_to = "overall_change_pct"
    ) |>
    dplyr::mutate(
      overall_change_pct = readr::parse_number(overall_change_pct) / 100
    )

  cleaned_tests <- coral_reef_conditions |>
    utils::head(-2) |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("^p_value"),
        ~ readr::parse_double(as.character(.x))
      )
    ) |>
    dplyr::rename("name2" = moku_1) |> 
    dplyr::select(-dplyr::starts_with("moku_")) |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("sig_"),
        ~ dplyr::if_else(. == "Statistically Significant", 1L, 0L)
      )
    ) 

  significance <- cleaned_tests |> 
    dplyr::select(name2, dplyr::starts_with("sig_")) |> 
    tidyr::pivot_longer(
      cols = -name2,
      names_to  = "metric",
      values_to = "significant"
    ) |> 
    dplyr::select(-metric)

  tidied_coral_reef_condition_indicators_df <- cleaned_tests |> 
    dplyr::select(name2, dplyr::starts_with("p_value_")) |> 
    tidyr::pivot_longer(
      cols = -name2,
      names_to  = "metric",
      values_to = "p_value"
    ) |>
    dplyr::mutate(
      significant = significance$significant
    ) |> 
    dplyr::filter(significant == 1) |> 
    dplyr::left_join(overall_change_pct, by = "metric") |> 
    tidyr::extract(
      col = metric,
      into = c("metric", "years"),
      # group 1 = all non-digits at start, group 2 = the rest (digits + underscores)
      regex  = "^([^2-9]+)([0-9].*)$"
    ) |> 
    dplyr::mutate(
      metric = stringr::str_replace(metric, "p_value_", ""),
      years = stringr::str_replace(years, "^(\\d{4})(.*)$", "\\1-\\2")
    )

  return(tidied_coral_reef_condition_indicators_df)
}