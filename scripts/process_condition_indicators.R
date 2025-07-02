
# File path(s) ------------------------------------------------------------

create_file_path <- function(dir, file_name, ext) {
  here::here(glue::glue("{dir}/{file_name}.{ext}"))
}

file_paths <- purrr::map_chr(
  # File names
  list(
    coral_reef_conditions_13_16 = "coral_reef_conditions_13_16",
    coral_reef_conditions_13_19 = "coral_reef_conditions_13_19",
    freshwater_wetlands_rainfall_90_13_19 = "freshwater_wetlands_rainfall_90_13_19",
    freshwater_wetlands_rainfall_16 = "freshwater_wetlands_rainfall_16",
    marine_conditions_13_16_19 = "marine_conditions_13_16_19",
    tree_cover_rainfall_90_13_19 = "tree_cover_rainfall_90_13_19",
    tree_cover_rainfall_16 = "tree_cover_rainfall_16"
  ),
  # Iteratively create file paths for each file
  ~ create_file_path("data/raw/conditions", .x, "csv")
)

# Import ------------------------------------------------------------------

import_csv <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }
  
  readr::read_csv(
    file = file_path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

# Iteratively import dfs
raw_condition_dfs <- purrr::map(
  file_paths,
  ~ import_csv(.x)
) |>
  rlang::set_names(names(file_paths))

# Tidy --------------------------------------------------------------------

# # Insert tidying function
# <tidying_helper_function>
#   
# tidied_<data> <- <tidying_function>(<raw_df>)

# Process rainfall by tree cover and freshwater wetlands ecosystem types
tidy_terrestrial_conditions <- function(raw_terrestrial_rainfall_90_13_19_df, raw_terrestrial_rainfall_16_df) {
  
  # 1. Clean and standardize datasets
  tidied_terrestrial_rainfall_90_13_19_df <- raw_condition_dfs$freshwater_wetlands_rainfall_90_13_19 |>
    janitor::clean_names() |>
    dplyr::rename(
      mean_rainfall = rain_avg,
      max_rainfall = rain_max
    ) |>
    dplyr::select(-c(x, x_1319))
  
  tidied_terrestrial_rainfall_16_df <- raw_condition_dfs$freshwater_wetlands_rainfall_16 |>
    janitor::clean_names() |>
    dplyr::rename(name2 = moku) |>
    dplyr::mutate(year = 2016)
  
  merged_terrestrial_rainfall_df <- dplyr::bind_rows(
    tidied_terrestrial_rainfall_90_13_19_df,
    tidied_terrestrial_rainfall_16_df
  ) |> 
    dplyr::rename(
      "mean_rainfall_mm" = mean_rainfall,
      "max_rainfall_mm" = max_rainfall
    ) |> 
    dplyr::mutate(year = as.integer(year))
  
  # 2. Combine and clean merged dataset
  mean_max_terrestrial_rainfall_df <- merged_terrestrial_rainfall_df |> 
    tidyr::pivot_wider(
      names_from = year,
      values_from = c("mean_rainfall_mm", "max_rainfall_mm")
    ) |>
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    ) 
  
  # 3. Compute difference from 1990 baseline
  baseline_change_terrestrial_rainfall_df <- merged_terrestrial_rainfall_df |>
    dplyr::select(-max_rainfall_mm) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = mean_rainfall_mm,
      names_prefix = "mean_rainfall_mm_"
    ) |>
    dplyr::mutate(
      # Difference between 1990 baseline mean rainfall (mm) and the mean rainfall
      # (mm) in recent years (2013, 2016, and 2019) per moku
      three_yr_avg_mm = (mean_rainfall_mm_2013 + mean_rainfall_mm_2016 + mean_rainfall_mm_2019) / 3,
      baseline_diff_mm = three_yr_avg_mm - mean_rainfall_mm_1990
    ) |> 
    dplyr::select(-starts_with("mean")) |>
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    )
  
  # 4. Compute year-over-year rainfall changes
  change_summary_terrestrial_rainfall_df <- 
    merged_terrestrial_rainfall_df |>
    # drop the precomputed max rainfall column
    dplyr::select(-max_rainfall_mm) |> 
    # pivot years into wide format
    tidyr::pivot_wider(
      names_from   = year,
      values_from  = mean_rainfall_mm,
      names_prefix = "mean_rainfall_mm_"
    ) |>
    # Maybe not as important to include here; included in baseline summaries
    dplyr::select(-mean_rainfall_mm_1990) |>
    dplyr::mutate(
      # absolute changes in mean rainfall (mm) for each interval
      change_mm_2013_2016 = mean_rainfall_mm_2016 - mean_rainfall_mm_2013,
      change_mm_2013_2019 = mean_rainfall_mm_2019 - mean_rainfall_mm_2013,
      change_mm_2016_2019 = mean_rainfall_mm_2019 - mean_rainfall_mm_2016,
      # change directions: -1 = decrease, 0 = no change, +1 = increase
      dir_2013_2016 = sign(change_mm_2013_2016),
      dir_2013_2019 = sign(change_mm_2013_2019),
      dir_2016_2019 = sign(change_mm_2016_2019),
      # percent changes relative to the earlier year
      pct_change_2013_2016 = (change_mm_2013_2016 / mean_rainfall_mm_2013) * 100,
      pct_change_2013_2019 = (change_mm_2013_2019 / mean_rainfall_mm_2013) * 100,
      pct_change_2016_2019 = (change_mm_2016_2019 / mean_rainfall_mm_2016) * 100
    ) |>
    dplyr::select(-c(mean_rainfall_mm_2019, mean_rainfall_mm_2016, mean_rainfall_mm_2013)) |> 
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    )
  
  summarized_terrestrial_rainfall_df <- dplyr::bind_rows(
    mean_max_terrestrial_rainfall_df,
    baseline_change_terrestrial_rainfall_df,
    change_summary_terrestrial_rainfall_df
  ) |>
    # Tree cover: No data for Kahoolawe and Niihau (n = 98)
    # Freshwater wetlands: No data for Honuaula and Kahikinui mokus and Kahoolawe and Niihau islands
    # (n = 110)
    dplyr::filter(
      value != 0, 
      !is.infinite(value)
    ) |> 
    tidyr::drop_na()

  return(summarized_terrestrial_rainfall_df)
}

terrestrial_condition_dfs <- list(
  "Tree Cover" = list(
    raw_90_13_19 = raw_condition_dfs$tree_cover_rainfall_90_13_19,
    raw_16 = raw_condition_dfs$tree_cover_rainfall_16
  ),
  "Freshwater Wetlands" = list(
    raw_90_13_19 = raw_condition_dfs$freshwater_wetlands_rainfall_90_13_19,
    raw_16 = raw_condition_dfs$freshwater_wetlands_rainfall_16
  )
)

summarized_terrestrial_conditions <- purrr::imap_dfr(
  terrestrial_condition_dfs,
  function(df_pair, ecosystem_label) {
    tidy_terrestrial_conditions(df_pair$raw_90_13_19, df_pair$raw_16) |>
      dplyr::mutate(ecosystem_type = ecosystem_label)
  }
)

# Process coral reef condition indicators
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

coral_reef_condition_dfs <- list(
  raw_13_16 = raw_condition_dfs$coral_reef_conditions_13_16,
  raw_13_19 = raw_condition_dfs$coral_reef_conditions_13_19
)

tidied_coral_condition_indicators <-
  purrr::map_dfr(
    coral_reef_condition_dfs,
    tidy_coral_reef_condition_indicators
  )

# Process marine condition indicators

# Process nearshore pressure indicators

# Export ------------------------------------------------------------------

# Utils helper function
get_todays_date <- function() {
  base::format(base::Sys.Date(), "%Y%m%d")
}

create_file_path <- function(dir, file_name, ext) {
  here::here(glue::glue("{dir}/{file_name}.{ext}"))
}

# Export helper function
export_to_csv <- function(df, dir, file_name) {
  file_path <- create_file_path(dir = dir, file_name = file_name, ext = "csv")
  utils::write.csv(df, file = file_path, row.names = FALSE)
  invisible(file_path)
}

export_to_parquet <- function(df, dir, file_name) {
  file_path <- create_file_path(dir = dir, file_name = file_name, ext = "parquet")
  arrow::write_parquet(df, file_path)
  invisible(file_path)
}

export_to_qs <- function(df, dir, file_name) {
  file_path <- create_file_path(dir = dir, file_name = file_name, ext = "qs")
  qs::qsave(df, file_path)
  invisible(file_path)
}

# List of dfs to export
dfs_to_export <- list(
  "summarized_terrestrial_conditions" = summarized_terrestrial_conditions,
  "tidied_coral_condition_indicators" = tidied_coral_condition_indicators
)

# Iteratively export dfs
purrr::iwalk(
  .x = dfs_to_export,
  .f = ~ export_to_csv(
    df = .x,
    dir = "data/processed/conditions",
    file_name = paste0(get_todays_date(), "_", .y)
  )
)
purrr::iwalk(
  .x = dfs_to_export,
  .f = ~ export_to_parquet(
    df = .x,
    dir = "data/processed/conditions",
    file_name = paste0(get_todays_date(), "_", .y)
  )
)

purrr::iwalk(
  dfs_to_export,
  ~ export_to_qs(
      df = .x,
      dir = "data/processed/shiny",
      file_name = paste0(get_todays_date(), "_", .y)
    )
)

# Check ------------------------------------------------------------------

check <- arrow::read_parquet(here::here("data/processed/conditions/20250701_tidied_coral_condition_indicators.parquet"))
