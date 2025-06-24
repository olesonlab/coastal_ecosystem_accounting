
# File path(s) ------------------------------------------------------------

create_file_path <- function(dir, file_name, ext) {
  here::here(glue::glue("{dir}/{file_name}.{ext}"))
}

file_paths <- purrr::map_chr(
  # File names
  list(
    coral_reef_conditions_13_16 = "coral_reef_conditions_13_16",
    coral_reef_conditions_13_19 = "coral_reef_conditions_13_19",
    freshwater_wetlands_rainfall_13_19 = "freshwater_wetlands_rainfall_13_19",
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
)

# Tidy --------------------------------------------------------------------

# # Insert tidying function
# <tidying_helper_function>
#   
# tidied_<data> <- <tidying_function>(<raw_df>)

# Tidy rainfall for tree cover ecosystem type for 1990, 2013, 2016 and 2019

tidy_tree_cover_rainfall_data <- function(raw_tree_cover_rainfall_90_13_19_df, raw_tree_cover_rainfall_16_df) {
  
  # 1. Clean and standardize datasets
  tidied_tree_cover_rainfall_90_13_19 <- raw_condition_dfs$tree_cover_rainfall_90_13_19 |>
    janitor::clean_names() |>
    dplyr::rename(
      mean_rainfall = rain_avg,
      max_rainfall = rain_max
    ) |>
    dplyr::select(-c(x, x_1319))
  
  tidied_tree_cover_rainfall_16 <- raw_condition_dfs$tree_cover_rainfall_16 |>
    janitor::clean_names() |>
    dplyr::rename(name2 = moku) |>
    dplyr::mutate(year = 2016)
  
  merged_tree_cover_rainfall_df <- dplyr::bind_rows(
    tidied_tree_cover_rainfall_90_13_19,
    tidied_tree_cover_rainfall_16
  )
  
  # 2. Combine and clean merged dataset
  mean_max_tree_cover_rainfall <- merged_tree_cover_rainfall_df |> 
    tidyr::drop_na() |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = c("mean_rainfall", "max_rainfall")
    ) |>
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    )
  
  # 3. Compute difference from 1990 baseline
  baseline_change_tree_cover_rainfall <- merged_tree_cover_rainfall_df |>
    dplyr::select(-max_rainfall) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = mean_rainfall,
      names_prefix = "mean_rainfall_"
    ) |>
    dplyr::mutate(
      # Difference between 1990 baseline mean rainfall (mm) and the mean rainfall
      # (mm) in recent years (2013, 2016, and 2019) per moku
      three_yr_avg = (mean_rainfall_2013 + mean_rainfall_2016 + mean_rainfall_2019) / 3,
      baseline_diff = three_yr_avg - mean_rainfall_1990
    ) |> 
    dplyr::select(-starts_with("mean")) |>
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    )
  
  # 4. Compute year-over-year rainfall changes
  change_summary_tree_cover_rainfall <- merged_tree_cover_rainfall_df |>
    dplyr::select(-max_rainfall) |> 
    tidyr::pivot_wider(
      names_from = year,
      values_from = mean_rainfall,
      names_prefix = "mean_rainfall_"
    ) |>
    dplyr::mutate(
      # Difference in mean rainfall (mm) between two years per each moku
      change_2013_2019 = mean_rainfall_2019 - mean_rainfall_2013,
      change_2013_2016 = mean_rainfall_2016 - mean_rainfall_2013,
      change_2016_2019 = mean_rainfall_2019 - mean_rainfall_2016,
      # 1 = positive, -1 = negative, 0 = no change
      change_direction = dplyr::case_when(
        change_2013_2019 > 0 ~ 1,
        change_2013_2019 < 0 ~ -1,
        change_2013_2016 > 0 ~ 1,
        change_2013_2016 < 0 ~ -1,
        change_2016_2019 > 0 ~ 1,
        change_2016_2019 < 0 ~ -1,
        TRUE ~ 0
      )
    ) |>
    tidyr::pivot_longer(
      cols = -name2,
      names_to = "metric",
      values_to = "value"
    )
  
  tidied_tree_cover_rainfall_df <- dplyr::bind_rows(
    mean_max_tree_cover_rainfall,
    baseline_change_tree_cover_rainfall,
    change_summary_tree_cover_rainfall
  )
  
  return(tidied_tree_cover_rainfall_df)
}

tidied_tree_cover_rainfall_df <- tidy_tree_cover_rainfall_data(
  raw_tree_cover_rainfall_90_13_19_df = raw_condition_dfs$tree_cover_rainfall_90_13_19,
  raw_tree_cover_rainfall_16_df = raw_condition_dfs$tree_cover_rainfall_16
) 

# Export ------------------------------------------------------------------

# Utils helper function
get_todays_date <- function() {
  base::format(base::Sys.Date(), "%Y%m%d")
}

# Export helper function
export_to_csv <- function(df, dir, file_name) {
  file_path <- create_file_path(dir = dir, file_name = file_name, ext = "csv")
  utils::write.csv(df, file = file_path, row.names = FALSE)
  invisible(file_path)
}

# List of dfs to export
dfs_to_export <- list(
  tidied_tree_cover_rainfall_df = tidied_tree_cover_rainfall_df
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
