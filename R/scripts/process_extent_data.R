
# Create file paths -------------------------------------------------------

create_file_path <- function(dir, file_name, ext) {
  here::here(glue::glue("{dir}/{file_name}.{ext}"))
}

file_paths <- purrr::map_chr(
  # File names
  list(
    marine_extents = "marine_et_areas_per_moku", 
    terrestrial_extents = "terrestrial_et_areas_per_moku_13_16_19"
  ),
  # Iteratively create file paths for each file
  ~ create_file_path("data/raw/extents", .x, "csv")
)

# Import data -------------------------------------------------------------

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

raw_extents_df <- purrr::map(
  file_paths,
  ~ import_csv(.x)
)

# Tidy marine extents df --------------------------------------------------

tidy_marine_extents <- function(raw_df) {
  # Validate input
  if (!inherits(raw_df, "data.frame")) {
    stop("Input must be a data.frame or tibble.", call. = FALSE)
  }
  
  raw_df |>
    # Clean column names (e.g., snake_case)
    janitor::clean_names() |>
    # Standardize for alignment with terrestrial extents
    dplyr::select(
      "ecosystem_type" = value,
      name2 = moku,
      area_km2 = area_km_2
    ) |>
    # Replicate rows for target analysis years
    tidyr::crossing(year = c(2013, 2016, 2019)) |>
    dplyr::mutate(
      realm = "Marine"
    )
}

tidied_marine_extents_df <- tidy_marine_extents(raw_extents_df$marine_extents)

# Tidy terrestrial extents df ---------------------------------------------

tidy_terrestrial_extents <- function(raw_df) {
  # Validate input
  if (!inherits(raw_df, "data.frame")) {
    stop("Input must be a data.frame or tibble.", call. = FALSE)
  }
  
  raw_df |> 
    janitor::clean_names() |> 
    # Convert wide ecosystem type columns to long format
    tidyr::pivot_longer(
      cols = -c(name2, year),
      names_to = "ecosystem_type",
      values_to = "area_km2"
    ) |> 
    dplyr::mutate(
      # Normalize some special cases
      ecosystem_type = dplyr::case_when(
        ecosystem_type == "grass_shrub" ~ "Grass/Shrub",
        ecosystem_type == "beaches_dunes" ~ "Beaches/Dunes",
        TRUE ~ ecosystem_type
      ),
      # Replace underscores and apply title case
      ecosystem_type = stringr::str_replace_all(ecosystem_type, "_", " "),
      ecosystem_type = stringr::str_to_title(ecosystem_type),
      realm = "Terrestrial"
    ) 
}

tidied_terrestrial_extents_df <- tidy_terrestrial_extents(raw_extents_df$terrestrial_extents)

# Merge tidied extent dfs -------------------------------------------------

merged_extents_df <- dplyr::bind_rows(
  tidied_marine_extents_df, tidied_terrestrial_extents_df
) |> 
  # No data for less populated islands (n = 148)
  dplyr::filter(area_km2 != 0) |> 
  # First aggregate any duplicates by sukm2ing their areas
  dplyr::group_by(ecosystem_type, name2, realm, year) |>
  dplyr::summarize(
    area_km2 = sum(area_km2, na.rm = TRUE),
    .groups = "drop"
  ) 

# Summaries ---------------------------------------------------------------

# 4. Compute year-over-year extent changes
extent_changes_df <- 
  merged_extents_df |>
  # pivot years into wide format
  tidyr::pivot_wider(
    names_from   = year,
    values_from  = area_km2,
    names_prefix = "area_km2_"
  ) |>
  dplyr::mutate(
    # absolute changes in area (km2) for each interval
    change_km2_2013_2016 = area_km2_2016 - area_km2_2013,
    change_km2_2013_2019 = area_km2_2019 - area_km2_2013,
    change_km2_2016_2019 = area_km2_2019 - area_km2_2016,
    # change directions: -1 = decrease, 0 = no change, +1 = increase
    dir_2013_2016 = sign(change_km2_2013_2016),
    dir_2013_2019 = sign(change_km2_2013_2019),
    dir_2016_2019 = sign(change_km2_2016_2019),
    # percent changes relative to the earlier year
    pct_change_2013_2016 = (change_km2_2013_2016 / area_km2_2013) * 100,
    pct_change_2013_2019 = (change_km2_2013_2019 / area_km2_2013) * 100,
    pct_change_2016_2019 = (change_km2_2016_2019 / area_km2_2016) * 100
  ) |>
  tidyr::pivot_longer(
    cols = -c(name2, ecosystem_type, realm),
    names_to = "metric",
    values_to = "value"
  ) |> 
  # No data for small islands; Kona, Oahu is questionable though 
  # for cropland (n = 68)
  tidyr::drop_na()

# Export df ---------------------------------------------------------------

export_to_csv <- function(df, dir, file_name) {
  file_path <- create_file_path(dir = dir, file_name = file_name, ext = "csv")
  utils::write.csv(df, file = file_path, row.names = FALSE)
  invisible(file_path)
}

get_todays_date <- function() {
  base::format(base::Sys.Date(), "%Y%m%d")
}

export_to_csv(
  extent_changes_df,
  dir = "data/processed/extents/",
  file_name = paste0(get_todays_date(), "_extent_changes")
)
