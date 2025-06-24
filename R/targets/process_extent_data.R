processed_extent_data <- list(
  # 0. Define file name mapping ----------------------------------------------
  tar_target(
    extent_file_names,
    list(
      marine_extents = "marine_et_areas_per_moku", 
      terrestrial_extents = "terrestrial_et_areas_per_moku_13_16_19"
    )
  ),
  # 1. Track file path for updates ------------------------------------------
  tar_target(
    extent_file_paths,
    purrr::map_chr(
      extent_file_names,
      ~ create_file_path("data/raw/extents", .x, "csv")
    ),
    format = "file"
  ),
  # 2. Import raw data ------------------------------------------------------
  tar_target(
    imported_extent_dfs,
    purrr::map(
      extent_file_paths,
      ~ readr::read_csv(.x, show_col_types = FALSE, progress = FALSE)
    ) |>
      rlang::set_names(names(extent_file_names))
  ),
  # 3. Split into marine and terrestrial dfs --------------------------------
  tar_target(
    raw_marine_extents_df,
    imported_extent_dfs$marine_extents
  ),
  tar_target(
    raw_terrestrial_extents_df,
    imported_extent_dfs$terrestrial_extents
  ),
  # 4. Tidy dfs -------------------------------------------------------------
  tar_target(
    tidied_marine_extents_df,
    tidy_marine_extents(raw_marine_extents_df)
  ),
  tar_target(
    tidied_terrestrial_extents_df,
    tidy_marine_extents(raw_terrestrial_extents_df)
  ),
  # Merge dfs ---------------------------------------------------------------
  tar_target(
    merged_extents_df,
    dplyr::bind_rows(
      tidied_marine_extents_df, tidied_terrestrial_extents_df
    )
  ),
  # 5. Export dfs as CSVs ---------------------------------------------------
  tar_target(
    exported_extents_csv,
    export_to_csv(
      merged_extents_df,
      dir = "data/processed/extents/",
      file_name = paste0(get_todays_date(), "_merged_extents_13_16_19")
    )
  )
)