processed_condition_indicators <- list(
  # 0. Define file name mapping ----------------------------------------------
  tar_target(
    condition_file_names,
    list(
        coral_reef_conditions_13_16 = "coral_reef_conditions_13_16",
        coral_reef_conditions_13_19 = "coral_reef_conditions_13_19",
        freshwater_wetlands_rainfall_90_13_19 = "freshwater_wetlands_rainfall_90_13_19",
        freshwater_wetlands_rainfall_16 = "freshwater_wetlands_rainfall_16",
        marine_conditions_13_16_19 = "marine_conditions_13_16_19",
        tree_cover_rainfall_90_13_19 = "tree_cover_rainfall_90_13_19",
        tree_cover_rainfall_16 = "tree_cover_rainfall_16"
      )
  ),
  # 1. Track file path for updates ------------------------------------------
  tar_target(
    raw_condition_file_paths,
    purrr::map_chr(
      # File names
      condition_file_names,
      # Iteratively create file paths for each file
      ~ create_file_path("data/raw/conditions", .x, "csv")
    ),
    format = "file"
  ),
  # 2. Import raw data ------------------------------------------------------
  tar_target(
    imported_condition_dfs,
    purrr::map(
      raw_condition_file_paths,
      ~ import_csv(.x)
    ) |>
      rlang::set_names(names(condition_file_names))
  ),

  # 3. Split dfs ------------------------------------------------------------
  tar_target(
    raw_terrestrial_condition_dfs,
    list(
      "Tree Cover" = list(
        raw_90_13_19 = imported_condition_dfs$tree_cover_rainfall_90_13_19,
        raw_16 = imported_condition_dfs$tree_cover_rainfall_16
      ),
      "Freshwater Wetlands" = list(
        raw_90_13_19 = imported_condition_dfs$freshwater_wetlands_rainfall_90_13_19,
        raw_16 = imported_condition_dfs$freshwater_wetlands_rainfall_16
      )
    )
  ),
  tar_target(
    raw_coral_reef_condition_dfs,
    list(
      raw_13_16 = imported_condition_dfs$coral_reef_conditions_13_16,
      raw_13_19 = imported_condition_dfs$coral_reef_conditions_13_19
    )
  ),
  # tar_target(
  #   raw_<data_type>_df,
  #   imported_<data>_dfs$raw_<data_type>_df
  # ),
  # 4. Tidy dfs -------------------------------------------------------------
  tar_target(
    summarized_terrestrial_conditions_df,
    purrr::imap_dfr(
      raw_terrestrial_condition_dfs,
      function(df_pair, ecosystem_label) {
        tidy_terrestrial_conditions(df_pair$raw_90_13_19, df_pair$raw_16) |>
          dplyr::mutate(ecosystem_type = ecosystem_label)
      }
    )
  ),
  tar_target(
    tidied_coral_condition_indicators_df,
    purrr::map_dfr(
      raw_coral_reef_condition_dfs,
      tidy_coral_reef_condition_indicators
    )
  ),
  # tar_target(
  #   tidied_<data_type>_df,
  #   <tidying_helper_function>(raw_<data_type>_df)
  # ),
  # 5. Export dfs as CSVs ---------------------------------------------------
  tar_target(
    dfs_to_export,
    list(
      "summarized_terrestrial_conditions" = summarized_terrestrial_conditions_df,
      "tidied_coral_condition_indicators" = tidied_coral_condition_indicators_df
    )
  ),
  tar_target(
    exported_condition_indicators_csv,
    purrr::iwalk(
      .x = dfs_to_export,
      .f = ~ export_to_csv(
        df = .x,
        dir = "data/processed/conditions",
        file_name = paste0(get_todays_date(), "_", .y)
      )
    )
  ),
  tar_target(
    exported_condition_indicators_qs,
    purrr::iwalk(
      .x = dfs_to_export,
      .f = ~ export_to_qs(
        df = .x,
        dir = "data/processed/shiny",
        file_name = paste0(get_todays_date(), "_", .y)
      )
    )
  )
)