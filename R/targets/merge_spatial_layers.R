# merged_spatial_layers <- list(
#   # 0. Define file name mapping ----------------------------------------------
#   tar_target(
#     <data>_file_names,
#     list(
#       <df_name> = "<file_name>"
#     )
#   ),
#   # 1. Track file path for updates ------------------------------------------
#   tar_target(
#     <data>_file_paths,
#     purrr::map_chr(
#       <data>_file_names,
#       ~ create_file_path("<dir>", .x, "ext")
#     ),
#     format = "file"
#   ),
#   # 2. Import raw data ------------------------------------------------------
#   tar_target(
#     imported_<data>_dfs,
#     purrr::map(
#       <data>_file_paths,
#       ~ <import_helper_function>
#     ) |>
#       rlang::set_names(names(<data>_file_names))
#   ),
#   
#   # 3. Split dfs ------------------------------------------------------------
#   tar_target(
#     raw_<data_type>_df,
#     imported_extent_dfs$raw_<data_type>_df
#   ),
#   tar_target(
#     raw_<data_type>_df,
#     imported_<data>_dfs$raw_<data_type>_df
#   ),
#   # 4. Tidy dfs -------------------------------------------------------------
#   tar_target(
#     tidied_<data_type>_df,
#     <tidying_helper_function>(raw_<data_type>_df)
#   ),
#   tar_target(
#     tidied_<data_type>_df,
#     <tidying_helper_function>(raw_<data_type>_df)
#   ),
#   # Merge dfs ---------------------------------------------------------------
#   tar_target(
#     merged_<data_type>_df,
#     dplyr::bind_rows(
#       tidied_<data_type>_df, tidied_<data_type>_df
#     )
#   ),
#   # 5. Export dfs as CSVs ---------------------------------------------------
#   tar_target(
#     exported_<data_type>_<ext>,
#     <export_helper_function>(
#       <df>,
#       dir = "<dr>",
#       file_name = paste0(get_todays_date(), "_<file_name>")
#     )
#   )
# )