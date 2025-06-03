processed_fisheries_valuation_data <- list(
  # 1. Track file path for updates ------------------------------------------
  tar_target(
    comm_and_noncomm_ev_file_path,
    create_file_path(
      dir = "data/raw/fisheries_valuation/",
      file_name = "20250527_comm_and_noncomm_ev",
      ext = "xlsx"
    ),
    format = "file"
  ),
  # 2. Import raw data ------------------------------------------------------
  tar_target(
    imported_comm_and_noncomm_ev_xlsx,
    import_xlsx_sheets(comm_and_noncomm_ev_file_path)
  ),
  # 3. Split into comm and non-comm ev dfs ----------------------------------
  tar_target(
    raw_comm_ev_df,
    extract_single_xlsx_sheet(
      imported_comm_and_noncomm_ev_xlsx, 
      "Comm"
    )
  ),
  tar_target(
    raw_noncomm_ev_df,
    extract_single_xlsx_sheet(
      imported_comm_and_noncomm_ev_xlsx, 
      "Non-comm"
    ),
  ),
  # 4. Tidy dfs -------------------------------------------------------------
  tar_target(
    tidied_comm_ev_df,
    tidy_comm_ev(raw_comm_ev_df)
  ),
  tar_target(
    tidied_noncomm_ev_df,
    tidy_noncomm_ev(raw_noncomm_ev_df)
  ),

  # 5. Export dfs as CSVs ---------------------------------------------------
  tar_target(
    exported_comm_ev_csv,
    export_to_csv(
      tidied_comm_ev_df,
      dir = "data/processed/fisheries_valuation/",
      file_name = paste0(get_todays_date(), "_tidied_comm_ev")
    )
  ),
  tar_target(
    exported_noncomm_ev_csv,
    export_to_csv(
      tidied_noncomm_ev_df,
      dir = "data/processed/fisheries_valuation/",
      file_name = paste0(get_todays_date(), "_tidied_noncomm_ev")
    )
  )
)