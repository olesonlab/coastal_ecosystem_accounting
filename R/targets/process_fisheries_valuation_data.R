processed_fisheries_valuation_data <- list(
  # 1. Track file path for updates ------------------------------------------
  tar_target(
    name = comm_and_noncomm_ev_file_path,
    command = create_file_path(
      dir = "data/raw/fisheries_valuation/",
      file_name = "20250527_comm_and_noncomm_ev",
      ext = "xlsx"
    ),
    format = "file"
  ),
  # 2. Import raw data ------------------------------------------------------
  tar_target(
    name = imported_comm_and_noncomm_ev_xlsx,
    command = import_xlsx_sheets(file_path = comm_and_noncomm_ev_file_path)
  ),
  # 3. Split into comm and non-comm ev dfs ----------------------------------
  tar_target(
    name = raw_comm_ev_df,
    command = extract_single_xlsx_sheet(
      sheets = imported_comm_and_noncomm_ev_xlsx, 
      sheet_name = "Comm"
    )
  ),
  tar_target(
    name = raw_noncomm_ev_df,
    command = extract_single_xlsx_sheet(
      sheets = imported_comm_and_noncomm_ev_xlsx, 
      sheet_name = "Non-comm"
    ),
  ),
  # 4. Tidy dfs -------------------------------------------------------------
  tar_target(
    name = tidied_comm_ev_df,
    command = tidy_comm_ev(df = raw_comm_ev_df)
  ),
  tar_target(
    name = tidied_noncomm_ev_df,
    command = tidy_noncomm_ev(df = raw_noncomm_ev_df)
  ),

  # 5. Export dfs as CSVs ---------------------------------------------------
  tar_target(
    name = exported_comm_ev_csv,
    command = export_to_csv(
      df = tidied_comm_ev_df,
      dir = "data/processed/fisheries_valuation/",
      file_name = paste0(get_todays_date(), "_tidied_comm_ev")
    )
  ),
  tar_target(
    name = exported_noncomm_ev_csv,
    command = export_to_csv(
      df = tidied_noncomm_ev_df,
      dir = "data/processed/fisheries_valuation/",
      file_name = paste0(get_todays_date(), "_tidied_noncomm_ev")
    )
  )
)