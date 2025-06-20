processed_mhi_moku_layers <- list(
  list(
    # 1. Track files for updates ----------------------------------------------
    tar_target(
      mhi_moku_raw_dir,
      here::here("data/raw/mhi_mokus"),
      format = "file"
    ),
    tar_target(
      mhi_moku_file_names,
      c("himarc_mhi_marine_mokus", "himarc_mhi_terrestrial_mokus")
    ),
    # 2. Import raw sfs -------------------------------------------------------
    tar_target(
      mhi_mokus_sf,
      import_shapefiles(mhi_moku_raw_dir, mhi_moku_file_names)
    ),
    tar_target(
      raw_marine_mhi_mokus_sf,
      mhi_mokus_sf$himarc_mhi_marine_mokus
    ),
    tar_target(
      raw_terrestrial_mhi_mokus_sf,
      mhi_mokus_sf$himarc_mhi_terrestrial_mokus
    ),
    # 3. Tidy sfs -------------------------------------------------------------
    tar_target(
      tidied_marine_mhi_mokus_sf,
      tidy_marine_mhi_mokus_sf(raw_marine_mhi_mokus_sf)
    ),
    tar_target(
      tidied_terrestrial_mhi_mokus_sf,
      tidy_terrestrial_mhi_mokus_sf(raw_terrestrial_mhi_mokus_sf)
    ),
    # Re-project sfs ----------------------------------------------------------
    tar_target(
      reprojected_marine_mhi_mokus_sf,
      reproject_sf_object(tidied_marine_mhi_mokus_sf, 3563)
    ),
    tar_target(
      reprojected_terrestrial_mhi_mokus_sf,
      reproject_sf_object(tidied_terrestrial_mhi_mokus_sf, 3563)
    ),
    # Fill gaps between sfs ---------------------------------------------------
    tar_target(
      filled_mhi_mokus_sf_3563,
      fill_gaps_between_sfs(
        marine_sf = reprojected_marine_mhi_mokus_sf,
        terrestrial_sf = reprojected_terrestrial_mhi_mokus_sf,
        buffer_dist = 215,
        group_col = "name2"
      )
    ),
    # Re-project to EPSG:4326 for viz -----------------------------------------
    tar_target(
      filled_mhi_mokus_sf_4326,
      reproject_sf_object(filled_mhi_mokus_sf, 4326)
    ),
    # Export as gpkg ----------------------------------------------------------
    tar_target(
      exported_mhi_mokus_sf_3563,
      export_to_gpkg(
        sf = filled_mhi_mokus_sf_3563, 
        file_name = paste0(get_todays_date(), "_tidied_mhi_mokus_epsg_3563"), 
        dir = here::here("data/processed/mhi_mokus")
      )
    ),
    tar_target(
      exported_mhi_mokus_sf_4326,
      export_to_gpkg(
        sf = filled_mhi_mokus_sf_4326, 
        file_name = paste0(get_todays_date(), "_tidied_mhi_mokus_epsg_4326"), 
        dir = here::here("data/processed/mhi_mokus")
      )
    ) 
  )
)