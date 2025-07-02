processed_marine_et_layer <- list(
  # 1. Track file path for updates ------------------------------------------
  tar_target(
    raw_marine_ets_file_path,
    create_file_path(
      dir = "data/raw/spatial/himarc_benthic_habitat_classes",
      file_name = "himarc_benthic_habitat_classes_2022",
      ext = "tif"
    ),
    format = "file"
  ),
  # Import raw raster -------------------------------------------------------
  tar_target(
    raw_marine_ets_rast,
    terra::rast(raw_marine_ets_file_path)
  ),
  # Reclassify raster -------------------------------------------------------
  tar_target(
    reclassified_marine_ets_rast,
    reclassify_raster(raw_marine_ets_rast)
  ),
  # Convert to vector for viz -----------------------------------------------
  tar_target(
    classified_marine_ets_sf,
    convert_rast_to_sf(reclassified_marine_ets_rast)
  ),
  # Re-project sf -----------------------------------------------------------
  tar_target(
    reprojected_marine_ets_sf_3563,
    reproject_sf_object(classified_marine_ets_sf, 3563)
  ),
  tar_target(
    reprojected_marine_ets_sf_4326,
    reproject_sf_object(classified_marine_ets_sf, 4326)
  ),
  # Export as gpkg ----------------------------------------------------------
  tar_target(
    exported_marine_ets_sf_3563,
    export_to_gpkg(
      sf = reprojected_marine_ets_sf_3563, 
      file_name = paste0(
        get_todays_date(), 
        "_classified_marine_ets_epsg_3563"
      ), 
      dir = here::here("data/processed/spatial")
    )
  ),
  tar_target(
    exported_marine_ets_sf_4326,
    export_to_gpkg(
      sf = reprojected_marine_ets_sf_4326, 
      file_name = paste0(
        get_todays_date(), 
        "_classified_marine_ets_epsg_4326"
      ), 
      dir = here::here("data/processed/spatial")
    )
  ) 
)