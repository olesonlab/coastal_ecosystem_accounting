list(
  # 1. Track file path for updates ------------------------------------------
  tar_target(
    fish_catch_areas_file_path,
    create_file_path(
      dir = "data/raw/spatial/dar_fish_catch_areas",
      file_name = "dar_fish_catch_areas_2008",
      ext = "shp"
    ),
    format = "file"
  ),
  # 2. Import raw data ------------------------------------------------------
  tar_target(
    imported_fish_catch_areas_sf,
    raw_fish_catch_areas_sf <- sf::st_read(
      fish_catch_areas_file_path, quiet = TRUE
    )
  ),
  # 3. Tidy sf --------------------------------------------------------------
  tar_target(
    tidied_fish_catch_areas_sf,
    tidy_fish_catch_areas_sf(raw_fish_catch_areas_sf)
  ),
  # Re-project sf -----------------------------------------------------------
  tar_target(
    reprojected_fish_catch_areas_sf_3563,
    reproject_sf_object(tidied_fish_catch_areas_sf, 3563)
  ),
  tar_target(
    reprojected_fish_catch_areas_sf_4326,
    reproject_sf_object(tidied_fish_catch_areas_sf, 4326)
  ),
  # Export as gpkg ----------------------------------------------------------
  tar_target(
    exported_fish_catch_areas_sf_3563,
    export_to_gpkg(
      sf = reprojected_fish_catch_areas_sf_3563, 
      file_name = paste0(get_todays_date(), "_tidied_fish_catch_areas_epsg_3563"), 
      dir = here::here("data/processed/spatial")
    )
  ),
  tar_target(
    exported_fish_catch_areas_sf_4326,
    export_to_gpkg(
      sf = reprojected_fish_catch_areas_sf_4326, 
      file_name = paste0(get_todays_date(), "_tidied_fish_catch_areas_epsg_4326"), 
      dir = here::here("data/processed/spatial")
    )
  ) 
)