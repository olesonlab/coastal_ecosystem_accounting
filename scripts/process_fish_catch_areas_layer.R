# Import shapefiles -------------------------------------------------------

create_file_path <- function(dir, file_name, ext) {
  here::here(glue::glue("{dir}/{file_name}.{ext}"))
}

fish_catch_areas_file_path <- create_file_path(
  dir = "data/raw/spatial/dar_fish_catch_areas",
  file_name = "dar_fish_catch_areas_2008",
  ext = "shp"
)

raw_fish_catch_areas_sf <- sf::st_read(fish_catch_areas_file_path, quiet = TRUE
)

# Tidy sf -----------------------------------------------------------------

tidy_fish_catch_areas_sf <- function(raw_sf) {
  raw_sf |>
    # Standardize column names
    janitor::clean_names() |>
    # Subset columns
    dplyr::select(island, area_id, type, geometry) |>
    # Filter to desired area types and islands (including NAs)
    dplyr::filter(
      type %in% c("Island", "MHI Inshore", "MHI Coastal"),
      island %in% c(
        "Kaho'olawe", "Lana'i", "Maui", "O'ahu", "Ni'ihau", "Kauai", "Hawai'i", "Moloka'i"
      ) | is.na(island),
      # Small islands near Oahu
      !(type == "Island" & is.na(island))
    )
}

tidied_fish_catch_areas_sf <- tidy_fish_catch_areas_sf(raw_fish_catch_areas_sf)

# Re-project sf -----------------------------------------------------------

reproject_sf_object <- function(sf_object, target_crs) {
  sf::st_transform(sf_object, crs = target_crs)
}

reprojected_fish_catch_areas_sf_3563 <- reproject_sf_object(
  tidied_fish_catch_areas_sf, 3563
)

reprojected_fish_catch_areas_sf_4326 <- reproject_sf_object(
  tidied_fish_catch_areas_sf, 4326
)

# Export sf ---------------------------------------------------------------

export_to_gpkg <- function(sf, file_name, dir) {
  # Use glue to safely combine directory and file name
  file_path <- glue::glue("{dir}/{file_name}.gpkg")
  
  # Write the sf object to a GeoPackage, overwriting if exists
  sf::st_write(sf, file_path, delete_dsn = TRUE)
  
  invisible(NULL)
}

get_todays_date <- function() {
  base::format(base::Sys.Date(), "%Y%m%d")
}

export_to_gpkg(
  sf = reprojected_fish_catch_areas_sf_3563, 
  file_name = paste0(get_todays_date(), "_tidied_fish_catch_areas_epsg_3563"), 
  dir = here::here("data/processed/spatial")
)

export_to_gpkg(
  sf = reprojected_fish_catch_areas_sf_4326, 
  file_name = paste0(get_todays_date(), "_tidied_fish_catch_areas_epsg_4326"), 
  dir = here::here("data/processed/spatial")
)

# Check -------------------------------------------------------------------

check <- sf::st_read(here::here("data/processed/spatial/20250620_tidied_fish_catch_areas_epsg_4326.gpkg"), quiet = TRUE) 
