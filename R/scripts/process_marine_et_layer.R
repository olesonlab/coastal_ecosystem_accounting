
# Import raster -----------------------------------------------------------

create_file_path <- function(dir, file_name, ext) {
  here::here(glue::glue("{dir}/{file_name}.{ext}"))
}

raw_marine_ets_file_path <- create_file_path(
  dir = "data/raw/spatial/himarc_benthic_habitat_classes",
  file_name = "himarc_benthic_habitat_classes_2022",
  ext = "tif"
)

raw_marine_ets_rast <- terra::rast(raw_marine_ets_file_path)

terra::plot(raw_marine_ets_rast)

# Reclassify raster -------------------------------------------------------

reclassify_raster <- function(et_rast) {
  # Define the class mapping using numeric values directly
  reclass_matrix <- matrix(c(
    1, 2,  # Soft Bottom to Open Ocean
    2, 1,  # Other Hard Bottom to Reef
    3, 1,  # Rock/Boulder to Reef
    4, 1,  # Pavement to Reef
    5, 1   # Coral Dominated Hard Bottom to Reef
  ), ncol = 2, byrow = TRUE)
  
  # Reclassify the raster
  reclassified_raster <- terra::classify(et_rast, rcl = reclass_matrix, others = NA)
  
  # Assign new class names to the reclassified raster
  levels(reclassified_raster) <- list(data.frame(value = c(1, 2), class = c("Reef", "Open Ocean")))
  
  # Replace NA values with 0 or another chosen value
  # reclassified_raster <- subst(reclassified_raster, NA, 0)
  
  return(reclassified_raster)
}

reclassified_marine_ets_rast <- reclassify_raster(raw_marine_ets_rast)

terra::plot(reclassified_marine_ets_rast)

# Convert to vector for viz -----------------------------------------------

convert_rast_to_sf <- function(rast) {
  terra::as.polygons(rast, dissolve = TRUE) |> 
    sf::st_as_sf() |> 
    sf::st_make_valid()
}

classified_marine_ets_sf <- convert_rast_to_sf(reclassified_marine_ets_rast)
terra::plot(classified_marine_ets_sf)

# Re-project layer --------------------------------------------------------

reproject_sf_object <- function(sf_object, target_crs) {
  sf::st_transform(sf_object, crs = target_crs)
}

reprojected_marine_ets_sf_3563 <- reproject_sf_object(classified_marine_ets_sf, 3563)
reprojected_marine_ets_sf_4326<- reproject_sf_object(classified_marine_ets_sf, 4326)

# Export as gpkg ----------------------------------------------------------

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

exported_marine_ets_sf_3563 <- export_to_gpkg(
  sf = reprojected_marine_ets_sf_3563, 
  file_name = paste0(
    get_todays_date(), 
    "_classified_marine_ets_epsg_3563"
  ), 
  dir = here::here("data/processed/spatial")
)

exported_marine_ets_sf_4326 <- export_to_gpkg(
  sf = reprojected_marine_ets_sf_4326, 
  file_name = paste0(
    get_todays_date(), 
    "_classified_marine_ets_epsg_4326"
  ), 
  dir = here::here("data/processed/spatial")
)