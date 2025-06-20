
# Import shapefiles -------------------------------------------------------

import_shapefiles <- function(root_dir, shapefile_names) {
  shapefile_paths <- file.path(root_dir, shapefile_names, paste0(shapefile_names, ".shp"))
  sf_list <- purrr::map(shapefile_paths, ~sf::st_read(.x, quiet = TRUE))
  names(sf_list) <- shapefile_names
  sf_list
}
mhi_moku_raw_dir <- here::here("data/raw/spatial/mhi_mokus")
mhi_moku_file_names <- list(
  "himarc_mhi_marine_mokus",
  "himarc_mhi_terrestrial_mokus"
)
mhi_mokus_sf <- import_shapefiles(mhi_moku_raw_dir, mhi_moku_file_names)
raw_marine_mhi_mokus_sf <- mhi_mokus_sf$himarc_mhi_marine_mokus
raw_terrestrial_mhi_mokus_sf <- mhi_mokus_sf$himarc_mhi_terrestrial_mokus

# Tidy data ---------------------------------------------------------------

tidy_marine_mhi_mokus_sf <- function(raw_sf) {
  raw_sf |>
    # Standardize column names
    janitor::clean_names() |>
    # Subset and rename columns
    dplyr::select(name2, island, moku_olelo, geometry) |>
    # Separate `moku_olelo` into `island_olelo` and `moku_olelo` by first space
    tidyr::separate(
      moku_olelo,
      into = c("island_olelo", "moku_olelo"),
      sep = " ",
      remove = TRUE
    ) |>
    # Remove rows missing `name2`
    dplyr::filter(!is.na(name2)) |>
    # Assign or standardize values for special cases
    dplyr::mutate(
      island_olelo = dplyr::case_when(
        name2 == "MANA" ~ "Kauaʻi",
        name2 == "KAUPO" ~ "Maui",
        name2 == "KAHIKINUI" ~ "Maui",
        TRUE ~ island_olelo
      ),
      moku_olelo = dplyr::case_when(
        name2 == "MANA" ~ stringr::str_to_title(name2),
        name2 == "KAUPO" ~ "Kaupō",
        name2 == "KAHIKINUI" ~ stringr::str_to_title(name2),
        TRUE ~ moku_olelo
      ),
      # Create cleaned `moku` column from `name2`, standardize, and remove from space onward
      moku = stringr::str_replace(stringr::str_to_title(name2), " .*$", "")
    ) |>
    # Rearrange columns
    dplyr::select(moku, island, moku_olelo, island_olelo, name2, geometry)
}

tidied_marine_mhi_mokus_sf <- tidy_marine_mhi_mokus_sf(raw_marine_mhi_mokus_sf)

tidy_terrestrial_mhi_mokus_sf <- function(raw_sf) {
  raw_sf |>
    # Standardize column names
    janitor::clean_names() |>
    # Subset and order columns
    dplyr::select(name2, island, geometry)
}

tidied_terrestrial_mhi_mokus_sf <- tidy_terrestrial_mhi_mokus_sf(raw_terrestrial_mhi_mokus_sf)

# Make valid, then re-project layers -------------------------------------------------------

# Check CRS's of each sf object 
sf::st_crs(tidied_marine_mhi_mokus_sf)
sf::st_crs(tidied_terrestrial_mhi_mokus_sf)

# Use EPSG:3563 (NAD83 / Hawaii Albers Equal Area Conic) to ensure accurate, consistent area calculations across all of the Main Hawaiian Islands
reproject_sf_object <- function(sf_object, target_crs) {
  sf_object |> 
    sf::st_make_valid() |>
    sf::st_transform(crs = target_crs)
}

# Correct any invalid geometries
reprojected_marine_mhi_mokus_sf <- tidied_marine_mhi_mokus_sf |> 
  reproject_sf_object(3563)
reprojected_terrestrial_mhi_mokus_sf <- tidied_terrestrial_mhi_mokus_sf |> 
  reproject_sf_object(3563)

# Check CRS's of each sf object 
sf::st_crs(reprojected_marine_mhi_mokus_sf)
sf::st_crs(reprojected_terrestrial_mhi_mokus_sf)

# Snap sf objects together ------------------------------------------------

fill_gaps_between_sfs <- function(marine_sf, terrestrial_sf, buffer_dist, group_col) {
  marine_buffer <- sf::st_buffer(marine_sf, buffer_dist)
  terrestrial_buffer <- sf::st_buffer(terrestrial_sf, buffer_dist)
  combined_buffers <- dplyr::bind_rows(marine_buffer, terrestrial_buffer)
  
  filled <- combined_buffers |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      geometry = sf::st_union(geometry),
      dplyr::across(dplyr::everything(),  # for every non‐geometry column
             ~ dplyr::first(.x)),   # grab its first value in the group
      .groups = "drop"
    ) |> 
    sf::st_buffer(-buffer_dist) |> 
    sf::st_make_valid()
  
  return(filled)
}

buffer_dist <- 215 # meters, adjust for your data's typical gap width

filled_mhi_mokus_sf <- fill_gaps_between_sfs(
  marine_sf = reprojected_marine_mhi_mokus_sf,
  terrestrial_sf = reprojected_terrestrial_mhi_mokus_sf,
  buffer_dist = buffer_dist,
  group_col = "name2"
)

mapview::mapview(filled_mhi_mokus_sf, col.regions = "green", zcol = "name2") +
  mapview::mapview(reprojected_marine_mhi_mokus_sf, col.regions = "blue") +
  mapview::mapview(reprojected_terrestrial_mhi_mokus_sf, col.regions = "red")

# Export to gpkg ----------------------------------------------------------

export_to_gpkg <- function(sf, file_name, dir) {
  # Use glue to combine directory, file name, and extension correctly
  st_write(sf, glue("{dir}/{file_name}.gpkg"), delete_dsn = TRUE)
}

exported_mhi_mokus_sf_3563 <- export_to_gpkg(
  sf = filled_mhi_mokus_sf_3563, 
  file_name = paste0(get_todays_date(), "_tidied_mhi_mokus_epsg_3563"), 
  dir = here::here("data/processed/spatial")
)

# Check -------------------------------------------------------------------

check <- sf::st_read(here::here("data/processed/spatial/20250620_tidied_mhi_mokus_epsg_3563.gpkg"), quiet = TRUE)
mapview::mapview(check, zcol = "name2")
