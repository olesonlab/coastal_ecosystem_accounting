
# File path(s) ------------------------------------------------------------

file_paths <- c(
  # Fisheries valuation data
  "noncomm_ev" = "data/processed/fisheries_valuation/20250619_tidied_noncomm_ev.csv",
  "comm_ev" = "data/processed/fisheries_valuation/20250619_tidied_comm_ev.csv",
  # Extents data
  "extents" = "data/processed/extents/20250625_extent_changes.csv",
  # Conditions data  
  "terrestrial_conditions" = "data/processed/conditions/20250701_summarized_terrestrial_conditions.csv",
  "coral_condition_indicators" = "data/processed/conditions/20250701_tidied_coral_condition_indicators.csv",
  # Spatial layers
  "classified_marine_ets" = "data/processed/spatial/20250625_classified_marine_ets_epsg_3563.gpkg",
  "reclassified_marine_ets" = "data/processed/spatial/20250625_reclassified_marine_ets_epsg_3563.gpkg",
  "fish_catch_areas" = "data/processed/spatial/20250624_tidied_fish_catch_areas_epsg_3563.gpkg", 
  "mhi_mokus" = "data/processed/spatial/20250620_tidied_mhi_mokus_epsg_3563.gpkg"
)

csv_paths <- file_paths[stringr::str_detect(file_paths, "\\.csv$")]
gpkg_paths <- file_paths[stringr::str_detect(file_paths, "\\.gpkg$")]

# Import ------------------------------------------------------------------

import_csv <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }
  
  readr::read_csv(
    file = file_path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

import_gpkg <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }
  
  if (!fs::file_exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }
  
  sf::st_read(file_path, quiet = TRUE) |> 
    sf::st_make_valid()
}

csv_dfs <- purrr::map(csv_paths, import_csv)
spatial_dfs <- purrr::map(gpkg_paths, import_gpkg)

dfs <- c(csv_dfs, spatial_dfs)

# Merge exchange values to spatial layers ---------------------------------

# For commercial exchange values
comm_ev_per_catch_area_et_sf <- 
  sf::st_intersection(
    dfs$fish_catch_areas |> 
      dplyr::select(-island),
    dfs$mhi_mokus |> 
      dplyr::select(name2, moku, island, moku_olelo, island_olelo)
  ) |>
  sf::st_intersection(
    dfs$reclassified_marine_ets,
    left = TRUE
  ) |> 
  sf::st_make_valid() |> 
  dplyr::select(-type) |> 
  dplyr::right_join(
    dfs$comm_ev |> dplyr::select(-county),
    by = c("area_id", "ecosystem_type"),
    relationship = "many-to-many"
  ) |> 
  # Witheld data (n = 5392)
  dplyr::filter(withheld != TRUE)

# For non-commercial exchange values
noncomm_ev_per_island_et_sf <- sf::st_intersection(
  dfs$mhi_mokus,
  dfs$reclassified_marine_ets
) |> 
  sf::st_make_valid() |> 
  dplyr::right_join(
    dfs$noncomm_ev,
    by = c("island", "ecosystem_type"),
    relationship = "many-to-many"
  )

# mapview::mapview(dfs$mhi_mokus, col.region = "green") +
#   mapview::mapview(dfs$marine_ets, zcol = "class") + 
#   mapview::mapview(dfs$fish_catch_areas, col.region = "salmon") +
#   mapview::mapview(comm_ev_per_catch_area_et_sf, zcol = "ecosystem_type")

mapview::mapview(comm_ev_per_catch_area_et_sf, zcol = "ecosystem_type")
  
mapview::mapview(noncomm_ev_per_island_et_sf, zcol = "ecosystem_type")


# Merge extents to classified marine et raster ----------------------------

marine_extents_per_moku_et <- sf::st_intersection(
  dfs$mhi_mokus,
  dfs$classified_marine_ets
) |> 
  sf::st_make_valid() |> 
  dplyr::right_join(
    dfs$extents,
    by = c("name2", "ecosystem_type"),
    relationship = "many-to-many"
  ) |> 
  dplyr::filter(
    realm == "Marine", 
    ecosystem_type != "Open Ocean"
  )

# Merge tree cover rainfall to moku layer ---------------------------------

tree_cover_rainfall_per_moku <- dplyr::left_join(
  dfs$tree_cover_rainfall,
  dfs$mhi_mokus,
  by = "name2"
) |> 
  sf::st_as_sf()

# Re-project sfs ----------------------------------------------------------

# Define the sf objects and their base names
sf_objects <- list(
  comm_ev_per_catch_area_et_sf = comm_ev_per_catch_area_et_sf,
  noncomm_ev_per_island_et_sf = noncomm_ev_per_island_et_sf
  # marine_extents_per_moku_et_sf = marine_extents_per_moku_et,
  # tree_cover_rainfall_per_moku_sf = tree_cover_rainfall_per_moku
)

# Define target CRS values
target_crs_list <- c(3563, 4326)

# Create all reprojected objects
reprojected_objects <- purrr::map(target_crs_list, function(crs) {
  purrr::map(sf_objects, ~ sf::st_transform(.x, crs = crs)) |>
    purrr::set_names(paste0("reprojected_", names(sf_objects), "_", crs))
}) |>
  purrr::flatten()

# Assign to global environment
purrr::iwalk(reprojected_objects, ~ assign(.y, .x, envir = .GlobalEnv))

# Export ------------------------------------------------------------------

# Helper functions 
export_to_gpkg <- function(layers, file_names = NULL, dir) {
  # Ensure output directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # Single sf object
  if (inherits(layers, "sf")) {
    if (!is.character(file_names) || length(file_names) != 1) {
      stop(
        "When exporting a single sf, 'file_names' must be a single string.",
        call. = FALSE
      )
    }
    out_path <- file.path(dir, paste0(file_names, ".gpkg"))
    sf::st_write(layers, out_path, delete_dsn = TRUE)
    
    # List of sf objects
  } else if (
    is.list(layers) &&
    all(vapply(layers, inherits, logical(1), what = "sf"))
  ) {
    # Determine file names
    if (is.null(file_names)) {
      if (is.null(names(layers)) || any(names(layers) == "")) {
        stop(
          "For multiple layers, supply 'file_names' or a named list of 'layers'.",
          call. = FALSE
        )
      }
      file_names <- names(layers)
    }
    if (length(file_names) != length(layers)) {
      stop(
        "Length of 'file_names' must match number of sf objects.",
        call. = FALSE
      )
    }
    
    # Export each layer
    purrr::walk2(
      layers, file_names,
      function(sf_obj, fn) {
        out_path <- file.path(dir, paste0(fn, ".gpkg"))
        sf::st_write(sf_obj, out_path, delete_dsn = TRUE)
      }
    )
    
  } else {
    stop(
      "'layers' must be either a single sf object or a list of sf objects.",
      call. = FALSE
    )
  }
  
  invisible(NULL)
}

get_todays_date <- function() {
  base::format(base::Sys.Date(), "%Y%m%d")
}

# remove leading "reprojected_" from each name
clean_names <- sub("^reprojected_", "", names(reprojected_objects))

file_names <- paste0(
  get_todays_date(), "_",
  clean_names
)

export_to_gpkg(
  layers = reprojected_objects,
  file_names = file_names,
  dir = here::here("data/processed/spatial")
)

# Check -------------------------------------------------------------------

check <- sf::st_read(here::here("data/processed/spatial/20250625_comm_ev_per_catch_area_et_sf_4326.gpkg"), quiet = TRUE) 

mapview::mapview(check)








# 1. Filter to 2019 Pelagics in Reef and aggregate per moku
df_map <- reprojected_comm_ev_per_catch_area_et_sf_4326 |>
  dplyr::filter(
    year           == 2019,
    species_group  == "Pelagics",
    ecosystem_type == "Reef"
  ) |>
  dplyr::group_by(moku) |>
  dplyr::summarise(
    exchange_value = sum(exchange_value, na.rm = TRUE),
    geometry       = sf::st_union(geom),
    .groups        = "drop"
  ) |>
  sf::st_drop_geometry()

# 2. Register the sf object globally as "moku_map"
echarts4r::e_map_register_p(df_map, "moku_map")

# 3. Plot the choropleth
df_map |>
  sf::st_drop_geometry() |>
  echarts4r::e_charts(moku) |>
  echarts4r::e_map(
    serie = exchange_value,
    map   = "moku_map",
    name  = "Exchange Value"
  ) |>
  echarts4r::e_visual_map(
    serie  = exchange_value,
    orient = "vertical",
    left   = "left"
  ) |>
  echarts4r::e_title("2019 Pelagics Exchange Value by Moku")

choropleth <- data.frame(
  countries = c(
    "France",
    "Brazil",
    "China",
    "Russia",
    "Canada",
    "India",
    "United States",
    "Argentina",
    "Australia"
  ),
  values = round(runif(9, 10, 25))
)

choropleth |>
  echarts4r::e_charts(countries) |>
  echarts4r::e_map(values) |>
  echarts4r::e_visual_map(min = 10, max = 25)
