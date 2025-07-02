
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

# Process raster ----------------------------------------------------------

assign_new_class_names <- function(rast, values, classes, remove_zeros = TRUE) {
  
  # Remove zeros if requested (set them to NA)
  if (remove_zeros) {
    rast <- terra::subst(rast, 0, NA)
  }
  
  # Assign factor levels with class names
  levels(rast) <- list(data.frame(value = values, class = classes))
  
  # Return the modified raster
  return(rast)
}

classified_marine_ets_rast <- raw_marine_ets_rast |> 
  assign_new_class_names(
    values = 1:5,
    classes = c(
      "Soft Bottom",
      "Other Hard Bottom", 
      "Rock/Boulder",
      "Pavement",
      "Coral Dominated Hard Bottom"
    ),
    remove_zeros = TRUE
  )
terra::plot(classified_marine_ets_rast)

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
  final_raster <- assign_new_class_names(
    rast = reclassified_raster, 
    values = 1:2,
    classes = c("Reef", "Open Ocean"),
    remove_zeros = FALSE
  )
  
  return(final_raster)
}

reclassified_marine_ets_rast <- reclassify_raster(classified_marine_ets_rast)

terra::plot(classified_marine_ets_rast)

# Convert to vector for viz -----------------------------------------------

convert_rast_to_sf <- function(rast) {
  terra::as.polygons(rast, dissolve = TRUE) |> 
    sf::st_as_sf() |> 
    sf::st_make_valid()
}

classified_marine_ets_sf <- convert_rast_to_sf(classified_marine_ets_rast) |> 
  dplyr::rename("ecosystem_type" = class)
terra::plot(classified_marine_ets_sf)

reclassified_marine_ets_sf <- convert_rast_to_sf(reclassified_marine_ets_rast) |> 
  dplyr::rename("ecosystem_type" = class)
terra::plot(reclassified_marine_ets_sf)

# Re-project layer --------------------------------------------------------

reproject_sf_object <- function(sf_object, target_crs) {
  sf::st_transform(sf_object, crs = target_crs)
}

reprojected_classified_marine_ets_sf_3563 <- reproject_sf_object(classified_marine_ets_sf, 3563)
reprojected_classified_marine_ets_sf_4326<- reproject_sf_object(classified_marine_ets_sf, 4326)
reprojected_reclassified_marine_ets_sf_3563 <- reproject_sf_object(reclassified_marine_ets_sf, 3563)
reprojected_reclassified_marine_ets_sf_4326<- reproject_sf_object(reclassified_marine_ets_sf, 4326)

# Export as gpkg ----------------------------------------------------------

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

# Named list
sf_exports <- list(
  "classified_marine_ets_epsg_3563" = reprojected_classified_marine_ets_sf_3563,
  "classified_marine_ets_epsg_4326" = reprojected_classified_marine_ets_sf_4326,
  "reclassified_marine_ets_epsg_3563" = reprojected_reclassified_marine_ets_sf_3563,
  "reclassified_marine_ets_epsg_4326" = reprojected_reclassified_marine_ets_sf_4326
)

file_names <- paste0(get_todays_date(), "_", names(sf_exports))

export_to_gpkg(
  layers = sf_exports,
  file_names = file_names,
  dir = here::here("data/processed/spatial")
)
