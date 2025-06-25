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