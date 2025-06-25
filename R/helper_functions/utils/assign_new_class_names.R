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