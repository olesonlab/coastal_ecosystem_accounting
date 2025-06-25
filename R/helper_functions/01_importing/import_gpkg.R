import_gpkg <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }
  
  if (!fs::file_exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }
  
  sf::st_read(file_path, quiet = TRUE)
}