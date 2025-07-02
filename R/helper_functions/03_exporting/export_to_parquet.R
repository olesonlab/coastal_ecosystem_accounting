export_to_parquet <- function(df, dir, file_name) {
  # build a "<dir>/<file_name>.parquet" path
  file_path <- create_file_path(dir = dir, file_name = file_name, ext = "parquet")
  # write it out
  arrow::write_parquet(df, file_path)
  invisible(file_path)
}