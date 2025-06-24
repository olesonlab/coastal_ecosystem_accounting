# app/logic/utils.R
box::use(
  readr[read_csv],
  here[here]
)

#' @export
import_csv <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single character string.", call. = FALSE)
  }
  
  read_csv(file = here(file_path), show_col_types = FALSE, progress = FALSE)
}