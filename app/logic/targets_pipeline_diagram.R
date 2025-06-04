box::use(
  targets[tar_visnetwork]
)

#' @export
generate_targets_diagram <- function(...) {
  targets::tar_visnetwork(...)
  return()
}
