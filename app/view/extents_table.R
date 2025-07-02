# app/view/extents_table.R
# Import functions from specific libraries
box::use(
  shiny[moduleServer, NS],
  reactable[reactableOutput, renderReactable]
)

# Import rhino module
box::use(
  app/logic/extents,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("table"))
}

#' @export
server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable(
      extents$extents_table(all_data)
    )
  })
}