# app/view/rainfall_change_summaries_table.R
# Import functions from specific libraries
box::use(
  shiny[moduleServer, NS],
  reactable[reactableOutput, renderReactable]
)

# Import rhino module
box::use(
  app/logic/conditions,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("rainfall_change_summaries_table"))
}

#' @export
server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    output$rainfall_change_summaries_table <- renderReactable(
      conditions$rainfall_change_summary_table(all_data)
    )
  })
}