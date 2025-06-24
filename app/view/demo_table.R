# Import functions from specific libraries
box::use(
  shiny[moduleServer, NS],
  reactable[reactableOutput, renderReactable]
)

# Import rhino module
box::use(
  app/logic/demo,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("table"))
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderReactable(
      demo$table(data())
    )
  })
}
