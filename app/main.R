box::use(
  shiny[div, moduleServer, NS, reactive],
)

# Import shiny modules
box::use(
  app/logic/demo,
  app/view[demo_table, demo_chart, layout],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  layout$ui(id)
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(demo$fetch_data())
    demo_table$server("table", data)
    demo_chart$server("chart", data)
  })
}