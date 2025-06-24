# Import functions from specific libraries
box::use(
  shiny[moduleServer, NS],
  echarts4r[echarts4rOutput, renderEcharts4r],
)

# Import rhino module
box::use(
  app/logic/demo,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  echarts4rOutput(ns("chart"))
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$chart <- renderEcharts4r(
      demo$chart(data())
    )
  })
}
