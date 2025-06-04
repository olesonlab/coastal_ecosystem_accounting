# Functions used
box::use(
  shiny[moduleServer, NS, fluidPage]
)

# Imported shiny modules
box::use(
  # Displays interactive `{targets}` pipeline for data 
  # processing workflow and transparency
  app/view/targets_pipeline_diagram
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Interactive targets pipeline
    targets_pipeline_diagram$ui("pipeline_diagram")
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Interactive targets pipeline
    targets_pipeline_diagram$server("pipeline_diagram")
    
  })
}
