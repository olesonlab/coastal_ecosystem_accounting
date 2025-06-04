box::use(
  bslib[card, card_header],
  shiny[NS, moduleServer],
  targets[tar_watch_ui, tar_watch_server]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  card( 
    card_header(
      "Commercial and Non-Commercial Fisheries Exchange Values 
      Data Processing"
    ),
    tar_watch_ui(ns("pipeline"))      
  )  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    tar_watch_server("pipeline")
    
  })
}
