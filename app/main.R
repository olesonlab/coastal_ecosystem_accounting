# app/main.R
box::use(
  shiny[div, moduleServer, NS, reactive],
)

# Import shiny modules
box::use(
  app/logic/data_loader[load_all_data],
  app/logic/conditions,
  app/view[
    layout, rainfall_change_summaries_table, extents_table
  ],
)

all_data <- load_all_data()

#' @export
ui <- function(id) {
  ns <- NS(id)
  layout$ui(id)
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    global_filter_values <- layout$server("layout", all_data)
    rainfall_change_summaries_table$server("rainfall_change_summaries_table", all_data) 
    extents_table$server("table", all_data)
  })
}

