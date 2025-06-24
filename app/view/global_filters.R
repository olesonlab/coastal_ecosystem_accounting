box::use(
  shiny[NS, moduleServer, sliderInput, checkboxGroupInput, selectInput, selectizeInput],
  bs4Dash[controlbarMenu, controlbarItem, dashboardControlbar]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  dashboardControlbar(
    id = ns("controlbar"),
    pinned = TRUE,
    overlay = FALSE,
    controlbarMenu(
      id = ns("controlbarmenu"),
      
      # Year range slider
      controlbarItem(
        title = "Years",
        sliderInput(
          inputId = ns("year_slider"),
          label = "Select Year Range:",
          min = 1990,
          max = 2025,
          value = c(2013, 2019),
          sep = "", step = 1
        )
      ),
      
      # Extents per year checkboxes
      controlbarItem(
        title = "Extents Per Year",
        checkboxGroupInput(
          inputId = ns("extents"),
          label = "Choose Extent Year(s)",
          choices = c("2013", "2016", "2019"),
          selected = c("2013", "2016", "2019")
        )
      ),
      
      # Island selectize multiple dropdown menu
      controlbarItem(
        title = "Island",
        selectizeInput(
          inputId = ns("island"),
          label = "Select Island(s):",
          choices = c(
            "Hawaiʻi",
            "Maui", 
            "Oʻahu",
            "Kauaʻi",
            "Molokaʻi",
            "Lānaʻi",
            "Niʻihau",
            "Kahoʻolawe"
          ),
          selected = "Oʻahu",
          multiple = TRUE,
          options = list(
            placeholder = "Select Island(s)",
            plugins = list("remove_button")
          )
        )
      ),
      
      # Ecosystem selectize multiple dropdown menu
      controlbarItem(
        title = "Ecosystem Type",
        selectizeInput(
          inputId = ns("ecosystem"),
          label = "Select Ecosystem Type:",
          choices = list(
            "Terrestrial" = c(
              "Developed",
              "Cropland",
              "Pasture",
              "Grass/Shrub",
              "Tree Cover",
              "Freshwater Wetlands",
              "Estuarine Wetlands",
              "Beaches/Dunes"
            ),
            "Marine" = c(
              "Soft Bottom",
              "Other Hard Bottom",
              "Pavement",
              "Coral Dominated Hard Bottom",
              "Rock/Boulder",
              "Open Ocean",
              "Reef"
            )
          ),
          selected = "Coral Dominated Hard Bottom",
          multiple = TRUE,
          options = list(
            placeholder = "Select Ecosystem Type(s)",
            plugins = list("remove_button")
          )
        )
      )
    )
  )
}

# Optional server (for now can be empty)
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add reactivity later if needed
  })
}
