box::use(
  shiny[
    NS, tagList, moduleServer, radioButtons, 
    sliderInput, checkboxGroupInput, selectInput, selectizeInput, conditionalPanel
  ],
  bs4Dash[controlbarMenu, dashboardControlbar, box], 
  bsicons[bs_icon]
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
      
      # toggle: choose whether you want a period *or* a year filter
      box(
        title = tagList(
          # bs_icon("filter"), 
          "Filter by"
        ),
        collapsible = TRUE,
        collapsed = FALSE,
        width = 12,
        radioButtons(
          inputId = ns("filter_mode"),
          label = "Filter type:",
          choices = c("Period", "Year"),
          selected = "Period",
          inline = TRUE
        )
      ),

      # Period filter (only when filter_mode == "Period")
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Period'", ns("filter_mode")),
        box(
          title = tagList(
            # bs_icon("calendar-date"), 
            "Period"
          ),
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          # selectInput or sliderInput, as before
          selectInput(
            inputId = ns("period_select"),
            label = "Select Period:",
            choices = c("2013–2016","2016–2019","2013–2019"),
            selected = "2013–2019"
          )
        )
      ),

      # Year filter (only when filter_mode == "Year")
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Year'", ns("filter_mode")),
        box(
          title = tagList(
            # bs_icon("calendar-alt"), 
            "Year"
          ),
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          selectInput(
            inputId = ns("year_select"),
            label = "Select Year:",
            choices = c(2013, 2016, 2019),
            selected = 2012    # default value
            # multiple = FALSE # default is single-select
          )
        )
      ),

      # Island Filter
      box(
        title = tagList(
          # bs_icon("globe"), 
          "Island"
        ),
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        selectizeInput(
          inputId = ns("island"),
          label = "Select Island(s):",
          choices = c(
            "Hawaiʻi", "Maui", "Oʻahu", "Kauaʻi",
            "Molokaʻi", "Lānaʻi", "Niʻihau", "Kahoʻolawe"
          ),
          selected = "Oʻahu",
          multiple = TRUE,
          options = list(
            placeholder = "Select Island(s)",
            plugins = list("remove_button")
          )
        )
      ),

      # Ecosystem Filter
      box(
        title = tagList(
          # bs_icon("bar-chart"), 
          "Ecosystem Type"
        ),
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        selectizeInput(
          inputId  = ns("ecosystem"),
          label = "Select Ecosystem Type:",
          choices = list(
            "Terrestrial" = c(
              "Developed", "Cropland", "Pasture", "Grass/Shrub",
              "Tree Cover", "Freshwater Wetlands",
              "Estuarine Wetlands", "Beaches/Dunes"
            ),
            "Marine" = c(
              "Soft Bottom", "Other Hard Bottom", "Pavement",
              "Coral Dominated Hard Bottom", "Rock/Boulder",
              "Open Ocean", "Reef"
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

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}
