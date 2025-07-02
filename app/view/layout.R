#app/view/layout.R
box::use(
  shiny[
    moduleServer,
    NS,
    icon,
    tagList,
    column,
    actionButton,
    tabPanel,
    fluidRow,
    reactive,
    br,
    div,
    h5,
    selectInput,
    selectizeInput
  ],
  bs4Dash[
    dashboardPage,
    dashboardHeader,
    dashboardSidebar,
    dashboardBody,
    dashboardBrand,
    sidebarMenu,
    menuItem,
    tabItems,
    tabItem,
    box,
    jumbotron,
    tabBox,
    boxSidebar
  ],
  htmltools[HTML, strong, h5],
  waiter[spin_fading_circles, spin_1, waiter_hide],
)

box::use(
  app/view[
      # noncomm_ev_table,
      # comm_ev_table,
      global_filters,
      theme,
      rainfall_change_summaries_table,
      extents_table
      # extent_area_chart
    ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  # --------------- Layout ----------------
  dashboardPage(
    # ----------- Custom Theme ------------
    freshTheme = theme$theme,

    # ------------- Settings --------------
    # Right nav bar
    fullscreen = TRUE,
    help = NULL,
    scrollToTop = TRUE,

    # ------------- Preloader --------------
    # preloader = list(
    #   html = tagList(spin_1(), "Loading…"),
    #   color = "#343a40"
    # ),

    # --------------- Title ---------------
    title = "Coastal Ecosystem Accounting",

    # -------------- Header ---------------
    header = dashboardHeader(
      title = dashboardBrand(
        title = HTML("<br>"),
        image = "static/logos/cea_logo.png"
      )
    ),

    # ----------- Left Side Bar -----------
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = ns("sidebar"),
        # menuItem(
        #   "About",
        #   tabName = "about",
        #   icon = icon("circle-info")
        # ),
        menuItem(
          "Extents",
          tabName = "extent",
          icon = icon("layer-group")
        ),
        menuItem(
          "Conditions",
          tabName = "condition",
          icon = icon("heartbeat")
        ),
        menuItem(
          "Fisheries Valuation",
          tabName = "fisheries_valuation",
          icon = icon("fish")
        ),
        menuItem(
          "Recreation",
          tabName = "recreation",
          icon = icon("umbrella-beach")
        )
      )
    ),

    # --------- Right Control Bar ---------
    controlbar = global_filters$ui(ns("filters")),

    # ---------------- Body ----------------
    body = dashboardBody(

      tabItems(
        # Extents Section
        tabItem(
          tabName = "extent",
          
          # Chart Box - Full width above the table
          # bs4Dash::box(
          #   title = strong("Ecosystem Areas by Moku"),
          #   status = "primary",
          #   solidHeader = TRUE,
          #   width = 12,
          #   maximizable = TRUE,
          #   collapsible = TRUE,
            
          #   extent_area_chart$ui(ns("extent_chart"))
          # ),
          
          # Existing Table Box with filters
          bs4Dash::box(
            title = strong("Ecosystem Extent Changes (km² & %)"),
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            maximizable = TRUE,
            collapsible = TRUE,
            
            # Filter toggle section
            div(
              style = "margin-bottom: 15px;",
              bs4Dash::actionButton(
                ns("toggle_filters"),
                "Toggle Filters",
                status = "secondary",
                size = "sm",
                icon = icon("filter")
              )
            ),
            
            # Collapsible filter panel
            shiny::conditionalPanel(
              condition = "input.toggle_filters % 2 == 1",
              ns = ns,
              div(
                class = "card",
                style = "margin-bottom: 20px;",
                div(
                  class = "card-body",
                  style = "background-color: #f8f9fa;",
                  fluidRow(
                    column(3,
                      selectizeInput(
                        inputId = ns("island_filter"),
                        label = "Island(s):",
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
                    column(3,
                      selectizeInput(
                        inputId = ns("moku_filter"),
                        label = "Moku(s):",
                        choices = list(
                          "Hawaiʻi" = c(
                            "Hāmākua", "Hilo", "Kaʻū", "Kohala", "Kona", "Puna"
                          ),
                          "Maui" = c(
                            "Hāmākualoa", "Hāmākuapoko", "Hāna", "Honuaʻula", "Kahikinui", 
                            "Kaupō", "Kīpahulu", "Kula", "Lāhainā", "Wailuku"
                          ),
                          "Oʻahu" = c(
                            "ʻEwa", "Haleleʻa", "Koʻolau", "Koʻolauloa", "Koʻolaupoko", 
                            "Waialua", "Waʻianae"
                          ),
                          "Kauaʻi" = c(
                            "Kāʻanapali", "Kealaloloa", "Mana", "Nāpali"
                          ),
                          "Molokaʻi" = c(
                            "Hālawa", "Kaluakoʻi", "Pālāʻau"
                          )
                        ),
                        selected = "ʻEwa",
                        multiple = TRUE,
                        options = list(
                          placeholder = "Select Moku(s)",
                          plugins = list("remove_button"),
                          optgroupField = "island",
                          labelField = "name",
                          searchField = c("name", "island")
                        )
                      )
                    ),
                    column(3,
                      selectizeInput(
                        inputId = ns("realm_filter"),
                        label = "Realm(s):",
                        choices = c("Marine", "Terrestrial"),
                        selected = "Terrestrial",
                        multiple = TRUE,
                        options = list(
                          placeholder = "Select Realm(s)",
                          plugins = list("remove_button")
                        )
                      )
                    ),
                    column(3,
                      selectizeInput(
                        inputId = ns("ecosystem_type_filter"),
                        label = "Ecosystem Type(s):",
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
                        selected = "Developed",
                        multiple = TRUE,
                        options = list(
                          placeholder = "Select Ecosystem Type(s)",
                          plugins = list("remove_button")
                        )
                      )
                    )
                  ),
                  
                  # Filter action buttons
                  div(
                    style = "text-align: center; margin-top: 15px; display: flex; justify-content: center; gap: 10px;",
                    bs4Dash::actionButton(
                      ns("apply_filters"),
                      "Apply Filters",
                      status = "secondary",
                      icon = icon("search"),
                      size = "sm"
                    ),
                    bs4Dash::actionButton(
                      ns("reset_filters"),
                      "Reset All Filters",
                      status = "secondary",
                      icon = icon("refresh"),
                      size = "sm"
                    )
                  )
                )
              )
            ),
            
            extents_table$ui(ns("table"))
          )
        ),
      
        # Conditoins Section
        tabItem(
          tabName = "condition",

          bs4Dash::box(
            title = strong("Mean Rainfall Change (mm & %)"),
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            maximizable = TRUE,
            collapsible = TRUE,
            
            # Filter toggle section
            div(
              style = "margin-bottom: 15px;",
              bs4Dash::actionButton(
                ns("toggle_filters"),
                "Toggle Filters",
                status = "secondary",
                size = "sm",
                icon = icon("filter")
              )
            ),
            
            # Collapsible filter panel
            shiny::conditionalPanel(
              condition = "input.toggle_filters % 2 == 1",
              ns = ns,
              div(
                class = "card",
                style = "margin-bottom: 20px;",
                div(
                  class = "card-body",
                  style = "background-color: #f8f9fa;",
                  fluidRow(
                    column(4,
                      selectizeInput(
                        inputId = ns("island_filter"),
                        label = "Island(s):",
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
                    column(4,
                      selectizeInput(
                        inputId = ns("moku_filter"),
                        label = "Moku(s):",
                        choices = list(
                          "Hawaiʻi" = c(
                            "Hāmākua", "Hilo", "Kaʻū", "Kohala", "Kona", "Puna"
                          ),
                          "Maui" = c(
                            "Hāmākualoa", "Hāmākuapoko", "Hāna", "Honuaʻula", "Kahikinui", 
                            "Kaupō", "Kīpahulu", "Kula", "Lāhainā", "Wailuku"
                          ),
                          "Oʻahu" = c(
                            "ʻEwa", "Haleleʻa", "Koʻolau", "Koʻolauloa", "Koʻolaupoko", 
                            "Waialua", "Waʻianae"
                          ),
                          "Kauaʻi" = c(
                            "Kāʻanapali", "Kealaloloa", "Mana", "Nāpali"
                          ),
                          "Molokaʻi" = c(
                            "Hālawa", "Kaluakoʻi", "Pālāʻau"
                          )
                        ),
                        selected = "ʻEwa",
                        multiple = TRUE,
                        options = list(
                          placeholder = "Select Moku(s)",
                          plugins = list("remove_button"),
                          optgroupField = "island",
                          labelField = "name",
                          searchField = c("name", "island")
                        )
                      )
                    ),
                    column(4,
                      selectizeInput(
                        inputId  = ns("ecosystem_type_filter"),
                        label = "Ecosystem Type(s):",
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
                        selected = "Developed",
                        multiple = TRUE,
                        options = list(
                          placeholder = "Select Ecosystem Type(s)",
                          plugins = list("remove_button")
                        )
                      )
                    )
                  ),
                  
                  # Centered button row matching the toggle filters button style
                  div(
                    style = "text-align: center; margin-top: 15px; display: flex; justify-content: center; gap: 10px;",
                    bs4Dash::actionButton(
                      ns("apply_filters"),
                      "Apply Filters",
                      status = "secondary",  # Same as toggle filters button
                      icon = icon("search"),
                      size = "sm"  # Same size as toggle filters and reset buttons
                    ),
                    bs4Dash::actionButton(
                      ns("reset_filters"),
                      "Reset All Filters",
                      status = "secondary",  # Same as toggle filters button (not yellow)
                      icon = icon("refresh"),
                      size = "sm"
                    )
                  )
                )
              )
            ),
            
            rainfall_change_summaries_table$ui(ns("rainfall_change_summaries_table"))
          )

          # box(
          #   title = strong("Mean Rainfall Change (mm & %)"),
          #   rainfall_change_summaries_table$ui(ns("rainfall_change_summaries_table")),
          #   width = 12
          # )
          # box(demo_chart$ui(ns("chart")), width = 12)
        ),

        # Fisheries Valuation Section
        tabItem(
          tabName = "fisheries_valuation",
          # box(conditions_section$ui(ns("rainfall_change_summaries_table")), width = 12)
          # box(demo_chart$ui(ns("chart")), width = 12)
        ),

        # Recreation Section
        tabItem(
          tabName = "reacreation",
          # box(conditions_section$ui(ns("rainfall_change_summaries_table")), width = 12)
          # box(demo_chart$ui(ns("chart")), width = 12)
        )
        
        # # About Section
        # tabItem(
        #   tabName = "about",
        #   # Welcome card
        #   jumbotron(
        #     # Title
        #     title = "Welcome!",
        #     # Color of card
        #     status = "info",
        #     # Subtitle
        #     lead = "Lorem ipsum dolor sit amet consectetur adipiscing elit. 
        #     Quisque faucibus ex sapien vitae pellentesque sem placerat. 
        #     In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean 
        #     sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus 
        #     bibenextent_area_chartdum egestas. Iaculis massa nisl malesuada lacinia integer nunc 
        #     posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad 
        #     litora torquent per conubia nostra inceptos himenaeos."
        #   #   # Download button
        #   #   btnName = "Download",
        #   #   # Dataset link
        #   #   href = "",
        #   #   # Extra text
        #   #   "Neque porro quisquam est qui dolorem ipsum quia dolor sit amet, 
        #   # consectetur, adipisci velit..."
        #   )
        # )
      )
    )
  )
}

#' @export
server <- function(id, all_data) {  # Add all_data parameter
  moduleServer(id, function(input, output, session) {
    # Initialize global stuff once
    global_filters$server("filters")
    # theme$server("theme")
  })
}

