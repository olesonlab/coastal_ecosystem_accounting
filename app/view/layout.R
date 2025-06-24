box::use(
  shiny[moduleServer, NS, icon],
  bs4Dash[
    dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody,
    dashboardBrand, sidebarMenu, menuItem, tabItems, tabItem, box,
    jumbotron
  ],
  htmltools[HTML],
  app/view[demo_table, demo_chart, global_filters, theme]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  dashboardPage(
    # ----------- Custom Theme ------------ 
    freshTheme = theme$theme,
    
    # ------------- Settings -------------- 
    # Right nav bar
    fullscreen = TRUE,
    help = NULL,
    scrollToTop = TRUE,
    
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
        menuItem("Dashboard", tabName = "dashboard", icon = icon("bar-chart")),
        menuItem("About", tabName = "about", icon = icon("circle-info"))
        
      )
    ),
    
    # --------- Right Control Bar --------- 
    controlbar = global_filters$ui(ns("filters")),
    
    # ---------------- Body ----------------
    body = dashboardBody(
      
      tabItems(
        # Dashboard Section
        tabItem(
          tabName = "dashboard",
          box(demo_table$ui(ns("table")), width = 12),
          box(demo_chart$ui(ns("chart")), width = 12)
        ),
        
        # About Section
        tabItem(
          tabName = "about",
          # Welcome card
          jumbotron(
            # Title
            title = "Welcome!",
            # Color of card
            status = "info",
            # Subtitle
            lead = "Lorem ipsum dolor sit amet consectetur adipiscing elit. 
            Quisque faucibus ex sapien vitae pellentesque sem placerat. 
            In id cursus mi pretium tellus duis convallis. Tempus leo eu aenean 
            sed diam urna tempor. Pulvinar vivamus fringilla lacus nec metus 
            bibendum egestas. Iaculis massa nisl malesuada lacinia integer nunc 
            posuere. Ut hendrerit semper vel class aptent taciti sociosqu. Ad 
            litora torquent per conubia nostra inceptos himenaeos."
          #   # Download button
          #   btnName = "Download",
          #   # Dataset link
          #   href = "",
          #   # Extra text
          #   "Neque porro quisquam est qui dolorem ipsum quia dolor sit amet, 
          # consectetur, adipisci velit..."
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    global_filters$server("filters")
    theme$server("theme")
  })
}
