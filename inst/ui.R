library(shinydashboard)


dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Monitoring OS"),
  # Creating tabs
  dashboardSidebar(sidebarMenu(
    selectInput("study", "Case study:",
                c("User","Polarix1", "Polarix2", "MonarchE", "Leda", "YTB323")),
    textInput("events", "Events (csv)", "110, 125, 131"),
    textInput("thres1", "Safety thresholds (csv)", "1.3, 1.2, 1"),
    textInput("thres2", "Stopping thresholds (csv)", "NULL"),
    textInput("hrs", "True HRs", "0.7, 1, 1.1, 1.2, 1.5"),
    selectInput("method", "Method :", c("joint", "cond"))
    # Tab-1
    # menuItem(
    #   "Run",
    #   tabName = "run",
    #   icon = icon("person-running"),
    #   selectInput("study", "Case study/User specified parameter:",
    #               c("User input","polarix1", "polarix2", "MonarchE", "Leda")),
    #   textInput("events", "Events (csv)", "110, 125, 131"),
    #   textInput("thres1", "Safety thresholds (csv)", "1.3, 1.2, 1"),
    #   textInput("thres2", "Stop thresholds (csv)", "1.5, 1.4, 1.3"),
    #   selectInput("method", "Method :", c("joint", "cond"))
    # )
  )
  ),
  # Define body
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "Modeling", class = "active"),
    # empty for now
    # Second tab content
    tabItem(
      tabName = "Sampling",
      class = "active",
      fluidRow(
        shinydashboard::box(status = "primary", plotOutput("plot1", height = "512px")),
        shinydashboard::box(status = "success", plotOutput("plot2", height = "512px")),
        shinydashboard::box(status = "primary", tableOutput("table1")),
        shinydashboard::box(status = "success", tableOutput("table2"))
      )
    )
  ))
)
