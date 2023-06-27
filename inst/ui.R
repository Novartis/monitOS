library(shinydashboard)


dashboardPage(
  skin = "blue",
  dashboardHeader(title = 'Monitoring OS'),
  # Creating tabs
  dashboardSidebar(sidebarMenu(
    # Tab-1
    menuItem(
      "Run",
      tabName = "run",
      icon = icon("person-running"),
      selectInput('method', 'Method :', c('joint', 'cond')),
      textInput('thres1', 'Threshold #1: (csv)', "1.3, 1.2, 1"),
      textInput('thres2', 'Threshold #2: (csv)', "1.5, 1.4, 1.3"),
      textInput('events', 'Events #', "110, 125, 131"))
    ),
    # Tab-2
    menuItem(
      "Use cases",
      tabName = "use cases",
      icon = icon("download"),
      selectInput('study', 'Case study :', c('polarix1', 'polarix2', 'MonarchE', 'Leda'))
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
        shinydashboard::box(status = "primary", plotOutput('plot1', height = '512px')),
        shinydashboard::box(status = "success", plotOutput('plot2', height = '512px')),
        shinydashboard::box(status = "primary", tableOutput("table1")),
        shinydashboard::box(status = "success", tableOutput("table2"))
      )
    )
  ))
)
