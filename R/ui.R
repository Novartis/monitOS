#' Title
#'
#' @param request todo
#'
#' @export
#' @import shinydashboard
app_ui <- function(request){

  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Monitoring OS - August 23rd"),
    # Creating tabs
    dashboardSidebar(sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = shiny::icon("info-circle")),
      menuItem("Settings", tabName = "Parameters", icon = shiny::icon("tachometer-alt")),
      menuItem("Calculation", tabName = "Results", icon = shiny::icon("square-poll-vertical"))
    )
    ),
    # Define body
    dashboardBody(
      tabItems(
        tabItem(tabName = "Welcome", shiny::includeMarkdown(glue::glue('{pkgload::pkg_path()}/README.md'))),
        tabItem(tabName = "Parameters",
                fluidRow(
                  column(3, selectInput("study", "Case study:",
                                        c("User","Polarix1", "Polarix2", "MonarchE", "Leda", "YTB323"))),
                  column(3, textInput("events", "Events (csv)", "110, 125, 131"))),
                fluidRow(
                  column(3, textInput("thres1", "Safety thresholds (csv)", "1.3, 1.2, 1")),
                  column(3, textInput("thres2", "Stopping thresholds (csv)", "NULL")),
                  column(3, textInput("hrs", "True HRs", "0.7, 1, 1.1, 1.2, 1.5")),
                  column(3, selectInput("method", "Method :", c("joint", "cond"))),
                )
        ),
        tabItem(tabName = "Results", fluidRow(
          shinydashboard::box(status = "primary", plotOutput("prob_plot", height = "512px")),
          shinydashboard::box(status = "success", plotOutput("flplot", height = "512px")),
          shinydashboard::box(status = "primary", tableOutput("ocs_trial")),
          shinydashboard::box(status = "success", tableOutput("ocs_stage"))
        )
        )
      )
    )
  )


}

