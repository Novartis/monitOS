#' Title
#'
#' @param request todo
#'
#' @export
#' @import shinydashboard glue shiny
app_ui <- function(request){

  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Monitoring OS"),
    # Creating tabs
    dashboardSidebar(sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("info-circle")),
      menuItem("Calculation", tabName = "Calculation", icon = icon("square-poll-vertical")),
      menuItem("Optional Settings", tabName = "Settings", icon = icon("tachometer-alt")))
      ),
    # Define body
    dashboardBody(
      tabItems(
        tabItem(tabName = "Welcome", includeMarkdown(glue('{pkgload::pkg_path()}/README.md'))),
        tabItem(tabName = "Calculation",
          fluidRow(column(3, textInput("events", "L (events)", "110, 125, 131"))),
          fluidRow(box(status = "primary", plotOutput("prob_plot", height = "512px")),
                   box(status = "success", plotOutput("flplot", height = "512px"))),
          fluidRow(box(status = "primary", tableOutput("ocs_trial")),
                   box(status = "success", tableOutput("ocs_stage")))
          ),
        tabItem(tabName = "Settings",
                fluidRow(column(3, selectInput("study", "Case study",
                                               c("User","Polarix1", "Polarix2", "MonarchE", "Leda", "YTB323")))
                ),
                fluidRow(
                  column(3, textInput("thres1", "Detrimental thresholds", "1.3, 1.2, 1")),
                  column(3, checkboxInput('thres2',  "Stopping thresholds", value = FALSE, width = NULL))
                ),
                fluidRow(
                  column(3,  textInput("hrs", "True HRs", "0.7, 1, 1.1, 1.2, 1.5")),
                  column(3, selectInput("method", "Method :", c("joint", "cond")))
                )
        )
      )
    )
  )


}

