#' Title
#'
#' @param request todo
#'
#' @export
#' @importFrom pkgload pkg_path
#' @importFrom glue glue
#' @import shiny shinydashboard
app_ui <- function(request){

  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Monitoring OS"),
    # Creating tabs
    dashboardSidebar(sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("house")),
      menuItem("Calculation", tabName = "Calculation", icon = icon("calculator")),
      menuItem("Comparison", tabName = "Comparison", icon = icon("socks")),
      menuItem("Settings", tabName = "Settings", icon = icon("sliders")))
      ),
    # Define body
    dashboardBody(
      tabItems(
        tabItem(tabName = "Welcome", includeMarkdown(glue('{pkg_path()}/README.md'))),
        tabItem(tabName = "Calculation",
          fluidRow(column(3, textInput("events", "L", "110, 125, 131"))),
          fluidRow(box(status = "primary", plotOutput("prob_plot", height = "512px")),
                   box(status = "success", plotOutput("flplot", height = "512px"))),
          fluidRow(box(status = "primary", tableOutput("ocs_trial")),
                   box(status = "success", tableOutput("ocs_stage")))
          ),
        tabItem(tabName = "Comparison",
                fluidRow(column(6, textInput("events1", "L1", "110, 125, 131")),
                         column(6, textInput("events2", "L2", "100, 115, 150"))),
                fluidRow(box(status = "primary", plotOutput("prob_plot1", height = "350px")),
                         box(status = "success", plotOutput("prob_plot2", height = "350px"))),
                fluidRow(box(status = "primary", plotOutput("flplot1", height = "350px")),
                         box(status = "success", plotOutput("flplot2", height = "350px")))
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

