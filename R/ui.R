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
      # menuItem("Welcome", tabName = "Welcome", icon = icon("house")),
      menuItem("Calculation", tabName = "Calculation", icon = icon("calculator")),
      # menuItem("Comparison", tabName = "Comparison", icon = icon("socks")),
      menuItem("Settings", tabName = "Settings", icon = icon("sliders")))
      ),
    # Define body
    dashboardBody(
      tabItems(
        # tabItem(tabName = "Welcome"), #, includeMarkdown(glue('{pkg_path()}/README.md'))),
        tabItem(tabName = "Calculation",
          fluidRow(column(3, textInput("events", "target number of OS events at each analysis?", "40,134,223,297")),
                   column(3, textInput("thres1", "HR continuation thresholds", "1.3, 1.2, 1"))),
          fluidRow(
            column(3, sliderInput('power_int', 'what power do we want to not flag a safety concern at an interim analysis if the true OS HR equals our target alternative?', min=0.7, max=1, value=0.9, step = 0.01)),
            column(3, sliderInput('t1error', 'what is the (one-sided) type I error rate that we will accept at the final analysis?', min=0, max=0.2, value=0.025, step = 0.005)),
            column(3, sliderInput('hr_null', 'what is the minimum unacceptable OS HR?', min=1, max=1.6, value=1.333, step = 0.05)),
            column(3, sliderInput('lhr_alt', 'what is a plausible alternative OS HR consistent with OS benefit?', min=0.5, max=1, value=0.9, step = 0.05))
          ),
          fluidRow(box(status = "primary", plotOutput("prob_plot", height = "400px")),
                   box(status = "success", plotOutput("flplot", height = "400px"))),
          fluidRow(box(status = "primary", tableOutput("bounds"), width = 12)),
          fluidRow(box(status = "primary", tableOutput("ocs_trial")),
                   box(status = "success", tableOutput("ocs_stage")))
          ),
        # tabItem(tabName = "Comparison",
        #         fluidRow(column(6, textInput("events1", "L", "110, 125, 131")),
        #                  column(6, textInput("events2", "L*", "100, 115, 150"))),
        #         fluidRow(box(status = "primary", plotOutput("prob_plot1", height = "350px")),
        #                  box(status = "success", plotOutput("prob_plot2", height = "350px"))),
        #         fluidRow(box(status = "primary", plotOutput("flplot1", height = "350px")),
        #                  box(status = "success", plotOutput("flplot2", height = "350px")))
        # ),
        tabItem(tabName = "Settings",
                fluidRow(column(3, selectInput("study", "Case study",
                                               c("User","Polarix1", "Polarix2", "MonarchE", "Leda", "YTB323")))
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

