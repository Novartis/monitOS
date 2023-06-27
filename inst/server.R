#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
# library(monitOS)
devtools::load_all()
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Load pre-specificed use case
    usecase <- reactive(monitOS::use_cases(input$study))

    # Base react
    react <- reactive({

      # Parse as vectors
      thres1 <- as.numeric(unlist(strsplit(gsub(" ", "", input$thres1),",")))
      thres2 <- pmax(as.numeric(unlist(strsplit(gsub(" ", "", input$thres2),","))), thres1, na.rm = TRUE)
      events <- as.numeric(unlist(strsplit(gsub(" ", "", input$events),",")))

      # Run simulation
      monitOS::ocs(thres1=thres1,
                   thres2=thres2,
                   events=events,
                   method=input$method,
                   hrs=c(0.7, 1, 1.1, 1.3, 1.5),
                   col=NULL)
    })

    # Rendering
    output$plot1 <- renderPlot(react()$plots$prob_plot)
    output$plot2 <- renderPlot(react()$plots$oprob_plot)
    output$table2 <- renderTable(react()$ocs_stage)
    output$table1 <- renderTable(react()$ocs_trial)
})
