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
shinyServer(function(input, output, session) {

    # Load pre-specificed use case
    # usecase <- reactive(monitOS::use_cases(input$study))

  observeEvent(input$study, {
    if(input$study == "random"){
      updateTextInput(session, 'events', value = "110, 125, 131")
      updateTextInput(session, 'thres1', value = "1.3, 1.2, 1")
      updateTextInput(session, 'thres2', value = NULL)
      updateTextInput(session, 'hrs', value = "0.7, 1, 1.1, 1.2, 1.5")
    } else {
      params <- monitOS::use_cases(input$study)
      updateTextInput(session, 'events', value = paste(round(params$events, 4), collapse = ','))
      updateTextInput(session, 'thres1', value = paste(round(params$thresh1, 4), collapse = ','))
      updateTextInput(session, 'thres2', value = if(params$thresh2 == 'NULL') params$thresh2 else paste(round(params$thresh2, 4), collapse = ','))
      updateTextInput(session, 'hrs', value = paste(round(params$hrs, 4), collapse = ','))
    }
  })
  ## if input events changes, then keep the thres1 and thres2 the same size as input event, add random values if missing
  observeEvent(input$events, {
    thres1 <- as.numeric(unlist(strsplit(gsub(" ", "", input$thres1),",")))
    thres2 <- pmax(as.numeric(unlist(strsplit(gsub(" ", "", input$thres2),","))), thres1, na.rm = TRUE)
    events <- as.numeric(unlist(strsplit(gsub(" ", "", input$events),",")))


    len_events <- length(events)
    len_thres1 <- length(thres1)
    len_thres2 <- length(thres2)

    ### if added events, add thres values by using last one
    if(len_events > len_thres1){
      thres1 <- c(thres1, rep(thres1[len_thres1], (len_events - len_thres1)))
      updateTextInput(session, 'thres1', value = paste0(thres1, collapse = ","))
    }
    if(len_events > len_thres2){
      thres2 <- c(thres2, rep(thres2[len_thres2], (len_events - len_thres2)))
      updateTextInput(session, 'thres2', value = paste0(thres2, collapse = ","))
    }

    ### if deleted events, keep the first len_events items of thres1 and thres2
    if(len_events < len_thres1){
      thres1 <- thres1[1:len_events]
      updateTextInput(session, 'thres1', value = paste0(thres1, collapse = ","))
    }
    if(len_events < len_thres2){
      thres2 <- thres2[1: len_events]
      updateTextInput(session, 'thres2', value = paste0(thres2, collapse = ","))
    }
  })


    # Base react
    react <- reactive({

      print(input$thres2)
      # Parse as vectors
      thres1 <- as.numeric(unlist(strsplit(gsub(" ", "", input$thres1),",")))
      thres2 <- if(input$thres2 == 'NULL') NULL else as.numeric(unlist(strsplit(gsub(" ", "", input$thres2),",")))
      events <- as.numeric(unlist(strsplit(gsub(" ", "", input$events),",")))
      hrs <- as.numeric(unlist(strsplit(gsub(" ", "", input$hrs),",")))

      # Run simulation
      monitOS::ocs(thres1=thres1,
                   thres2=thres2,
                   events=events,
                   method=input$method,
                   hrr=seq(0.3, 1.5, by = 0.01),
                   hrs=hrs,
                   col=NULL)
    })

    # Rendering
    output$plot1 <- renderPlot(react()$plots$prob_plot)
    output$plot2 <- renderPlot(react()$plots$oprob_plot)
    output$table2 <- renderTable(react()$ocs_stage)
    output$table1 <- renderTable(react()$ocs_trial)
})
