# monitOS shiny app
rm(list=ls())

require(shiny)
# library(monitOS)
devtools::load_all()

# Helpers
wrap <- function(X, decimal=3) return(paste(round(X, decimal), collapse = ","))
unwrap <- function(X) return(as.numeric(unlist(strsplit(gsub(" ", "", X),","))))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # User cases
  observeEvent(input$study, {
    if(input$study == "User"){
      updateTextInput(session, "events", value = "125, 131, 150")
      updateTextInput(session, "thres1", value = "1.3, 1.2, 1")
      updateTextInput(session, "thres2", value = NULL)
      updateTextInput(session, "hrs", value = "0.7, 1, 1.1, 1.2, 1.5")
    } else {
      params <- monitOS:::use_cases(input$study)
      updateTextInput(session, "events", value = wrap(params$events))
      updateTextInput(session, "thres1", value = wrap(params$thres1))
      updateTextInput(session, "thres2", value = if(is.null(params$thresh2)) params$thresh2 else wrap(params$thresh2))
      updateTextInput(session, "hrs", value = wrap(params$hrs))
    }
  })

  ## Compute new bounds when event changes
  observeEvent(input$events, {
    # Events
    events <- unwrap(input$events)
    # Safety threshold
    updateTextInput(session, "thres1", value = wrap(exp(monitOS::bounds(events)$lhr_con)))
    # Stop threshold
    thres2 <- if(is.null(input$thresh2)) input$thresh2 else wrap(exp(monitOS::bounds(events)$lhr_null))
    updateTextInput(session, "thres2", value = thres2)
  })

    # Core reactive function - OCs plots & results
    react <- reactive({

      # Parse as vectors
      thres1 <- unwrap(input$thres1)
      thres2 <- if(input$thres2 == 'NULL') NULL else unwrap(input$thres2) # check later why its 'NULL' instead of NULL
      events <- unwrap(input$events)
      hrs <- unwrap(input$hrs)

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
    output$prob_plot <- renderPlot(react()$plots$prob_plot)
    output$flplot <- renderPlot(react()$plots$flplot)
    output$ocs_trial <-  renderTable(react()$ocs_trial)
    output$ocs_stage <- renderTable(react()$ocs_stage)
})
