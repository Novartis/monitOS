#' Title
#'
#' @param input todo
#' @param output todo
#' @param session todo
#'
#' @export
#' @import shiny
app_server <- function(input, output, session) {

  # User cases
  observeEvent(input$study, {
    if(input$study == "User"){
      updateTextInput(session, "events", value = "125, 131, 150")
      updateTextInput(session, "thres1", value = "1.3, 1.2, 1")
      updateTextInput(session, "hrs", value = "0.7, 1, 1.1, 1.2, 1.5")
    } else {
      params <- monitOS:::use_cases(input$study)
      updateTextInput(session, "events", value = wrap(params$events))
      updateTextInput(session, "thres1", value = wrap(params$thres1))
      updateTextInput(session, "hrs", value = wrap(params$hrs))
    }
  })

  ## Compute new bounds when event changes
  observeEvent(input$events, {
    # Events
    events <- unwrap(input$events)
    # Detrimental threshold
    updateTextInput(session, "thres1", value = wrap(exp(monitOS::bounds(events)$lhr_con)))
  })

    # Core reactive function - OCs plots & results
    react <- reactive({

      # Parse as vectors
      thres1 <- unwrap(input$thres1)
      events <- unwrap(input$events)
      hrs <- unwrap(input$hrs)
      # browser()
      # one liner doesn't work
      thres2 <- NULL
      if(is.null(input$thresh2)) thres2 <- exp(bounds(events)$lhr_null)



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
}

