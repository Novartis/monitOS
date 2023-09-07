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
      if(input$study != "User"){
        params <- use_cases(input$study)
        updateTextInput(session, "events", value = wrap(params$events))
        updateTextInput(session, "thres1", value = wrap(params$thres1))
        updateTextInput(session, "hrs", value = wrap(params$hrs))
      }
    })
    # Core reactive function - OCs plots & results
    react <- reactive({

    # Parse as vectors
    events <- unwrap(input$events)
    hrs <- unwrap(input$hrs)

    # BOUNDS
    bounds_ = bounds(events, input$power_int, t1error = input$t1error,
                     lhr_null = log(input$hr_null), lhr_alt = log(input$lhr_alt))

    thres1 = exp(bounds_$lhr_con)
    updateTextInput(session, "thres1", value = wrap(thres1))

    # Run simulation
    ocs <- ocs(thres1=thres1,
               thres2=NULL,
               events=events,
               method=input$method,
               hrr=seq(0.3, 1.5, by = 0.01),
               hrs=hrs,
               col=NULL)

    return(list(bounds=bounds_,
                ocs=ocs))

  })

  # Rendering
  output$prob_plot <- renderPlot(react()$ocs$plots$prob_plot)
  output$flplot <- renderPlot(react()$ocs$plots$flplot)
  output$bounds <- renderTable(react()$bounds$df)
  output$ocs_trial <-  renderTable(react()$ocs$ocs_trial)
  output$ocs_stage <- renderTable(react()$ocs$ocs_stage)

}

