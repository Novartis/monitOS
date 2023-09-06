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


    # PART I: Single calculation
    observeEvent(input$events, {
      # Events
      events <- unwrap(input$events)
      # Detrimental threshold
      updateTextInput(session, "thres1", value = wrap(exp(bounds(events)$lhr_con)))
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
    if(!is.null(input$thresh2)) thres2 <- exp(bounds(events)$lhr_null)

    # Run simulation
    res <- ocs(thres1=thres1,
                 thres2=thres2,
                 events=events,
                 method=input$method,
                 hrr=seq(0.3, 1.5, by = 0.01),
                 hrs=hrs,
                 col=NULL)



    # PART II:  Comparisons
    # Inputs
    events1 <- unwrap(input$events1)
    events2 <- unwrap(input$events2)
    thres11 <-exp(bounds(events1)$lhr_con)
    thres12 <-exp(bounds(events2)$lhr_con)
    thres21 <- NULL
    thres22 <- NULL
    if(is.null(input$thresh2)) thres21 <- exp(bounds(events1)$lhr_null)
    if(is.null(input$thresh2)) thres22 <- exp(bounds(events2)$lhr_null)



    # Run simulation
    res1 <- ocs(thres1=thres11,
                 thres2=thres21,
                 events=events,
                 method=input$method,
                 hrr=seq(0.3, 1.5, by = 0.01),
                 hrs=hrs,
                 col=NULL)
    res2 <- ocs(thres1=thres12,
                thres2=thres22,
                events=events,
                method=input$method,
                hrr=seq(0.3, 1.5, by = 0.01),
                hrs=hrs,
                col=NULL)

    return(list(res=res, res1=res1, res2=res2))

  })


  # Rendering
  output$prob_plot <- renderPlot(react()$res$plots$prob_plot)
  output$flplot <- renderPlot(react()$res$plots$flplot)
  output$ocs_trial <-  renderTable(react()$res$ocs_trial)
  output$ocs_stage <- renderTable(react()$res$ocs_stage)
  output$prob_plot1 <- renderPlot(react()$res1$plots$prob_plot)
  output$prob_plot2 <- renderPlot(react()$res2$plots$prob_plot)
  output$flplot1 <- renderPlot(react()$res1$plots$flplot)
  output$flplot2 <- renderPlot(react()$res2$plots$flplot)

}

