#' Shiny app server
#'
#' @param input generic shiny var
#' @param output generic shiny var
#' @param session generic shiny var
app_server <- function(input, output, session) {
  wrap <-
    function(X, decimal = 3)
      return(paste(round(X, decimal), collapse = ","))
  unwrap <-
    function(X)
      return(as.numeric(unlist(strsplit(
        gsub(" ", "", X), ","
      ))))

  # Core reactive function - OCs plots & results
  react <- reactive({
    # Parse as vectors

    events <- c(unwrap(input$eventPA), input$eventOS)
    updateTextInput(session, "events", value = wrap(events))

    # boundaries
    boundaries = bounds(
      events = events,
      power_int = input$power_int,
      falsepos = input$falsepos,
      hr_null = input$hr_null,
      hr_alt = input$hr_alt,
      rand_ratio = input$rand_ratio,
      hr_marg_benefit = input$hr_marg_benefit
    )


    hr_pos = exp(boundaries$lhr_pos)
    updateTextInput(session, "hr_pos", value = wrap(hr_pos))

    return(boundaries)
  })


  # Rendering
  output$bounds <- renderTable(react()$summary[, -c(6,7)])

}
