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
  output$bounds <- renderTable(
    react()$summary %>%
      select(
        -c(
          Delta_null,
          Delta_alt,
          Positivity_Thres_Posterior,
          Positivity_Thres_PredProb
        )
      ) %>%
      rename(Deaths = Events) %>%
      rename('OS HR threshold for positivity' = Positivity_Thres_HR) %>%
      rename('One-sided false positive error rate' = OneSided_falsepos) %>%
      rename('Level of 2-sided CI needed to rule out delta null' = TwoSided_CI_level) %>%
      rename('Probability of meeting positivity threshold under delta alt' = Power_Alt) %>%
      rename(
        'Probability of meeting positivity threshold under marginal HR' = Power_Marg_Benefit
      )
  )
}
