#' Shiny app server function
#'
#' @description Server-side logic for the monitOS Shiny dashboard. Computes OS
#'   monitoring boundaries reactively based on user inputs and renders the
#'   summary table of positivity thresholds.
#'
#' @param input Shiny input object containing UI widget values.
#' @param output Shiny output object for rendering results.
#' @param session Shiny session object for updating UI elements.
#' @import shiny
# nocov start
app_server <- function(input, output, session) {
  wrap <-
    function(X, decimal = 3) {
      return(paste(round(X, decimal), collapse = ","))
    }
  unwrap <-
    function(X) {
      return(as.numeric(unlist(strsplit(
        gsub(" ", "", X),
        ","
      ))))
    }

  # Core reactive function - OCs plots & results
  react <- reactive({
    events <- c(unwrap(input$eventPA), input$eventOS)
    updateTextInput(session, "events", value = wrap(events))

    boundaries <- bounds(
      events = events,
      power_int = input$power_int,
      falsepos = input$falsepos,
      hr_null = input$hr_null,
      hr_alt = input$hr_alt,
      rand_ratio = input$rand_ratio,
      hr_marg_benefit = input$hr_marg_benefit
    )

    hr_pos <- exp(boundaries$lhr_pos)
    updateTextInput(session, "hr_pos", value = wrap(hr_pos))

    return(boundaries)
  })

  # Exclude posterior and predictive probability columns (cols 6, 7) from the
  # displayed table — these Bayesian metrics are computed internally but not
  # shown in the dashboard UI to keep the summary concise.
  output$bounds <- renderTable(react()$summary[, -c(6, 7)])
}
# nocov end
