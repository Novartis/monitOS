#' Shiny app server function
#'
#' @description Server-side logic for the monitOS Shiny application. Computes OS
#'   monitoring boundaries reactively based on user inputs and renders the
#'   summary table of positivity thresholds.
#'
#' @param input Shiny input object containing UI widget values.
#' @param output Shiny output object for rendering results.
#' @param session Shiny session object for updating UI elements.
#' @import shiny
# nocov start
app_server <- function(input, output, session) {
  primary_defaults <- c(28, 42)
  primary_values <- reactiveVal(primary_defaults)
  primary_count <- reactiveVal(length(primary_defaults))
  value_or <- function(x, fallback) if (is.null(x)) fallback else x

  get_primary_events <- function(n, fallback = primary_values()) {
    observed <- vapply(seq_len(n), function(i) {
      value_or(input[[paste0("eventPA_", i)]], NA_real_)
    }, numeric(1))

    if (length(fallback) < n) {
      fallback <- c(fallback, rep(NA_real_, n - length(fallback)))
    }

    ifelse(is.na(observed), fallback[seq_len(n)], observed)
  }

  output$primary_events_ui <- renderUI({
    n <- primary_count()
    current_values <- get_primary_events(n)

    div(
      class = "event-row-list",
      lapply(seq_len(n), function(i) {
        div(
          class = "event-row",
          div(class = "event-row-label", paste("Interim analysis", i)),
          div(
            class = "event-row-control",
            numericInput(
              inputId = paste0("eventPA_", i),
              label = NULL,
              value = current_values[i],
              min = 1
            ),
            if (n > 1) {
              actionLink(
                inputId = paste0("remove_event_row_", i),
                label = "Remove",
                class = "event-remove-link"
              )
            }
          )
        )
      })
    )
  })

  observeEvent(input$add_event_row, {
    current <- get_primary_events(primary_count())
    primary_values(c(current, NA_real_))
    primary_count(primary_count() + 1)
  })

  observe({
    n <- primary_count()

    lapply(seq_len(n), function(i) {
      observeEvent(input[[paste0("remove_event_row_", i)]], {
        current <- get_primary_events(primary_count())
        if (length(current) > 1) {
          updated <- current[-i]
          primary_values(updated)
          primary_count(length(updated))
        }
      }, ignoreInit = TRUE)
    })
  })

  summary_data <- reactive({
    primary_events <- get_primary_events(primary_count())

    validate(
      need(length(primary_events) > 0, "Enter at least one interim-analysis event count."),
      need(all(!is.na(primary_events)), "Every interim-analysis row needs a numeric death count."),
      need(!is.na(input$eventOS), "Final-analysis deaths must be numeric."),
      need(all(diff(primary_events) >= 0), "Interim-analysis deaths should increase across planned looks."),
      need(input$eventOS >= max(primary_events), "Final-analysis deaths must be greater than or equal to the last interim analysis.")
    )

    primary_values(primary_events)

    summary <- bounds(
      events = c(primary_events, input$eventOS),
      power_int = input$power_int,
      falsepos = input$falsepos,
      hr_null = input$hr_null,
      hr_alt = input$hr_alt,
      rand_ratio = input$rand_ratio,
      hr_marg_benefit = input$hr_marg_benefit
    )$summary

    ci_col <- "Level of 2-sided CI needed to rule out delta null"
    summary[[ci_col]] <- paste0(summary[[ci_col]], "%")

    summary[, setdiff(seq_along(summary), c(6, 7)), drop = FALSE]
  })

  output$final_threshold_summary <- renderText({
    summary <- summary_data()
    final_row <- summary[nrow(summary), , drop = FALSE]
    paste0(
      "Observed OS HR must be at or below ",
      sprintf("%.3f", final_row[["OS HR threshold for positivity"]]),
      "."
    )
  })

  output$analysis_count_summary <- renderText({
    interim_n <- primary_count()
    total_n <- interim_n + 1
    paste0(interim_n, " interim + 1 final analysis (", total_n, " total).")
  })

  output$bounds <- renderUI({
    summary <- summary_data()
    header <- tags$tr(lapply(names(summary), function(name) tags$th(name)))
    body <- lapply(seq_len(nrow(summary)), function(i) {
      row_class <- if (i == nrow(summary)) "final-analysis-row" else NULL
      tags$tr(
        class = row_class,
        lapply(summary[i, , drop = TRUE], function(value) tags$td(as.character(value)))
      )
    })

    tags$table(
      class = "table",
      tags$thead(header),
      tags$tbody(body)
    )
  })
}
# nocov end
