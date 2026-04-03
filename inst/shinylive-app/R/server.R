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
  primary_rows <- reactiveVal(list(
    list(id = 1L, value = 28),
    list(id = 2L, value = 42)
  ))
  next_primary_id <- reactiveVal(3L)

  get_primary_events <- function(rows = primary_rows()) {
    vapply(rows, function(row) {
      input_value <- input[[paste0("eventPA_", row$id)]]
      if (is.null(input_value) || is.na(input_value)) row$value else input_value
    }, numeric(1))
  }

  output$primary_events_ui <- renderUI({
    rows <- primary_rows()
    current_values <- get_primary_events(rows)

    div(
      class = "event-editor",
      div(
        class = "event-row-head",
        div("Planned look"),
        div("Deaths"),
        div()
      ),
      div(
        class = "event-row-list",
        lapply(seq_along(rows), function(i) {
          row <- rows[[i]]
          div(
            class = "event-row",
            div(class = "event-row-label", paste("Interim analysis", i)),
            div(
              class = "event-row-control",
              numericInput(
                inputId = paste0("eventPA_", row$id),
                label = NULL,
                value = current_values[i],
                min = 1
              )
            ),
            if (length(rows) > 1) {
              actionLink(
                inputId = paste0("remove_event_row_", row$id),
                label = HTML("&#10005;"),
                class = "event-remove-link",
                title = "Remove interim analysis"
              )
            } else {
              div()
            }
          )
        })
      ),
      div(
        class = "event-footer",
        actionLink(
          "add_event_row",
          HTML("+ Add interim analysis"),
          class = "event-add-link"
        )
      )
    )
  })

  observeEvent(input$add_event_row, {
    rows <- primary_rows()
    current_values <- get_primary_events(rows)
    updated_rows <- Map(function(row, value) {
      row$value <- value
      row
    }, rows, current_values)
    new_id <- next_primary_id()
    updated_rows[[length(updated_rows) + 1]] <- list(id = new_id, value = NA_real_)
    primary_rows(updated_rows)
    next_primary_id(new_id + 1L)
  })

  observe({
    rows <- primary_rows()

    lapply(rows, function(row) {
      observeEvent(input[[paste0("remove_event_row_", row$id)]], {
        current_rows <- primary_rows()
        current_values <- get_primary_events(current_rows)
        current_rows <- Map(function(existing_row, value) {
          existing_row$value <- value
          existing_row
        }, current_rows, current_values)

        if (length(current_rows) > 1) {
          keep <- vapply(current_rows, function(existing_row) existing_row$id != row$id, logical(1))
          primary_rows(current_rows[keep])
        }
      }, ignoreInit = TRUE)
    })
  })

  summary_data <- reactive({
    rows <- primary_rows()
    primary_events <- get_primary_events(rows)

    validate(
      need(length(primary_events) > 0, "Enter at least one interim-analysis event count."),
      need(all(!is.na(primary_events)), "Every interim-analysis row needs a numeric death count."),
      need(!is.na(input$eventOS), "Final-analysis deaths must be numeric."),
      need(all(diff(primary_events) >= 0), "Interim-analysis deaths should increase across planned looks."),
      need(input$eventOS >= max(primary_events), "Final-analysis deaths must be greater than or equal to the last interim analysis.")
    )

    primary_rows(Map(function(row, value) {
      row$value <- value
      row
    }, rows, primary_events))

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
    interim_n <- length(primary_rows())
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
