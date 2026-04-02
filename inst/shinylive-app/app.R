# monitOS Shinylive App
#
# Self-contained Shiny app for shinylive (WebAssembly) deployment.
# Mirrors the package's R/ui.R and R/server.R so the browser
# experience matches `monitOS::run_app()`.

library(bslib)
library(shiny)

calc_posterior <- function(lhr_con, lhr_null, events) {
  info <- events / 4
  se <- sqrt(1 / info)
  1 - pnorm((lhr_null - lhr_con) / se)
}

calc_predictive <- function(lhr_con, events) {
  nstage <- length(events)
  info <- events / 4
  vapply(1:(nstage - 1), function(i) {
    pnorm(
      lhr_con[nstage] * sqrt(info[nstage]),
      mean = lhr_con[i] * sqrt(info[i]) * sqrt(info[nstage] / info[i]),
      sd = sqrt((info[nstage] - info[i]) / info[i])
    )
  }, numeric(1))
}

meeting_probs <- function(summary, lhr_pos, lhr_target = 1, rand_ratio = 1) {
  events <- summary$Deaths
  info <- rand_ratio * events / ((rand_ratio + 1)^2)
  se <- sqrt(1 / info)
  vapply(seq_along(events), function(i) {
    pnorm(lhr_pos[i], mean = lhr_target, sd = se[i], lower.tail = TRUE)
  }, numeric(1))
}

bounds <- function(
  events,
  power_int = 0.9,
  falsepos = 0.025,
  hr_null = 1.3,
  hr_alt = 0.9,
  rand_ratio = 1,
  hr_marg_benefit = NULL
) {
  lhr_null <- log(hr_null)
  lhr_alt <- log(hr_alt)
  nstage <- length(events)
  info <- rand_ratio * events / ((rand_ratio + 1)^2)
  se <- sqrt(1 / info)

  power_final <- pnorm(
    (lhr_null - qnorm(1 - falsepos) * se[nstage] - lhr_alt) / se[nstage]
  )

  gamma <- 2 * (1 - pnorm(
    ((lhr_null - lhr_alt) / se[1:(nstage - 1)]) - qnorm(power_int)
  ))
  falsepos_all <- c(gamma / 2, falsepos)
  ci_level_monit_null <- 100 * (1 - 2 * falsepos_all)
  power_all <- c(rep(power_int, times = nstage - 1), power_final)

  lhr_pos <- lhr_null - qnorm(1 - falsepos_all) * se

  post_pos <- calc_posterior(lhr_pos, lhr_null, events)
  pred_pos <- calc_predictive(lhr_pos, events)

  summary <- data.frame("Deaths" = events)
  summary$"OS HR threshold for positivity" <- round(exp(lhr_pos), 3)
  summary$"One-sided false positive error rate" <- round(falsepos_all, 3)
  summary$"Level of 2-sided CI needed to rule out delta null" <- round(
    pmax(0, ci_level_monit_null),
    0
  )
  summary$"Probability of meeting positivity threshold under delta alt" <- round(
    power_all,
    3
  )
  summary$"Posterior probability the true OS HR exceeds delta null given the data" <- round(
    post_pos,
    3
  )
  summary$"Predictive probability the OS HR estimate at Final Analysis does not exceed the positivity threshold" <- c(
    round(pred_pos * 100, 3),
    NA
  )

  if (!is.null(hr_marg_benefit)) {
    summary$"Probability of meeting positivity threshold under incremental benefit" <-
      round(
        meeting_probs(
          summary = summary,
          lhr_pos = lhr_pos,
          lhr_target = log(hr_marg_benefit),
          rand_ratio = rand_ratio
        ),
        3
      )
  }

  list(
    lhr_null = lhr_null,
    lhr_alt = lhr_alt,
    lhr_pos = lhr_pos,
    summary = summary
  )
}

app_css <- "
  :root {
    --monitos-bg: #FCFCFC;
    --monitos-fg: #161616;
    --monitos-orange: #FF4E00;
    --monitos-grey-1: #DADADA;
    --monitos-grey-2: #F5F5F5;
    --monitos-blue: #A5BFF5;
    --monitos-pink: #EDA8D1;
    --monitos-shadow: 0 16px 36px rgba(22, 22, 22, 0.05);
    --monitos-radius: 22px;
  }

  body {
    background:
      radial-gradient(circle at top left, rgba(165, 191, 245, 0.18), transparent 28%),
      linear-gradient(180deg, #ffffff 0%, var(--monitos-bg) 42%, #ffffff 100%);
    color: var(--monitos-fg);
    font-family: 'Avenir Next', 'Segoe UI', 'Helvetica Neue', sans-serif;
  }

  h1, h2, h3, h4, .display-title {
    font-family: 'Iowan Old Style', 'Palatino Linotype', 'Book Antiqua', serif;
    letter-spacing: -0.02em;
  }

  .page-shell {
    max-width: 1360px;
    margin: 0 auto;
    padding: 1.25rem 0 2.75rem;
  }

  .hero-card,
  .panel-card {
    border: 1px solid rgba(22, 22, 22, 0.08);
    border-radius: var(--monitos-radius);
    background: rgba(255, 255, 255, 0.96);
    box-shadow: var(--monitos-shadow);
  }

  .hero-card {
    position: relative;
    padding: 1.75rem 1.9rem;
    margin-bottom: 1.5rem;
    background:
      linear-gradient(135deg, rgba(165, 191, 245, 0.22), rgba(255, 255, 255, 0.96) 52%, rgba(237, 168, 209, 0.14) 100%);
  }

  .hero-grid {
    display: grid;
    grid-template-columns: minmax(0, 1.5fr) minmax(220px, 0.7fr);
    gap: 1.5rem;
    align-items: center;
    position: relative;
    z-index: 1;
  }

  .hero-eyebrow {
    display: inline-flex;
    align-items: center;
    gap: 0.45rem;
    margin-bottom: 0.7rem;
    padding: 0;
    border-radius: 999px;
    color: var(--monitos-orange);
    font-size: 0.78rem;
    font-weight: 700;
    letter-spacing: 0.08em;
    text-transform: uppercase;
  }

  .hero-title {
    max-width: 14ch;
    margin: 0 0 0.7rem;
    font-size: clamp(2.1rem, 3.4vw, 3.6rem);
    line-height: 1;
  }

  .hero-copy {
    max-width: 42rem;
    margin: 0;
    font-size: 1rem;
    line-height: 1.65;
    color: rgba(22, 22, 22, 0.78);
  }

  .hero-logo-shell {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    min-height: 180px;
  }

  .hero-logo {
    width: min(100%, 380px);
    padding: 0.9rem 1.1rem;
    border-radius: 18px;
    background: rgba(255, 255, 255, 0.82);
    border: 1px solid rgba(22, 22, 22, 0.06);
  }

  .app-grid {
    display: grid;
    gap: 1.25rem;
  }

  .panel-card {
    padding: 1.2rem 1.2rem 1.3rem;
  }

  .panel-card h2,
  .panel-card h3 {
    margin-top: 0;
    margin-bottom: 0.5rem;
  }

  .section-kicker {
    display: inline-block;
    margin-bottom: 0.55rem;
    color: rgba(255, 78, 0, 0.82);
    font-size: 0.8rem;
    font-weight: 500;
    letter-spacing: 0.08em;
    text-transform: uppercase;
  }

  .lede {
    color: rgba(22, 22, 22, 0.72);
    line-height: 1.65;
  }

  .field-grid {
    display: grid;
    grid-template-columns: minmax(0, 1fr) minmax(220px, 0.72fr);
    gap: 1rem;
    align-items: start;
  }

  .field-note {
    padding-top: 0.35rem;
    color: rgba(22, 22, 22, 0.72);
    line-height: 1.55;
  }

  .field-note strong {
    color: var(--monitos-fg);
    font-weight: 700;
  }

  .field-note em {
    color: rgba(22, 22, 22, 0.64);
    font-style: normal;
  }

  .subtle-list {
    margin: 0.4rem 0 0;
    padding-left: 1rem;
    color: var(--monitos-fg);
    line-height: 1.6;
  }

  .shiny-input-container {
    width: 100%;
    margin-bottom: 0;
  }

  .field-stack {
    display: grid;
    gap: 1rem;
  }

  .field-stack .form-control,
  .field-stack .irs,
  .field-stack .selectize-input {
    margin-top: 0.35rem;
  }

  .event-row-list {
    display: grid;
    gap: 0.75rem;
    margin-top: 0.35rem;
  }

  .event-row {
    display: grid;
    grid-template-columns: minmax(140px, 180px) minmax(0, 1fr);
    gap: 0.9rem;
    align-items: center;
    padding: 0.75rem 0.9rem;
    border: 1px solid rgba(22, 22, 22, 0.08);
    border-radius: 14px;
    background: rgba(255, 255, 255, 0.92);
  }

  .event-row .shiny-input-container {
    margin-bottom: 0;
  }

  .event-row-label {
    color: rgba(22, 22, 22, 0.78);
    font-size: 0.92rem;
    font-weight: 600;
    white-space: nowrap;
  }

  .event-row .form-control {
    max-width: 220px;
  }

  .event-actions {
    display: flex;
    flex-wrap: wrap;
    gap: 0.65rem;
    margin-top: 0.85rem;
  }

  .event-actions .btn {
    border-radius: 999px;
    padding: 0.38rem 0.82rem;
    font-size: 0.9rem;
    font-weight: 500;
  }

  .event-actions .btn-default {
    border-color: rgba(22, 22, 22, 0.12);
    background: #fff;
    color: var(--monitos-fg);
  }

  .helper-copy {
    margin-top: 0.5rem;
    color: rgba(22, 22, 22, 0.68);
    line-height: 1.55;
  }

  .inline-note {
    color: rgba(22, 22, 22, 0.62);
    font-size: 0.92rem;
    line-height: 1.55;
  }

  .results-card {
    position: sticky;
    top: 0;
    z-index: 5;
    border-top-left-radius: 0;
    border-top-right-radius: 0;
  }

  .summary-strip {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 0.75rem;
    margin-top: 0.9rem;
  }

  .summary-item {
    padding: 0.8rem 0.9rem;
    border-radius: 14px;
    background: rgba(245, 245, 245, 0.95);
    border: 1px solid rgba(22, 22, 22, 0.06);
  }

  .summary-item-label {
    display: block;
    margin-bottom: 0.2rem;
    color: rgba(22, 22, 22, 0.56);
    font-size: 0.76rem;
    font-weight: 600;
    letter-spacing: 0.06em;
    text-transform: uppercase;
  }

  .summary-item-value {
    color: var(--monitos-fg);
    font-size: 0.96rem;
    line-height: 1.45;
    font-weight: 600;
  }

  .form-stack {
    display: grid;
    gap: 1rem;
  }

  .form-stack .panel-card {
    width: 100%;
  }

  .results-frame {
    margin-top: 1rem;
    overflow-x: auto;
    border: 1px solid rgba(22, 22, 22, 0.08);
    border-radius: 16px;
    background: #fff;
  }

  .results-card table {
    width: 100%;
    margin: 0;
    font-size: 0.95rem;
  }

  .results-card thead th {
    position: sticky;
    top: 0;
    z-index: 1;
    padding: 0.85rem 0.85rem;
    border: 0;
    background: rgba(245, 245, 245, 0.98);
    color: var(--monitos-fg);
    font-weight: 700;
  }

  .results-card tbody td {
    padding: 0.85rem;
    vertical-align: top;
    border-color: rgba(22, 22, 22, 0.08);
  }

  .results-card tbody tr:nth-child(odd) td {
    background: rgba(252, 252, 252, 0.95);
  }

  .results-card tbody tr.final-analysis-row td {
    background: rgba(255, 78, 0, 0.08);
    font-weight: 600;
  }

  .results-card .table {
    margin-bottom: 0;
  }

  .event-remove-link {
    display: inline-block;
    margin-top: 0.35rem;
    color: rgba(22, 22, 22, 0.56);
    font-size: 0.86rem;
    text-decoration: none;
  }

  .event-remove-link:hover,
  .event-remove-link:focus {
    color: var(--monitos-orange);
  }

  .reference-links {
    display: grid;
    gap: 0.65rem;
    margin-top: 0.9rem;
  }

  .reference-links a {
    color: var(--monitos-fg);
    font-weight: 700;
    text-decoration: none;
  }

  .reference-links a:hover,
  .reference-links a:focus {
    color: var(--monitos-orange);
  }

  .support-card details + details {
    margin-top: 0.75rem;
  }

  .support-card summary {
    cursor: pointer;
    color: var(--monitos-fg);
    font-weight: 600;
    list-style: none;
  }

  .support-card summary::-webkit-details-marker {
    display: none;
  }

  .support-card .details-body {
    margin-top: 0.7rem;
  }

  .list-tight {
    margin: 0;
    padding-left: 1.15rem;
    line-height: 1.7;
  }

  @media (max-width: 991px) {
    .hero-grid,
    .field-grid {
      grid-template-columns: 1fr;
    }

    .event-row {
      grid-template-columns: 1fr;
      gap: 0.55rem;
    }

    .hero-logo-shell {
      justify-content: flex-start;
    }

    .results-card {
      position: static;
    }
  }
"

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(
    version = 5,
    bg = "#FCFCFC",
    fg = "#161616",
    primary = "#FF4E00",
    secondary = "#A5BFF5"
  ),
  title = "monitOS",
  lang = "en",
  tags$head(tags$style(HTML(app_css))),
  div(
    class = "page-shell",
    div(
      class = "hero-card",
      div(
        class = "hero-grid",
        div(
          span(class = "hero-eyebrow", "Monitoring overall survival"),
          h1(class = "hero-title", "Configure OS monitoring thresholds."),
        p(
          class = "hero-copy",
          paste(
            "Translate effect-size assumptions and event timing into",
            "positivity thresholds for interim and final OS reviews."
          )
          )
        ),
        div(
          class = "hero-logo-shell",
          tags$img(
            src = "logo.png",
            alt = "Novartis",
            class = "hero-logo"
          )
        )
      )
    ),
    div(
      class = "app-grid",
      div(
        class = "panel-card results-card",
        span(class = "section-kicker", "Positivity thresholds"),
        h2("Review the monitoring table"),
        p(
          class = "inline-note",
          "Updates automatically as you change assumptions."
        ),
        div(
          class = "summary-strip",
          div(
            class = "summary-item",
            span(class = "summary-item-label", "Final Analysis"),
            span(class = "summary-item-value", textOutput("final_threshold_summary", container = span))
          ),
          div(
            class = "summary-item",
            span(class = "summary-item-label", "Planned Looks"),
            span(class = "summary-item-value", textOutput("analysis_count_summary", container = span))
          )
        ),
        div(class = "results-frame", uiOutput("bounds"))
      ),
      div(
        class = "form-stack",
        div(
          class = "panel-card",
          span(class = "section-kicker", "Workflow"),
          h2("Set assumptions, then review the threshold"),
        p(
          class = "inline-note",
          "1. Set effect sizes. 2. Add interim event counts. 3. Review the final-analysis threshold and interim operating characteristics."
        ),
        p(
          class = "helper-copy",
          "HR below 1 indicates benefit."
        )
      ),
        div(
          class = "panel-card",
          span(class = "section-kicker", "Trial assumptions"),
          h3("Clinically meaningful effect sizes"),
          div(
            class = "field-stack",
            div(
              class = "field-grid",
              div(
                sliderInput(
                  "hr_null",
                  "Unacceptable detrimental effect on OS",
                  min = 1,
                  max = 2,
                  value = 4 / 3,
                  step = 0.01
                )
              ),
              div(
                class = "field-note",
                tags$strong("Use for: "),
                "choose the OS hazard ratio that would already be too harmful to accept. ",
                tags$em("Example: set delta null to 1.33 if any HR at or above 1.33 would be unacceptable.")
              )
            ),
            div(
              class = "field-grid",
              div(
                sliderInput(
                  "hr_alt",
                  "Plausible beneficial effect on OS",
                  min = 0.05,
                  max = 1,
                  value = 0.7,
                  step = 0.01
                )
              ),
              div(
                class = "field-note",
                tags$strong("Use for: "),
                "reflect the benefit you could reasonably expect from prior evidence or mechanism of action. ",
                tags$em("Example: use 0.90 if a 10% OS hazard reduction would still be clinically meaningful.")
              )
            )
          )
        ),
        div(
          class = "panel-card",
          span(class = "section-kicker", "Design inputs"),
          h3("Event timing and decision thresholds"),
          div(
            class = "field-stack",
            div(
              class = "field-grid",
              div(
                numericInput(
                  "eventOS",
                  "Deaths at final analysis",
                  value = 70,
                  min = 1
                ),
                p(
                  class = "helper-copy",
                  "Use the longest feasible follow-up window for the final analysis."
                )
              ),
              div(
                tags$label(
                  `for` = "primary_events_ui",
                  "How many deaths are expected at the interim analyses?"
                ),
                uiOutput("primary_events_ui"),
                div(
                  class = "event-actions",
                  actionButton("add_event_row", "Add interim")
                ),
                p(
                  class = "helper-copy",
                  "Add one row per planned interim look."
                )
              )
            ),
            div(
              class = "field-grid",
              div(
                sliderInput(
                  "falsepos",
                  "One-sided false positive rate at final analysis",
                  min = 0,
                  max = 0.3,
                  value = 0.1,
                  step = 0.005
                )
              ),
              div(
                class = "field-note",
                tags$strong("Interpretation: "),
                paste(
                  "this controls the final-analysis reassurance threshold when the true OS HR",
                  "is equal to your unacceptable detriment."
                )
              )
            ),
            div(
              class = "field-grid",
              div(
                sliderInput(
                  "power_int",
                  "Required power at interim analyses under delta alt",
                  min = 0.7,
                  max = 1,
                  value = 0.9,
                  step = 0.01
                )
              ),
              div(
                class = "field-note",
                tags$strong("Typical choice: "),
                "0.80 or 0.90, depending on how strict the primary-analysis reassurance should be."
              )
            )
          )
        ),
        div(
          class = "panel-card",
          span(class = "section-kicker", "Optional sensitivity inputs"),
          h3("Stress-test the plan"),
          div(
            class = "field-stack",
            div(
              class = "field-grid",
              div(
                sliderInput(
                  "hr_marg_benefit",
                  "Marginal-benefit OS HR",
                  min = 0.7,
                  max = 1.1,
                  value = 0.95,
                  step = 0.01
                )
              ),
              div(
                class = "field-note",
                tags$strong("When to use: "),
                "use this when the drug may still be valuable under a smaller OS benefit than delta alt."
              )
            ),
            div(
              class = "field-grid",
              div(
                sliderInput(
                  "rand_ratio",
                  "Randomization ratio (experimental : control)",
                  min = 1,
                  max = 3,
                  value = 1,
                  step = 1
                )
              ),
              div(
                class = "field-note",
                tags$strong("Example: "),
                "use 1 for 1:1 randomization and 2 for 2:1 randomization."
              )
            )
          )
        ),
        div(
          class = "panel-card support-card",
          span(class = "section-kicker", "Support"),
          h3("Method notes"),
          tags$details(
            tags$summary("How to read the table"),
            div(
              class = "details-body",
              tags$ul(
                class = "list-tight",
                tags$li("OS HR threshold for positivity: the observed HR must be at or below this value."),
                tags$li("One-sided false positive error rate: the tolerated risk of passing when the true effect equals delta null."),
                tags$li("Level of 2-sided CI needed to rule out delta null: the confidence level required to exclude the unacceptable detriment."),
                tags$li("Probability of meeting positivity threshold under delta alt: the chance of passing when the treatment effect equals your plausible benefit.")
              )
            )
          ),
          tags$details(
            tags$summary("Paper and package resources"),
            div(
              class = "details-body reference-links",
              tags$a(href = "https://www.tandfonline.com/doi/full/10.1080/19466315.2024.2365648", target = "_blank", rel = "noopener noreferrer", "Read the monitOS paper"),
              tags$a(href = "https://opensource.nibr.com/monitOS/articles/monitOS.html", target = "_blank", rel = "noopener noreferrer", "Open the package vignette"),
              tags$a(href = "https://github.com/Novartis/monitOS/issues", target = "_blank", rel = "noopener noreferrer", "Report an issue or request help")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  primary_defaults <- c(28, 42)
  primary_count <- reactiveVal(length(primary_defaults))
  value_or <- function(x, fallback) if (is.null(x)) fallback else x

  get_primary_events <- function(n) {
    vapply(seq_len(n), function(i) {
      input[[paste0("eventPA_", i)]]
    }, numeric(1))
  }

  output$primary_events_ui <- renderUI({
    n <- primary_count()

    div(
      class = "event-row-list",
      lapply(seq_len(n), function(i) {
        div(
          class = "event-row",
          div(class = "event-row-label", paste("Interim analysis", i)),
          numericInput(
            inputId = paste0("eventPA_", i),
            label = NULL,
            value = value_or(
              isolate(input[[paste0("eventPA_", i)]]),
              value_or(primary_defaults[i], NA_real_)
            ),
            min = 1
          )
        )
      })
    )
  })

  observeEvent(input$add_event_row, {
    primary_count(primary_count() + 1)
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

shinyApp(ui, server)
