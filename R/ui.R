#' Shiny app UI definition
#'
#' @description User interface for the monitOS Shiny application. Provides a
#'   branded, guided workflow for setting OS monitoring parameters and
#'   reviewing the resulting positivity thresholds.
#'
#' @param request Shiny internal request object for bookmarking support.
#' @param logo_src Optional image source override for standalone exports.
#' @param register_resources Whether to register local app resources.
#' @param app_www_dir Optional local app asset directory.
#' @import shiny
#' @import bslib
# nocov start
app_ui <- function(
  request = NULL,
  logo_src = NULL,
  register_resources = TRUE,
  app_www_dir = NULL
) {
  www_candidates <- c(
    app_www_dir,
    file.path("inst", "app", "www")
  )
  www_dir <- www_candidates[nzchar(www_candidates) & dir.exists(www_candidates)][1]

  if (register_resources && !is.na(www_dir)) {
    suppressWarnings(shiny::addResourcePath("monitos-assets", www_dir))
  }

  resolved_logo_src <- if (!is.null(logo_src)) {
    logo_src
  } else if (!is.na(www_dir)) {
    "monitos-assets/logo.png"
  } else {
    NULL
  }

  theme <- bslib::bs_theme(
    version = 5,
    bg = "#FCFCFC",
    fg = "#161616",
    primary = "#FF4E00",
    secondary = "#A5BFF5"
  )

  bslib::page_fluid(
    theme = theme,
    title = "monitOS",
    lang = "en",
    tags$head(
      tags$style(HTML("
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

        .event-editor {
          margin-top: 0.35rem;
          padding: 0.65rem 0.7rem 0.45rem;
          border: 1px solid rgba(22, 22, 22, 0.08);
          border-radius: 14px;
          background: rgba(252, 252, 252, 0.92);
        }

        .event-row-head,
        .event-row {
          display: grid;
          grid-template-columns: minmax(0, 1fr) 120px 32px;
          gap: 0.75rem;
          align-items: center;
        }

        .event-row-head {
          padding: 0 0.2rem 0.35rem;
          color: rgba(22, 22, 22, 0.52);
          font-size: 0.72rem;
          font-weight: 600;
          letter-spacing: 0.06em;
          text-transform: uppercase;
        }

        .event-row-list {
          display: grid;
          gap: 0.4rem;
        }

        .event-row {
          padding: 0.42rem 0.55rem;
          border: 1px solid rgba(22, 22, 22, 0.08);
          border-radius: 10px;
          background: #fff;
        }

        .event-row .shiny-input-container {
          margin-bottom: 0;
        }

        .event-row-label {
          color: rgba(22, 22, 22, 0.78);
          font-size: 0.9rem;
          font-weight: 500;
          white-space: nowrap;
        }

        .event-row-control .form-control {
          max-width: 120px;
          height: 36px;
          padding: 0.3rem 0.55rem;
        }

        .event-footer {
          display: flex;
          justify-content: flex-start;
          margin-top: 0.45rem;
          padding: 0.15rem 0.15rem 0;
        }

        .event-add-link {
          color: var(--monitos-orange);
          font-size: 0.88rem;
          font-weight: 600;
          text-decoration: none;
        }

        .event-add-link:hover,
        .event-add-link:focus {
          color: var(--monitos-fg);
          text-decoration: none;
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
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 26px;
          height: 26px;
          border: 1px solid rgba(22, 22, 22, 0.1);
          border-radius: 999px;
          color: rgba(22, 22, 22, 0.56);
          font-size: 0.82rem;
          line-height: 1;
          text-decoration: none;
          background: rgba(245, 245, 245, 0.9);
          transition: color 120ms ease, border-color 120ms ease, background 120ms ease;
        }

        .event-remove-link:hover,
        .event-remove-link:focus {
          color: var(--monitos-orange);
          border-color: rgba(255, 78, 0, 0.28);
          background: rgba(255, 78, 0, 0.06);
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

          .event-row-head {
            display: none;
          }

          .hero-logo-shell {
            justify-content: flex-start;
          }

          .results-card {
            position: static;
          }
        }
      "))
    ),
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
            if (!is.null(resolved_logo_src)) {
              tags$img(
                src = resolved_logo_src,
                alt = "Novartis",
                class = "hero-logo"
              )
            }
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
            p(class = "helper-copy", "HR below 1 indicates benefit.")
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
            h3("Event timing"),
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
                  "Interim analysis schedule"
                ),
                uiOutput("primary_events_ui"),
                  p(
                    class = "helper-copy",
                    "Add one row per planned look, ordered by increasing deaths."
                  )
                )
              )
            )
          ),
          div(
            class = "panel-card",
            span(class = "section-kicker", "Decision thresholds"),
            h3("Statistical threshold settings"),
            div(
              class = "field-stack",
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
}
# nocov end
