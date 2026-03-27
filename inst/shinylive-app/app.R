# monitOS Shinylive App
#
# Self-contained Shiny app for shinylive (WebAssembly) deployment.
# Mirrors the package's R/ui.R and R/server.R exactly so the browser
# experience matches `monitOS::run_app()`.

library(shiny)
library(shinydashboard)

# ---------------------------------------------------------------------------
# Core statistical functions (embedded from monitOS package)
# ---------------------------------------------------------------------------

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
  power_all <- c(rep(power_int, times = (nstage - 1)), power_final)

  lhr_pos <- lhr_null - qnorm(1 - falsepos_all) * se

  post_pos <- calc_posterior(lhr_pos, lhr_null, events)
  pred_pos <- calc_predictive(lhr_pos, events)

  summary <- data.frame("Deaths" = events)
  summary$"OS HR threshold for positivity" <- round(exp(lhr_pos), 3)
  summary$"One-sided false positive error rate" <- round(falsepos_all, 3)
  summary$"Level of 2-sided CI needed to rule out delta null" <- round(
    pmax(0, ci_level_monit_null), 0
  )
  summary$"Probability of meeting positivity threshold under delta alt" <- round(
    power_all, 3
  )
  summary$"Posterior probability the true OS HR exceeds delta null given the data" <- round(
    post_pos, 3
  )
  summary$"Predictive probability the OS HR estimate at Final Analysis does not exceed the positivity threshold" <- c(
    round(pred_pos * 100, 3), NA
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

# ---------------------------------------------------------------------------
# UI — mirrors R/ui.R from the monitOS package
# ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Monitoring OS"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(box(
      title = "Context",
      collapsed = FALSE,
      collapsible = TRUE,
      width = 12,
      "These guidelines are meant to provide a pragmatic, yet rigorous, help to drug developers and decision makers,
                since they are shaped by three fundamental ingredients: the clinically determined margin of detriment on OS that
                is unacceptably high (delta null); the benefit on OS that is plausible given the mechanism of action of the novel
                intervention (delta alt); and the quantity of information (i.e. survival events) it is feasible to accrue given
                the clinical and drug development setting. The proposed guidelines facilitate transparent discussions between
                stakeholders focusing on the risks of erroneous decisions and what might be an acceptable trade-off between power
                and the false positive error rate."
    )),
    fluidRow(box(
      title = "Instruction for use",
      collapsed = TRUE,
      collapsible = TRUE,
      width = 12,
      status = "primary",
      " Monitoring guidelines assume that the hazard ratio (HR) can adequately summarize the size of the benefits and harms of the experimental intervention vs control on overall survival (OS). Furthermore, guidelines assume that an OS HR < 1 is consistent with a beneficial effect of the intervention on OS (and smaller OS HRs <1 indicate increased efficacy). For more details about how OS monitoring guidelines are formulated, please refer to ",
      tags$a(href = "https://www.tandfonline.com/doi/full/10.1080/19466315.2024.2365648", "monitOS paper")
    )),
    fluidRow(
      box(
        title = "Unacceptable detrimental effect",
        collapsed = TRUE,
        collapsible = TRUE,
        width = 12,
        status = "info",
        column(
          4,
          sliderInput(
            "hr_null",
            "What is the smallest OS HR that represents an unacceptable detrimental effect of the novel intervention vs control?",
            min = 1,
            max = 2,
            value = 4 / 3,
            step = 0.05
          )
        ),
        column(
          4,
          'This "margin of detriment" should be specified on a case-by-case basis. Specification should be informed by disease setting (adjuvant vs potentially curative treatment); expected median OS on control; and how strong the observed treatment effect on the trial primary outcome (eg PFS) will need to be to achieve statistical significance.'
        ),
        column(
          4,
          "Example: If any OS HR >= 1.333 would be unacceptably large, the answer to Q1 is 1.333."
        )
      )
    ),
    fluidRow(
      box(
        title = "Plausible beneficial effect",
        collapsed = TRUE,
        collapsible = TRUE,
        width = 12,
        status = "info",
        column(
          4,
          sliderInput(
            "hr_alt",
            "What is a plausible OS HR consistent with a beneficial effect of the experimental intervention vs control?",
            min = 0.05,
            max = 1,
            value = 0.7,
            step = 0.05
          )
        ),
        column(
          4,
          'This "margin of benefit" should reflect the beneficial effect that one could reasonably expect from the experimental indication given its mechanism of action (MoA).'
        ),
        column(
          4,
          "Example: Based on results for competitor drugs with the same MoA, an OS HR = 0.9 is a plausible beneficial effect on OS and would be considered clinically relevant if statistical significance is achieved on the trial's primary endpoint."
        )
      )
    ),
    fluidRow(
      box(
        title = "Expected deaths at Final Analysis",
        collapsed = TRUE,
        collapsible = TRUE,
        width = 12,
        status = "info",
        column(
          4,
          numericInput(
            "eventOS",
            "How many deaths is it feasible to expect by the time of the final analysis?",
            value = 70,
            min = 1
          )
        ),
        column(
          4,
          "Final analysis is scheduled at longest feasible duration of follow-up."
        )
      )
    ),
    fluidRow(
      box(
        title = "Expected deaths at trial Primary Analysis",
        collapsed = TRUE,
        collapsible = TRUE,
        width = 12,
        status = "info",
        column(
          4,
          textInput(
            "eventPA",
            "How many deaths are expected at the time of the trial's primary outcome analysis?",
            "28, 42"
          )
        ),
        column(
          4,
          "If the trial's primary outcome will be analyzed at multiple timepoints, enter here the number of deaths expected at the time of each of these analyses."
        ),
        column(
          4,
          "Example: PFS will be analyzed at 20 and 40 months after start of enrolment, at which times 28 and 42 deaths are expected to be observed."
        )
      )
    ),
    fluidRow(
      box(
        title = "False positive error rate",
        collapsed = TRUE,
        collapsible = TRUE,
        width = 12,
        column(
          4,
          sliderInput(
            "falsepos",
            "What (one-sided) false positive error rate can be tolerated at the Final Analysis?",
            min = 0,
            max = 0.3,
            value = 0.1,
            step = 0.005
          )
        ),
        column(
          4,
          'A false positive error arises at the OS final analysis when true OS HR equals your answer to Q1 but no concerning evidence of unacceptable OS detriment is flagged. Should be set with consideration given to the false negative rate at the OS final analysis. OS "Positivity threshold" is the value below which the observed OS HR must be in order to provide sufficient reassurance that the effect on OS does not reach the unacceptable level of detriment (your answer to Q1).'
        )
      )
    ),
    fluidRow(
      box(
        title = "Power required",
        collapsed = TRUE,
        collapsible = TRUE,
        width = 12,
        column(
          4,
          sliderInput(
            "power_int",
            'What power is required to meet the OS "positivity threshold" at time of the trial\'s Primary Analysis when the true OS HR equals your answer to Q2?',
            min = 0.7,
            max = 1,
            value = 0.9,
            step = 0.01
          )
        ),
        column(
          4,
          "Typical choices for power at Primary Analysis are 0.8 or 0.9, but should be set with consideration given to the power at the Final Analysis. Example: Setting power at Primary Analysis to be greater than or equal to the
                           power achieved at the Final Analysis is sufficient to ensure positivity thresholds at successive analyses form a decreasing sequence."
        ),
        column(4, "Example: A power of 0.95")
      )
    ),
    fluidRow(box(
      title = "Optional Parameters",
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      fluidRow(
        column(
          4,
          sliderInput(
            "hr_marg_benefit",
            "What is a plausible marginal OS HR consistent with OS benefit?",
            min = 0.7,
            max = 1.1,
            value = 0.95,
            step = 0.05
          )
        ),
        column(
          4,
          "This HR is use to calculate the probability of meeting positivity threshold under marginal HR. The app will evaluate
                                the probability of meeting the positivity threshold at each analysis under this incremental benefit."
        ),
        column(
          4,
          "Example: Suppose our
                                answer to Q2 is OS HR = 0.9 but it is also plausible that OS HR = 0.95 and this would still represent an important clinical benefit
                                given other benefits of the novel intervention (such as milder side effect profile)"
        )
      ),
      fluidRow(
        column(
          4,
          sliderInput(
            "rand_ratio",
            "What is the randomization ratio?",
            min = 1,
            max = 3,
            value = 1,
            step = 1
          )
        ),
        column(
          4,
          "If patients are randomized k:1 between experimental intervention and control, rand_ratio should be inputted as k."
        ),
        column(
          4,
          "Example: if patients are randomized 1:1 between experimental and control, k=1. If patients are randomized 2:1 between experimental and control, k=2."
        )
      )
    )),
    fluidRow(box(
      title = "Thresholds for positivity",
      status = "success",
      tableOutput("bounds"),
      width = 12
    ))
  )
)

# ---------------------------------------------------------------------------
# Server — mirrors R/server.R from the monitOS package
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
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

  # Exclude posterior and predictive probability columns (cols 6, 7)
  output$bounds <- renderTable(react()$summary[, -c(6, 7)])
}

shinyApp(ui, server)
