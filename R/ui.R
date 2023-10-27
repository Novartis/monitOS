#' Shiny app UI
#'
#' @param request generic shiny var
#' @importFrom glue glue
#' @import shiny shinydashboard
app_ui <- function(request){

  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Monitoring OS"),
    # Creating tabs
    dashboardSidebar(disable = TRUE),
    # Define body
    dashboardBody(
     fluidRow(box(title='Context', with=12, collapsed = FALSE, collapsible = TRUE, width = 12,
                  'These guidelines are meant to provide a pragmatic, yet rigorous, help to drug developers and decision makers,
                  since they are shaped by three fundamental ingredients: the clinically determined margin of detriment on OS that
                  is unacceptably high (delta null); the benefit on OS that is plausible given the mechanism of action of the novel
                  intervention (delta alt); and the quantity of information (i.e. survival events) it is feasible to accrue given
                  the clinical and drug development setting. The proposed guidelines facilitate transparent discussions between
                  stakeholders focusing on the risks of erroneous decisions and what might be an acceptable trade-off between power
                  and the false positive error rate.')),
                fluidRow(box(title='Instruction for use',collapsed = TRUE, collapsible = TRUE,  width=12, status='primary',
                  ' Monitoring guidelines assume that the hazard ratio (HR) can adequately summarize the size of the benefits and harms of the experimental intervention vs control on overall survival (OS). Furthermore, guidelines assume that an OS HR < 1 is consistent with a beneficial effect of the intervention on OS (and smaller OS HRs <1 indicate increased efficacy). For more details about how OS monitoring guidelines are formulated, please refer to ', tags$a(href="https://arxiv.org/", "monitOS paper"))),
                fluidRow(
                  box(title = 'Unacceptable detrimental effect', collapsed = TRUE, collapsible = TRUE, width = 12, status = "info",
                  column(4, sliderInput('hr_null', 'What is the smallest OS HR that represents an unacceptable detrimental effect of the novel intervention vs control?', min=1, max=2, value=4/3, step = 0.05)),
                  column(4, 'This "margin of detriment" should be specified on a case-by-case basis. Specification should be informed by disease setting (adjuvant vs potentially curative treatment); expected median OS on control; and how strong the observed treatment effect on the trial primary outcome (eg PFS) will need to be to achieve statistical significance.'),
                  column(4, 'Example: If any OS HR >= 1.333 would be unacceptably large, the answer to Q1 is 1.333.'))),
                fluidRow(
                  box(title = 'Plausible beneficial effect', collapsed = TRUE, collapsible = TRUE, width = 12, status = "info",
                  column(4, sliderInput('hr_alt', 'What is a plausible OS HR consistent with a beneficial effect of the experimental intervention vs control?', min=0.05, max=1, value=0.7, step = 0.05)),
                  column(4, 'This "margin of benefit" should reflect the beneficial effect that one could reasonably expect from the experimental indication given its mechanism of action (MoA).'),
                  column(4, 'Example: Based on results for competitor drugs with the same MoA, an OS HR = 0.9 is a plausible beneficial effect on OS and would be considered clinically relevant if statistical significance is achieved on the trial\'s primary endpoint.'))),
                # Events
                fluidRow(
                  box(title='Expected deaths at OS Final Analysis', collapsed = TRUE, collapsible = TRUE, width = 12, status = "info",
                  column(4, numericInput("eventOS", "How many deaths is it feasible to expect by the time of the final OS analysis?", value = 70, min=1)),
                  column(4, 'Final OS analysis is scheduled at longest feasible duration of follow-up.'))),
                fluidRow(
                  box(title='Expected deaths at trial Primary Analysis', collapsed = TRUE, collapsible = TRUE, width = 12, status = "info",
                  column(4, textInput("eventPA", "How many deaths are expected at the time of the trial\'s primary outcome analysis?", "28, 42")),
                  column(4, 'If the trial\'s primary outcome will be analyzed at multiple timepoints, enter here the number of deaths expected at the time of each of these analyses.'),
                  column(4, 'Example: PFS will be analyzed at 20 and 40 months after start of enrolment, at which times 28 and 42 deaths are expected to be observed.'))),
                # False positive
                fluidRow(
                  box(title='False positive error rate', collapsed = TRUE, collapsible = TRUE, width = 12,
                      column(4, sliderInput('falsepos', 'What (one-sided) false positive error rate can be tolerated at the OS Final Analysis?', min=0, max=0.3, value=0.1, step = 0.005)),
                      column(4, 'OS "Positivity threshold" is the value below which the observed OS HR must be in order to provide sufficient reassurance that the effect on OS does not reach the unacceptable level of detriment (your answer to Q1).'))),
                # Power interim
                fluidRow(
                  box(title='Power required', collapsed = TRUE, collapsible = TRUE, width = 12,
                      column(4, sliderInput('power_int', 'What power is required to meet the OS "positivity threshold" at time of the trial\'s Primary Analysis when the true OS HR equals your answer to Q2?', min=0.7, max=1, value=0.9, step = 0.01)),
                      column(4, 'A false positive error arises at the OS final analysis when true OS HR equals your answer to Q1 but no concerning evidence of unacceptable OS detriment is flagged. Should be set with consideration given to the false negative rate at the OS final analysis.'))),
                fluidRow(box(title = 'Other Parameters', collapsible = TRUE, collapsed = TRUE, width = 12,
                         fluidRow(
                           column(3, sliderInput('hr_marg_benefit', 'What is a plausible marginal OS HR consistent with OS benefit?',
                                                 min=0.7, max=1.1, value=0.95, step = 0.05)),
                           column(3, 'This HR is use to calculate the probability of meeting positivity threshold under marginal HR'),
                           column(3, sliderInput('rand_ratio', 'What is the randomization ratio?', min=0.5, max=3, value=1, step = 0.5)),
                           column(3, 'If patients are randomized k:1 between experimental intervention and control, rand_ratio should be inputted as k. For Example: if patients are randomized 1:1 between experimental and control, k=1. If patients are randomized 2:1 between experimental and control, k=2.')))),
                # Key results
                fluidRow(box(title = 'Thresholds for positivity', status = "success", tableOutput("bounds"), width = 12))
        )
  )

}

