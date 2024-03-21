#' @title Bounds
#'
#' @description OS monitoring guidelines as proposed in manuscript "Monitoring Overall Survival in Pivotal Trials in Indolent Cancers".
#' Calculate thresholds for positivity that can be used at an analysis to judge whether emerging
#' evidence about the effect of treatment on OS is concerning or not. The threshold for positivity at any given analysis
#' is the value below which the observed hazard ratio must be in order to provide sufficient reassurance that the effect
#' on OS does not reach the selected unacceptable level of detriment (the margin hr_null).
#' Terminology follows the manuscript "Monitoring Overall Survival in Pivotal Trials in Indolent Cancers", publication submitted
#' @details Monitoring guidelines assume that the hazard ratio (HR) can adequately summarize the size of the benefits and harms of the experimental
#' intervention vs control on overall survival (OS). Furthermore, guidelines assume that an OS HR < 1 is consistent with a beneficial effect of the
#' intervention on OS (and smaller OS HRs <1 indicate increased efficacy).
#' @param events Vector. Target number of deaths at each analysis
#' @param power_int Scalar. Marginal power required at the Primary Analysis when true hazard ratio (HR) is hr_alt.
#' @param falsepos Scalar. Marginal one-sided false positive error rate we are prepared to tolerate at the Final Analysis. Determines the  positivity threshold at Final Analysis
#' @param hr_null Scalar. The unacceptably large detrimental effect of treatment on OS we want to rule out (on HR scale)
#' @param hr_alt Scalar. Plausible clinically relevant beneficial effect of treatment on OS (on HR scale)
#' @param rand_ratio Integer. If patients are randomized k:1 between experimental intervention and control, rand_ratio should be inputted as k.
#' Example: if patients are randomized 1:1 between experimental and control, k=1. If patients are randomized 2:1 between experimental and control, k=2.
#' @param hr_marg_benefit Scalar. We may be uncertain about what a plausible beneficial effect of treatment on OS is. User can enter a second plausible OS benefit (on HR scale)
#' and function will evaluate the probability we meet the positivity threshold at each analysis under this HR. This second OS benefit will usually be closer to 1 than hr_alt.
#' @importFrom stats pnorm qnorm
#' @return List that contains:
#' * `lhr_null`: Scalar, unacceptable OS log-HR,
#' * `lhr_alt`: Scalar, plausible clinically relevant log-HR,
#' * `lhr_pos`: Scalar, positivity thresholds for log-HR estimates,
#' * `summary`: Dataframe, which contains:
#'   * `OS HR threshold for positivity`,
#'   * `One sided false positive error rate`,
#'   * `Level of 2 sided CI needed to rule out hr_null`,
#'   * `Probability of meeting positivity threshold under hr_alt`,
#'   * `Positivity_Thres_Posterior`: Pr(true OS HR >= minimum unacceptable OS HR | current data),
#'   * `Positivity_Thres_PredProb`: Pr(OS HR estimate at Final Analysis <= Final Analysis positivity threshold | current data)
#' @export
#' @examples
#' # Example 01: OS monitoring guideline retrospectively applied to Motivating Example 1
#' # with delta null = 1.3, delta alt = 0.80, gamma_FA = 0.025 and  beta_PA = 0.10.
#' bounds(
#'   events = c(60, 89, 110, 131, 178),
#'   power_int = 0.9, # beta_PA
#'   falsepos = 0.025, # gamma_FA
#'   hr_null = 1.3, # delta_null
#'   hr_alt = 0.8, # delta_alt
#'   rand_ratio = 1, # rand_ratio
#'   hr_marg_benefit = NULL
#' )
#' # Example 02: OS monitoring guideline applied to Motivating Example 2
#' # with delta null = 4/3, delta alt = 0.7, gamma_FA = 0.20 and beta_PA = 0.1.
#' bounds(
#'   events = c(60, 89, 110, 131, 178),
#'   power_int = 0.9, # beta_PA
#'   falsepos = 0.025, # gamma_FA
#'   hr_null = 1.3, # delta_null
#'   hr_alt = 0.8, # delta_alt
#'   rand_ratio = 1, # rand_ratio
#'   hr_marg_benefit = 0.95
#' )
bounds <- function(events,
                   # OS events at each analysis
                   power_int = 0.9,
                   # 1-Beta PA, what power do we want to not flag a safety concern at an interim analysis if the true OS HR equals our target alternative?
                   falsepos = 0.025,
                   # Gamme FA, What is the (one-sided) type I error rate that we will accept at the final analysis?
                   hr_null = 1.3,
                   # Delta null, what is the minimum unacceptable OS HR?
                   hr_alt = 0.9,
                   # Delta alt, what is a plausible alternative OS HR consistent with OS benefit?
                   rand_ratio = 1,
                   # for every patient randomized to control, rand_ratio patients are allocated to experimental intervention
                   hr_marg_benefit = NULL
                   # evaluate probability of meeting positivity thresholds under a second plausible beneficial effect of treatment on OS (HR = hr_marg_benefit)
) {
  # Log scale
  lhr_null <- log(hr_null)
  lhr_alt <- log(hr_alt)

  # Init variables
  nstage <- length(events) # total number of analyses planned
  info <-
    rand_ratio * events / ((rand_ratio + 1)^2) # Fisher's information for log-HR at each analysis
  se <-
    sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  # Calculate the attained power when true HR = hr_alt at Final Analysis
  power_final <-
    pnorm((lhr_null - qnorm(1 - falsepos) * se[nstage] - lhr_alt) / se[nstage])

  # calculate the levels of the two-sided CIs used to monitor the OS log-HR
  # at each interim analysis and the corresponding one-sided false positive error rate
  # assuming we want marginal power = power_int to 'rule out' hr_Lnull at required
  # evidentiary level when true OS HR = hr_alt
  gamma <-
    2 * (1 - pnorm(((
      lhr_null - lhr_alt
    ) / se[1:(nstage - 1)]) - qnorm(power_int)))
  falsepos_all <- c(gamma / 2, falsepos)
  CI_level_monit_null <- 100 * (1 - 2 * falsepos_all)
  power_all <-
    c(rep(power_int, times = (nstage - 1)), power_final)

  lhr_pos <- lhr_null - qnorm(1 - falsepos_all) * se

  # Given the positivity thresholds, re-express these via Bayesian metrics
  post_pos <- calc_posterior(lhr_pos, lhr_null, events)
  pred_pos <- calc_predictive(lhr_pos, events)

  summary <- data.frame("Deaths" = events)

  # OS HR thresholds for positivity
  summary$"OS HR threshold for positivity" <- round(exp(lhr_pos), 3)

  # One sided false positive error_rate at each analysis
  summary$"One-sided false positive error rate" <- round(falsepos_all, 3)

  # Level of 2-sided CI needed to rule out δnull at given analysis (%)
  summary$"Level of 2-sided CI needed to rule out delta null" <- round(pmax(0, CI_level_monit_null), 0)

  # Probability of meeting positivity threshold under plausible OS benefit
  summary$"Probability of meeting positivity threshold under delta alt" <- round(power_all, 3)

  # Pr(true OS HR >= detrimental OS HR | current data)
  summary$"Posterior probability the true OS HR exceeds delta null given the data" <- round(post_pos, 3)
  summary$"Predictive probability the OS HR estimate at Final Analysis does not exceed the positivity threshold" <- c(round(pred_pos * 100, 3), NA)

  if (!is.null(hr_marg_benefit)) {
    # calculate the probability of meeting positivity thresholds under lhr_marg_benefit
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

  return(list(
    lhr_null = lhr_null,
    lhr_alt = lhr_alt,
    lhr_pos = lhr_pos,
    summary = summary
  ))
}
