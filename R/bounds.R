#' Adapted version of TF monitoring procedure which considers the same detrimental OS log-HR
#' at each analysis
#'
#' @description Calculates bounds of versions of the Fleming monitoring procedure where lhr_null
#' and lhr_alt are specified by the user and are both kept constant throughout the
#' procedure (instead the false positive or false negative risks vary across analyses)
#'
#' @param events Vector. Target number of OS events at each analysis
#' @param power_int Scalar. Should be NULL if the user specifies t1error. If t1error is NULL, the user specifies
#' the required marginal power to rule out lhr_null when lhr = lhr_alt at the final analysis
#' (and at each interim analysis too). This specification then drives the false positive risks we are
#' willing to accept at each analysis
#' @param t1error Scalar or Vector. Should be NULL if the user specifies power. If power
#' is NULL, the user must input this argument. User can specify either a scalar or a
#' vector of length K. If the user inputs a scalar, this is interpreted as the one-sided
#' type I error rate agreed with the HA for the final analysis. We then
#' calculate the attained power under lhr_alt and require this at every interim analysis. If the user
#' inputs t1error as a vector argument, this is interpreted as the sequence of (marginal, one-sided) type I error
#' rates we are willing to accept at each OS analysis.
#' @param lhr_null Scalar. The minimum unacceptable OS log-HR
#' @param lhr_alt Scalar. Plausible clinically relevant beneficial effect of treatment (log-HR)
#'
#' @return List. A list object that contains `t1error` (one-sided marginal type I error rates at each analysis),
#' `CI_level_monit_null` (level of two-sided CIs used to rule out lhr_null at each analysis),
#' `lhr_null` (minimum unacceptable OS log-HR), `lhr_alt` (plausible clinically relevant log-HR)
#' `lhr_con` (log-HR for 'continuation'), `Continuation_Thres_Posterior` ('continuation' thresholds
#' expressed as posterior probabilities true log-HR exceeds lhr_null), `Continuation_Thres_PredProb`
#' ('continuation' thresholds expressed as predictive probabilities of ruling out
#' lhr_null at final analysis). Also prints a table with this information.
#' @export
#'
#' @examples
#' res <- bounds(events=c(20, 50, 60),
#'               power=0.9,
#'               t1error=0.025,
#'               lhr_null = log(1.333),
#'               lhr_alt = log(0.9))
bounds <- function(events,  # OS events at each analysis
                   power_int = 0.9,  # 1-βPA, what power do we want to not flag a safety concern at an interim analysis if the true OS HR equals our target alternative?
                   t1error = 0.025,  # γFA, What is the (one-sided) type I error rate that we will accept at the final analysis?
                   lhr_null = log(1.3), # δnull, what is the minimum unacceptable OS HR?
                   lhr_alt = log(0.9)){  # δalt, what is a plausible alternative OS HR consistent with OS benefit?

  # ********************* NOTE FOR THIBAUD ***************************************
  # the function expects log-HRs to be inputted, but I suggest we ask
  # the user to input HRs and then we take log transformation before passing
  # to this function.
  # ******************************************************************************

  nstage <- length(events) # total number of analyses planned
  info <- events / 4 # Fisher's information for log-HR at each analysis
  se <- sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  if(!is.null(t1error)){
    # if user specifies one-sided type I error rate at which we need to rule out lhr_null
    if(length(t1error) == 1){
      # user has only specified type I error rate at final analysis
      # Calculate the attained power at lhr_alt at final analysis
      power_final <- pnorm((lhr_null - qnorm(1 - t1error)*se[nstage] - lhr_alt)/se[nstage])

      # calculate the levels of the two-sided CIs used to monitor the OS log-HR
      # at each interim analysis and the corresponding one-sided t1 error rates
      # assuming we want marginal power = power_int to rule out lhr_null at each IA
      gamma <- 2*(1 - pnorm(((lhr_null - lhr_alt)/se[1:(nstage-1)]) - qnorm(power_int)))
      t1error_all <- c(gamma/2, t1error)
      CI_level_monit_null <- 100*(1 - 2*t1error_all)
      power_all <- c(rep(power_int, times=(nstage-1)), power_final)
    }else{
      # length(t1error) == nstage
      # user has specified a sequence of one-sided t1 error rates which will determine
      # the levels of the two-sided confidence intervals used to monitor the OS log-HR
      t1error_all <- t1error
      CI_level_monit_null <- 100*(1 - 2*t1error_all)
      power_all <- pnorm((lhr_null - qnorm(1 - t1error_all)*se - lhr_alt)/se)
    }
  }else{
    # user inputs the required marginal power under log-HR = lhr_alt
    # at final OS analysis and at each interim analysis too
    gamma <- 2*(1 - pnorm(((lhr_null - lhr_alt)/se[1:(nstage-1)]) - qnorm(power_int)))
    gamma <- c(gamma,
               2*(1 - pnorm(((lhr_null - lhr_alt)/se[nstage]) - qnorm(power_final))))
    t1error_all <- gamma/2
    power_all <- c(rep(power_int, times=(nstage-1)), power_final)
    CI_level_monit_null <- 100*(1 - gamma)
  }
  lhr_con <- lhr_null - qnorm(1 - t1error_all)*se

  # Given the continuation thresholds, re-express these via Bayesian metrics
  post_con <- calc_posterior(lhr_con, lhr_null, events)
  pred_con <- calc_predictive(lhr_con, events)

  summary <- data.frame(Events = events,
                        Detla_null = rep(exp(lhr_null), times=nstage),
                        Delta_alt = rep(exp(lhr_alt), times=nstage),
                        Continuation_Thres_HR = exp(lhr_con),  # OS HR thresholds for positivity
                        OneSided_t1error = t1error_all,  # One sided false positive error_rate
                        CI_level_monit_null = pmax(0, CI_level_monit_null),  # Level of 2 sided CI needed to rule out δnull
                        Power_Alt = power_all,  # Probability of meeting positivity threshold under δalt
                        Continuation_Thres_Posterior = post_con, # Pr(true OS HR >= minimum acceptable OS HR | current data)
                        Continuation_Thres_PredProb = c(pred_con, NA))

  print(summary)

  return(list(
   lhr_null = lhr_null,
   lhr_alt = lhr_alt,
   lhr_con = lhr_con,
   summary = summary)
         )

}


#' Calculate Tom Fleming monitoring guideline boundaries
#'
#' @description Calculate the overall survival hazard ratio thresholds at each
#' interim analysis stage according to the Tom Fleming monitoring guideline.
#' @param events Vector. A vector with the planned number of events at the
#' analysis stages; `events` should be at least 1.
#' @param power Scalar. Power for the analysis. Default is 0.9.
#' @param t1error Scalar. One-sided Type-1 error. Default is 0.025.
#' @param lhr_null Scalar. The log hazard ratio (log-HR) threshold to rule out
#' under the null hypothesis at the final analysis satge (maximum information
#' level). Default is `NULL`. If `lhr_null` is `NULL`, then, the continuation
#' threshold at maximum information level is anchored at log-HR = 0 (that is a
#' hazard ratio equal to 1), and the log-HR threshold to rule out under the null
#' hypothesis at the final analysis is adjusted accordingly.
#' @returns List. A list object, that includes `lhr_con` (log-HR for
#' 'continuation'), `lhr_null` (log-HR under the null hypothesis), `lhr_alt`
#' (log-HR under the alternative hypothesis) and `CI_level_monit_nullmax`
#' (levels of two-sided CIs which can be used to rule out lhr_null at each
#' analysis. Also, prints a table with the computed hazard ratio thresholds.
#' @export
#' @examples
#' res <- bounds_dyn(c(15, 30, 50, 69),
#'                    lhr_null = log(1.3))
bounds_dyn <- function(events,         # Q1: Label for UI: target number of OS events at each analysis
                       power = 0.9,    # Q4: Power to rule out detrimental OS HR applicable at each analysis
                       t1error = 0.025, # Q3: What is the (one-sided) type I error rate that we will accept at the final OS analysis?
                       lhr_null = NULL) {  # Q2: What is the minimum unacceptable OS HR that we want to rule out at final OS analysis?

  # ********************* NOTE FOR THIBAUD ***************************************
  # the function expects log-HRs to be inputted, but I suggest we ask
  # the user to input HRs and then we take log transformation before passing
  # to this function.
  # ******************************************************************************

  nstage <- length(events) # total number of analysis stages planned
  info <- events / 4 # Fisher's information for log-HR at each analysis
  se <- sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  zalpha <- qnorm(1 - t1error)
  if (!is.null(lhr_null)) {
    lhr_conf <- lhr_null - zalpha * se[nstage]
  } else {
    # At the final analysis, we want 't1error' chance of falsely claiming a
    # detrimental effect on OS
    lhr_null <- zalpha * se[nstage]
    lhr_conf <- 0
  }

  # Calculate 'alternative' log-HR such that at final analysis:
  # Pr(theta.hat + zalpha*se(theta.hat) <= zalpha*se(theta.hat) | theta=delta) =
  # power
  altfin <- lhr_conf - qnorm(power) * se[nstage]

  # Procedure keeps the alternative value of the log-HR constant across all
  # analyses
  lhr_alt <- rep(altfin, times = nstage)

  # Update 'null' log-HR for each stage
  lhr_null <- lhr_alt[1:(nstage)] + se * (qnorm(power) + zalpha)

  # Calculate continuation thresholds for the obtained hazard ratio estimate
  # at each analysis stage
  lhr_con <- lhr_null - zalpha * se

  # Derive an alternative way of representing the TF procedure decision thresholds:
  # calculate gamma_k s.t. decision rule is equivalent to continuing w/o flagging safety
  # concern if upper bound of (1-gamma_k)100% two-sided CI lies below lhr_null
  inflFtr <- exp(lhr_null - lhr_null)
  gamma <- 2*(1 - pnorm(qnorm(1 - t1error) - (log(inflFtr)/sqrt(4/events))))

  # Given the continuation thresholds, re-express these via Bayesian metrics
  post_con <- calc_posterior(lhr_con, lhr_null, events)
  pred_con <- calc_predictive(lhr_con, events)

  summary <-
    data.frame(Events = events,
               Null_Thres = exp(lhr_null),
               Alt_Thres = exp(lhr_alt),
               Continuation_Thres = exp(lhr_con),
               Continuation_Thres_Posterior = post_con,
               Continuation_Thres_PredProb = c(pred_con, NA),
               CI_level_monit_nullmax = pmax(0, 100*(1 - gamma)))

  print(summary)

  return(list(lhr_con = lhr_con,
              lhr_null = lhr_null,
              lhr_alt = lhr_alt,
              summary= summary))}


