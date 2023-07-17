#' Calculate Tom Fleming monitoring guideline boundaries
#'
#' @description Calculate the overall survival hazard ratio thresholds at each
#' interim analysis stage according to the Tom Fleming monitoring guideline.
#' @param events Vector. A vector with the planned number of events at the
#' analysis stages; `events` should be at least 1.
#' @param power Scalar. Power for the analysis. Default is 0.9.
#' @param t1error Scalar. One-sided Type-1 error. Default is 0.025.
#' @param delta_imax Scalar. The log hazard ratio (log-HR) threshold to rule out
#' under the null hypothesis at the final analysis satge (maximum information
#' level). Default is `NULL`. If `delta_imax` is `NULL`, then, the continuation
#' threshold at maximum information level is anchored at log-HR = 0 (that is a
#' hazard ratio equal to 1), and the log-HR threshold to rule out under the null
#' hypothesis at the final analysis is adjusted accordingly.
#' @returns List. A list object, that includes `lhr_con` (log-HR for
#' 'continuation'), `lhr_null` (log-HR under the null hypothesis) and `lhr_alt`
#' (log-HR under the alternative hypothesis). Also, prints a table with the
#' computed hazard ratio thresholds.
#' @export
#' @examples
#' events <- c(15, 30, 50, 69)
#' procs <- monitOS::bounds(events, delta_imax = log(1.3))
bounds <- function(events,
                   power = 0.9,
                   t1error = 0.025,
                   delta_imax = NULL) {

  # Perform sanity checks
  bounds_checks(events, power, t1error, delta_imax)

  nstage <- length(events) # total number of analysis stages planned
  info <- events / 4 # Fisher's information for log-HR at each analysis
  se <- sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  zalpha <- qnorm(1 - t1error)
  if (!is.null(delta_imax)) {
    lhr_conf <- delta_imax - zalpha * se[nstage]
  } else {
    # At the final analysis, we want 't1error' chance of falsely claiming a
    # detrimental effect on OS
    delta_imax <- zalpha * se[nstage]
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
  lhr_null <- lhr_alt[1:(nstage - 1)] +
    se[1:(nstage - 1)] * (qnorm(power) + zalpha)
  lhr_null <- c(lhr_null, delta_imax)

  # Calculate continuation thresholds for the obtained hazard ratio estimate
  # at each analysis stage
  lhr_con <- lhr_null - zalpha * se

  print(
    data.frame(Events = events,
               Null_Thres = exp(lhr_null),
               Alt_Thres = exp(lhr_alt),
               Continuation_Thres = exp(lhr_con))
  )

  return(list(lhr_con = lhr_con,
              lhr_null = lhr_null,
              lhr_alt = lhr_alt))
}

# Perform sanity checks
bounds_checks <- function(events, power, t1error, delta_imax) {

  # Power should be between 0 and 1.
  stopifnot("power must be between 0 and 1." = (power > 0 & power < 1))

  # Type I error should be between 0 and 1.
  stopifnot("t1error must be between 0 and 1." = (t1error > 0 & t1error < 1))

  # Number of events should be at least 1
  stopifnot("events should be greater than 1." = all(events > 1))
}
