#' Compute the overall survival hazard ratio continuation threshold at each
#' interim analysis stage according to Version 1 of the Tom Fleming procedure.
#'
#' @param power Power for the analysis. A float ranging from 0 to 1. Default is
#' 0.9.
#' @param t1error One sided type-1 error. A float ranging from 0 to 1. Default
#' is 0.025.
#' @param events A vector with the planned number of events at the analysis
#' stages. It should be at least 1.
#' @param nullfin The log-HR threshold to rule out under the null hypothesis at
#' the final analysis in case of low number of events (<220). Default is NULL.
#' @param low A logical object showing whether the trial will reach a small OS
#' number of events at final analysis. We recommend to set as TRUE if the trial
#' yields less than 220 events. If TRUE, nullfin should be specified.
#' @returns A list object, that includes lhr_con, lhr_null and lhr_alt. Also,
#' prints a table with the computed thresholds
#' @export
#' @examples
#' events1 <- c(68, 135, 220, 340) # sample size at each analysis stage
#' procs <- tf_bounds2(events1)
#'
#' events2 <- c(15, 30, 50, 69)
#' procs <- tf_bounds2(events1, low = TRUE, nullfin = log(1.33))
#'
tf_bounds2 <- function(events, power = 0.9, t1error = 0.025, low = FALSE,
                       nullfin = NULL) {

  # Perform sanity checks.
  tf_bounds2_checks(events, power, t1error, low, nullfin)

  nstage <- length(events) # total number of analyses planned
  info <- events / 4 # Fisher's information for log-HR at each analysis
  se <- sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  zalpha <- qnorm(1 - t1error)
  if (low) {
    lhr_conf <- nullfin - zalpha * se[nstage]
  } else {
    # At the final analysis, we want 'type1error' chance of falsely claiming a
    # detrimental effect on OS (i.e. if lower limit of (1 - 2 * t1error) * 100%
    # level CI for log-HR exceeds 0).
    # Calculate the normal quantile z_(1-alpha) which determines the length of
    # the (1 - 2 * type1) * 100% level CI.
    nullfin <- zalpha * se[nstage]
    lhr_conf <- 0
  }

  # Calculate 'alternative' log-HR (delta) such that at final analysis:
  # Pr(theta.hat + zalpha*se(theta.hat) <= zalpha*se(theta.hat) | theta=delta) =
  # power, i.e. have prob of (power) to rule out the null log-HR based on
  # (1 - 2 * t1error) * 100% - level CI.
  altfin <- lhr_conf - qnorm(power) * se[nstage]

  # Procedure keeps the alternative value of the log-HR constant across all
  # analyses.
  lhr_alt <- rep(altfin, times = nstage)

  # Compute 'null' log-HR at each stage such that we have prob (power) to rule
  # out the null log-HR when the log-HR is equal to delta.
  lhr_null <- lhr_alt[1:(nstage - 1)] +
    se[1:(nstage - 1)] * (qnorm(power) + qnorm(1 - t1error))
  lhr_null <- c(lhr_null, nullfin)

  # Calculate the critical values for the log-HR at each analysis (i.e. the
  # log-HR s.t the upper bound of the (1 - 2 * type1) * 100% - level CI is equal
  # to the null value).
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
tf_bounds2_checks <- function(events, power, t1error, low, nullfin) {

  # Power should be between 0 and 1.
  stopifnot("power must be between 0 and 1." = (power > 0 & power < 1))

  # Type I error should be between 0 and 1.
  stopifnot("t1error must be between 0 and 1." = (t1error > 0 & t1error < 1))

  # Number of events should be at least 1
  stopifnot("events should be greater than 1." = all(events > 1))

  # If low is TRUE then nullfin should be specified
  stopifnot("When low is TRUE, nullfin value should not be NULL." = low &
              !is.null(nullfin))
}
