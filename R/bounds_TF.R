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
#' 'continuation'), `lhr_null` (log-HR under the null hypothesis), `lhr_alt`
#' (log-HR under the alternative hypothesis) and `CI_level_monit_nullmax`
#' (levels of two-sided CIs which can be used to rule out delta_imax at each
#' analysis. Also, prints a table with the computed hazard ratio thresholds.
#' @export
#' @examples
#' events <- c(15, 30, 50, 69) #,....
#' procs <- bounds_TF(events, delta_imax = log(1.3))
bounds_TF <- function(events,         # Q1: Label for UI: target number of OS events at each analysis
                   power = 0.9,    # Q4: Power to rule out detrimental OS HR applicable at each analysis
                   t1error = 0.025, # Q3: What is the (one-sided) type I error rate that we will accept at the final OS analysis?
                   delta_imax = NULL) {  # Q2: What is the minimum unacceptable OS HR that we want to rule out at final OS analysis?

  # ********************* NOTE FOR THIBAUD ***************************************
  # the function expects log-HRs to be inputted, but I suggest we ask
  # the user to input HRs and then we take log transformation before passing
  # to this function.
  # ******************************************************************************

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

  # Derive an alternative way of representing the TF procedure decision thresholds:
  # calculate gamma_k s.t. decision rule is equivalent to continuing w/o flagging safety
  # concern if upper bound of (1-gamma_k)100% two-sided CI lies below delta_imax
  inflFtr <- exp(lhr_null - delta_imax)
  gamma <- 2*(1 - pnorm(qnorm(1 - t1error) - (log(inflFtr)/sqrt(4/events))))

  # Given the continuation thresholds, re-express these via Bayesian metrics
  post_con <- calc_posterior(lhr_con, lhr_null, events)
  pred_con <- calc_predictive(lhr_con, events)

  print(
    data.frame(Events = events,
               Null_Thres = exp(lhr_null),
               Alt_Thres = exp(lhr_alt),
               Continuation_Thres = exp(lhr_con),
               Continuation_Thres_Posterior = post_con,
               Continuation_Thres_PredProb = c(pred_con, NA),
               CI_level_monit_nullmax = 100*(1 - gamma))
  )

  return(list(lhr_con = lhr_con,
              lhr_null = lhr_null,
              lhr_alt = lhr_alt,
              CI_level_monit_nullmax = 100*(1 - gamma),  # We do not flag safety concern at analysis k if CI_level_monit_nullmax level 2-sided CI excludes delta_imax
              Continuation_Thres_Posterior = post_con,   # Pr(true OS HR >= minimum acceptable OS HR | current data) (what I think Norbert asked for)
              Continuation_Thres_PredProb = pred_con))   # Pr(will rule out minimum acceptable OS HR at final analysis | current data) -
                                                         # predictive probability Samson mentioned he has calculated - good to cross check we match results
}

#' Function which calculates for k=1, ..., K, Pr(log-HR >= lhr_null | theta.hat.k = lhr_con.k)
#' i.e. the posterior probability the true OS log-hr exceeds the minimum unacceptable
#' OS log-HR given the estimate of the log-hr at analysis k equals lhr_con.k (i.e. the estimate
#' is equal to the stage k 'continuation threshold').
#'
#' @param lhr_con vector of length K (# number of looks at OS data) containing 'continuation' thresholds on log-HR scale
#' @param lhr_null scalar - minumum unacceptable OS log-HR
#' @param events vector length K - number of OS events at each look at the data
#'
#' @return vector of length K - continuation thresholds expressed on posterior probability scale
#' @export
#'
#' @examples
calc_posterior <- function(lhr_con, lhr_null, events){

  info <- events / 4 # Fisher's information for log-HR at each analysis
  se <- sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  # calculating Pr(log-hr >= lhr_null | theta.hat.k = lk)
  # where lk is the threshold (for the partial likelihood estimate of the OS log-HR) for 'continuation'
  post <- 1 - pnorm((lhr_null - lhr_con)/se)
  return(post)
}

#' Calculates the posterior predictive probability of 'ruling out' lhr_null at final OS analysis
#' given current estimate of OS log-HR is lhr_cont_k, for k=1, ..., K-1
#'
#' @param lhr_con vector of length K (# number of looks at OS data) containing 'continuation' thresholds on log-HR scale
#' @param events vector length K - number of OS events at each look at the data
#'
#' @return vector of length K-1: continuation thresholds at analyses k=1, ..., K-1 expressed on scale of
#' posterior predictive probability of ruling out lhr_null at final OS analysis
#' @export
#'
#' @examples
calc_predictive <- function(lhr_con, events){

  nstage <- length(events)
  info <- events / 4 # Fisher's information for log-HR at each analysis
  se <- sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  # calculating Pr(ZK <= lK*sqrt(info.K) | Z.k = sqrt(info.k)*lk)
  # where lk is the OS log-HR threshold for 'continuation' at analysis k
  pred_pos <- vector(mode="numeric", length=(nstage-1))
  for(i in 1:(nstage-1)){
    pred_pos[i] <- pnorm(lhr_con[nstage]*sqrt(info[nstage]),
                         mean = lhr_con[i]*sqrt(info[i])*sqrt(info[nstage]/info[i]),
                         sd = sqrt((info[nstage] - info[i])/info[i]))
  }
  return(pred_pos)
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
