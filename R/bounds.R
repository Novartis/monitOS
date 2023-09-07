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
#'               power_int=0.9,
#'               t1error=0.025,
#'               lhr_null = log(1.333),
#'               lhr_alt = log(0.9))
bounds <- function(events,  # label for UI: target number of OS events at each analysis
                   power_int = 0.9,  # what power do we want to not flag a safety concern at an interim analysis if the true OS HR equals our target alternative?
                                    # NOTE: should flag an error if this is lower than Power_Alt[nstage]: ask the user to increase to a value > Power_Alt[nstage]
                   t1error = 0.025,  # what is the (one-sided) type I error rate that we will accept at the final analysis?
                   lhr_null = log(1.333), # what is the minimum unacceptable OS HR?
                   lhr_alt = log(0.9)){  # what is a plausible alternative OS HR consistent with OS benefit?

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


  df <- data.frame(Events = events,
                   Null_Thres = rep(exp(lhr_null), times=nstage),
                   Alt_Thres = rep(exp(lhr_alt), times=nstage),
                   Continuation_Thres_HR = exp(lhr_con),
                   Continuation_Thres_Posterior = post_con,
                   Continuation_Thres_PredProb = c(pred_con, NA),
                   Power_Alt = power_all,
                   OneSided_t1error = t1error_all,
                   CI_level_monit_null = CI_level_monit_null)
  print(df)


  return(list(power = power_all,
              t1error = t1error_all,
              CI_level_monit_null = CI_level_monit_null,
              lhr_null = lhr_null,
              lhr_alt = lhr_alt,
              lhr_con = lhr_con,
              Power_Alt = power_all,                     # Probability of not flagging a safety concern when true OS equals the alternative OS
              Continuation_Thres_Posterior = post_con,   # Pr(true OS HR >= minimum acceptable OS HR | current data) (what I think Norbert asked for)
              Continuation_Thres_PredProb = pred_con,
              df=df))   # Pr(will rule out minimum acceptable OS HR at final analysis | current data) -
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

# debugging my derivation of conditional distribution of ZK|Zk
# set.seed(05092023)
# nsamps <- 10000000
# z1 = 1
# info = c(50/4, 100/4)
# crit = 1.5
# theta.samps <- rnorm(nsamps, mean=z1/sqrt(info[1]), sd= sqrt(1/info[1]))
# cond.power <- pnorm(crit,
#                     mean = z1*sqrt(info[1]/info[2]) + theta.samps*(info[2] - info[1])/sqrt(info[2]),
#                     sd = sqrt(1 - info[1]/info[2]))
# pos <- mean(cond.power)
# pos_mycalc <- pnorm(crit,
#                     mean = z1*sqrt(info[2]/info[1]),
#                     sd= sqrt((info[2] - info[1])/info[1]))
# print(pos_mycalc - pos)

