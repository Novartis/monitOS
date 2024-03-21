#' Function which calculates for k=1, ..., K, Pr(log-HR >= lhr_null | theta.hat.k = lhr_con.k)
#' i.e. the posterior probability the true OS log-hr exceeds the minimum unacceptable
#' OS log-HR given the estimate of the log-hr at analysis k equals lhr_con.k (i.e. the estimate
#' is equal to the stage k 'continuation threshold').
#'
#' @param lhr_con vector of length K (# number of looks at OS data) containing 'continuation' thresholds on log-HR scale
#' @param lhr_null scalar - minumum unacceptable OS log-HR
#' @param events vector length K - number of OS events at each look at the data
#' @importFrom stats pnorm
#' @return vector of length K - continuation thresholds expressed on posterior probability scale
calc_posterior <- function(lhr_con, lhr_null, events) {
  info <-
    events / 4 # Fisher's information for log-HR at each analysis
  se <-
    sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  # calculating Pr(log-hr >= lhr_null | theta.hat.k = lk)
  # where lk is the threshold (for the partial likelihood estimate of the OS log-HR) for 'continuation'
  post <- 1 - pnorm((lhr_null - lhr_con) / se)
  return(post)
}

#' Title"
#' @description Calculates the posterior predictive probability of 'ruling out' lhr_null at final OS analysis
#' given current estimate of OS log-HR is lhr_cont_k, for k=1, ..., K-1
#' @param lhr_con vector of length K (# number of looks at OS data) containing 'continuation' thresholds on log-HR scale
#' @param events vector length K - number of OS events at each look at the data
#' @return vector of length K-1: continuation thresholds at analyses k=1, ..., K-1 expressed on scale of
#' posterior predictive probability of ruling out lhr_null at final OS analysis
#' @importFrom stats pnorm
calc_predictive <- function(lhr_con, events) {
  nstage <- length(events)
  info <-
    events / 4 # Fisher's information for log-HR at each analysis
  se <-
    sqrt(1 / info) # asymptotic standard error for log-HR at each analysis

  # calculating Pr(ZK <= lK*sqrt(info.K) | Z.k = sqrt(info.k)*lk)
  # where lk is the OS log-HR threshold for 'continuation' at analysis k
  pred_pos <- vector(mode = "numeric", length = (nstage - 1))
  for (i in 1:(nstage - 1)) {
    pred_pos[i] <- pnorm(
      lhr_con[nstage] * sqrt(info[nstage]),
      mean = lhr_con[i] * sqrt(info[i]) * sqrt(info[nstage] /
        info[i]),
      sd = sqrt((info[nstage] - info[i]) / info[i])
    )
  }
  return(pred_pos)
}
