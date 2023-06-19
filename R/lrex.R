#' Calculate the exact test p-value given a true HR scenario and the observed
#' number of events in a two-arm trial.
#' 
#' @param events (integer) The number of observed events.
#' @param n0 (integer) Sample size in the control group.
#' @param n1 (integer) Sample size in the treatment group.
#' @param crit (float) The observed critical value of the log-rank test.
#' @returns (numeric) Exact test p-value, i.e., Pr(LR < crit | hr) across all possible
#' permutations where LR is the log-rank test result given the specific permutation.
#' @example 
#' set.seed(12345)
#' n <- 20
#' dt <- data.frame(id = 1:n,
#'                   time = rexp(n),
#'                   status = rbinom(n, size = 1, prob = 0.2),
#'                   group = rbinom(n, size = 1, prob = 0.5)
#' )
#' lrdt <- survival::survdiff(survival::Surv(time, status) ~ group, data=dt)
#' # 3 events occurred
#' lrex(events = sum(dt$status), n0 = sum(dt$group==0),
#'      n1 = sum(dt$group==1), crit = lrdt$chisq)

lrex <- function(events, n0, n1, crit){

  lrex_checks(events, n0, n1, crit)
  hr <- 1 # under the null HR is assumed to be equal to 1
  
  # generate all cases (2^events)
  vec <- 2^((events-1):0)
  output <- matrix(0, nrow = 1, ncol = events)
  for (i in 1:(2^events-1)) {
    q <- (i %/% vec) %% 2
    output <- rbind(output, q)
  }

  # initialization
  logrank_num <- 0
  logrank_den <- 0
  prob <- 1
  for (k in 1:events) {
    if (k == 1) {
      atRiskT <- matrix(n1, nrow = 2^events, ncol = 1)
      atRiskC <- matrix(n0, nrow = 2^events, ncol = 1)
    } else {
      atRiskT <- matrix(n1 - apply(as.matrix(output[,1:(k-1)]), 1, sum))
      atRiskC <- matrix(n0 - apply(1-as.matrix(output[,1:(k-1)]), 1, sum))
    }
    
    prob0 <- atRiskT / (atRiskC + atRiskT)
    logrank_num <- logrank_num + (output[,k] - matrix(prob0))
    logrank_den <- logrank_den + prob0 * (1 - prob0)
    prob <- prob * ((hr * atRiskT * output[,k] + atRiskC * (1 - output[,k])) / (hr * atRiskT + atRiskC))
  }
  
  logrank <- logrank_num / sqrt(logrank_den)  # logrank test statistic (2^events values)

  return(sum(prob * (logrank > crit)))
}


# Perform sanity checks
lrex_checks <- function(events, n0, n1, crit) {
  
  # crit must be numeric
  stopifnot("crit must be numeric" = is.numeric(crit))
  
  # Sample size must be larger than number of events
  stopifnot("total sample size should be >= number of events" = (n0 + n1 >= events))
  
  # Number of events and sample size should be 0 or more
  stopifnot("events cannot be negative." = (events >= 0))
  stopifnot("n0 and n1 cannot be negative." = ((n0 >= 0) & (n1 >= 0)))
}
