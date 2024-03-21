#' Probabilities of meeting positivity threshold under target HR
#'
#' @param lhr_pos List. Log HRs for positive threshold
#' @param summary DataFrame. Summary dataframe from bounds.R
#' @param lhr_target Scalar. Target log HR to calculate the probability of meeting positivity thresholds
#' @param rand_ratio Integer. If patients are randomized k:1 between experimental intervention and control, rand_ratio should be inputted as k.
#' Example: if patients are randomized 1:1 between experimental and control, k=1. If patients are randomized 2:1 between experimental and control, k=2.
#'
#' @return Array. Probabilities of meeting positivity threshold under target HR
meeting_probs <-
  function(summary,
           lhr_pos,
           lhr_target = 1,
           rand_ratio = 1) {
    events <- summary$Deaths
    info <-
      rand_ratio * events / ((rand_ratio + 1)^2) # Fisher's information for log-HR at each analysis
    se <-
      sqrt(1 / info) # asymptotic standard error for log-HR at each analysis
    prob <- list()
    for (i in 1:length(events)) {
      prob[i] <-
        pnorm(lhr_pos[i],
          mean = lhr_target,
          sd = se[i],
          lower.tail = TRUE
        )
    }
    return(as.numeric(prob))
  }
