#' Find Positivity Threshold
#'
#' This function calculates the positivity threshold based on various criteria.
#'
#' @param pos_thld Numeric. The initial positivity threshold.
#' @param events Numeric vector of length 2. The number of events at each analysis.
#' @param rand_ratio Numeric. The randomization ratio.
#' @param hr_null Numeric. The hazard ratio under the null hypothesis.
#' @param hr_alt Numeric. The hazard ratio under the alternative hypothesis.
#' @param which_crit Integer. The criterion to be used for finding the positivity threshold:
#'   \itemize{
#'     \item 1: False positive / False negative equals required value.
#'     \item 2: False positive / (False negative + False positive) equals required value.
#'     \item 3: False positive equals required value.
#'     \item 4: Predictive probability equals required value.
#'   }
#' @param targ Numeric. The target value for the chosen criterion.
#'
#' @return Numeric. The calculated positivity threshold based on the specified criterion.
#'
#' @examples
#' find_pos(
#'   pos_thld = 1.5,
#'   events = c(100, 200),
#'   rand_ratio = 1,
#'   hr_null = 1,
#'   hr_alt = 1.5,
#'   which_crit = 1,
#'   targ = 0.05
#' )
#'
#' @export
find_pos <- function(
  pos_thld,
  events,
  rand_ratio,
  hr_null,
  hr_alt,
  which_crit,
  targ
) {
  log_pos_thld <- log(pos_thld)
  lhr_null <- log(hr_null)
  lhr_alt <- log(hr_alt)

  # Fisher's information for log-HR at each analysis
  info1 <- rand_ratio * events[1] / ((rand_ratio + 1)^2)

  # asymptotic standard error for log-HR at each analysis
  se1 <- sqrt(1 / info1)

  # Fisher's information for log-HR at each analysis
  info2 <- rand_ratio * events[2] / ((rand_ratio + 1)^2)

  # asymptotic standard error for log-HR at each analysis
  se2 <- sqrt(1 / info2)

  fp <- pnorm(log_pos_thld, mean = lhr_null, sd = se1, lower.tail = TRUE)
  fn <- pnorm(log_pos_thld, mean = lhr_alt, sd = se1, lower.tail = FALSE)

  # positivity threshold FA
  log_pos_fa <- qnorm(
    1 - 0.05,
    mean = lhr_null,
    sd = se2,
    lower.tail = FALSE
  )
  pred_prob <- monitOS::calc_predictive(c(log_pos_thld, log_pos_fa), events)

  # Switch based on criterion
  switch(
    which_crit,
    `1` = {
      # search for the positivity threshold such that false positive/false negative equals required value
      as.numeric((fp / fn) - targ)
    },
    `2` = {
      # search for the positivity threshold such that false positive/(false negative + false pos) equals required value
      as.numeric((fp / (fp + fn)) - targ)
    },
    `3` = {
      as.numeric(fp - targ)
    },
    `4` = {
      # search for the positivity threshold such that pred prob equals required value
      as.numeric(pred_prob - targ)
    }
  )
}
