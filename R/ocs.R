#' Produce operational characteristics outputs for monitoring guideline
#'
#' @description Function to assess the operational characteristics of Tom
#' Fleming's monitoring guideline. The function gets as input the thresholds of
#' continuation at different stages of the study, calculated using the `bounds`
#' function and returns the operational characteristics of the monitoring rule
#' under varying true hazard ratio scenarios inserted by the user.
#' @param thres1 (Scalar | Vector). The hazard ratio thresholds of continuation
#' to flag a safety issue at each trial stage.
#' @param thres2 (Scalar | Vector). The hazard ratio thresholds  of continuation
#' to stop the trial due to safety issue. Default is `NULL`. Leave to `NULL`
#' if specific stopping boundaries are not known. Note that `thres2` is expected
#' to be greater to `thres1` for all trial stages.
#' @param events Vector.  A vector with the planned number of events at the
#' analysis stages; `events` should be at least 1.
#' @param method String. Either "joint" for operational characteristics
#' calculated based on joint probabilities, i.e., P(Reach analysis stage k and
#' flag a safety issue) or "cond" for operational characteristics calculated
#' based on conditional probabilities, i.e., P(Flag a safety issue at stage k|
#' have not flagged a safety issue at any precedent analysis 1, ..., k-1).
#' Method "joint" is appropriate to explore at analysis stage, whereas "cond"
#' may be explored when the trial is actually in progress.
#' @param hrr Vector. The range of true hazard ratio values to plot the
#' probability of flagging a safety concern at least once across all trial
#' stages and the marginal probabilities of flagging an safety concern at the
#' first interim and final analysis. Notice that this argument is used for
#' plotting purposes, as compared to `hrs` which is used to return the exact
#' computed probability values. If `thres2` is not `NULL`, the `hrr` hazard
#' ratio range is used to plot the probability to flag a safety issue at least
#' once, suggest stopping the trial at least once (i.e., exceed the thres2
#' bounds) and claim accelerated approval (i.e., not exceed `thres1`/flag a
#' safety issue at any stage before the final analysis) during the course of
#' the trial.
#' @param hrs (Scalar | Vector). The specific true hazard ratio values to
#' calculate and return the joint/conditional probabilities and the overall
#' probability of flagging a safety concern at least once across all trial
#' stages. If `thres2` is not `NULL`, the probability to stop at least once
#' during the trial course and the probability to flag a safety issue but not
#' suggest stopping the trial are calculated instead.
#' @param col (Optional) Vector. A vector of color names for the plots. Must
#' have same length to `hrs`, if specified.
#' @import dplyr ggplot2 tibble
#' @importFrom dplyr %>% mutate
#' @return List. Returns a list with two tables and three figures. The
#' tables include information on the stopping probabilities based on the
#' scenarios specified by the user and the plots include visualizations of these
#' results.
#' @export
#' @examples
#'
#' events <- c(50, 100) # A trial with an interim and a final analysis
#' thres1 <- c(1.3, 1) # 1st set of thresholds; if exceeded flag a safety issue
#' thres2 <- c(1.8, 1.2) # 2nd set of thresholds; if exceeded suggest stopping
#'
#' hrr <- seq(0.6, 1.5, by = 0.01)
#' hrs <- c(0.7, 1, 1.2, 1.5, 1.8)
#'
#' # Get the operational characteristics (OCs) based on the above scenario
#' method <- "joint" # OCs calculated at design stage
#'
#' res <- monitOS::ocs(thres1=thres1,
#'                     thres2=thres2,
#'                     method=method,
#'                     events=events,
#'                     hrr=hrr,
#'                     hrs=hrs)
#'
ocs <- function(thres1,
                thres2 = NULL,
                method = 'joint',
                events,
                hrs=1.3,
                hrr=seq(0.6, 1.5, by = 0.01),
                col = NULL){

  # Sanity checks
  sanity_checks(thres1, thres2, events, hrr, hrs, col)

  # Log scale
  logthres1 <-
    log(thres1) # log transform thresholds for continuation
  logthres2 <- if(is.null(thres2)) thres2 else log(thres2)
  loghrs <-
    log(hrs) # log transform hazard ratios to calculate stopping probs
  loghrr <-
    log(hrr) # log transform hazard ratios to calculate stopping probs

  # Calculate joint & conditional probabilities
  logthres <- if(is.null(thres2)) logthres1 else logthres2

  # Probs
  prob_stop <-
    sapply(
      loghrs,
      FUN = flag_stop,
      method = method,
      logthres = logthres,
      events = events
    )

  prob_flag <- if (!is.null(thres2)) {
    sapply(
      loghrs,
      FUN = flagprob,
      logthres1 = logthres1,
      logthres2 = logthres2,
      events = events
    )
  } else {
    NA
  }

  # Compile results
  tab1 <-
    tibble(
      numev = rep(events, length(loghrs)),
      stage = rep(seq(1, length(events)), length(loghrs)),
      prob_stop = c(prob_stop),
      prob_flag_si  = c(prob_flag),
      true_hr_num = rep(hrs, each = length(events)),
      true_hr_fac = factor(round(rep(hrs, each = length(events)), 2))
    )

  tab2 <- tab1 %>%
    mutate(prob_stop_joint = c(
      if(method == "joint"){
        prob_stop
      } else {
        sapply(
          loghrs,
          FUN = flag_stop,
          method = "joint",
          logthres = logthres,
          events = events
        )})) %>%
    group_by(true_hr_num) %>%
    summarise(prob_flag_si =
                       if(!is.null(thres2)) sum(prob_flag_si) else sum(prob_stop_joint),
                     prob_stop =
                       if(!is.null(thres2)) sum(prob_stop_joint) else NA) %>%
    rename(true_hr = true_hr_num)

  # Get probability to flag safety issue and/or stop at least once for the
  # range of hazard ratios provided
  flagsi <- 1 - sapply(loghrr,
                       flag_safety,
                       method=method,
                       logthres = logthres1,
                       events = events)

  stoptrial <- if (!is.null(logthres2)) {
    1 - sapply(loghrr,
               flag_safety,
               method=method,
               logthres = logthres2,
               events = events)
  } else {
    NULL
  }

  claimap <- if (!is.null(logthres2)) {
    sapply(loghrr,
           flag_safety,
           method=method,
           logthres = logthres1[-length(logthres1)],
           events = events[-length(events)])
  } else {
    NULL
  }

  tab3 <- if (!is.null(logthres2)) {
    tibble(
      prob_atleast1 = c(flagsi),
      true_hr = round(hrr, 2))
  } else {
    tibble(
      prob_atleast11 = c(flagsi),
      prob_atleast12 = c(stoptrial),
      claimap = c(claimap),
      true_hr = round(hrr, 2))
  }

  # OCs at first interim and final analysis
  info <- events/4
  tab4 <- tibble(
    flag_si = c(sapply(log(hrr),
                       pnorm,
                       q = log(thres1[1]),
                       sd = sqrt(1/info[1]),
                       lower.tail = FALSE),
                sapply(log(hrr),
                       pnorm,
                       q = log(thres1[length(events)]),
                       sd = sqrt(1/info[length(events)]),
                       lower.tail = FALSE)),
    true_hr = rep(hrr, 2),
    analysis = rep(c("First Interim Analysis", "Final Analysis"),
                   each = length(hrr))
  )

  # Generate plots
  plots <- plot_ocs(logthres1,
                             logthres2,
                             method,
                             hrr,
                             hrs,
                             ocs_stage = tab1,
                             ocs_trial = tab3,
                             ocs_iafa = tab4,
                             col)

  return(list(
    ocs_stage = tab1 %>%
      select(-true_hr_num) %>%
      rename(true_hr = true_hr_fac),
    ocs_trial = tab2,
    plots = plots
  ))
}


# Table 1 names (bottom left):
#
#   true_hr = "True/Assumed HR" or "True/Assumed hazard ratio"
# prob_flag_si = if (thres2 == NULL) {"Probability to flag a safety issue at least once" or "Flag a SI at least once (prob.)"} else {"Probability to flag a safety issue but NOT suggest stopping the trial"}
# prob_stop =  if (thres2 == NULL) {DELETE/HIDE COLUMN}else{"Probability for early termination" or "Probability to suggest stopping the trial"}
#
# Table 2 names (bottom right):
#
#   numev = "Events (#)"
# stage = "Stage"
# prob_stop = if (thres2 == NULL) {"Probability to flag a safety issue"} else {"Probability to suggest stopping the trial"}
# prob_flag_si = if (thres2 == NULL) {DELETE/HIDE COLUMN} else {"Probability to flag a safety issue"}
# true_hr = "True/Assumed HR" or "True/Assumed hazard ratio"


# Perform sanity checks
sanity_checks <- function(thres1, thres2, events, hrr, hrs, col) {
  # Hazard ratios should be > 0
  stopifnot("thres1 must be positive." = all(thres1 > 0))

  if (!is.null(thres2)) {
    # Hazard ratios should be > 0
    stopifnot("thres2 must be positive." = all(thres2 > 0))
    # thres2 values should be greater than thres1 for all trial stages
    stopifnot(
      "All thres2 values should be greater than the corresponding thres1
              values." = all(thres2 > thres1)
    )
  }

  # Number of events should be at least 1
  stopifnot("events should be greater than 1." = all(events > 1))

  # Hazard ratios should be > 0
  stopifnot("hrr must be positive." = all(hrr > 0))

  # Hazard ratios should be > 0
  stopifnot("hrs must be positive." = all(hrs > 0))

  # Colors must have the same length to hrs
  if (!is.null(col))
    stopifnot("col must have the same length to hrs." =
                length(col) == length(hrs))
}
