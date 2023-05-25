#' Function to assess the operational characteristics of Tom Fleming's overall
#' survival monitoring guideline. The function gets as input the thresholds for
#' continuation at different stages of the study and returns the operational
#' characteristics of the monitoring rule under varying hazard ratio scenarios
#' inserted by the user.
#'
#' @param thres1 The hazard ratio thresholds to flag a safety issue at each
#' trial stage.
#' @param thres2 The hazard ratio thresholds to flag a safety issue and stop the
#' trial. Default is NULL. Leave to default if specific stopping boundaries are
#' not known. Notice that thres2 is expected to be greater to thres1 for all
#' trial stages.
#' @param events A vector with the number of events at the specified analysis
#' stages.
#' @param hrr The range of hazard ratios to calculate the overall probability of
#' flagging a safety concern.
#' @param hrs The specific hazard ratio values to calculate stopping
#' probabilities.
#' @param col (Optional) A character vector with color names for the stopping
#' probabilities plot, must have same length to hrs.
#' @import dplyr ggplot2
#' @return (list): Returns a list with two dataframes and three plots. The
#' dataframes include information on the stopping probabilities based on the
#' scenarios specified by the user and the plots include visualizations of these
#' results.
#' @export
# @examples
# res <- tf_ocs(thres1=c(1.3, 1),
#               thres2=c(1.8, 1.3),
#               events=c(50, 100),
#               hrr=seq(0.6, 1.5, by = 0.01),
#               hrs=1.3)
tf_ocs <- function(thres1, thres2 = NULL, events, hrr, hrs, col = NULL) {

  # Sanity checks
  sanity_checks(thres1, thres2, events, hrr, hrs, col)

  # Default condition if competing risk extracted for event
  if (is.null(col)) col <- generate_colors(length(hrs))

  # Load functions needed to calculate operational characteristics
  source("code/functions/flag_safety.R")

  logthres1 <- log(thres1) # log transform thresholds for continuation
  loghrs <- log(hrs) # log transform hazard ratios to calculate stopping probs

  safflag_probs <- 0

  if (is.null(thres2)) {
    safstop_probs <- sapply(loghrs, FUN = stopprob,
                            logthres = logthres1, events = events)
    safstop_cprobs <-  sapply(loghrs, FUN = stopprobc,
                              logthres = logthres1, events = events)
  } else {
    logthres2 <- log(thres2) # log transform threshold

    # Calculate joint probabilities to stop the trial at each stage
    safstop_probs <- sapply(loghrs, FUN = stopprob,
                            logthres = logthres2, events = events)
    # Calculate joint probabilities to flag a safety issue at each stage but not
    # stop
    safflag_probs <- sapply(loghrs, FUN = flagprob,
                            logthres1 = logthres1, logthres2 = logthres2,
                            events = events)

    # Calculate conditional probabilities to stop the trial
    safstop_cprobs <-  sapply(loghrs, FUN = stopprobc,
                              logthres = logthres2, events = events)
  }

  # Generate dataframe with results
  resstop <- data.frame(stage = rep(seq(1, length(events)), length(loghrs)),
                        prob_stop = c(safstop_probs),
                        flag_prob  = c(safflag_probs),
                        cond_prob_stop = c(safstop_cprobs),
                        true_hr_num = rep(hrs, each = length(events)),
                        true_hr_fac = factor(
                          round(rep(hrs, each = length(events)), 2)
                        ))

  stop_plot <- NULL
  # Plot joint probabilities results
  if (is.null(thres2)) {
    joint_plot <-
      ggplot(resstop, aes(x = stage, col = true_hr_fac)) +
      geom_line(aes(y = prob_stop)) +
      theme_bw() +
      labs(x = "Analysis stage (k)",
           y = "P{Reach stage k and flag a safety issue | HR}",
           col = "HR",
           linetype = "") +
      theme(legend.position = "top") +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_colour_manual(values = col) +
      scale_x_continuous(breaks = seq(1, length(events))) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))

    # Plot conditional probabilities results
    cond_plot <-
      ggplot(resstop, aes(x = stage, col = true_hr_fac)) +
      geom_line(aes(y = cond_prob_stop)) +
      theme_bw() +
      labs(x = "Analysis stage (k)",
           y = "P{Flag a safety issue at stage k | HR,\n not flagged a safety
         issue at stage 1,...,k-1}",
           col = "HR") +
      theme(legend.position = "top") +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_colour_manual(values = col) +
      scale_x_continuous(breaks = seq(1, length(events))) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
  } else {
    joint_plot <-
      ggplot(resstop, aes(x = stage, col = true_hr_fac)) +
      geom_line(aes(y = flag_prob)) +
      theme_bw() +
      labs(x = "Analysis stage (k)",
           y = "P{Reach stage k and flag a safety issue\nbut not stop the trial
           | HR}",
           col = "HR",
           linetype = "") +
      theme(legend.position = "top") +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_colour_manual(values = col) +
      scale_x_continuous(breaks = seq(1, length(events))) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))

    stop_plot <-
      ggplot(resstop, aes(x = stage, col = true_hr_fac)) +
      geom_line(aes(y = prob_stop)) +
      theme_bw() +
      labs(x = "Analysis stage (k)",
           y = "P{Reach stage k and stop the trial | HR}",
           col = "HR",
           linetype = "") +
      theme(legend.position = "top") +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_colour_manual(values = col) +
      scale_x_continuous(breaks = seq(1, length(events))) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))

    # Plot conditional probabilities results
    cond_plot <-
      ggplot(resstop, aes(x = stage, col = true_hr_fac)) +
      geom_line(aes(y = cond_prob_stop)) +
      theme_bw() +
      labs(x = "Analysis stage (k)",
           y = "P{Stop the trial at stage k | HR,\n not flagged a safety
         issue at stage 1,...,k-1}",
           col = "HR") +
      theme(legend.position = "top") +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_colour_manual(values = col) +
      scale_x_continuous(breaks = seq(1, length(events))) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
  }

  # Find overall probabilities to flag a safety issue for the specified HRs
  # within hrs
  oprob_stop <- resstop %>%
    dplyr::group_by(true_hr_num) %>%
    dplyr::summarise(oprob_flag = sum(flag_prob),
                     oprob_stop = sum(prob_stop))


  # Find the overall probability to flag a safety issue/stop the trial for the
  # range of HRs inserted by the user
  loghrr <- log(hrr)
  if (is.null(thres2)) {
    safstop_atleast1 <- 1 - sapply(loghrr, safprob,
                                   logthres = logthres1, events = events)
    resatleast1 <- data.frame(prob_atleast1 = c(safstop_atleast1),
                              true_hr = round(hrr, 2))

    # Create plot with overall probability results
    oprob_plot <-
      ggplot(resatleast1, aes(x = true_hr, y = prob_atleast1)) +
      geom_vline(xintercept = 1, col = "pink", linetype = "dashed") +
      geom_line() +
      theme_bw() +
      labs(y = "P{Flag a safety issue at least once | HR}",
           x = "HR") +
      scale_x_continuous(breaks = seq(min(hrr), max(hrr), by = 0.1)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
  } else {
    safstop_atleast11 <- 1 - sapply(loghrr, safprob,
                                    logthres = logthres1, events = events)
    safstop_atleast12 <- 1 - sapply(loghrr, safprob,
                                    logthres = logthres2, events = events)
    # Probability of not stopping at any stage 1,...,j-1
    claimap <- sapply(loghrr, safprob, logthres = logthres1[-length(logthres1)],
                      events = events[-length(events)])
    resatleast1 <- data.frame(prob_atleast11 = c(safstop_atleast11),
                              prob_atleast12 = c(safstop_atleast12),
                              claimap = c(claimap),
                              true_hr = round(hrr, 2))

    # Create plot with overall probability results
    oprob_plot <-
      ggplot(resatleast1, aes(x = true_hr)) +
      geom_vline(xintercept = 1, color = "pink", linetype = "dashed") +
      geom_line(aes(y = prob_atleast11, color = "A = Flag safety issue at any stage")) +
      geom_line(aes(y = prob_atleast12, color = "A = Stop at any stage")) +
      geom_line(aes(y = claimap, color = "A = Claim accelarated approval")) +
      theme_bw() +
      labs(y = "P{ A | HR}",
           x = "HR",
           col = "") +
      scale_x_continuous(breaks = seq(min(hrr), max(hrr), by = 0.1)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))+
      theme(legend.position = "top")+
      scale_color_manual(values = c("A = Flag safety issue at any stage" = "darkgrey",
                                    "A = Stop at any stage" = "red",
                                    "A = Claim accelarated approval" = "steelblue"))
  }


  return(list(
    resstop = resstop,
    oprob_stop = oprob_stop,
    joint_plot = joint_plot,
    cond_plot = cond_plot,
    oprob_plot = oprob_plot,
    stop_plot = stop_plot
  ))
}

# Perform sanity checks
sanity_checks <- function(thres1, thres2, events, hrr, hrs, col) {

  # Hazard ratios should be > 0
  stopifnot("thres1 must be positive." = all(thres1 > 0))

  if ( ! is.null(thres2)) {
    # Hazard ratios should be > 0
    stopifnot("thres2 must be positive." = all(thres2 > 0))
    # thres2 values should be greater than thres1 for all trial stages
    stopifnot("All thres2 values should be greater than the corresponding thres1
              values." = all(thres2 > thres1))
  }

  # Number of events should be at least 1
  stopifnot("events should be greater than 1." = all(events > 1))

  # Hazard ratios should be > 0
  stopifnot("hrr must be positive." = all(hrr > 0))

  # Hazard ratios should be > 0
  stopifnot("hrs must be positive." = all(hrs > 0))

  # Colors must have the same length to hrs
  if (!is.null(col)) stopifnot("col must have the same length to hrs." =
                                 length(col) == length(hrs))
}


# Color generation function
generate_colors <- grDevices::colorRampPalette(
  colors = c("green4", "grey", "red")
)


#' Probability to stop the trial at each stage (joint probability to stop the trial at
#' stage k and not stop the trial at stage 1 to k-1)
#'
#' Probability to flag a safety issue but not stop the trial = P(threshold to be
#' between two values at stage k & the threshold was less than lower bound at
#' all previous stages)
#'
#' Probability to flag a safety issue at least once
#'
#' Probability tosuggest stopping the trial at least once
#'
#'
