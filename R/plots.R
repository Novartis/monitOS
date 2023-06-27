#' Title
#'
#' @param logthres1 (Scalar | Vector). The log hazard ratio thresholds of continuation
#' to flag a safety issue at each trial stage.
#' @param logthres2 (Scalar | Vector).The log hazard ratio thresholds  of continuation
#' to flag a safety issue and stop the trial. Default is NULL. Leave to default,
#' if specific stopping boundaries are not known. Note that thres2 is expected
#' to be greater to thres1 for all trial stages.
#' @param hrr Vector. The range of true hazard ratio values to plot the
#' probability of flagging a safety concern at least once across all trial
#' stages. If thres2 is not NULL, the hrr hazard ratio range is use to plot the
#' probability to flag a safety issue at least once, suggest stopping the trial
#' at least once (i.e., exceed the thres2 bounds) and claim accelerated approval
#' (i.e., not exceed thres1/flagged a safety issue at any stage before the final
#' analysis) during the course of the trial.
#' @param hrs (Scalar | Vector). The specific true hazard ratio values to
#' calculate and return the joint/conditional probabilities and the overall
#' probability of flagging a safety concern at least once across all trial
#' stages. If thres2 is not NULL, the probability to stop at least once during
#' the trial course and the probability to flag a safety issue but not suggest
#' stopping the trial are calculated instead given these true hazard ratios.
#' @param method  String. Either "joint" for operational characteristics
#' calculated based on joint probabilities, i.e., P(Reach analysis stage k and
#' flag a safety issue) or "cond" for operational characteristics calculated
#' based on conditional probabilities, i.e., P(Flag a safety issue at stage k|
#' have not flagged a safety issue at any precedent analysis 1, ..., k-1).
#' Method "joint" is appropriate to explore at analysis stage, whereas "cond"
#' may be explored when the trial is actually in progress.
#' @param ocs_stage Tibble. Table with operational characteristics' results
#' calculated for each trial stage.
#' @param ocs_trial Tibble. Table with overall trial operational characteristics'
#' results based on the given hazard ratio range hrr.
#' @param ocs_iafa Tibble. Table with operational characteristics for the first
#' and final analysis based on the given hazard ratio range hrr.
#' @param col (Optional) Vector. A vector of color names for the plots. Must
#' have same length to hrs.
#' @import ggplot2
#' @return List of figures.
#' @export
plot_ocs <- function(logthres1,
                     logthres2 = NULL,
                     method = 'joint',
                     hrr = seq(0.6, 1.5, by = 0.01),
                     hrs = 1.3,
                     ocs_stage,
                     ocs_trial,
                     ocs_iafa,
                     col = NULL) {

  # Default condition if competing risk extracted for event
  if (is.null(col)) {
    theme <- colorRampPalette(colors = c("green4", "grey", "red"))
    col <- theme(length(hrs))
  }

  # Log scale
  loghrr <- log(hrr)
  loghrs <- log(hrs)

  # Get events
  events <- sort(unique(ocs_stage$numev))

  # Setup label
  label <-
    ifelse(is.null(logthres2),
           ifelse(method == 'joint',
                  "P{Reach stage k and flag a safety issue | HR}",
                  "P{Flag a safety issue at stage k | HR,\n not flagged a safety issue at stage 1,...,k-1}"),
           ifelse(method == 'joint',
                  "P{Reach stage k and flag a safety issue\nbut not stop the trial | HR}",
                  "P{Stop the trial at stage k | HR,\n not flagged a safety issue at stage 1,...,k-1}"))

  # Plot probability to flag a safety issue at each trial stage
  prob_plot <-
    ggplot(ocs_stage, aes(x = stage, col = true_hr_fac)) +
    geom_line(aes(y = prob_stop)) +
    theme_bw() +
    labs(
      # title = method,
      x = "Analysis stage (k)",
      y = label,
      col = "HR",
      linetype = ""
    ) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_colour_manual(values = col) +
    scale_x_continuous(breaks = seq(1, length(events))) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    ggplot2::theme(legend.position="top")


  # Plot the overall probability to flag a safety issue/stop the trial for the
  # range of HRs inserted by the user
  oprob_plot <- if(is.null(logthres2)) {
    ggplot(ocs_trial, aes(x = true_hr, y = prob_atleast1)) +
      geom_vline(xintercept = 1,
                 col = "pink",
                 linetype = "dashed") +
      geom_line() +
      theme_bw() +
      labs(y = "P{Flag a safety issue at least once | HR}",
           x = "HR") +
      scale_x_continuous(breaks = seq(min(hrr), max(hrr), by = 0.1)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
  } else {
    ggplot(ocs_trial, aes(x = true_hr)) +
      geom_vline(xintercept = 1,
                 color = "pink",
                 linetype = "dashed") +
      geom_line(aes(y = prob_atleast11, color = "A = Flag safety issue at any stage")) +
      geom_line(aes(y = prob_atleast12, color = "A = Stop at any stage")) +
      geom_line(aes(y = claimap, color = "A = Claim accelarated approval")) +
      theme_bw() +
      ggplot2::theme(legend.position = "top") +
      labs(y = "P{ A | HR}",
           x = "HR",
           col = "") +
      scale_x_continuous(breaks = seq(min(hrr), max(hrr), by = 0.1)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      scale_color_manual(
        values = c(
          "A = Flag safety issue at any stage" = "darkgrey",
          "A = Stop at any stage" = "red",
          "A = Claim accelarated approval" = "steelblue"
        )
      )
  }

  flplot <-
    ggplot(data = ocs_iafa,
           aes(x = true_hr, y = flag_si, linetype = analysis)) +
    geom_vline(xintercept = 1, col = "pink", linetype = "dotted") +
    geom_line() +
    theme_bw() +
    ggplot2::theme(legend.position = "top")+
    labs(y = "P{Flag an OS detriment | HR}",
         x = "True HR",
         linetype = "") +
    scale_x_continuous(breaks = seq(min(hrr), max(hrr), by = 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))


  return(list(
    prob_plot = prob_plot,
    oprob_plot = oprob_plot,
    flplot = flplot
  ))

}
