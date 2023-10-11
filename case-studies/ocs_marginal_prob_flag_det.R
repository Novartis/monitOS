# Prepared on 07Sept2023 to generate additional operating characteristics
# not currently supported in the app for the Pluvicto case study
rm(list=ls())

library(ggplot2)
set.seed(07092023)

# load plotting parameters
My_Theme = theme(
  axis.title.x = element_text(size = 15),
  axis.text.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  axis.text.y = element_text(size = 15),
  legend.text=element_text(size=12),
  legend.title = element_text(size=12))

events <- c(134, 223, 297)
info <- events/4
se <- sqrt(4/events)
hr_alt <- c(0.9, 0.95, 1.0)
true_hr <- seq(0.8, 1.3, by=0.05)
npts <- length(true_hr)

for(i in 1:length(hr_alt)){
  # generate the procedure for monitoring OS under the stated value for the
  # alternative OS HR representing an incremental efficacy benefit
  os_monit <- bounds(events,
                     power_int = 0.9,
                     t1error = 0.025,
                     lhr_null = log(1.3),
                     lhr_alt = log(hr_alt[i]))

  # calculate marginal probability of flagging a detrimental effect at IA3 under
  # different true HRs
  flag_detmnt_IA3 <- 1 - pnorm((os_monit$lhr_con[2] - log(true_hr))/se[2])
  flag_detmnt_FA <- 1 - pnorm((os_monit$lhr_con[3] - log(true_hr))/se[3])

  title <- paste0("Operating characteristics of guideline when HR1 = ", hr_alt[i])
  file_nam <- paste0("OCS_hr_alt_", hr_alt[i], ".jpeg")

  marg_prob_flag <- data.frame(stage = c(rep("OS Interim Analysis 3", times= npts), rep("OS Final Analysis", times=npts)),
                               OS_HR = rep(true_hr, times=2),
                               prob_flag = c(flag_detmnt_IA3, flag_detmnt_FA))

  # generating plot of these marginal probabilities
  marginal_prob_flag <- ggplot(marg_prob_flag, aes(x = OS_HR, y = prob_flag, col=stage)) +
    geom_line(size=1) +
    ylim(0, 1) +
    xlab("True OS HR") +
    ylab("Marginal prob to flag potential OS detriment") +
    ggtitle(title) +
    theme(legend.position = "bottom",
          panel.grid.major.x = element_line(size=1, color="grey"),
          panel.grid.major.y = element_line(size=1, color="grey"))

  ggsave(filename = file_nam, marginal_prob_flag, width=8.17, height=4.45, units="in")
}
