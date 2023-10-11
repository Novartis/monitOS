# Prepared on 14/09/2023 to generate the examples given in
# OS monitoring paper
rm(list=ls())

#library(ggplot2)
set.seed(07092023)

# load plotting parameters
My_Theme = theme(
  axis.title.x = element_text(size = 15),
  axis.text.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  axis.text.y = element_text(size = 15),
  legend.text=element_text(size=12),
  legend.title = element_text(size=12))

# Example 1
events <- c(22, 34)
info <- events/4
se <- sqrt(4/events)
hr_alt_range <- c(0.7, 0.85, 0.95, 1, 4/3, 1.5)
hr_alt <- c(0.7)
hr_null <- 4/3
t1error <- c(0.2)
power_int <- c(0.9)

eg1 <- bounds(events,
              power_int = power_int[1],
              t1error = t1error[1],
              lhr_null = log(hr_null),
              lhr_alt = log(hr_alt[1]))

# evaluate OCs needed for manuscript
# 1. Probability of failing to meet posivity threshold
# under hr_alt_range at IA1
# 2. Probability of failing to meet posivity threshold
# under hr_alt_range at final analysis

# Property 1
miss_positivity_IA <- pnorm(eg1$lhr_con[1], mean=log(hr_alt_range), sd=se[1], lower.tail = FALSE)
# Property 2
miss_positivity_FA <- pnorm(eg1$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = FALSE)
# meet the positivity threshold at the final analysis
meet_positivity_FA <- pnorm(eg1$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = TRUE)

cat("Design 1 \n")
cat("prob we miss positivity threshold at IA under hr = ", hr_alt_range, "\n")
print(round(miss_positivity_IA, 3))
cat("\n")
cat("prob we meet positivity threshold at IA under hr = ", hr_alt_range, "\n")
print(round(1 - miss_positivity_IA, 3))
cat("\n")
cat("prob we miss positivity threshold at FA under hr = ", hr_alt_range, "\n")
print(round(miss_positivity_FA, 3))
cat("\n")
cat("prob we meet the positivity threshold at FA under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_FA, 3))
cat("\n")
stop()

# eg2 <- bounds(events,
#               power_int = power_int[1],
#               t1error = t1error[2],
#               lhr_null = log(hr_null),
#               lhr_alt = log(hr_alt[1]))
#
# # Property 1
# miss_positivity_IA <- pnorm(eg2$lhr_con[1], mean=log(hr_alt_range), sd=se[1], lower.tail = FALSE)
# # Property 2
# miss_positivity_FA <- pnorm(eg2$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = FALSE)
# # meet the positivity threshold at the final analysis
# meet_positivity_FA <- pnorm(eg2$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = TRUE)
# cat("Design 2 \n")
# cat("prob we miss positivity threshold at IA under hr = ", hr_alt_range, "\n")
# print(round(miss_positivity_IA, 3))
# cat("\n")
# cat("prob we meet positivity threshold at IA under hr = ", hr_alt_range, "\n")
# print(round(1 - miss_positivity_IA, 3))
# cat("\n")
# cat("prob we miss positivity threshold at FA under hr = ", hr_alt_range, "\n")
# print(round(miss_positivity_FA, 3))
# cat("\n")
# cat("prob we meet the positivity threshold at FA under hr = ", hr_alt_range, "\n")
# print(round(meet_positivity_FA, 3))
# cat("\n")
#
# eg3 <- bounds(events,
#               power_int = power_int[2],
#               t1error = t1error[3],
#               lhr_null = log(hr_null),
#               lhr_alt = log(hr_alt[2]))
#
# # Property 1
# miss_positivity_IA <- pnorm(eg3$lhr_con[1], mean=log(hr_alt_range), sd=se[1], lower.tail = FALSE)
# # Property 2
# miss_positivity_FA <- pnorm(eg3$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = FALSE)
# # meet the positivity threshold at the final analysis
# meet_positivity_FA <- pnorm(eg3$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = TRUE)
# cat("Design 3 \n")
# cat("prob we miss positivity threshold at IA under hr = ", hr_alt_range, "\n")
# print(round(miss_positivity_IA, 3))
# cat("\n")
# cat("prob we meet positivity threshold at IA under hr = ", hr_alt_range, "\n")
# print(round(1 - miss_positivity_IA, 3))
# cat("\n")
# cat("prob we miss positivity threshold at FA under hr = ", hr_alt_range, "\n")
# print(round(miss_positivity_FA, 3))
# cat("\n")
# cat("prob we meet the positivity threshold at FA under hr = ", hr_alt_range, "\n")
# print(round(meet_positivity_FA, 3))
# cat("\n")


# Motivating example 2

# design 1 trying to rule out 1.333
events <- c(110, 125, 131)
info <- events/4
se <- sqrt(4/events)
hr_alt_range <- c(0.73, 0.85, 1, 4/3, 1.5)
hr_alt <- 0.73
hr_null <- c(4/3)
t1error <- c(0.025)
power_int <- c(0.95)

eg4 <- bounds(events,
              power_int = power_int,
              t1error = t1error,
              lhr_null = log(hr_null),
              lhr_alt = log(hr_alt))

# meet the positivity threshold at IA1 (marketing analysis)
meet_positivity_MA <- pnorm(eg4$lhr_con[1], mean=log(hr_alt_range), sd=se[1], lower.tail = TRUE)
# meet the positivity threshold at FA (final analysis)
meet_positivity_FA <- pnorm(eg4$lhr_con[3], mean=log(hr_alt_range), sd=se[3], lower.tail = TRUE)

cat("Example 2 - design 1 \n")
cat("prob we meet positivity threshold at marketing analysis under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_MA, 3))
cat("\n")
cat("prob we meet positivity threshold at final analysis under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_FA, 3))
cat("\n")


# design 2 trying to rule out 1.333 but under a different information sequence
events <- c(ceiling(0.3*131), ceiling(0.5*131), 131)
info <- events/4
se <- sqrt(4/events)
eg5 <- bounds(events,
              power_int = power_int,
              t1error = t1error,
              lhr_null = log(hr_null[1]),
              lhr_alt = log(hr_alt))


# meet the positivity threshold at IA1
meet_positivity_IA1 <- pnorm(eg5$lhr_con[1], mean=log(hr_alt_range), sd=se[1], lower.tail = TRUE)
# meet the positivity threshold at IA2 (marketing analysis)
meet_positivity_IA2 <- pnorm(eg5$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = TRUE)
# meet the positivity threshold at final OS analysis
meet_positivity_FA <- pnorm(eg5$lhr_con[3], mean=log(hr_alt_range), sd=se[3], lower.tail = TRUE)
cat("Example 2 - design 2 \n")
cat("prob we meet positivity threshold at IA1 under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_IA1, 3))
cat("\n")
cat("prob we meet positivity threshold at IA2 under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_IA2, 3))
cat("\n")
cat("prob we meet positivity threshold at final OS analysis under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_FA, 3))
cat("\n")


# Motivating example 3
events <- c(186, 330, 650)
info <- events/4
se <- sqrt(4/events)
hr_alt_range <- c(0.8, 0.9, 1, 1.2, 1.3)
hr_alt <- 0.9
hr_null <- c(1.2)
t1error <- c(0.025)
power_int <- c(0.95)

eg6 <- bounds(events,
              power_int = power_int,
              t1error = t1error,
              lhr_null = log(hr_null),
              lhr_alt = log(hr_alt))

# meet the positivity threshold at IA2 (marketing analysis)
meet_positivity_MA <- pnorm(eg6$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = TRUE)
cat("Example 3 - design 1 \n")
cat("prob we meet positivity threshold at marketing analysis under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_MA, 3))
cat("\n")

# new version of the POLARIX example
events <- c(60, 89, 110, 131, 178)
info <- events/4
se <- sqrt(4/events)
hr_alt <- 0.8
hr_alt_range <- c(0.8, 0.85, 1.3, 1.5)
hr_null <- c(1.3)
t1error <- c(0.025)
power_int <- c(0.9)

eg7 <- bounds(events,
              power_int = power_int,
              t1error = t1error,
              lhr_null = log(hr_null),
              lhr_alt = log(hr_alt))

# meet the positivity threshold at IA3 (primary analysis)
meet_positivity_IA3 <- pnorm(eg7$lhr_con[3], mean=log(hr_alt_range), sd=se[3], lower.tail = TRUE)
meet_positivity_IA4 <- pnorm(eg7$lhr_con[4], mean=log(hr_alt_range), sd=se[4], lower.tail = TRUE)
meet_positivity_FA <- pnorm(eg7$lhr_con[5], mean=log(hr_alt_range), sd=se[5], lower.tail = TRUE)
cat("POLARIX example - new version \n")
cat("prob we meet positivity threshold at IA3 under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_IA3, 3))
cat("\n")
cat("prob we meet positivity threshold at IA4 under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_IA4, 3))
cat("\n")
cat("prob we meet positivity threshold at FA under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_FA, 3))
cat("\n")

# new example 2 in the manuscript
events <- c(28, 42, 70)
info <- events/4
se <- sqrt(4/events)
hr_alt <- c(0.7, 0.95)
hr_alt_range <- c(0.7, 0.8, 0.95, 1, 1.3)
hr_null <- c(1.3)
t1error <- c(0.1, 0.15)
power_int <- c(0.9, 0.75)

eg8 <- bounds(events,
              power_int = power_int[1],
              t1error = t1error[1],
              lhr_null = log(hr_null),
              lhr_alt = log(hr_alt[1]))

meet_positivity_IA1 <- pnorm(eg8$lhr_con[1], mean=log(hr_alt_range), sd=se[1], lower.tail = TRUE)
meet_positivity_IA2 <- pnorm(eg8$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = TRUE)
meet_positivity_FA <- pnorm(eg8$lhr_con[3], mean=log(hr_alt_range), sd=se[3], lower.tail = TRUE)
cat("Example 2 - new version \n")
cat("prob we meet positivity threshold at IA1 under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_IA1, 3))
cat("\n")
cat("prob we meet positivity threshold at IA2 under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_IA2, 3))
cat("\n")
cat("prob we meet positivity threshold at FA under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_FA, 3))
cat("\n")

# setting the alternative OS HR to be much closer to 1
eg9 <- bounds(events,
              power_int = power_int[2],
              t1error = t1error[2],
              lhr_null = log(hr_null),
              lhr_alt = log(hr_alt[2]))

meet_positivity_IA1 <- pnorm(eg9$lhr_con[1], mean=log(hr_alt_range), sd=se[1], lower.tail = TRUE)
meet_positivity_IA2 <- pnorm(eg9$lhr_con[2], mean=log(hr_alt_range), sd=se[2], lower.tail = TRUE)
meet_positivity_FA <- pnorm(eg9$lhr_con[3], mean=log(hr_alt_range), sd=se[3], lower.tail = TRUE)
cat("Example 2 - with reduced alternative HR \n")
cat("prob we meet positivity threshold at IA1 under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_IA1, 3))
cat("\n")
cat("prob we meet positivity threshold at IA2 under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_IA2, 3))
cat("\n")
cat("prob we meet positivity threshold at FA under hr = ", hr_alt_range, "\n")
print(round(meet_positivity_FA, 3))
cat("\n")



