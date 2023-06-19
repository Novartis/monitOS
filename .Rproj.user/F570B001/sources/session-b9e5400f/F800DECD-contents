#'
#' Given unequal patient follow-up time, it gets the survival data and the
#' number of permutations to be performed and returns the probability that we
#' observe a p-value as extreme as the one we have observed based on the
#' log-rank test of the data.
#'
#' @param data (data.frame) Time to event data must be inserted with status, 0
#' if censored or 1 if dead; time and group columns
#' @param n_perm (integer) The number of permutations
#' @param seed (integer) The seed for the permutations
#' @returns (list) A list with log-rank test results, exact test statistics and
#' p-values.
#' @import survival, tidyverse
#' @example
#' set.seed(12345)
#' n <- 10 # number of patients
#' dt <- tibble(time = rexp(n),
#'                  status = rbinom(10, size = 1, prob = 0.2),
#'                  group = rbinom(10, size = 1, prob = 0.5)
#' )
#' ext <- lrexu(data = dt, n_perm = 100, seed = 12345)
#' ext$logrank_data
#' ext$exact_pvalue
#'
lrexu <- function(data, n_perm, seed){

  lrexu_checks(data, n_perm, seed)

  # get the log-rank statistic from the observed data
  logrank_data = survdiff(Surv(time, status) ~ group, data=data)


  # Implement the the Exact test conditioning on observed follow-up

  # STEP 1: From survival time and survival status, derive follow-up time and
  # follow-up status

  data <- data %>% dplyr::mutate(follow_time = time, follow_status = 1-status)

  # split the data into 2 groups
  data_g0 <- data %>% dplyr::filter(group == 0)
  data_g1 <- data %>% dplyr::filter(group == 1)

  # STEP 2: From the pairs of survival and follow-up time and status, estimate
  # the empirical cumulative distribution functions for death time and potential
  # follow-up time using the Kaplan-Meier method.

  # obtain the KM estimate without considering group information
  KM_survival <- survfit(Surv(time, status) ~ 1, data = data)
  cdf_survival <- 1 - KM_survival$surv
  data$cdf_surv <- 1 - KM_survival$surv[match(data$time, KM_survival$time)]
  if(any(is.na(data$cdf_surv))) {
      for(pos in which(is.na(data$cdf_surv))) {
        data$cdf_surv[pos] <- 1 -
          KM_survival$surv[which.min(abs(data$time[pos] - KM_survival$time))]
      }
  }

  # obtain the KM estimate for follow-up times for the first group
  KM_followup_g0 <- survfit(Surv(follow_time, follow_status) ~ 1,
                            data = data_g0)
  cdf_followup_g0 <- 1 - KM_followup_g0$surv
  data_g0$cdf_followup <- 1 - KM_followup_g0$surv[match(data_g0$time,
                                                        KM_followup_g0$time)]
  if (any(is.na(data_g0$cdf_followup))) {
    for(pos in which(is.na(data_g0$cdf_followup))) {
      data_g0$cdf_followup[pos] <- 1 -
        KM_followup_g0$surv[which.min(abs(data_g0$time[pos] -
                                            KM_followup_g0$time))]
    }
  }

  # obtain the KM estimate for follow-up times for the second group
  KM_followup_g1 <- survfit(Surv(follow_time, follow_status) ~ 1,
                            data = data_g1)
  cdf_followup_g1 <- 1 - KM_followup_g1$surv
  data_g1$cdf_followup <- 1 - KM_followup_g1$surv[match(data_g1$time,
                                                        KM_followup_g1$time)]
  if (any(is.na(data_g1$cdf_followup))) {
    for (pos in which(is.na(data_g1$cdf_followup))) {
      data_g1$cdf_followup[pos] <- 1 -
        KM_followup_g1$surv[which.min(abs(data_g1$time[pos] -
                                            KM_followup_g1$time))]
    }
  }

  # STEP 3: Generate a random permutation of the data pairs by permuting the
  # patient index

  set.seed(seed) # set the seed to generate the permutations
  # create n_perm permutations of the subject ids
  perm <- t(replicate(n_perm, sample(1:nrow(data))))


  # check for duplicated permutations
  if (sum(duplicated(perm)) != 0) {
    # add alternative if there are duplicate permutations
  }

  max_time <- max(data$time) # maximum follow-up time
  sorted_time <- sort(data$time)  # sort survival times
  sorted_followt_g0 <- sort(data_g0$follow_time) # sort follow-up times (group1)
  sorted_followt_g1 <- sort(data_g1$follow_time) # sort follow-up times (group2)

  # initialize vectors for steps 4, 5 and 6
  data$follow_max <- NA # maximum follow-up per group
  data$follow_max[data$group == 0] <- max(data_g0$follow_time)
  data$follow_max[data$group == 1] <- max(data_g1$follow_time)
  data$x_star <- NA
  data$c_star <- NA
  data$y_star <- NA
  data$time_p <- NA
  data$status_p <- NA
  exact_stat <- numeric(length = n_perm)

  pb = txtProgressBar(min = 0, max = n_perm, initial = 0)

  for (rep in 1:n_perm) {

    # generate permuted survival time/status pairs
    data$time_star <- data$time[perm[rep,]]
    data$status_star <- data$status[perm[rep,]]

    # STEP 4: Impute survival and follow-up times for permuted data (before
    # comparing them)

    set.seed(seed)
    # draw random survival time
    data$u <- runif(nrow(data), min = data$cdf_surv, max = 1)
    # draw random follow-up time for the first group
    data_g0$v <- runif(nrow(data_g0), min = data_g0$cdf_followup, max = 1)
    # draw random follow-up time for the second group
    data_g1$v <- runif(nrow(data_g1), min = data_g1$cdf_followup, max = 1)
    data$v <- c(data_g0$v, data_g1$v)

    for (i in 1:nrow(data)) { # for each subject

      # obtain estimates of the partly unobservable death times
      if (data$status_star[i] == 0) {
        if (data$u[i] <= max(cdf_survival)) {
          # partly unobservable death times
          data$x_star[i] <- sorted_time[which(cdf_survival >= data$u[i])[1]]
          data$c_star[i] <- 1
        } else {
          data$x_star[i] <- max_time
          data$c_star[i] <- 0
        }
      } else {
        data$x_star[i] <- data$time_star[i]
        data$c_star[i] <- 1
      }

      # impute follow-up times
      if (data$follow_status[i] == 0) {
        if (data$v[i] <= data$follow_max[i]) {
          data$y_star[i] <- ifelse(data$group[i] == 0,
                                 sorted_followt_g0[which(cdf_followup_g0 >=
                                                           data$v[i])[1]],
                                 sorted_followt_g1[which(cdf_followup_g1 >=
                                                           data$v[i])[1]])
        } else {
          data$y_star[i] <- max_time
        }
      } else {
        data$y_star[i] <- data$follow_time[i]
      }
      # Impute follow-up time using the maximum follow-up time of the combined
      # sample if the random number generated leads to an unknown empirical
      # follow-up distribution value
      data$y_star[i] <- ifelse(is.na(data$y_star[i]), max_time, data$y_star[i])

    }


    # STEP 5: Compare imputed death and follow-up times to obtain new survival
    # times

    for(i in 1:nrow(data)) {

      if(data$x_star[i] < data$y_star[i] |
         ((data$x_star[i] == data$y_star[i]) & (data$c_star[i] == 1))) {
        data$time_p[i] <- data$x_star[i]
        data$status_p[i] <- 1

      } else if(data$x_star[i] > data$y_star[i] |
                ( (data$x_star[i] == data$y_star[i]) & (data$c_star[i] == 0))) {

        data$time_p[i] <- data$y_star[i]
        data$status_p[i] <- 0
      }

    }

    # STEP 6: From the permuted data compute and store the log-rank statistic
    exact_stat[rep] = survdiff(Surv(time_p, status_p) ~ group, data=data)$chisq

    setTxtProgressBar(pb,rep)
  }

  close(pb)

  # Obtain the one-sided p-value by determining the proportion
  # of values that a re more extreme than, or equal to, the observed
  return(list(logrank_data = logrank_data,
              exact_statistics = exact_stat,
              exact_pvalue = sum(exact_stat>=logrank_data$chisq)/length(exact_stat)))

}

# Perform sanity checks
lrexu_checks <- function(data, n_perm, seed){

  # seed and n_perm should be numeric
  stopifnot("seed and n_perm should be numeric" =
              (is.numeric(seed) & isTRUE(is.numeric(n_perm))))

  # data should be tibble
  stopifnot("incorrect data format; data should be a tibble" =
              is_tibble(data))

  # data should include the following variables: time, status, group
  stopifnot("incorrect data format; variable name 'time' should be used to
            report survival times within data" =
              "time" %in% names(data))
  stopifnot("incorrect data format; variable name 'status' should be used to
            report survival status within data" =
              "status" %in% names(data))
  stopifnot("incorrect data format; variable name 'group' should be used to
            report treatment group within data" =
              "group" %in% names(data))
}

