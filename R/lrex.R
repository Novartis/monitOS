#' Function to perform the exact test for a two-arm trial.
#'
#' @param method String. Method can be either "standard" or "heinze". Default is
#' "standard", calculating the exact test p-value given the data provided. The
#' standard exact test inputs the number of observed events along with
#' the total sample size per arm and the log-rank test statistic value. If
#' "heinze" method is selected, users need to input the time-to-event data and
#' the number of permutations to be performed.
#' @param events Scalar. The total number of observed events.
#' @param n0 Scalar. The sample size in the control group.
#' @param n1 Scalar. Sample size in the treatment group.
#' @param crit Scalar. The observed critical value of the log-rank test.
#' @param data Tibble. Time-to-event data must be inserted with status, 0
#' if censored or 1 if dead; time and group variables.
#' @param n_perm Scalar. The number of permutations to be performed.
#' @param seed Scalar. The seed for the permutations.
#' @export
#' @returns Scalar. The exact test p-value, i.e., Pr(LR < crit | HR) across all
#' permutations where LR is the log-rank test result given the specific
#' permuted data set.
#' @examples
#' set.seed(12345)
#' n <- 20
#' dt <- tibble::tibble(id = 1:n,
#'                      time = rexp(n),
#'                      status = rbinom(n, size = 1, prob = 0.2),
#'                      group = rbinom(n, size = 1, prob = 0.5)
#' )
#' # 3 events occurred
#' # compute log-rank test result
#' lrdt <- survival::survdiff(survival::Surv(time, status) ~ group, data=dt)
#'
#' method <- "standard"
#' events <- sum(dt$status)
#' n0 <- sum(dt$group==0)
#' n1 <- sum(dt$group==1)
#' crit <- lrdt$chisq
#' lrex(method = method,
#'      events = events,
#'      n0 = n0,
#'      n1 = n1,
#'      crit = crit)
#'
#'n_perm <- 100
#'lrex(method = "heinze", data = dt, n_perm = n_perm, seed = 12345)
#'
lrex <- function(method = 'standard', events = NULL, n0 = NULL, n1 = NULL, crit = NULL,
                 data = NULL, n_perm = NULL, seed = 1234) {

  res <- switch(
    method,
    standard = lrexs(events, n0, n1, crit),
    heinze = lrexu(data, n_perm, seed)
  )
  return(as.numeric(res))
}

# lrex_checks <- function(method, events, n0, n1, crit, data, n_perm) {
#
#   if(method == "standard"){
#     # If method = "standard", events, n0, n1, crit should not be NULL
#     stopifnot("For method = 'standard', events, n0, n1 and crit should not be NULL." =
#                 !is.null(events) & !is.null(n0) & !is.null(n1) & !is.null(crit))
#   } else {
#     # If method = "heinze", data, n_perm should not be NULL
#     stopifnot("For method = 'heinze', data and n_perm should not be NULL." =
#                 !is.null(data) & !is.null(n_perm))
#   }
# }


#### SUBFUNCTIONS BELOW ####

#' Calculate the exact test p-value given a true HR scenario and the observed
#' number of events in a two-arm trial.
#'
#' @param events Scalar. The total number of observed events.
#' @param n0 Scalar. The sample size in the control group.
#' @param n1 Scalar. Sample size in the treatment group.
#' @param crit Scalar. The observed critical value of the log-rank test.
#' @returns Scalar. The exact test p-value, i.e., Pr(LR < crit | HR) across all
#' permutations where LR is the log-rank test result given the specific
#' permuted data set.
#' @examples
#' set.seed(12345)
#' n <- 20
#' dt <- tibble::tibble(id = 1:n,
#'                   time = rexp(n),
#'                   status = rbinom(n, size = 1, prob = 0.2),
#'                   group = rbinom(n, size = 1, prob = 0.5)
#' )
#' lrdt <- survival::survdiff(survival::Surv(time, status) ~ group, data=dt)
#'
#' monitOS:::lrexs(events = sum(dt$status), n0 = sum(dt$group==0),
#'      n1 = sum(dt$group==1), crit = lrdt$chisq)
#'
lrexs <- function(events, n0, n1, crit){

  lrexs_checks(events, n0, n1, crit)
  hr <- 1 # under the null HR is assumed to be equal to 1


  # generate cases
  vec <- 2^((events-1):0)
  output <- matrix(0, nrow = 1, ncol = events)
  nump <- 2^events
  for (i in 1:(nump-1)) {
    q <- (i %/% vec) %% 2
    output <- rbind(output, q)
  }

  # initialization
  logrank_num <- 0
  logrank_den <- 0
  prob <- 1
  for (k in 1:events) {
    if (k == 1) {
      atRiskT <- matrix(n1, nrow = nump, ncol = 1)
      atRiskC <- matrix(n0, nrow = nump, ncol = 1)
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
lrexs_checks <- function(events, n0, n1, crit) {

  stopifnot("too large number of events, method = 'standard' currently cannot handle >15 events.\n Switching to method = 'heinze' is recommended." = events <=15)

  # crit must be numeric
  stopifnot("crit must be numeric" = is.numeric(crit))

  # Sample size must be larger than number of events
  stopifnot("total sample size should be >= number of events" = (n0 + n1 >= events))

  # Number of events and sample size should be 0 or more
  stopifnot("events cannot be negative." = (events >= 0))
  stopifnot("n0 and n1 cannot be negative." = ((n0 >= 0) & (n1 >= 0)))
}

#' Given unequal patient follow-up time, it gets the survival data and the
#' number of permutations to be performed and returns the probability that we
#' observe a p-value as extreme as the one we have observed based on the
#' log-rank test of the data.
#'
#' @param data Tibble. Time-to-event data must be inserted with status, 0
#' if censored or 1 if dead; time and group variables.
#' @param n_perm Scalar. The number of permutations to be performed.
#' @param seed Scalar. The seed for the permutations.
#' @returns Scalar. The exact test p-value, i.e., Pr(LR < crit | HR) across all
#' permutations where LR is the log-rank test result given the specific
#' permuted data set.
#' @import survival tidyverse tibble
#' @examples
#' set.seed(12345)
#' n <- 10 # number of patients
#' dt <- tibble::tibble(time = rexp(n),
#'                  status = rbinom(10, size = 1, prob = 0.2),
#'                  group = rbinom(10, size = 1, prob = 0.5)
#' )
#' ext <- monitOS:::lrexu(data = dt, n_perm = 100, seed = 12345)
#'
lrexu <- function(data, n_perm, seed){

  lrexu_checks(data, n_perm, seed)

  # get the log-rank statistic from the observed data
  logrank_data = survival::survdiff(survival::Surv(time, status) ~ group, data=data)


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
  KM_survival <- survival::survfit(survival::Surv(time, status) ~ 1, data = data)
  cdf_survival <- 1 - KM_survival$surv
  data$cdf_surv <- 1 - KM_survival$surv[match(data$time, KM_survival$time)]
  if(any(is.na(data$cdf_surv))) {
    for(pos in which(is.na(data$cdf_surv))) {
      data$cdf_surv[pos] <- 1 -
        KM_survival$surv[which.min(abs(data$time[pos] - KM_survival$time))]
    }
  }

  # obtain the KM estimate for follow-up times for the first group
  KM_followup_g0 <- survival::survfit(survival::Surv(follow_time, follow_status) ~ 1,
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
  KM_followup_g1 <- survival::survfit(survival::Surv(follow_time, follow_status) ~ 1,
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
    exact_stat[rep] = survival::survdiff(survival::Surv(time_p, status_p) ~ group, data=data)$chisq

    setTxtProgressBar(pb,rep)
  }

  close(pb)

  # Obtain the one-sided p-value by determining the proportion
  # of values that a re more extreme than, or equal to, the observed
  # return(list(logrank_data = logrank_data,
  #             exact_statistics = exact_stat,
  #             exact_pvalue = sum(exact_stat>=logrank_data$chisq)/length(exact_stat)))
  return(sum(exact_stat>=logrank_data$chisq)/length(exact_stat))

}

# Perform sanity checks
lrexu_checks <- function(data, n_perm, seed){

  # seed and n_perm should be numeric
  stopifnot("seed and n_perm should be numeric" =
              (is.numeric(seed) & isTRUE(is.numeric(n_perm))))

  # data should be tibble
  stopifnot("incorrect data format; data should be a tibble" =
              tibble::is_tibble(data))

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


