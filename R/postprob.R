
#' Function to calculate the posterior probability of observing an inserted
#' number of events pattern for the treatment and control arms given prior
#' knowledge on each arm event rates  (Beta-Binomial model).
#'
#' @param method String. Method for calculating the posterior probabilities. Two
#' methods are provided, 'beta' (Beta-Binomial conjugate family) and 'gamma'
#' (Gamma-Poisson conjugate family).
#' @param param0 Vector. Shape parameters for the Beta prior distribution for
#' the control arm.
#' @param param1 Vector. Shape parameters for the Beta prior distribution for
#' the treatment arm.
#' @param events0 Scalar. Number of events in the control arm.
#' @param events1 Scalar. Number of events in the treatment arm.
#' @param time0 Scalar. Total subject's exposure time up to t in the control arm.
#' Default is NULL. Leave as NULL if method is Beta.
#' @param time1 Scalar. Total subject's exposure time up to t in the treatment arm.
#' Default is NULL. Leave as NULL if method is Beta.
#' @param n0 Scalar. Total number of patients in the control arm.
#' @param n1 Scalar. Total number of patients in the treatment arm.
#' @param delta Scalar. Risk ratio difference under the null hypothesis.
#' @return Scalar. Returns the posterior probability of observing the inserted
#' number of events pattern.
#' @export
#' @examples
#' postrr <- postprob(param0 = c(1, 1), param1 = c(1, 1),
#' events0 = 2, events1 = 1, n0 = 100, n1 = 100, delta = 1, method = 'beta')
#' postrr
#'
#' postrr <- postprob(param0 = c(1, 1), param1 = c(1, 1), time0 = 10, time1 = 1,
#' events0 = 2, events1 = 1, n0 = 100, n1 = 100, delta = 1, method = 'gamma')
#' postrr
postprob <- function(method = 'beta', param0, param1, events0, events1, time0, time1, n0, n1, delta) {
  res <- switch(
    method,
    beta = postprobbin(param0, param1, events0, events1,n0, n1, delta),
    gamma = postprobgam(param0, param1, events0, events1, time0, time1, n0, n1, delta)
  )
  return(as.numeric(res))
}


#' Function to calculate the posterior probability of observing an inserted
#' number of events pattern for the treatment and control arms given prior
#' knowledge on each arm event rates  (Beta-Binomial model).
#'
#' @param param0 Vector. Shape parameters for the Beta prior distribution for
#' the control arm.
#' @param param1 Vector. Shape parameters for the Beta prior distribution for
#' the treatment arm.
#' @param events0 Scalar. Number of events in the control arm.
#' @param events1 Scalar. Number of events in the treatment arm.
#' @param n0 Scalar. Total number of patients in the control arm.
#' @param n1 Scalar. Total number of patients in the treatment arm.
#' @param delta Scalar. Risk ratio difference under the null hypothesis.
#' @return Scalar. Returns the posterior probability of observing the inserted
#' number of events pattern.
#' @export
#' @examples
#' postrr <- postprobbin(param0 = c(1, 1), param1 = c(1, 1), events0 = 2, events1 = 1,
#' n0 = 100, n1 = 100, delta = 1)
#' postrr
postprobbin <- function(param0 = c(1, 1),
                     param1 = c(1, 1),
                     events0,
                     events1,
                     n0,
                     n1,
                     delta = 1){

  postprob_checks(param0, param1, events0, events1, n0, n1, delta)

  # Randomly sample from the two posteriors
  x1 <- rbeta(10e3,
                 shape1 = param1[1] + events1,
                 shape2 = param1[2] + n1 - events1)
  x0 <- rbeta(10e3,
                 shape1 = param0[1] + events0,
                 shape2 = param0[2] + n0 - events0)

  # Get the risk ratio
  diff <- x1/x0

  # Compute the probability that [...]
  prob <- sum(diff>delta)/length(diff)

  return(prob)
}

#' Function to calculate the posterior probability of observing an inserted
#' number of events pattern for the treatment and control arms given prior
#' knowledge on each arm event rates (Gamma-Poisson model).
#'
#' @param param0 Vector. Shape parameters for the Beta prior distribution for
#' the control arm.
#' @param param1 Vector. Shape parameters for the Beta prior distribution for
#' the treatment arm.
#' @param events0 Scalar. Number of events in the control arm.
#' @param events1 Scalar. Number of events in the treatment arm.
#' @param time0 Scalar. Total subject's exposure time up to t in the control arm.
#' @param time1 Scalar. Total subject's exposure time up to t in the treatment arm.
#' @param n0 Scalar. Total number of patients in the control arm.
#' @param n1 Scalar. Total number of patients in the treatment arm.
#' @param delta Scalar. Risk ratio difference under the null hypothesis.
#' @return Scalar. Returns the posterior probability of observing the inserted
#' number of events pattern.
#' @export
#' @examples
#' postrr <- postprobgam(param0 = c(1, 1), param1 = c(1, 1), time0 = 10, time1 = 10, events0 = 2, events1 = 1,
#' n0 = 100, n1 = 100, delta = 1)
#' postrr
postprobgam <- function(param0 = c(1, 1),
                     param1 = c(1, 1),
                     time0,
                     time1,
                     events0,
                     events1,
                     n0,
                     n1,
                     delta = 1){

  postprob_checks(param0, param1, events0, events1, n0, n1, delta)

  # Randomly sample from the two posteriors
  x1 <- rgamma(10e3,
              shape = param1[1] + events1,
              rate = param1[2] + time1)
  x0 <- rgamma(10e3,
               shape = param0[1] + events0,
               rate = param0[2] + time0)

  # Get the risk ratio
  diff <- x1/x0

  # Compute the probability that [...]
  prob <- sum(diff>delta)/length(diff)

  return(prob)
}

# Perform sanity checks
postprob_checks <- function(param0, param1, events0, events1, n0, n1, delta) {

  # Shape and range parameters for the beta distributions should be > 0
  stopifnot("each entry of param0 and param1 must be positive number" = ((param0 > 0) & (param1 > 0)))

  # Same number of parameters should be provided for the prior distributions
  stopifnot("two entries must be provided for param0 and param1" = (length(param0) == 2 & length(param0) == 2))

  # Sample size must be larger than number of events
  stopifnot("n0 should be >= events0" = (n0 >= events0))
  stopifnot("n1 should be >= events1" = (n1 >= events1))

  # Number of events and sample size should be 0 or more
  stopifnot("events0 and events1 cannot be negative." = ((events0 >= 0) & (events1 >= 0)))
  stopifnot("n0 and n1 cannot be negative." = ((n0 >= 0) & (n1 >= 0)))
}

