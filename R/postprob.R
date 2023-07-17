
#' Calculate posterior probabilities for safety monitoring stopping rule
#'
#' @description Function to calculate the posterior probability of observing the
#' specified number of events pattern within a two-arm study given prior
#' knowledge on each arm's event rate distribution.
#'
#' @param method String. Method for calculating the posterior probabilities. Two
#' methods are provided, 'beta' (Beta-Binomial conjugate family) and 'gamma'
#' (Gamma-Poisson conjugate family). Default is set to 'beta'.
#' @param param0,param1 Vectors with two values each. Parameter values for the
#' prior distributions of the control and treatment arms respectively.
#' @param events0,events1 Scalars. Assumed number of events within the control
#' and treatment arms respectively.
#' @param time0,time1 Scalars. Specify only if `method = 'gamma'`, otherwise
#' leave as `NULL`. This is the total subjects' exposure time in the control and
#' treatment arms. Default is `NULL`.
#' @param n0,n1 Scalars. Total number of patients in the control and treatment
#' arms respectively.
#' @param delta Scalar. Assumed risk ratio difference under the null hypothesis.
#' @return Scalar. Returns the posterior probability of observing the specified
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
postprob <- function(method = 'beta', param0, param1, events0, events1, time0,
                     time1, n0, n1, delta) {
  res <- switch(
    method,
    beta = postprobbin(param0, param1, events0, events1,n0, n1, delta),
    gamma = postprobgam(param0, param1, events0, events1, time0, time1, n0, n1,
                        delta)
  )
  return(as.numeric(res))
}


#' Calculate posterior probability based on Beta-Binomial model
#'
#' @description Function to calculate the posterior probability of observing the
#' specified number of events pattern within a two-arm study given prior
#' knowledge on each arm's event rate distribution. Method is based on a
#' Beta-Binomial model.
#'
#' @param param0,param1 Vectors with two values each. Shape parameters for the
#' Beta prior distribution of the control and treatment arms respectively.
#' @param events0,events1 Scalars. Assumed number of events within the control
#' and treatment arms respectively.
#' @param n0,n1 Scalars. Total number of patients in the control and treatment
#' arms respectively.
#' @param delta Scalar. Assumed risk ratio difference under the null hypothesis.
#' @return Scalar. Returns the posterior probability of observing the specified
#' number of events pattern assuming a Beta-Binomial model.
#' @export
#' @examples
#' postrr <- postprobbin(param0 = c(1, 1),
#'                       param1 = c(1, 1),
#'                       events0 = 2,
#'                       events1 = 1,
#'                       n0 = 100,
#'                       n1 = 100,
#'                       delta = 1)
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

  # Compute the posterior probability
  prob <- sum(diff>delta)/length(diff)

  return(prob)
}

#' Calculate posterior probability based on Gamma-Poisson model
#'
#' @description Function to calculate the posterior probability of observing the
#' specified number of events pattern within a two-arm study given prior
#' knowledge on each arm's event rate distribution. Method is based on a
#' Gamma-Poisson model.
#' @param param0,param1 Vectors with two values each. Shape and rate parameters
#' for the Gamma prior distribution of the control and treatment arms
#' respectively.
#' @param events0,events1 Scalars. Assumed number of events within the control
#' and treatment arms respectively.
#' @param time0,time1 Scalars. Total subjects' exposure time in the control and
#' treatment arms respectively.
#' @param n0,n1 Scalars. Total number of patients in the control and treatment
#' arms respectively.
#' @param delta Scalar. Risk ratio difference under the null hypothesis.
#' @return Scalar. Returns the posterior probability of observing the specified
#' number of events pattern assuming a Gamma-Poisson model.
#' @export
#' @examples
#' postrr <- postprobgam(param0 = c(1, 1),
#'                       param1 = c(1, 1),
#'                       time0 = 10,
#'                       time1 = 10,
#'                       events0 = 2,
#'                       events1 = 1,
#'                       n0 = 100,
#'                       n1 = 100,
#'                       delta = 1)
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

  # Compute the posterior probability
  prob <- sum(diff>delta)/length(diff)

  return(prob)
}

# Perform sanity checks
postprob_checks <- function(param0, param1, events0, events1, n0, n1, delta) {

  # Shape and, if applicable, rate parameters for the prior distributions should
  # positive numbers.
  stopifnot("each entry of param0 and param1 must be positive number" =
              ((param0 > 0) & (param1 > 0)))

  # Same number of parameters should be provided for the two prior distributions
  stopifnot("two entries must be provided for param0 and param1" =
              (length(param0) == 2 & length(param0) == 2))

  # Sample size must be larger than number of events
  stopifnot("n0 should be >= events0" = (n0 >= events0))
  stopifnot("n1 should be >= events1" = (n1 >= events1))

  # Number of events and sample size should be 0 or more
  stopifnot("events0 and events1 cannot be negative." =
              ((events0 >= 0) & (events1 >= 0)))
  stopifnot("n0 and n1 cannot be negative." = ((n0 >= 0) & (n1 >= 0)))
}

