#' Function to calculate the posterior probability of observing an inserted
#' number of events pattern for the treatment and control arms given prior
#' knowledge on each arm event rates.
#'
#' @param shapes0 (vector) Shape parameters for the Beta prior distribution for
#' the control arm.
#' @param shapes1 (vector) Shape parameters for the Beta prior distribution for
#' the treatment arm.
#' @param events0 (numeric) Number of events in the control arm.
#' @param events1 (numeric) Number of events in the treatment arm.
#' @param n0 (numeric) Total number of patients in the control arm.
#' @param n1 (numeric) Total number of patients in the treatment arm.
#' @param delta (numeric) Risk ratio difference under the null hypothesis.
#' @return (numeric): Returns the posterior probability of observing the inserted
#' number of events pattern.
#' @export
#' @examples
#' postrr <- postprob(shapes0 = c(1, 1), shapes1 = c(1, 1), events0 = 2, events1 = 1,
#' n0 = 100, n1 = 100, delta = 1)
#' postrr
postprob <- function(shapes0 = c(1, 1), 
                     shapes1 = c(1, 1),
                     events0,
                     events1,
                     n0,
                     n1,
                     delta = 1){
  
  postprob_checks(shapes0, shapes1, events0, events1, n0, n1, delta)
  
  # Randomly sample from the two posteriors
  x1 <- rbeta(10e3, 
                 shape1 = shapes1[1] + events1,
                 shape2 = shapes1[2] + n1 - events1)
  x0 <- rbeta(10e3, 
                 shape1 = shapes0[1] + events0,
                 shape2 = shapes0[2] + n0 - events0)
  
  # Get the risk ratio
  diff <- x1/x0
  
  # Compute the probability that [...]
  prob <- sum(diff>delta)/length(diff)
  
  return(prob)
}

# Perform sanity checks
postprob_checks <- function(shapes0, shapes1, events0, events1, n0, n1, delta) {
  
  # Shape and range parameters for the beta distributions should be > 0 
  stopifnot("each entry of shapes0 and shapes1 must be positive number" = ((shapes0 > 0) & (shapes1 > 0)))
  
  # Same number of parameters should be provided for the prior distributions
  stopifnot("two entries must be provided for shapes0 and shapes1" = (length(shapes0) == 2 & length(shapes0) == 2))

  # Sample size must be larger than number of events
  stopifnot("n0 and n1 should be >= events0 and events1 respectively" = ((n0 >= events0) & (n1 >= events1)))
  
  # Number of events and sample size should be 0 or more
  stopifnot("events0 and events1 cannot be negative." = ((events0 >= 0) & (events1 >= 0)))
  stopifnot("n0 and n1 cannot be negative." = ((n0 >= 0) & (n1 >= 0)))
}
