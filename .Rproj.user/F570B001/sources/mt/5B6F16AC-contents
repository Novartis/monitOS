#' Function to calculate the joint P{Reach stage k and do not declare a safety
#' issue| log-HR = theta}.
#'
#' @param logthres The log hazard ratio thresholds for continuation.
#' @param events A vector with the number of events at analysis stages.
#' @param loghr The true log hazard ratio.
#' @import mvtnorm
#' @return (vector): Returns a vector with the computed probabilities at each
#' analysis stage.
#' @export
#' @examples
#' logthres <- log(c(1.3, 1))
#' events <- c(50, 100)
#' loghr <- log(1)
#' res <- safprob(logthres, events, loghr)
#'
safprob <- function(logthres, events, loghr) {

  nstage <- length(events) # total number of analyses planned
  info <- events / 4 # Fisher's information for log-HR at each analysis

  # Calculate the variance-covariance matrix for the sequence of logHR estimates
  mnloghr <- rep(loghr, times = nstage)
  if (nstage > 1) {
    sigma <- mat.or.vec(nr = nstage, nc = nstage)
    diag(sigma) <- 1 / info
    for (i in 1:(nstage - 1)) {
      for (j in (i + 1):nstage) {
        sigma[i, j] <- 1 / info[j]
        sigma[j, i] <- sigma[i, j]
      }
    }
  } else {
    sigma <- matrix(1 / info)
  }

  sprob <- 0
  # Calculate P{Continue to stage k and do not flag a safety signal; log-HR}
  sprob <- pmvnorm(lower = rep(-Inf, times = nstage),
                   upper = logthres[1:nstage], mean = mnloghr,
                   sigma = sigma, keepAttr = FALSE)

  return(sprob)
}

#' Function to the joint P{Reach stage k and declare a safety issue |
#' log-HR = theta}.
#'
#' @param logthres The log hazard ratio thresholds for continuation.
#' @param events A vector with the number of events at analysis stages.
#' @param loghr The true log hazard ratio.
#' @import mvtnorm
#' @return (vector): Returns a vector with the computed stopping probabilities
#' at each analysis stage.
#' @export
#' @examples
#' logthres <- log(c(1.3, 1))
#' events <- c(50, 100)
#' loghr <- log(1)
#' res <- stopprob(logthres, events, loghr)
#'
stopprob <- function(logthres, events, loghr) {

  nstage <- length(events) # total number of analyses planned
  info <- events / 4 # Fisher's information for log-HR at each analysis

  # Calculate the variance-covariance matrix for the sequence of log-HR
  # estimates
  mnloghr <- rep(loghr, times = nstage)
  sigma <- mat.or.vec(nr = nstage, nc = nstage)
  diag(sigma) <- 1 / info
  for (i in 1:(nstage - 1)) {
    for (j in (i + 1):nstage) {
      sigma[i, j] <- 1 / info[j]
      sigma[j, i] <- sigma[i, j]
    }
  }

  sprob <- vector(mode = "numeric", length = nstage)
  sprob[1] <- pnorm(logthres[1], mean = mnloghr[1], sd = sqrt(sigma[1, 1]),
                    lower.tail = FALSE)
  for (j in 2:nstage) {
    # Calculate P{Continue to stage k and flag a safety signal; log-HR}
    sprob[j] <- pmvnorm(lower = c(rep(-Inf, times = (j - 1)), logthres[j]),
                        upper = c(logthres[1:(j - 1)], Inf),
                        mean = mnloghr[1:j], sigma = sigma[1:j, 1:j],
                        keepAttr = FALSE)
  }

  return(sprob)
}

#' Function to calculate the P{Reach stage k and not declare a safety issue |
#' log-HR = theta, didn't declare a safety issue at any previous stage}.
#'
#' @param logthres The log hazard ratio thresholds for continuation.
#' @param events A vector with the number of events at analysis stages.
#' @param loghr The true log hazard ratio.
#' @import mvtnorm
#' @return (vector): Returns a vector with the conditional probabilities
#' at each analysis stage.
#' @export
#' @examples
#' logthres <- log(c(1.3, 1))
#' events <- c(50, 100)
#' loghr <- log(1)
#' res <- safprobc(logthres, events, loghr)
#'
safprobc <- function(logthres, events, loghr) {

  nstage <- length(events) # total number of analyses planned
  info <- events / 4 # Fisher's information for log-HR at each analysis

  # Calculate the variance-covariance matrix for the sequence of log-HR
  # estimates
  mnloghr <- rep(loghr, times = nstage)
  sigma <- mat.or.vec(nr = nstage, nc = nstage)
  diag(sigma) <- 1 / info
  for (i in 1:(nstage - 1)) {
    for (j in (i + 1):nstage) {
      sigma[i, j] <- 1 / info[j]
      sigma[j, i] <- sigma[i, j]
    }
  }

  jpnum <- 0
  jpnum <- pmvnorm(lower = rep(-Inf, times = nstage),
                   upper = logthres[1:nstage], mean = mnloghr,
                   sigma = sigma, keepAttr = FALSE)

  jpdenom <- 0
  jpdenom <- pmvnorm(lower = rep(-Inf, times = nstage - 1),
                     upper = logthres[1:(nstage - 1)],
                     mean = mnloghr[1:(nstage - 1)],
                     sigma = sigma[1:(nstage - 1), 1:(nstage - 1)],
                     keepAttr = FALSE)

  return(jpnum / jpdenom)
}

#' Function to calculate the P{Reach stage k and declare a safety issue |
#' log-HR = theta, didn't declare a safety issue at any previous stage}.
#'
#' @param logthres The log hazard ratio thresholds for continuation.
#' @param events A vector with the number of events at analysis stages.
#' @param loghr The true log hazard ratio.
#' @import mvtnorm
#' @return (vector): Returns a vector with the computed conditional
#' probabilities at each analysis stage.
#' @export
#' @examples
#' logthres <- log(c(1.3, 1))
#' events <- c(50, 100)
#' loghr <- log(1)
#' res <- stopprobc(logthres, events, loghr)
#'
stopprobc <- function(logthres, events, loghr) {

  nstage <- length(events) # total number of analyses planned
  info <- events / 4 # Fisher's information for log-HR at each analysis

  # Calculate the variance-covariance matrix for the sequence of logHR estimates
  mnloghr <- rep(loghr, times = nstage)
  sigma <- mat.or.vec(nr = nstage, nc = nstage)
  diag(sigma) <- 1 / info
  for (i in 1:(nstage - 1)) {
    for (j in (i + 1):nstage) {
      sigma[i, j] <- 1 / info[j]
      sigma[j, i] <- sigma[i, j]
    }
  }

  numer <- denom <- sprob <- vector(mode = "numeric", length = nstage)
  sprob[1] <- pnorm(logthres[1], mean = mnloghr[1], sd = sqrt(sigma[1, 1]),
                    lower.tail = FALSE)
  for (j in 2:nstage) {
    # Calculate P{Continue to stage k and not flag a safety signal; log-HR}
    numer[j] <- pmvnorm(lower = rep(-Inf, times = j),
                        upper = logthres[1:j],
                        mean = mnloghr[1:j],
                        sigma = sigma[1:j, 1:j],
                        keepAttr = FALSE)

    denom[j] <- pmvnorm(lower = rep(-Inf, times = j - 1),
                        upper = logthres[1:(j - 1)],
                        mean = mnloghr[1:(j - 1)],
                        sigma = sigma[1:(j - 1), 1:(j - 1)],
                        keepAttr = FALSE)
    sprob[j] <- 1 - numer[j] / denom[j]
  }

  return(sprob)
}

#' Function to the joint P{Reach stage k and declare a safety issue but not
#' stop the trial| log-HR = theta}.
#'
#' @param logthres1 The log hazard ratio thresholds for flagging a safety issue.
#' @param logthres2 The log hazard ratio thresholds for stopping the trial.
#' @param events A vector with the number of events at analysis stages.
#' @param loghr The true log hazard ratio.
#' @import mvtnorm
#' @return (vector): Returns a vector with the computed stopping probabilities
#' at each analysis stage.
#' @export
#' @examples
#' logthres1 <- log(c(1.3, 1))
#' logthres2 <- log(c(1.8, 1.3))
#' events <- c(50, 100)
#' loghr <- log(1)
#' res <- flagprob(logthres1, logthres2, events, loghr)
#'
flagprob <- function(logthres1, logthres2, events, loghr = log(1)) {

  nstage <- length(events) # total number of analyses planned
  info <- events / 4 # Fisher's information for log-HR at each analysis

  # Calculate the variance-covariance matrix for the sequence of log-HR
  # estimates
  mnloghr <- rep(loghr, times = nstage)
  sigma <- mat.or.vec(nr = nstage, nc = nstage)
  diag(sigma) <- 1 / info
  for (i in 1:(nstage - 1)) {
    for (j in (i + 1):nstage) {
      sigma[i, j] <- 1 / info[j]
      sigma[j, i] <- sigma[i, j]
    }
  }

  sprob <- vector(mode = "numeric", length = nstage)
  sprob[1] <- pnorm(logthres2[1], mean = mnloghr[1], sd = sqrt(sigma[1, 1])) -
    pnorm(logthres1[1], mean = mnloghr[1], sd = sqrt(sigma[1, 1]))

  for (j in 2:nstage) {
    # Calculate P{Continue to stage k and flag a safety signal but not suggest
    # stopping the trial; log-HR}
    sprob[j] <- pmvnorm(lower = c(rep(-Inf, times = (j - 1)), logthres1[j]),
                        upper = c(logthres1[1:(j - 1)], logthres2[j]),
                        mean = mnloghr[1:j], sigma = sigma[1:j, 1:j],
                        keepAttr = FALSE)
  }

  return(sprob)
}
