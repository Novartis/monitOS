# Inputs
logthres <- log(c(1.3, 1))
events <- c(50, 100)
loghr <- log(1)
TOLERANCE <- 1e-5

# Tests
test_that("SAFETY works", {
  expect_equal(flag_safety(method='joint', logthres, events, loghr),
               0.4843694,
               TOLERANCE)
  expect_equal(flag_safety(method='cond', logthres, events, loghr),
               0.5884041,
               TOLERANCE)
})

test_that("STOP works", {
  expect_equal(flag_stop(method='joint', logthres, events, loghr),
               c(0.1768081, 0.3388224),
               TOLERANCE)
  expect_equal(flag_stop(method='cond', logthres, events, loghr),
               c(0.1768081, 0.4115959),
               TOLERANCE)
})

test_that("flagprob works", {
  logthres1 <- log(c(1.3, 1))
  logthres2 <- log(c(1.8, 1.3))
  events <- c(50, 100)
  loghr <- log(1)
  flagprob_res <- flagprob(logthres1, logthres2, events, loghr)
  expect_equal(flagprob_res, c(0.1579599, 0.3070579), TOLERANCE)
})



# test_that("safety works", {
#   logthres <- log(c(1.3, 1))
#   events <- c(50, 100)
#   loghr <- log(1)
#   safprob_res <- safprob(logthres, events, loghr)
#
#   expect_equal(safprob_res, 0.4843694, tolerance=1e-5)
# })

# test_that("stopprob works", {
#   logthres <- log(c(1.3, 1))
#   events <- c(50, 100)
#   loghr <- log(1)
#   stopprob_res <- stopprob(logthres, events, loghr)
#
#   expect_equal(stopprob_res, c(0.1768081, 0.3388224), tolerance=1e-5)
# })

# test_that("safprobc works", {
#   logthres <- log(c(1.3, 1))
#   events <- c(50, 100)
#   loghr <- log(1)
#   safprobc_res <- as.numeric(safprobc(logthres, events, loghr))
#
#   expect_equal(safprobc_res, c(0.5884041), tolerance=1e-5)
# })
#
# test_that("stopprobc works", {
#   logthres <- log(c(1.3, 1))
#   events <- c(50, 100)
#   loghr <- log(1)
#   stopprobc_res <- stopprobc(logthres, events, loghr)
#
#   expect_equal(stopprobc_res, c(0.1768081, 0.4115959), tolerance=1e-5)
# })


