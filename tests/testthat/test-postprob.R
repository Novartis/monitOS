context('test_postprob')

set.seed(123456)

param1 <- c(2,10) # specify Beta prior parameters for the two arms
param0 <- c(3,5)
n0 <- 19
n1 <- 18
delta <- 1

res <- postprob(param1 = param1,
                param0 = param0,
                events0 = 1,
                events1 = 2,
                n0 = n0,
                n1 = n1,
                delta = delta)

res2 <- postprob(param1 = param1,
                 param0 = param0,
                 events0 = 3,
                 events1 = 4,
                 n0 = n0,
                 n1 = n1,
                 delta = delta)

res3 <- postprob(method = "gamma",
                 param1 = param1,
                 param0 = param0,
                 events0 = 3,
                 events1 = 4,
                 time0 = 10,
                 time1 = 10,
                 n0 = n0,
                 n1 = n1,
                 delta = delta)

res4 <- postprob(method = "gamma",
                 param1 = param1,
                 param0 = param0,
                 events0 = 3,
                 events1 = 4,
                 time0 = 10,
                 time1 = 10,
                 n0 = n0,
                 n1 = n1,
                 delta = delta)

test_that("postprob works", {
  expect_equal(res, 0.4395, tolerance=1e-5)
  expect_equal(res2, 0.42, tolerance=1e-5)
  expect_equal(res3, 0.0582, tolerance=1e-5)
  expect_equal(res4, 0.0564, tolerance=1e-5)
})
