context('test_postprob')

set.seed(123456)

beta1 <- c(2,10) # specify Beta prior parameters for the two arms
beta0 <- c(3,5)
n0 <- 9
n1 <- 8
delta <- 1

res <- postprob(shapes1 = beta1,
                shapes0 = beta0,
                events0 = 1,
                events1 = 2,
                n0 = n0,
                n1 = n1,
                delta = delta)

res2 <- postprob(shapes1 = beta1,
                 shapes0 = beta0,
                 events0 = 3,
                 events1 = 4,
                 n0 = n0,
                 n1 = n1,
                 delta = delta)

test_that("postprob works", {
  expect_equal(res, 0.4005, tolerance=1e-5)
  expect_equal(res2, 0.3662, tolerance=1e-5)
})
