context('test_flag')

logthres <- log(c(1.3, 1))
events <- c(50, 100)
loghr <- log(1)
res <- safprob(logthres, events, loghr)

test_that("flag works", {
  expect_equal(res, 0.4843694, tolerance=1e-5)
})
