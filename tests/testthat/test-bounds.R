TOLERANCE <- 1e-5

test_that("Calculation of the TF bounds works", {

  res <- bounds(events=c(20, 50, 60),
                power_int=0.9,
                t1error=0.025,
                lhr_null = log(1.333),
                lhr_alt = log(0.9))

  # check for errors in the provided results.
  expect_equal(as.numeric(res$lhr_con),
               c(0.4677668, 0.2571170, -0.2186285),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$lhr_null),
               0.287432,
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$lhr_alt),
               -0.1053605,
               tolerance=TOLERANCE)

})
