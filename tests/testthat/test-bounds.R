TOLERANCE <- 1e-5

test_that("Calculation of the TF bounds works", {

  procs <- monitOS::bounds(events=c(15, 30, 50, 69),
                           delta_imax = log(1.33))

  # check for errors in the provided results.
  expect_equal(as.numeric(procs$lhr_con),
               c(0.16650410, -0.02732982, -0.13280877, -0.18672493),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(procs$lhr_null),
               c(1.1786251, 0.6883478, 0.4215528, 0.2851789),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(procs$lhr_alt),
               c(-0.4952863, -0.4952863, -0.4952863, -0.4952863),
               tolerance=TOLERANCE)

})
