test_that("calc_posterior returns correct posterior probabilities", {
  lhr_con <- c(0.2, 0.15, 0.1)
  lhr_null <- 0.25
  events <- c(100, 200, 300)

  result <- calc_posterior(lhr_con, lhr_null, events)

  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))
  # With lhr_con < lhr_null, posterior probability should decrease
  # as events increase (more information → tighter posterior)
  expect_true(result[3] < result[1])
})

test_that("calc_posterior handles single analysis", {
  result <- calc_posterior(
    lhr_con = 0.2, lhr_null = 0.3, events = 100
  )
  expect_length(result, 1)
  expect_true(result >= 0 && result <= 1)
})

test_that("calc_posterior boundary: lhr_con equals lhr_null", {
  result <- calc_posterior(
    lhr_con = 0.25, lhr_null = 0.25, events = 100
  )
  # When estimate equals null, posterior probability should be ~0.5
  expect_equal(result, 0.5, tolerance = 1e-10)
})

test_that("calc_predictive returns correct predictive probabilities", {
  lhr_con <- c(0.2, 0.15, 0.1)
  events <- c(100, 200, 300)

  result <- calc_predictive(lhr_con, events)

  # Should return K-1 values (one for each interim)

  expect_length(result, 2)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("calc_predictive with two analyses returns single value", {
  result <- calc_predictive(
    lhr_con = c(0.2, 0.1), events = c(100, 200)
  )
  expect_length(result, 1)
  expect_true(result >= 0 && result <= 1)
})

test_that("calc_predictive matches bounds output", {
  # Cross-validate: run bounds() and check that its predictive

  # probabilities match a direct call to calc_predictive()
  b <- bounds(
    events = c(60, 89, 110, 131, 178),
    power_int = 0.9,
    falsepos = 0.025,
    hr_null = 1.3,
    hr_alt = 0.8,
    rand_ratio = 1,
    hr_marg_benefit = NULL
  )

  pred <- calc_predictive(b$lhr_pos, c(60, 89, 110, 131, 178))
  # bounds() multiplies by 100 and rounds to 3 decimals
  pred_from_bounds <- b$summary[
    1:4,
    "Predictive probability the OS HR estimate at Final Analysis does not exceed the positivity threshold"
  ]
  expect_equal(round(pred * 100, 3), pred_from_bounds, tolerance = 1e-2)
})
