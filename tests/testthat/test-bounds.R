TOLERANCE <- 1e-2

test_that("Table #2: POLARIX ", {

  # OS monitoring guideline retrospectively applied to Motivating Example 1 with δnull = 1.3, δalt = 0.80, γFA = 0.025 and βPA = 0.10.
  res <- bounds(events=c(60, 89, 110, 131, 178),
                power_int=0.9,
                t1error=0.025,
                lhr_null = log(1.3),
                lhr_alt = log(0.8))

  # check for errors in the provided results.
  expect_equal(as.numeric(res$summary$Continuation_Thres_HR),  # OS HR threshold for positivity
               c(1.114,  1.050, 1.021, 1.001,  0.969),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$summary$OneSided_t1error),  # One-sided false positive error rate
               c(0.275, 0.157, 0.103, 0.067, 0.025),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$summary$CI_level_monit_null),  # Level of 2-sided CI needed to rule out
               c(45, 69, 79, 87, 95),
               tolerance=1e-1)
  expect_equal(as.numeric(res$summary$Power_Alt),  # Probability of meeting positivity threshold under δalt
               c(0.90, 0.90, 0.90, 0.90, 0.90),
               tolerance=TOLERANCE)

})

test_that("Table#3: Intermediate events", {

  # OS monitoring guideline with δnull = 1.3, δalt = 0.70, γFA = 0.10 and βPA = 0.10.
  res <- bounds(events=c(28, 42, 70),
                power_int=0.9,
                t1error=0.1,
                lhr_null = log(1.3),
                lhr_alt = log(0.7))

  # check for errors in the provided results.
  expect_equal(as.numeric(res$summary$Continuation_Thres_HR),  # OS HR threshold for positivity
               c(1.136, 1.040, 0.957),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$summary$OneSided_t1error),  # One-sided false positive error rate
               c(0.36, 0.23, 0.1),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$summary$CI_level_monit_null),  # Level of 2-sided CI needed to rule out
               c(28, 53, 80),
               tolerance=1e-1)
  expect_equal(as.numeric(res$summary$Power_Alt),  # Probability of meeting positivity threshold under δalt
               c(0.9, 0.9, 0.905),
               tolerance=TOLERANCE)
})


test_that("Table#4: Example 2", {

  # OS monitoring guideline with δnull = 1.3, δalt = 0.95, γFA = 0.15 and βPA = 0.25.
  res <- bounds(events=c(28, 42, 70),
                power_int=0.75,
                t1error=0.15,
                lhr_null = log(1.3),
                lhr_alt = log(0.95))

  # check for errors in the provided results.
  expect_equal(as.numeric(res$summary$Continuation_Thres_HR),  # OS HR threshold for positivity
               c(1.226, 1.170, 1.015),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$summary$OneSided_t1error),  # One-sided false positive error rate
               c(0.438, 0.366, 0.150),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$summary$CI_level_monit_null),  # Level of 2-sided CI needed to rule out
               c(12, 27, 70),
               tolerance=1e-1)
  expect_equal(as.numeric(res$summary$Power_Alt),  # Probability of meeting positivity threshold under δalt
               c(0.750, 0.750, 0.609),
               tolerance=TOLERANCE)
})



test_that("Table#5: Example 2", {

  # OS monitoring guideline applied to Motivating Example 2 with δnull = 1.333, δalt = 0.7, γFA = 0.20 and βPA = 0.1.
  res <- bounds(events=c(22, 34),
                power_int=0.9,
                t1error=0.20,
                lhr_null = log(1.333),
                lhr_alt = log(0.7))

  # check for errors in the provided results.
  expect_equal(as.numeric(res$summary$Continuation_Thres_HR),  # OS HR threshold for positivity
               c(1.209, 0.999),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$summary$OneSided_t1error),  # One-sided false positive error rate
               c(0.409, 0.2),
               tolerance=TOLERANCE)
  expect_equal(as.numeric(res$summary$CI_level_monit_null),  # Level of 2-sided CI needed to rule out
               c(18, 60),
               tolerance=1e-1)
  expect_equal(as.numeric(res$summary$Power_Alt),  # Probability of meeting positivity threshold under δalt
               c(0.9, 0.85),
               tolerance=TOLERANCE)
})
