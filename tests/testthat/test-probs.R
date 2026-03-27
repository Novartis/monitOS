test_that("meeting_probs returns correct probabilities", {
  b <- bounds(
    events = c(60, 89, 110, 131, 178),
    power_int = 0.9,
    falsepos = 0.025,
    hr_null = 1.3,
    hr_alt = 0.8,
    rand_ratio = 1,
    hr_marg_benefit = NULL
  )

  result <- meeting_probs(
    summary = b$summary,
    lhr_pos = b$lhr_pos,
    lhr_target = log(0.95),
    rand_ratio = 1
  )

  expect_length(result, 5)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("meeting_probs with target HR = 1 (no benefit)", {
  b <- bounds(
    events = c(100, 200),
    power_int = 0.9,
    falsepos = 0.025,
    hr_null = 1.3,
    hr_alt = 0.8,
    rand_ratio = 1,
    hr_marg_benefit = NULL
  )

  # Under no treatment effect (HR = 1), probability of meeting
  # a positivity threshold < 1 should be moderate
  result <- meeting_probs(
    summary = b$summary,
    lhr_pos = b$lhr_pos,
    lhr_target = log(1),
    rand_ratio = 1
  )

  expect_length(result, 2)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("meeting_probs integrates with bounds via hr_marg_benefit", {
  # When hr_marg_benefit is provided, bounds() calls meeting_probs
  # internally — verify the column is present and reasonable
  b <- bounds(
    events = c(60, 89, 110, 131, 178),
    power_int = 0.9,
    falsepos = 0.025,
    hr_null = 1.3,
    hr_alt = 0.8,
    rand_ratio = 2,
    hr_marg_benefit = 0.95
  )

  col <- "Probability of meeting positivity threshold under incremental benefit"
  expect_true(col %in% names(b$summary))
  expect_length(b$summary[[col]], 5)
  expect_true(all(b$summary[[col]] >= 0 & b$summary[[col]] <= 1))
})

test_that("meeting_probs with rand_ratio > 1", {
  b <- bounds(
    events = c(100, 200),
    power_int = 0.9,
    falsepos = 0.025,
    hr_null = 1.3,
    hr_alt = 0.8,
    rand_ratio = 2,
    hr_marg_benefit = NULL
  )

  result <- meeting_probs(
    summary = b$summary,
    lhr_pos = b$lhr_pos,
    lhr_target = log(0.9),
    rand_ratio = 2
  )

  expect_length(result, 2)
  expect_true(all(result >= 0 & result <= 1))
})
