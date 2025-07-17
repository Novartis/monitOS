tolerance <- 1e-2

test_that("find_pos calculates correctly for criterion 1", {
  expect_equal(
    find_pos(
      pos_thld = 1.5,
      events = c(100, 200),
      rand_ratio = 1,
      hr_null = 1,
      hr_alt = 1.5,
      which_crit = 1,
      targ = 0.05
    ),
    1.907,
    tolerance = tolerance
  )
})

test_that("find_pos calculates correctly for criterion 2", {
  expect_equal(
    find_pos(
      pos_thld = 1.5,
      events = c(100, 200),
      rand_ratio = 1,
      hr_null = 1,
      hr_alt = 1.5,
      which_crit = 2,
      targ = 0.05
    ),
    0.611,
    tolerance = tolerance
  )
})

test_that("find_pos calculates correctly for criterion 3", {
  expect_equal(
    find_pos(
      pos_thld = 1.5,
      events = c(100, 200),
      rand_ratio = 1,
      hr_null = 1,
      hr_alt = 1.5,
      which_crit = 3,
      targ = 0.05
    ),
    0.928,
    tolerance = tolerance
  )
})

test_that("find_pos calculates correctly for criterion 4", {
  expect_equal(
    find_pos(
      pos_thld = 1.5,
      events = c(100, 200),
      rand_ratio = 1,
      hr_null = 1,
      hr_alt = 1.5,
      which_crit = 4,
      targ = 0.05
    ),
    numeric(0),
    tolerance = tolerance
  )
})
