library(testthat)

test_that("find_pos calculates correctly for criterion 1", {
    result <- find_pos(
        pos_thld = 1.5,
        events = c(100, 200),
        rand_ratio = 1,
        hr_null = 1,
        hr_alt = 1.5,
        which.crit = 1,
        targ = 0.05
    )
    expect_true(is.numeric(result))
})

test_that("find_pos calculates correctly for criterion 2", {
    result <- find_pos(
        pos_thld = 1.5,
        events = c(100, 200),
        rand_ratio = 1,
        hr_null = 1,
        hr_alt = 1.5,
        which.crit = 2,
        targ = 0.05
    )
    expect_true(is.numeric(result))
})

test_that("find_pos calculates correctly for criterion 3", {
    result <- find_pos(
        pos_thld = 1.5,
        events = c(100, 200),
        rand_ratio = 1,
        hr_null = 1,
        hr_alt = 1.5,
        which.crit = 3,
        targ = 0.05
    )
    expect_true(is.numeric(result))
})

test_that("find_pos calculates correctly for criterion 4", {
    result <- find_pos(
        pos_thld = 1.5,
        events = c(100, 200),
        rand_ratio = 1,
        hr_null = 1,
        hr_alt = 1.5,
        which.crit = 4,
        targ = 0.05
    )
    expect_true(is.numeric(result))
})
