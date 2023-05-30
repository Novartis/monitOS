
test_that("Ensure that OCs are calculated correctly", {

  res <- tf_ocs(thres1=c(1.3, 1),
                thres2=c(1.8, 1.3),
                events=c(50, 100),
                hrr=seq(0.6, 1.5, by = 0.01),
                hrs=1.3)

  expect_equal(res$oprob_stop,
               tibble(true_hr_num = 1.3, oprob_flag = 0.660, oprob_stop = 0.508),
               tolerance=1e-3)
  expect_equal(res$resstop$prob_stop, c(0.1249604, 0.3828472), tolerance=1e-4)
  expect_equal(res$resstop$flag_prob, c(0.3750396, 0.2847024), tolerance=1e-4)
  expect_equal(res$resstop$cond_prob_stop, c(0.1249604, 0.4375198),
               tolerance=1e-4)
  expect_equal(max(res$resstop$stage), 2)

})
