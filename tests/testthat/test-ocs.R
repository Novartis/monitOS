TOLERANCE <- 1e-3

test_that("Ensure that OCs are calculated correctly", {
  res <- monitOS::ocs(thres1=c(1.3, 1),
                      thres2=c(1.8, 1.3),
                      events=c(50, 100),
                      hrr=seq(0.6, 1.5, by = 0.01),
                      hrs=1.3,
                      col = NULL)
  expect_equal(res$ocs_trial,
               tibble::tibble(
                 true_hr = 1.3,
                 prob_flag_si = 0.660,
                 prob_stop = 0.508),
               TOLERANCE)
  expect_equal(as.numeric(res$ocs_stage$prob_stop),
               c(0.1249604, 0.3828472),
               TOLERANCE)
  expect_equal(as.numeric(res$ocs_stage$prob_flag_si),
               c(0.3750396, 0.2847024),
               TOLERANCE)
  expect_equal(max(res$ocs_stage$stage),
               2)
})
