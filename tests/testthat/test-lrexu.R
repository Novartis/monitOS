context('test_lrexu')

  set.seed(12345)
  n <- 300
  dts <- tibble::tibble(id = 1:n,
                time = rexp(n),
                status = rbinom(n, size = 1, prob = 0.02),
                group = rbinom(n, size = 1, prob = 0.5)
  )

  res <- lrexu(data = dts, n_perm = 100, seed = 12345)

test_that("lrexu works", {
  expect_equal(res$exact_pvalue, 0.49, tolerance=1e-5)
  expect_equal(res$exact_statistics[1], 0.002988614, tolerance=1e-5)
})
