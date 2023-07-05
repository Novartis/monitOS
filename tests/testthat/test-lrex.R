context('test_lrex')

set.seed(12345)
n <- 20
dt <- tibble::tibble(id = 1:n,
                     time = rexp(n),
                     status = rbinom(n, size = 1, prob = 0.2),
                     group = rbinom(n, size = 1, prob = 0.5)
)

n_perm <- 100
res1 <- lrex(method = "standard", data = dt, n_perm = n_perm, seed = 12345)
res2 <- lrex(method = "heinze", data = dt, n_perm = n_perm, seed = 12345)

test_that("lrex works", {
  expect_equal(res1, 0.1875, tolerance=1e-5)
  expect_equal(res2, 0.4, tolerance=1e-5)
})
