context('test_lrex')

set.seed(12345)
n <- 20
dt <- tibble::tibble(id = 1:n,
                     time = rexp(n),
                     status = rbinom(n, size = 1, prob = 0.2),
                     group = rbinom(n, size = 1, prob = 0.5)
)
# 3 events occurred
# compute log-rank test result
lrdt <- survival::survdiff(survival::Surv(time, status) ~ group, data=dt)

method <- "standard"
events <- sum(dt$status)
n0 <- sum(dt$group==0)
n1 <- sum(dt$group==1)
crit <- lrdt$chisq
res1 <- lrex(method = method,
     events = events,
     n0 = n0,
     n1 = n1,
     crit = crit)

n_perm <- 100
res2 <- lrex(method = "heinze", data = dt, n_perm = n_perm, seed = 12345)

test_that("lrex works", {
  expect_equal(res1, 0.1517028, tolerance=1e-5)
  expect_equal(res2, 0.4, tolerance=1e-5)
})
