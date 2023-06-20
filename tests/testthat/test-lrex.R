context('test_lrex')

set.seed(12345)
n <- 300
dts <- tibble::tibble(id = 1:n,
                      time = rexp(n),
                      status = rbinom(n, size = 1, prob = 0.02),
                      group = rbinom(n, size = 1, prob = 0.5)
)
table(dts$status, dts$group)

lrdt <- survival::survdiff(survival::Surv(time, status) ~ group, data=dts)

res <- lrex(events = sum(dts$status), n0 = sum(dts$group==0),
            n1 = sum(dts$group==1), crit = lrdt$chisq)

test_that("lrex works", {
  expect_equal(res, 0.1922284, tolerance=1e-5)
})
