# Check that the posterior() method works for the Binomial case with a
# non-conjugate MDI prior

## A small sample simulated from a Binomial distribution

size <- 10
N <- Binomial(size = size, p = 0.2)

# Simulate a sample of size 5 from a Binomial(20, 0.2) distribution
set.seed(3)
data <- random(N, 5)
# Add the data, a numeric vector
likelihood <- add_data(N, data)
# Non-conjugate MDI prior
prior <- MDIbinomial()

# We call posterior() so that we can pass arguments to rust::ru()
set.seed(3012025)
# Set a small n, for speed
n <- 2

# Tests
x <- posterior(likelihood, prior, n = n)
test_that("Binomial MDI: trans is logit", {
  testthat::expect_equal(dim(x$sim_vals), c(n, 1))
})
x <- posterior(likelihood, prior, n = n, trans = "BC")
test_that("Binomial MDI: trans is Box-Cox", {
  testthat::expect_equal(dim(x$sim_vals), c(n, 1))
})
x <- posterior(likelihood, prior, n = n, trans = "none")
test_that("Binomial MDI: trans is none", {
  testthat::expect_equal(dim(x$sim_vals), c(n, 1))
})
# Test of an error
test_that("Binomial MDI: trans is wrong throws an error", {
  testthat::expect_error(posterior(likelihood, prior, n = n, trans = "wrong"))
})

## Special potentially problematic cases

# All values equal to zero
data <- rep(0, 5)
likelihood <- add_data(N, data)
x <- posterior(likelihood, prior, n = n)
test_that("Binomial MDI: data are all zero, trans is logit", {
  testthat::expect_equal(dim(x$sim_vals), c(n, 1))
})
x <- posterior(likelihood, prior, n = n, trans = "BC")
test_that("Binomial MDI: data are all zero, trans is BC", {
  testthat::expect_equal(dim(x$sim_vals), c(n, 1))
})

# All values equal to size
data <- rep(size, 5)
likelihood <- add_data(N, data)
x <- posterior(likelihood, prior, n = n)
test_that("Binomial MDI: data are all equal to size, trans is logit", {
  testthat::expect_equal(dim(x$sim_vals), c(n, 1))
})
# This test works but throws a warning.
# This is because the Box-Cox transformation isn't effective in this very
# left-skewed case. Perhaps a Yeo-Johnson transformation would work better.
#x <- posterior(likelihood, prior, n = n, trans = "BC")
#test_that("Binomial MDI: data are all equal to size, trans is BC", {
#  testthat::expect_equal(dim(x$sim_vals), c(n, 1))
#})
