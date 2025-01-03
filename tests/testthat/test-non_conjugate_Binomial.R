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
# Just one call with the default n = 1000
x <- posterior(likelihood, prior)
test_that("Binomial MDI: trans is logit", {
  testthat::expect_equal(dim(x$sim_vals), c(1000, 1))
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

## Two Binomial distributions, with sizes 5 and 10, non-conjugate case

# Note: the value of p is only use to simulate example data
M1 <- Binomial(size = 5, p = 0.8)
M2 <- Binomial(size = c(5, 5), p = 0.8)
set.seed(3)
data <- random(M, 3)
likelihood2 <- add_data(M2, data)
data_vec <- unlist(attr(likelihood2, "data"))
likelihood1 <- add_data(M1, data_vec)
prior <- MDIbinomial()

# Check that we get the same results from posterior() if we pass
# (1) 1 Binomial(5, ?) distribution
# (2) 2 Binomial(5, ?) distributions
# using the same data

x1 <- posterior(likelihood1, prior, n = n)
x2 <- posterior(likelihood2, prior, n = n)
test_that("Binomial MDI: 1 Binomial vs 2 Binomials", {
  testthat::expect_equal(x1$box, x2$box)
})


