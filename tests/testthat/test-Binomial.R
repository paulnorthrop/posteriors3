# Check that the posterior() method works for the Binomial case with a
# conjugate Beta prior

library(distributions3)

# One Binomial distribution, 1 prior

# Note: the value of p is irrelevant here
size <- 10
N <- Binomial(size = size)
# Test data
data <- 0:10
to_add_to_alpha <- sum(data)
to_add_to_beta <- size * length(data) - sum(data)
# Add the data, a numeric vector
likelihood <- add_data(N, data)
# Set a conjugate (uniform) prior distribution
alpha <- 1
beta <- 1
prior <- Beta(alpha = alpha, beta = beta)
# Construct the posterior distribution
posterior <- likelihood * prior
correct_posterior <- Beta(alpha + to_add_to_alpha, beta + to_add_to_beta)

# posterior has extra attributes
test_that("posterior: 1 Binomial distribution, 1 prior", {
  testthat::expect_equal(posterior, correct_posterior, ignore_attr = TRUE)
})

# One Binomial distribution, 2 priors

alpha <- c(1, 10)
beta <- c(1, 10)
prior <- Beta(alpha = alpha, beta = beta)
# Determine the posterior distribution
posterior <- likelihood * prior
correct_posterior <- Beta(alpha + to_add_to_alpha, beta + to_add_to_beta)
# posterior has extra attributes
test_that("posterior: 1 Binomial distribution, 1 prior", {
  testthat::expect_equal(posterior, correct_posterior, ignore_attr = TRUE)
})

# Two Binomial distributions, 1 prior

# Note: the value of p is irrelevant here
size <- c(5, 10)
M <- Binomial(size = size)
# Simulate samples of size 8 from
#   a Binomial(5, 0.8) distribution, and
#   a Binomial(10, 0.8) distribution
# Test data
data <- list(1:4, 4:8)
alpha_fn <-  function(x) {
  sum(x)
}
to_add_to_alpha <- sum(sapply(data, FUN = alpha_fn))
beta_fn <- function(i) {
  size[i] * length(data[[i]]) - sum(data[[i]])
}
to_add_to_beta <- sum(sapply(1:2, FUN = beta_fn))
# Add the data, a numeric vector
likelihood <- add_data(M, data)
# Set a conjugate (uniform) prior distribution
alpha <- 1.25
beta <- 1.75
prior <- Beta(alpha = alpha, beta = beta)
# Construct the posterior distribution
posterior <- likelihood * prior
correct_posterior <- Beta(alpha + to_add_to_alpha, beta + to_add_to_beta)

# posterior has extra attributes
test_that("posterior: 2 Binomial distribution, 1 prior", {
  testthat::expect_equal(posterior, correct_posterior, ignore_attr = TRUE)
})
