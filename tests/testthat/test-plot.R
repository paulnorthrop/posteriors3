# Check the object returned from the plot() method

# One Binomial distribution, 1 prior

# Note: the value of p is irrelevant here
size <- 10
N <- Binomial(size = size)
# Test data
data <- 0:10
# Add the data, a numeric vector
likelihood <- add_data(N, data)
# Set a conjugate (uniform) prior distribution
alpha <- 1
beta <- 1
prior <- Beta(alpha = alpha, beta = beta)
# Construct the posterior distribution
posterior <- likelihood * prior
z <- plot(posterior)

# posterior has extra attributes
test_that("posterior: 1 Binomial distribution, 1 prior", {
  testthat::expect_equal(attr(posterior, "prior"), z[2], ignore_attr = TRUE)
})
