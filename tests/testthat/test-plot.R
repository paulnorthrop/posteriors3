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

# posterior has extra attributes
z <- plot(posterior)
test_that("posterior: 1 Binomial distribution, 1 prior", {
  testthat::expect_equal(attr(posterior, "prior"), z[2], ignore_attr = TRUE)
})

# posterior has extra attributes
z <- plot(posterior, prior = FALSE)
test_that("posterior: 1 Binomial distribution, 1 prior, no prior in plot", {
  testthat::expect_equal(posterior, z[1], ignore_attr = TRUE)
})

# One Normal distribution, 1 prior

X <- Normal(mu = 10, sigma = 2)

# Simulate a sample of size 15 from a Normal(10, 2^2) distribution
set.seed(3)
data <- random(X, 15)
# Add the data, a numeric vector
likelihood <- add_data(X, data)

# Set a Normal-Gamma prior for (mean mu, precision tau)
prior <- NormalGamma(names = c("mu", "tau"))
posterior <- likelihood * prior
posterior

# Plot the marginal prior and posterior distributions for mu and tau
z <- plot(posterior, margin = "mu")
margins <- marginals(posterior)
test_that("posterior: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(margins[[1]], z[1], ignore_attr = TRUE)
})

# Plot the 2D contours of the posterior and prior for (mu, tau)
z <- plot(posterior)
test_that("posterior contours: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(z[1], posterior, ignore_attr = TRUE)
})
test_that("prior contours: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(z[2], attr(posterior, "prior"), ignore_attr = TRUE)
})

