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

## Error triggering
test_that("posterior plot for Binomial: margin is not relevant", {
  testthat::expect_error(plot(posterior, margin = 1))
})

# posterior has extra attributes
z <- plot(posterior, lty = 1:2, lwd = c(2, 2), col = c("purple", "orange"),
          cdf = TRUE)
test_that("posterior: 1 Binomial distribution, 1 prior", {
  testthat::expect_equal(attr(posterior, "prior"), z[2], ignore_attr = TRUE)
})

# No prior and plot cdf
z <- plot(posterior, prior = FALSE, xlim = c(0, 1), ylim = c(0, 9), cdf = TRUE)
test_that("posterior: 1 Binomial distribution, 1 prior, no prior in plot", {
  testthat::expect_equal(posterior, z[1], ignore_attr = TRUE)
})

# No prior, likelihood
z <- plot(posterior, prior = FALSE, likelihood = TRUE)
test_that("posterior: 1 Binomial distribution, 1 prior, no prior in plot", {
  testthat::expect_equal(posterior, z[1], ignore_attr = TRUE)
})

# No prior, no likelihood
z <- plot(posterior, prior = FALSE, likelihood = FALSE)
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

# Plot the marginal prior and posterior distributions for mu and tau
z <- plot(posterior, margin = "mu")
margins <- marginals(posterior)
test_that("posterior: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(margins[[1]], z[1], ignore_attr = TRUE)
})

# Same, with numeric margin
z <- plot(posterior, margin = 1)
margins <- marginals(posterior)
test_that("posterior: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(margins[[1]], z[1], ignore_attr = TRUE)
})

# Plot the 2D contours of the posterior and prior for (mu, tau)
z <- plot(posterior, col = c("red", "blue"), lwd = 1, lty = 1:2,
          xlim = c(-5, 11), ylim = c(0, 5))
test_that("posterior contours: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(z[1], posterior, ignore_attr = TRUE)
})
test_that("prior contours: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(z[2], attr(posterior, "prior"), ignore_attr = TRUE)
})

# Repeat with likelihood in plot but no prior
z <- plot(posterior, prior = FALSE, likelihood = TRUE, col = 1:2)
test_that("posterior contours: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(z[1], posterior, ignore_attr = TRUE)
})

# Repeat with likelihood in plot and with prior
z <- plot(posterior, prior = TRUE, likelihood = TRUE, col = 1:3)
test_that("posterior contours: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(z[1], posterior, ignore_attr = TRUE)
})
test_that("prior contours: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(z[2], attr(posterior, "prior"), ignore_attr = TRUE)
})

# Repeat with no prior and no likeihood
z <- plot(posterior, prior = FALSE, likelihood = FALSE)
test_that("posterior contours: 1 Normal distribution, 1 prior", {
  testthat::expect_equal(z[1], posterior, ignore_attr = TRUE)
})

## Error triggering
test_that("posterior contour plot: margin must be numeric or character", {
  testthat::expect_error(plot(posterior, margin = function(x) x))
})
test_that("posterior contour plot: margin too large", {
  testthat::expect_error(plot(posterior, margin = 1:3))
})
# Wrong name of marginal
test_that("posterior contour plot: margin with incorrect name", {
  testthat::expect_error(plot(posterior, margin = "wrong_name"))
})
