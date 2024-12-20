# Check that the posterior() method works for the Normall case with a
# conjugate NormalGamma prior

# Special case: n = 1, x = mu0

X <- Normal()
mu <- 10
lambda <- 1
shape <- 1
rate <- 1
prior <- NormalGamma(mu = mu, lambda = lambda, shape = shape, rate = rate,
                     names = c("mu", "tau"))
data <- mu
likelihood <- add_data(X, data)
posterior <- likelihood * prior
correct_posterior <- NormalGamma(mu = data, lambda = lambda + 1,
                                 shape = shape + 1 / 2, rate = rate)
test_that("posterior: 1 Normal distribution, n = 1", {
  testthat::expect_equal(posterior, correct_posterior, ignore_attr = TRUE)
})

# Special case: n = 2, x = c(mu0, mu0)

prior <- NormalGamma(mu = mu, lambda = lambda, shape = shape, rate = rate,
                     names = c("mu", "tau"))
data <- c(mu, mu)
likelihood <- add_data(X, data)
posterior <- likelihood * prior
correct_posterior <- NormalGamma(mu = mean(data), lambda = lambda + 2,
                                 shape = shape + 1, rate = rate)
test_that("posterior: 1 Normal distribution, n = 2", {
  testthat::expect_equal(posterior, correct_posterior, ignore_attr = TRUE)
})

# Special case: n = 3, x = c(mu0 - 1, mu0, mu0 + 1)

prior <- NormalGamma(mu = mu, lambda = lambda, shape = shape, rate = rate,
                     names = c("mu", "tau"))
data <- c(mu - 1, mu, mu + 1)
likelihood <- add_data(X, data)
posterior <- likelihood * prior
correct_posterior <- NormalGamma(mu = mean(data), lambda = lambda + 3,
                                 shape = shape + 3 / 2, rate = rate + 1)
test_that("posterior: 1 Normal distribution, n = 3", {
  testthat::expect_equal(posterior, correct_posterior, ignore_attr = TRUE)
})
