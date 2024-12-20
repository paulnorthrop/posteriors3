# Check the NormalGamma() function and its methods

mu <- 0
lambda <- 1
shape <- 2
rate <- 1
X <- NormalGamma(mu = mu, lambda = lambda, shape = shape, rate = rate)

# Mean and variance
test_that("NormalGamma: mean", {
  testthat::expect_equal(mean(X), c(mu, shape / rate), ignore_attr = TRUE)
})
test_that("NormalGamma: variance", {
  testthat::expect_equal(variance(X), c(rate / (lambda * (shape - 1)),
                                        shape / rate ^ 2), ignore_attr = TRUE)
})

# random
set.seed(42)
mu <- 0:1
X2 <- NormalGamma(mu = mu, lambda = lambda, shape = shape, rate = rate)
test_that("NormalGamma: random", {
  testthat::expect_equal(dim(random(X2, 5)), c(2, 5), ignore_attr = TRUE)
})

# pdf and log_pdf
mu <- 10
lambda <- 1
shape <- 1 / 2
rate <- 1
X3 <- NormalGamma(mu = mu, lambda = lambda, shape = shape, rate = rate)
pdf_val <- pdf(X3, matrix(c(10, 1), ncol = 2))
manual_val <- exp(-1) / (pi * sqrt(2))
log_pdf_val <- log_pdf(X3, matrix(c(10, 1), ncol = 2))
test_that("NormalGamma: pdf", {
  testthat::expect_equal(pdf_val, manual_val, ignore_attr = TRUE)
})
test_that("NormalGamma: log_pdf", {
  testthat::expect_equal(log_pdf_val, log(manual_val), ignore_attr = TRUE)
})

# support
infs <- c(-Inf, Inf, -Inf, Inf)
test_that("NormalGamma: support, 1 distribution", {
  testthat::expect_equal(support(X), infs, ignore_attr = TRUE)
})
test_that("NormalGamma: support, 2 distributions", {
  testthat::expect_equal(support(X2), rbind(infs, infs), ignore_attr = TRUE)
})

# is_discrete, is_continuous
test_that("NormalGamma: is_continuous, 1D", {
  testthat::expect_equal(is_continuous(X), TRUE)
})
test_that("NormalGamma: is_discrete, 1D", {
  testthat::expect_equal(is_discrete(X), FALSE)
})
test_that("NormalGamma: is_continuous, 2D", {
  testthat::expect_equal(is_continuous(X2), c(TRUE, TRUE))
})
test_that("NormalGamma: is_discrete, 2D", {
  testthat::expect_equal(is_discrete(X2), c(FALSE, FALSE))
})
