# Check that the methods for the "LocationScaleT" class are consistent with
# those for the "StudentsT" class in the distributions3 package

mu <- 10
sigma <- 2
X <- LocationScaleT(mu = mu, sigma = sigma, df = 6)
Y <- StudentsT(6)

# Moments
test_that("LocationScaleT vs StudentsT: mean", {
  testthat::expect_equal(mean(X) - mu, mean(Y))
})
test_that("LocationScaleT vs StudentsT: variance", {
  testthat::expect_equal(variance(X) / sigma ^ 2, variance(Y))
})
test_that("LocationScaleT vs StudentsT: skewness", {
  testthat::expect_equal(skewness(X), skewness(Y))
})
test_that("LocationScaleT vs StudentsT: kurtosis", {
  testthat::expect_equal(kurtosis(X), kurtosis(Y))
})

# pdf, log_pdf, cdf, quantile
xvals <- c(-2:2)
pvals <- c(0.05, 0.25, 0.5, 0.75, 0.95)
test_that("LocationScaleT vs StudentsT: pdf", {
  testthat::expect_equal(pdf(X, xvals), pdf(Y, (xvals - mu) / sigma) / sigma)
})
test_that("LocationScaleT vs StudentsT: log_pdf", {
  testthat::expect_equal(log_pdf(X, xvals),
                         log_pdf(Y, (xvals - mu) / sigma) - log(sigma))
})
test_that("LocationScaleT vs StudentsT: cdf", {
  testthat::expect_equal(cdf(X, xvals), cdf(Y, (xvals - mu) / sigma))
})
test_that("LocationScaleT vs StudentsT: quantile", {
  testthat::expect_equal(cdf(X, xvals), cdf(Y, (xvals - mu) / sigma))
})
test_that("LocationScaleT vs StudentsT: quantile", {
  testthat::expect_equal(quantile(X, pvals), sigma * quantile(Y, pvals) + mu)
})

# random
set.seed(42)
randomX <- random(X, 5)
set.seed(42)
randomY <- sigma * random(Y, 5) + mu
test_that("LocationScaleT vs StudentsT: random", {
  testthat::expect_equal(randomX, randomY)
})
test_that("LocationScaleT: random for n = 0", {
  testthat::expect_equal(random(X, 0), numeric(0))
})

# support, is_continuous, is_discrete
test_that("LocationScaleT vs StudentsT: support", {
  testthat::expect_equal(support(X), c(min = -Inf, max = Inf))
})
test_that("LocationScaleT: is_continuous", {
  testthat::expect_equal(is_continuous(X), TRUE)
})
test_that("LocationScaleT: support", {
  testthat::expect_equal(is_discrete(X), FALSE)
})

# errors
test_that("LocationScaleT: sigma < 0 throws an error", {
  testthat::expect_error(LocationScaleT(sigma = -1))
})
test_that("LocationScaleT: df = 0 throws an error", {
  testthat::expect_error(LocationScaleT(df = 0))
})
