# Check that the marginals() method works for the NormalGamma case

mu <- 0
lambda <- 1
shape <- 1
rate <- 1

joint <- NormalGamma(mu = mu, lambda = 1, shape = shape, rate = rate,
                     names = c("mu", "tau"))
marginal_Gamma <- Gamma(shape = joint$shape, rate = joint$rate)
marginal_LocationScaleT <- LocationScaleT(mu = mu,
                                          sigma = rate / (lambda * shape),
                                          df = 2 * shape)
marginal_from_marginals <- marginals(joint)

test_that("marginals: NormalGamma, gamma marginal", {
  testthat::expect_equal(marginal_Gamma, marginal_from_marginals[[2]],
                         ignore_attr = TRUE)
})

test_that("marginals: NormalGamma, LocationScaleT marginal", {
  testthat::expect_equal(marginal_LocationScaleT, marginal_from_marginals[[1]],
                         ignore_attr = TRUE)
})
