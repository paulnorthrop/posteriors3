#' @export
#' @rdname posterior
posterior.Normal <- function(x, y) {
  # Extract the name of the prior distribution
  prior_distribution <- class(y)[1]
  # If the prior is a conjugate Normal-Gamma prior for (mu, tau), where mu is
  # the Normal mean and tau = 1 / sigma ^ 2 = 1 / variance is the precision
  # then infer the posterior distribution
  # Extract the data
  # Use unlist() to force the data to be a vector
  data <- unlist(attr(x, "data"))
  # Sample size
  n <- length(data)
  # Calculate the parameters of the Normal-Gamma posterior for (mu, tau)
  # Find the normal posterior for mu
  sample_mean <- mean(data)
  sample_variance <- ifelse(n == 1, 0, stats::var(data))
  if (prior_distribution == "NormalGamma") {
    # Extract the parameter values of the Normal prior for mu
    # We use the parameterisation in the continuous table on the Wikipedia page
    # https://en.wikipedia.org/wiki/Conjugate_prior, that is,
    # (mu, lambda, shape, rate) = (mu0, nu, alpha, beta)
    mu0 <- y$mu
    nu <- y$lambda
    alpha <- y$shape
    beta <- y$rate
    posterior_mu <- (nu * mu0 + n * mean(data)) / (nu + n)
    posterior_lambda <- nu + n
    posterior_shape <- alpha + n / 2
    posterior_rate <- beta + (n - 1) * sample_variance / 2 +
      (n * nu) * (sample_mean - mu0) ^ 2 / (2 * (nu + n))
    # Return the posterior as a "distribution" object
    z <- NormalGamma(mu = posterior_mu, lambda = posterior_lambda,
                     shape = posterior_shape, rate = posterior_rate,
                     names = attr(y, "variable_names"))
    # Add attributes containing the marginal posterior and prior for each of
    # the variables
    variables <- attr(y, "variable_names")
    # Posterior distributions
    attr(z, paste0("posterior for ", variables[1])) <- marginals(z)[1]
    attr(z, paste0("posterior for ", variables[2])) <- marginals(z)[2]
    # Prior distributions
    attr(z, paste0("prior for ", variables[1])) <- marginals(y)[1]
    attr(z, paste0("prior for ", variables[2])) <- marginals(y)[2]
  } else {
    stop("A conjugate prior must be used for Normal data")
  }
  # Add the prior(s) as an attribute for later use
  attr(z, "prior") <- y
  # Add the likelihood as an attribute for later use
  # Exclude the degenerate cases where rate = 0
  if (sample_variance > 0) {
    attr(z, "likelihood") <- NormalGamma(mu = sample_mean, lambda = n,
                                         shape = (n + 1) / 2,
                                         rate = (n - 1) * sample_variance / 2)
    attr(z, paste0("likelihood for ", variables[1])) <-
      marginals(attr(z, "likelihood"))[1]
    attr(z, paste0("likelihood for ", variables[2])) <-
      marginals(attr(z, "likelihood"))[2]
  } else {
    attr(x, "likelihood") <- NA
  }
  # Add "posterior" as the second component of the class
  class(z) <- c(class(z)[1], "posterior", class(z)[2])
  return(z)
}
