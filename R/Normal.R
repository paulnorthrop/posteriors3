#' @export
#' @rdname posterior
posterior.Normal <- function(x, y) {
  # Extract the name of the prior distribution
  prior_distribution <- class(y)[1]
  # If the prior is a conjugate Normal-Gamma prior for (mu, tau), where mu is
  # the Normal mean and tau = 1 / sigma ^ 2 = 1 / variance is the precision
  # then infer the posterior distribution
  if (prior_distribution == "NormalGamma") {
    # Extract the parameter values of the Normal prior for mu
    # We use the parameterisation in the continuous table on the Wikipedia page
    # https://en.wikipedia.org/wiki/Conjugate_prior, that is,
    # (mu, lambda, shape, rate) = (mu0, nu, alpha, beta)
    mu0 <- y$mu
    nu <- y$lambda
    alpha <- y$shape
    beta <- y$rate
    # Extract the data
    # Use unlist() to force the data to be a vector
    data <- unlist(attr(x, "data"))
    # Sample size
    n <- length(data)
    # Calculate the parameters of the Normal-Gamma posterior for (mu, tau)
    # Find the normal posterior for mu
    posterior_mu <- (nu * mu0 + n * mean(data)) / (nu + n)
    posterior_lambda <- nu + n
    posterior_shape <- alpha + n / 2
    posterior_rate <- beta + (n - 1) * stats::var(data) / 2 +
      (n * nu) * (mean(x) - mu0) ^ 2 / (2 * (nu + n))
    # Return the posterior as a "distribution" object
    z <- NormalGamma(mu = posterior_mu, lambda = posterior_lambda,
                     shape = posterior_shape, rate = posterior_rate,
                     names = attr(y, "variable_names"))
  } else {
    stop("A conjugate prior must be used for Normal data")
  }
  # Add the prior(s) as an attribute for later use
  attr(z, "prior") <- y
  # Add the likelihood as an attribute for later use
  attr(z, "likelihood") <- x
  # Add "posterior" as the second component of the class
  class(z) <- c(class(z)[1], "posterior", class(z)[2])
  return(z)
}
