#' @export
#' @rdname posterior
posterior.Normal <- function(x, y) {
  # Extract the name of the prior distribution
  prior_distribution <- class(y)[1]
  # If prior is conjugate then infer the posterior distribution
  if (prior_distribution == "NormalGamma") {
    # Extract the parameter values of the Normal prior for mu
    mu0 <- y[[1]]$mu
    nu <- y[[1]]$sigma ^ 2
    # Extract the parameter values of the gamma prior for tau = 1 / sigma ^ 2
    alpha <- y[[2]]$shape
    beta <- y[[2]]$rate
    # Extract the data
    # Use unlist() to force data to be a vector
    data <- unlist(attr(x, "data"))
    # Sample size
    n <- length(data)
    # Find the normal posterior for mu
    mu_mean <- (nu * mu0 + n * mean(data)) / (nu + n)
    mu_sd <- 1 / sqrt(nu + n)
    posterior_mu <- distributions3::Normal(mu = mu_mean, sigma = mu_sd)
    # Find the gamma posterior for tau = 1 / sigma ^ 2
    tau_alpha <- alpha + n / 2
    tau_beta <- beta + (n - 1) * stats::var(data) / 2 +
      (n * nu) * (mean(x) - mu0) ^ 2 / (2 * (nu + n))
    posterior_tau <- distributions3::Gamma(shape = tau_alpha, rate = tau_beta)
    # Return the posterior as a "distribution" object
    z <- posterior_mu * posterior_tau
    # Adjust the class, by removing the second component ("prior")
    class(z) <- class(z)[-2]
  } else {
    stop("A conjugate prior must be used for Normal data")
  }
  # Add the prior(s) as an attribute for later use
  attr(z, "prior") <- y
  # Add the likelihood as an attribute for later use
  attr(z, "likelihood") <- x
  # Store the name of the parameter of interest
  attr(z, "parameter") <- c("mu", "tau")
  # Add "posterior" as the second component of the class
  class(z) <- c(class(z)[1], "posterior", class(z)[2])
  return(z)
}
