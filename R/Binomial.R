#' @export
#' @rdname Bayesian
posterior.Binomial <- function(x, y) {
  # Check that the likelihood object x has appropriate data as an attribute
  check_data(x)
  # Extract the name of the prior distribution
  prior_distribution <- class(y)[1]
  # If prior is conjugate then infer the posterior distribution
  if (prior_distribution == "Beta") {
    # Extract the parameter values of the Beta prior
    prior_alpha <- y$alpha
    prior_beta <- y$beta
    # Extract the data and the Binomial size parameter
    data <- attr(x, "data")
    # data is a list of length length(x$size) = length(x$p)
    size <- x$size
    sum_data <- sum(unlist(data))
    f <- function(x, y) x - y
    size_minus_data <- mapply(f, as.list(size), data)
    sum_size_minus_data <- sum(unlist(size_minus_data))
    # Calculate the parameters of the Beta posterior
    posterior_alpha = prior_alpha + sum_data
    posterior_beta = prior_beta + sum_size_minus_data
    # Return the Beta posterior as a "distribution" object
    z <- Beta(alpha = posterior_alpha, beta = posterior_beta)
  }
  # Add the prior(s) as an attribute for later use
  attr(z, "prior") <- y
  # Add the likelihood as an attribute for later use
  attr(z, "likelihood") <- x
  # Store the name of the parameter of interest
  attr(z, "parameter") <- "p"
  # Add "posterior" as the second component of the class
  class(z) <- c(class(z)[1], "posterior", class(z)[2])
  return(z)
}
