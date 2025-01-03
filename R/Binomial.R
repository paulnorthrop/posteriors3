#' @export
#' @rdname posterior
posterior.Binomial <- function(x, y, ...) {
  # Extract the name of the prior distribution
  prior_distribution <- class(y)[1]
  # Extract the data and the Binomial size parameter
  data <- attr(x, "data")
  # data is a list of length length(x$size) = length(x$p)
  size <- x$size
  sum_data <- sum(unlist(data))
  f <- function(x, y) x - y
  size_minus_data <- mapply(f, as.list(size), data)
  sum_size_minus_data <- sum(unlist(size_minus_data))
  # If prior is conjugate then infer the posterior distribution
  if (prior_distribution == "Beta") {
    # Extract the parameter values of the Beta prior
    prior_alpha <- y$alpha
    prior_beta <- y$beta
    # Calculate the parameters of the Beta posterior
    posterior_alpha = prior_alpha + sum_data
    posterior_beta = prior_beta + sum_size_minus_data
    # Return the Beta posterior as a "distribution" object
    z <- Beta(alpha = posterior_alpha, beta = posterior_beta)
    # Add "posterior" as the second component of the class
    class(z) <- c(class(z)[1], "posterior", class(z)[2])
  } else {
    # Find the mle to use as an initial estimate
    mle <- sum_data / (sum_data + sum_size_minus_data)
    # Avoid numerical problems in the cases where the mle is 0 or 1
    if (mle == 0) {
      mle <- 0.01
    } else if (mle == 1) {
      mle <- 0.99
    }
    # Create a function to calculate the log-posterior
    log_posterior_fn <- function(prob) {
      if (any(prob < 0) || any(prob > 1)) {
        return(-Inf)
      }
      # A function to sum the contributions from the Binomial distributions
      # involved. There could be Binomial distributions with different values
      # of the parameter size
      # We need to create a distribution object d_i inside binomial_loglik()
      # because log_posterior_fn() must be vectorised with respect to prob
      # for possible use by rust::find_lambda_one_d()
      binomial_loglik <- function(i) {
        # Create a Binomial distribution object
        d_i <- distributions3::Binomial(size = size[i], p = prob)
        log_likelihood(d_i, x = unlist(data[i]))
      }
      log_lik <- sum(sapply(X = 1:length(size), FUN = binomial_loglik))
      log_prior <- log_pdf(y, x = prob)
      return(log_lik + log_prior)
    }
    # Has the user provided arguments in ... to be passed to rust::ru()?
    for_ru <- list(...)
    # If the simulation size n has not been supplied then set it to 1000
    if (is.null(for_ru$n)) {
      for_ru$n <- 1000
    }
    # If the transformation argument trans has not been supplied then use
    # trans = "user" to use a logit transformation on prob
    # If trans = "BC" has been supplied then call rust::find_lambda_one_d() to
    # set a suitable value of the Box-Cox parameter lambda
    # If trans = "none" then use no transformation, but do set the lower and
    # upper bounds of 0 and 1 for prob
    if (is.null(for_ru$trans)) {
      phi_to_theta <- function(phi) {
        ephi <- exp(phi)
        return(ephi / (1 + ephi))
      }
      log_j <- function(theta) {
        return(-log(theta) - log(1 - theta))
      }
      trans_list <- list(phi_to_theta = phi_to_theta, log_j = log_j)
      phi_init <- log(mle / (1 - mle))
      # Set for_ru$trans to NULL in case the user supplied trans = NULL
      for_ru$trans <- NULL
      for_ru <- c(for_ru, list(init = phi_init, trans = "user"), trans_list)
    } else if (for_ru$trans == "BC") {
      lambda <- rust::find_lambda_one_d(logf = log_posterior_fn,
                                        min_phi = 0.01, max_phi = 0.99)
      for_ru <- c(for_ru, list(lambda = lambda, lower = 0, upper = 1))
    } else if (for_ru$trans == "none") {
      for_ru <- c(for_ru, list(init = mle, lower = 0, upper = 1))
    } else {
      stop("''trans'' must be NULL, \"BC\" or \"none\"")
    }
    # Add the log_posterior_fn to the list of argument for rust::ru()
    for_ru <- c(list(logf = log_posterior_fn), for_ru)
    # Call rust:ru()
    z <- do.call(rust::ru, for_ru)
    # Add "posterior" as the second component of the class
    class(z) <- c(class(z)[1], "posterior")
  }
  # Add the prior(s) as an attribute for later use
  attr(z, "prior") <- y
  # Add the likelihood function as an attribute for later use
  attr(z, "likelihood") <- Beta(alpha = sum_data, beta = sum_size_minus_data)
  # Store the name of the parameter of interest
  attr(z, "variable_names") <- "p"
  return(z)
}
