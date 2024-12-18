#' Create a posterior distribution
#'
#' Generic functions for performing univariate Bayesian inference using
#' `"distribution"` objects. A generic multiplication operator `*.distribution`
#' creates a posterior distribution as a product of a likelihood and
#' prior distribution. Alternatively, `posterior(x, y)` or can be used.
#'
#' @param x To specify a **likelihood**. A probability distribution object
#'   inheriting from class `"distribution"` such as those created by a call to
#'   [`Normal()`][distributions3::Normal()] or
#'   [`Binomial()`][distributions3::Binomial()] etc. `x` must have an extra
#'   attribute `"data"`, added using [`add_data()`].
#' @param y To specify a **prior** distribution. A probability distribution
#'   object inheriting from class `"distribution"`.
#'
#' @details If `x` has an attribute `"data"`, added using [`add_data()`], then
#'   `x` is used to define a likelihood based on treating `attr(x, "data")` as
#'   a random sample from the probability distribution underlying `x`. All
#'   observations in the data must be in the support of the probability
#'   distribution `d`. See [`support()`][distributions3::support()]. The
#'   posterior distribution for the unknown parameters of `x` is inferred using
#'   the prior distribution supplied by `y`. `posterior(x, y)` will give the
#'   same output.
#'
#'   A message will note likelihood and prior combination used to create the
#'   posterior distribution. Such messages can be suppressed using
#'   [`suppressMessages()`][message()] if desired.
#'
#'   **Explain conjugacy and give an example (Binomial-Beta?).
#'   If prior is conjugate then ... distribution object: same distribution type
#'   as the prior distribution in `y`.
#'   If not then ... sample from posterior.**
#'
#' @returns A probability distribution object from the same family as the prior
#'   distribution used.
#'
#' @seealso [`add_data()`] for adding data to a `"distribution"` object.
#' @seealso [`plot.posterior()`] for plotting posterior and prior distributions.
#'
#' @examples
#' library(distributions3)
#'
#' ### Binomial likelihood, conjugate Beta prior for p
#'
#' ## One Binomial distribution, with size 10
#'
#' # Note: the value of p is only use to simulate example data
#' N <- Binomial(size = 10, p = 0.2)
#'
#' # Simulate a sample of size 5 from a Binomial(20, 0.2) distribution
#' set.seed(3)
#' data <- random(N, 5)
#'
#' # Add the data, a numeric vector
#' likelihood <- add_data(N, data)
#'
#' # Set a conjugate (uniform) prior distribution
#' prior <- Beta(alpha = 1, beta = 1)
#' # Construct the posterior distribution
#' posterior <- likelihood * prior
#' posterior
#' plot(posterior)
#'
#' # Compare two different conjugate prior distributions
#' prior <- Beta(alpha = c(1, 10), beta = c(1,10))
#' # Determine the posterior distribution
#' posterior <- likelihood * prior
#' posterior
#' plot(posterior)
#'
#' ## Two Binomial distributions, with sizes 5 and 10
#'
#' # Note: the value of p is only use to simulate example data
#' M <- Binomial(size = c(5, 10), p = 0.8)
#'
#' # Simulate samples of size 8 from
#' #   a Binomial(5, 0.8) distribution, and
#' #   a Binomial(10, 0.8) distribution
#'
#' set.seed(3)
#' data <- random(M, 8)
#' # data is a matrix with 2 rows: one per Binomial distribution
#'
#' # Below are two more equivalent ways to supply these data
#'
#' # Extract the individual samples
#' data1 <- data[1, ]
#' data2 <- data[2, ]
#' # data is a list of length 2: one numeric vector per Binomial distribution
#' data <- list(data1 = data1, data2 = data2)
#' # data is a data frame with 2 variables: one per Binomial distribution
#' data <- data.frame(data1, data2)
#'
#' likelihood <- add_data(M, data)
#' prior <- Beta(alpha = 1.5, beta = 2.1)
#' posterior <- likelihood * prior
#' posterior
#' plot(posterior, legend_args = list(x = "topleft"))
#'
#' ### Normal likelihood, conjugate (normal, gamma) prior for (mu, 1/sigma^2)
#'
#' # Note: the values of mu and sigma are only used to simulate example data
#' X <- Normal(mu = 10, sigma = 2)
#'
#' # Simulate a sample of size 15 from a Normal(10, 2^2) distribution
#' set.seed(3)
#' data <- random(X, 15)
#'
#' # Add the data, a numeric vector
#' likelihood <- add_data(X, data)
#'
#' # Set a Normal-Gamma prior for (mean, precision)
#'
#' prior <- NormalGamma(names = c("mean", "precision"))
#' posterior <- likelihood * prior
#' posterior
#' # Find the marginal posterior distributions for mean and precision
#' marginals(posterior)
#'
#' # plot(posterior)
#' @name posterior
NULL
## NULL

#' @rdname posterior
#' @order 1
#' @export
`*.distribution` <- function(x, y) {
  # Check that x and y are distribution objects
  if (!inherits(x, "distribution") | !inherits(y, "distribution")) {
    stop("'x' and 'y' must each be a \"distribution\" object")
  }
  # Check that the likelihood object x has appropriate "data" as an attribute
  check_data(x)
  # If it has then call the relevant S3 posterior() method based on class(x)[1]
  z <- posterior(x, y)
  message("Posterior: ", class(x)[1], " likelihood, ", class(y)[1], " prior ")
  return(z)
}

#' @rdname posterior
#' @order 2
#' @export
posterior <- function(x, y) {
  UseMethod("posterior")
}
