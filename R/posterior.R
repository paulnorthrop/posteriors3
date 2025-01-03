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
#' @param ... Further arguments to be passed to [`ru()`][rust::ru()]. See
#'   **Details**.
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
#'   posterior distribution. These messages can be suppressed using
#'   [`suppressMessages()`][message()] if desired.
#'
#'   **Conjugate priors**. For some likelihoods, it is possible to specify a
#'   family of prior distribution such that the posterior distribution is in
#'   the same family. For example, for a likelihood based on a random sample
#'   from a [`Binomial()`][distributions3::Binomial()] distribution, specifying
#'   a [`Beta()`][distributions3::Beta()] prior distribution for the
#'   probability parameter \eqn{p} results in a Beta posterior distribution.
#'   In cases like this, `posterior()` returns a distribution object with the
#'   same distribution type as the distribution object `y` used to specify
#'   the prior distribution. See the table of conjugate distributions in the
#'   wikipedia article
#'   [Conjugate prior](https://en.wikipedia.org/wiki/Conjugate_prior)
#'   for details and examples of other conjugate cases.
#'
#'   **Non-conjugate priors**. If `y` is not a conjugate prior distribution for
#'   the likelihood `x`, then [`ru()`][rust::ru()] is used to simulate from the
#'   posterior distribution. In this event `...` may be used in a call to
#'   `posterior(x, y, ...)` to pass arguments to [`ru()`][rust::ru()]. The
#'   argument `n` may be useful because it specifies the size of the posterior
#'   sample. The default set in the `posterior` methods supplied in
#'   `posteriors3` is `n = 1000`. Other arguments to [`ru()`][rust::ru()] have
#'   default settings designed to work well in specific examples. For example,
#'   in [`posterior.Binomial()`][posterior()] sampling from the posterior for
#'   the probability parameter \eqn{p} is conducted on a logit scale, that is,
#'   we sample from the posterior distribution for \eqn{\log(p / (1-p))}.
#'   See **Examples** for an example based on the (non-conjugate) Maximal Data
#'   Information prior [`MDIbinomial()`].
#'
#' @returns In conjugate cases, a probability distribution object from the same
#'   family as the prior distribution. The first component of the class of this
#'   object is the name of the prior (and posterior) distribution.
#'
#'   In non-conjugate cases, an object, of class `c("ru", "posterior")`,
#'   returned from [`ru()`][rust::ru()].
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
#' prior <- Beta(alpha = c(1, 10), beta = c(1, 10))
#' # Determine the posterior distribution
#' posterior <- likelihood * prior
#' posterior
#' plot(posterior)
#'
#' # Non-conjugate MDI prior
#' prior <- MDIbinomial()
#' posterior <- likelihood * prior
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
#' # Add the data, a numeric vector
#' likelihood <- add_data(X, data)
#'
#' # Set a Normal-Gamma prior for (mean mu, precision tau)
#' prior <- NormalGamma(names = c("mu", "tau"))
#' posterior <- likelihood * prior
#' posterior
#'
#' # Plot the marginal prior and posterior distributions for mu and tau
#' plot(posterior, margin = "mu")
#' plot(posterior, margin = "tau")
#'
#' # Find the marginal posterior distributions for mean and precision
#' x <- marginals(posterior)
#' # plot the marginal posterior distributions individually
#' plot(x[[1]])
#' plot(x[[2]])
#'
#' # Contour plots of posterior and prior distributions
#' plot(posterior)
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
posterior <- function(x, y, ...) {
  UseMethod("posterior")
}
