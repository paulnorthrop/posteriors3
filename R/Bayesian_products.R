#' Products of Functions for Bayesian Inference
#'
#' Generic functions for performing univariate Bayesian inference using
#' `"distribution"` objects. A generic multiplication operator `*.distribution`
#' can create either a posterior distribution as a product of a likelihood and
#' prior distribution or a single prior distribution as a product of two
#' constituent prior distributions. Alternatively, `posterior(x, y)` or
#' `prior(x, y)` can be used, respectively.
#'
#' @param x To specify either a **likelihood** or **prior** distribution. A
#'   probability distribution object inheriting from class `"distribution"`
#'   such as those created by a call to [`Normal()`][distributions3::Normal()]
#'   or [`Binomial()`][distributions3::Binomial()] etc. If `x` is intended to
#'   specify a **likelihood** then `x` must have an extra attribute `"data"`,
#'   added using [`add_data()`].
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
#'   Otherwise, both `x` and `y` are assumed to provide independent prior
#'   distributions for two different parameters of a distribution. For example,
#'   `x` could be used to set a prior distribution for the mean \eqn{\mu}, and
#'   `y` a prior distribution for the variance \eqn{\sigma^2}, of a Normal
#'   distribution. `prior(x, y)` will give the same output.
#'
#'   A message will note whether a posterior distribution or a prior
#'   distribution has been created. Such messages can be suppressed using
#'   [`suppressMessages()`][message()] if desired.
#'
#'   Explain conjugacy and give an example (Binomial-Beta?).
#'   If prior is conjugate then ... distribution object: same distribution type
#'   as the prior distribution in `y`.
#'   If not then ... sample from posterior.
#'
#' @returns A probability distribution object from the same family as the prior
#'   distribution used.
#'
#' @seealso [`add_data()`] for adding data to a `"distribution"` object.
#'
#' @examples
#' library(distributions3)
#'
#' ## One Binomial distribution, with size 10
#' ## Note: the argument p is irrelevant
#'
#' N <- Binomial(size = 10)
#'
#' # Add the data, a numeric vector c(1, 2, 3)
#' likelihood <- add_data(N, 1:3)
#'
#' # Set a conjugate prior distribution
#' prior <- Beta(alpha = 1, beta = 1)
#' # Determine the posterior distribution
#' posterior <- likelihood * prior
#' posterior
#'
#' ## Two Binomial distributions, with sizes 5 and 10
#'
#' M <- Binomial(size = c(5, 10))
#'
#' # Below are three equivalent ways to supply the data
#' #   1, 2 for the first Binomial distribution, and
#' #   1, 2, 3 for the second Binomial distribution
#'
#' # data is a list of length 2: one numeric vector per Binomial distribution
#' data <- list(1:2, 1:3)
#' # data is a data frame with 2 variables: one per Binomial distribution
#' data <- data.frame(data1 = c(1:2, NA), data2 = 1:3)
#' # data is a matrix with 2 columns: one per Binomial distribution
#' data <- matrix(c(1:2, NA, 1:3), nrow = 3, ncol = 2)
#'
#' likelihood <- add_data(M, data)
#' prior <- Beta(alpha = 1.5, beta = 2.1)
#' posterior <- likelihood * prior
#' posterior
#'
#' @name Bayesian
NULL
## NULL

#' @rdname Bayesian
#' @order 1
#' @export
`*.distribution` <- function(x, y) {
  # Check that x and y are distribution objects
  if (!inherits(x, "distribution") | !inherits(y, "distribution")) {
    stop("'x' and 'y' must each be a \"distribution\" object")
  }
  # Check whether x has data as an attribute
  # If it has then call the relevant S3 posterior() method based on class(x)[1]
  # Otherwise call the relevant S3 prior() method based on class(x)[1]
  if (!is.null(attr(x, "data"))) {
    z <- posterior(x, y)
    message("Posterior: ", class(x)[1], " likelihood, ", class(y)[1], " prior ")
  } else {
    z <- prior(x, y)
    message("Prior: ", class(x)[1], " x ", class(y)[1])
  }
  return(z)
}

#' @rdname Bayesian
#' @order 3
#' @export
posterior <- function(x, y) {
  UseMethod("posterior")
}

#' @rdname Bayesian
#' @order 2
#' @export
prior <- function(x, y) {
  UseMethod("prior")
}
