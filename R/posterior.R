#' Posterior distributions
#'
#' Generic functions for performing Bayesian inference using `"distribution"`
#' objects. After using [`add_data()`] to add a random sample of data to a
#' distribution object `x` the generic multiplication `*.distribution`
#' multiplies the implied likelihood in `x` by the prior distribution specified
#' in `y` to create a posterior distribution. `posterior(x, y)` will give the
#' same output.
#'
#' @param x To specify the **likelihood**. A probability distribution object
#'   inheriting from class `"distribution"` such as those created by a call to
#'   [`Normal()`][distributions3::Normal()] or
#'   [`Binomial()`][distributions3::Binomial()] etc. `x` must have an extra
#'   attribute `"data"`, added using [`add_data()`].
#' @param y To specify the **prior** distribution. A probability distribution
#'   object inheriting from class `"distribution"`.
#'
#' @details Repeat the explanation in `add_data` concerning the data being in
#'   the support of `x`. Explain conjugacy and give an example (Binomial-Beta).
#'   If prior is conjugate then ... distribution object: same distribution type
#'   as the prior distribution in `y`.
#'   If not then ... Sample from posterior.
#'
#' @returns A probability distribution object with the same class as the prior
#'   distribution object `y`.
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
#' #   1,2 for the first Binomial distribution, and
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
#' @export
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
  # Call the relevant S3 posterior method based on data distribution, that is,
  # class(x)[1], passing the prior distribution y
  z <- posterior(x, y)
  return(z)
}

#' @rdname posterior
#' @order 2
#' @export
posterior <- function(x, y) {
  UseMethod("posterior")
}
