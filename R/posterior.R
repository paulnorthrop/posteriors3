#' Posterior distributions
#'
#' Generic functions for performing Bayesian inference using `"distribution"`
#' objects. After using [`add_data()`] to add a random sample of data to a
#' distribution object `x` the generic multiplication `*.distribution`
#' multiplies the likelihood by the prior distribution specified in `y` to
#' create a posterior distribution.
#'
#' @param x To specify the **likelihood**. A probability distribution object
#'   inheriting from class `"distribution"` such as those created by a call to
#'   [`Normal()`][distributions3::Normal()] or
#'   [`Binomial()`][distributions3::Binomial()] etc. `x` must have a data
#'   attribute, added using [`add_data()`].
#' @param y To specify the **prior** distribution. A probability distribution
#'   object inheriting from class `"distribution"`.
#' @details Repeat the explanation in `add_data` concerning the data being in
#'   the support of `x`. Explain conjugacy and give an example (Binomial-Beta).
#'   If prior is conjugate then ... distribution object: same distribution type
#'   as the prior distribution in `y`.
#'   If not then ... Sample from posterior.
#' @returns A probability distribution object with the same class as the prior
#'   distribution object `y`.
#' @examples
#' # Note: the value of p is irrelevant
#' x <- Binomial(size = 10)
#' x <- add_data(x, 1:3)
#' # Using a conjugate prior distribution
#' y <- Beta(alpha = 1, beta = 1)
#' z <- x * y
#' z
#'
#' # Two binomial distributions
#' x <- Binomial(size = c(5, 10))
#' # Two vectors of data
#' data <- list(1:2, 1:3)
#' x <- add_data(x, data)
#' y <- Beta(alpha = 2, beta = 2)
#' z <- x * y
#' z
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
    stop("x and y must each be a \"distribution\" object")
  }
  # Check that the likelihood object x has data as an attribute
  if (is.null(attr(x, "data"))) {
    stop("x must have a data attribute, added using add_data()")
  }
  # Make the data into a list?


  # Check that the data are in the support of x
  the_support <- distributions3::support(x)



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
