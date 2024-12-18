#' Extract marginal distributions
#'
#' Extract marginal distributions from a multivariate distribution inheriting
#' from class `"distribution"`.
#'
#' @param d A probability distribution object inheriting from class
#'   `"distribution"` such as those created by a call to [`NormalGamma()`].
#' @param ... Not used.
#'
#' @details Additional details...
#' @return A list with `length(d)` components. Each component is a univariate
#'   distribution object inheriting from class `"distribution"`.
#' @name marginals
NULL
## NULL

#' @rdname marginals
#' @export
marginals <- function(object, ...) {
  UseMethod("marginals")
}

#' @rdname marginals
#' @export
marginals.NormalGamma <- function(d, ...) {
  # Check that object is a distribution object
  if (!inherits(d, "distribution")) {
    stop("'d' must be a \"distribution\" object")
  }
  # The marginal for X is a Location-Scale t distribution with location
  # parameter d$mu, squared scale parameter d$rate / (d$lambda * d$shape) and
  # degrees of freedom, df = 2 * d$shape
  lst_mu <- d$mu
  lst_sigma <- sqrt(d$rate / (d$lambda * d$shape))
  lst_df <- 2 * d$shape
  X <- LocationScaleT(mu = lst_mu, sigma = lst_sigma, df = lst_df)
  # The marginal for Y
  Y <- distributions3::Gamma(shape = d$shape, rate = d$rate)
  return(setNames(list(X, Y), attr(d, "variable_names")))
}
