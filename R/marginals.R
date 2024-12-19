#' Extract marginal distributions
#'
#' Extract marginal distributions from a multivariate distribution inheriting
#' from class `"distribution"`.
#'
#' @param d A probability distribution object inheriting from class
#'   `"distribution"` such as those created by a call to [`NormalGamma()`].
#' @param names A character vector. Names to be used for the individual
#'   variables in the multivariate disribution.
#'
#' @details Additional details...
#' @return A list with `length(d)` components. Each component is a univariate
#'   distribution object inheriting from class `"distribution"`.
#' @examples
#' X <- NormalGamma(names = c("mu", "tau"))
#' marginals(X)
#'
#' @name marginals
NULL
## NULL

#' @rdname marginals
#' @export
marginals <- function(d, names = NULL) {
  UseMethod("marginals")
}

#' @rdname marginals
#' @export
marginals.NormalGamma <- function(d, names = c("mu", "tau")) {
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
  class(X) <- c(class(X)[1], "posterior", class(X)[2])
  attr(X, "variable_names") <- names[1]
  # The marginal for Y
  Y <- distributions3::Gamma(shape = d$shape, rate = d$rate)
  class(Y) <- c(class(Y)[1], "posterior", class(Y)[2])
  attr(Y, "variable_names") <- names[2]
  return(stats::setNames(list(X, Y), attr(d, "variable_names")))
}
