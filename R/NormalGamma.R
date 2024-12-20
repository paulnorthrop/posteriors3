#' Create a Normal-Gamma distribution
#'
#' Creates a bivariate distribution for \eqn{(X, Y)} where \eqn{X \mid Y} has a
#' Normal distribution with mean \eqn{\mu} and precision \eqn{\lambda Y}
#' (variance \eqn{1/\lambda Y}), and \eqn{Y} has a Gamma distribution.
#'
#' @param mu,lambda Parameters \eqn{\mu} (location) and \eqn{\lambda}
#'   (the precision is \eqn{\lambda Y}) for a Normal distribution.
#'   See [`Normal()`][distributions3::Normal()]. `lambda` must be positive.
#' @param shape,rate  Shape and rate parameters of a Gamma distribution. See
#'   [`Gamma()`][distributions3::Gamma()]. `shape` and `rate` must be positive.
#' @param names A character vector of length 2. Names to be used for the
#'   variables \eqn{X} and \eqn{Y}.
#'
#' @details See the Wikipedia page
#'   [Normal-gamma distribution](https://en.wikipedia.org/wiki/Normal-gamma_distribution).
#'   See [`Normal()`][distributions3::Normal()] and
#'   [`Gamma()`][distributions3::Gamma()] for information about the Normal and
#'   Gamma distributions. The joint p.d.f. of \eqn{(X, Y)} is the product of
#'   the conditional (normal) p.d.f. of \eqn{X \mid Y} and the (gamma) p.d.f.
#'   of \eqn{Y}.
#'
#' @return A `NormalGamma` object, inheriting from class `"distribution"`.
#' @seealso [`posterior`] for an example where a Normal-Gamma distribution is
#'   used as a conjugate prior for the location and precision parameters of a
#'   Normal distribution.
#'
#' @examples
#' Z <- NormalGamma()
#' Z
#' @export
NormalGamma <- function(mu = 0, lambda = 1, shape = 1, rate = 1,
                        names = c("X", "Y")) {

  if (any(lambda <= 0)) {
    stop("lambda must be positive")
  }
  if (any(shape <= 0 | rate <= 0)) {
    stop("alpha and beta must be positive")
  }

  lengths <- unique(c(length(mu), length(lambda), length(shape), length(rate)))
  stopifnot(
    "parameter lengths do not match (only scalars may be recycled)" =
      length(lengths) == 1 | (length(lengths) == 2 & is.element(1, lengths))
  )

  d <- data.frame(mu = mu, lambda = lambda, shape = shape, rate = rate)
  attr(d, "variable_names") <- names
  class(d) <- c("NormalGamma", "distribution")
  return(d)
}

#' @export
mean.NormalGamma <- function(x, ...) {
  rlang::check_dots_used()
  rval <- cbind(x$mu, x$shape / x$rate)
  colnames(rval) <- attr(x, "variable_names")
  return(rval)
}

#' @export
variance.NormalGamma <- function(x, ...) {
  rval <- cbind(x$rate / (x$lambda * ifelse(x$shape > 1, x$shape - 1, NA)),
                x$shape / x$rate ^ 2)
  colnames(rval) <- attr(x, "variable_names")
  return(rval)
}

#' Draw a random sample from a Normal-Gamma distribution
#'
#' @param x A `NormalGamma` object created by a call to [NormalGamma()].
#' @param n The number of samples to draw. Defaults to `1L`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Unused. Unevaluated arguments will generate a warning to
#'   catch mispellings or other possible errors.
#'
#' @return In case of a single distribution object or `n = 1`, either a numeric
#'   vector of length `n` (if `drop = TRUE`, default) or a `matrix` with `n`
#'   columns (if `drop = FALSE`). If `x` contains more than one distribution
#'   then a matrix with `n` columns is returned, with each row containing a
#'   random sample from one of the distributions.
#' @examples
#' library(distributions3)
#' X <- NormalGamma(mu = 10, lambda = 1, shape = 5, rate = 1)
#' random(X, 10)
#'
#' @export
random.NormalGamma <- function(x, n = 1L, drop = TRUE, ...) {
  n <- make_positive_integer(n)
  if (n == 0L) {
    return(numeric(0L))
  }
  FUN <- function(at, d) {
    tvalue <- stats::rgamma(n = at, shape = d$shape, rate = d$rate)
    sigma <- 1 / sqrt(d$lambda * tvalue)
    stats::rnorm(n = at, mean = d$mu, sd = sigma)
  }
  return(distributions3::apply_dpqr(d = x, FUN = FUN, at = n, type = "random",
                                    drop = drop))
}

#' Evaluate the joint probability density function of a Normal-Gamma distribution
#'
#' @param d A `NormalGamma` object created by a call to [NormalGamma()].
#' @param x A numeric matrix with 2 columns. Column 1 provides the values for
#'   the \eqn{X \mid Y} component and column 2 the corresponding values of the
#'   \eqn{Y} components of the joint distribution of \eqn{(X, Y)}, where
#'   \eqn{X \mid Y} has a Normal distribution with mean \eqn{\mu} and
#'   precision \eqn{\lambda Y} and has a Gamma distribution.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Not used.
#'
#' @return If `d` is a single distribution object, either a numeric
#'   vector of length `max(length(x), length(y))` (if `drop = TRUE`, default)
#'   or a `matrix` with `length(x)` columns (if `drop = FALSE`). In case of a
#'   vectorized distribution object, a matrix with `length(x)` columns, one for
#'   each distribution.
#'
#' @examples
#' library(distributions3)
#' X <- NormalGamma()
#' pdf(X, matrix(c(0, 0), ncol = 2))
#' @export
pdf.NormalGamma <- function(d, x, drop = TRUE, ...) {
  if (!is.matrix(x) | dim(x)[2] != 2) {
    stop("'x' must be a numeric matrix with 2 columns")
  }
  y <- x[, 2]
  x <- x[, 1]
  FUN <- function(i, x, y, d) {
    y_log_density <- stats::dgamma(x = y, shape = d$shape[i], rate = d$rate[i],
                                   log = TRUE)
    sigma <- 1 / sqrt(d$lambda[i] * y)
    x_log_density <- stats::dnorm(x = x, mean = d$mu[i], sd = sigma,
                                  log = TRUE)
    return(exp(y_log_density + x_log_density))
  }
  rval <- vapply(1:length(d), FUN = FUN, FUN.VALUE = rep_len(0.0, length(x)),
                 x = x, y = y, d = d)
  if (length(d) == 1) {
    if (drop) {
      rval <- c(rval)
    } else {
      rval <- as.matrix(rval)
    }
  }
  return(rval)
}

#' @rdname pdf.NormalGamma
#' @export
log_pdf.NormalGamma <- function(d, x, drop = TRUE, ...) {
  if (!is.matrix(x) | dim(x)[2] != 2) {
    stop("'x' must be a numeric matrix with 2 columns")
  }
  y <- x[, 2]
  x <- x[, 1]
  FUN <- function(i, x, y, d) {
    y_log_density <- stats::dgamma(x = y, shape = d$shape[i], rate = d$rate[i],
                                   log = TRUE)
    sigma <- 1 / sqrt(d$lambda[i] * y)
    x_log_density <- stats::dnorm(x = x, mean = d$mu[i], sd = sigma,
                                  log = TRUE)
    return(y_log_density + x_log_density)
  }
  rval <- vapply(1:length(d), FUN = FUN, FUN.VALUE = rep_len(0.0, length(x)),
                 x = x, y = y, d = d)
  if (length(d) == 1) {
    if (drop) {
      rval <- c(rval)
    } else {
      rval <- as.matrix(rval)
    }
  }
  return(rval)
}

#' Return the support of a Normal-Gamma distribution
#'
#' @param d A `NormalGamma` object created by a call to [NormalGamma()].
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Currently not used.
#'
#' @return In case of a single distribution object, a numeric vector of length 2
#' with the minimum and maximum value of the support (if `drop = TRUE`, default)
#' or a `matrix` with 2 columns. In case of a vectorized distribution object, a
#' matrix with 2 columns containing all minima and maxima.
#'
#' @export
support.NormalGamma <- function(d, drop = TRUE, ...) {
  rlang::check_dots_used()
  rval <- matrix(c(-Inf, Inf), nrow = length(d), ncol = 4, byrow = TRUE)
  colnames(rval) <- paste(c("min", "max", "min", "max"),
                           rep(attr(d, "variable_names"), each = 2), sep = "_")
  return(rval)
}

#' @exportS3Method
is_discrete.NormalGamma <- function(d, ...) {
  rlang::check_dots_used()
  return(stats::setNames(rep.int(FALSE, length(d)), names(d)))
#  return(rep.int(FALSE, length(d)), names(d))
}

#' @exportS3Method
is_continuous.NormalGamma <- function(d, ...) {
  rlang::check_dots_used()
  return(stats::setNames(rep.int(TRUE, length(d)), names(d)))
#  return(rep.int(TRUE, length(d)), names(d))
}
