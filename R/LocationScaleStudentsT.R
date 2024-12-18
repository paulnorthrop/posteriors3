#' Create a Location-Scale t distribution
#'
#' Creates a t distribution with location \eqn{\mu} and scale
#' \eqn{\sigma} parameters, that is, \eqn{(X - \mu) / \sigma} has a Student's
#' t distribution.
#'
#' @param mu,sigma Parameters \eqn{\mu} (location) and \eqn{\sigma} (scale)
#'   parameters. `sigma` must be positive.
#' @param df Degrees of freedom \eqn{\nu}. `df` must be positive.
#'
#' @details See the Wikipedia page
#'   [Location-scale t distribution](https://en.wikipedia.org/wiki/Student%27s_t-distribution#Location-scale_t_distribution).
#'   and [`StudentsT()`][distributions3::StudentsT()].
#' @return A `LocationScaleT` object, inheriting from class `"distribution"`.
#'   Objects of class `"LocationScaleT"` have the following generic methods.
#'   * [`mean()`], [`variance()`][distributions3::variance()],
#'     [`skewness()`][distributions3::skewness()],
#'     [`kurtosis()`][distributions3::kurtosis()].
#'   * [`random()`][distributions3::random()], [`pdf()`][distributions3::pdf()],
#'     [`log_pdf()`][distributions3::log_pdf()],
#'     [`cdf()`][distributions3::cdf()],
#'     [`quantile()`][distributions3::quantile.StudentsT()].
#'   * [`support()`][distributions3::support()],
#'     [`is_discrete()`][distributions3::is_discrete()],
#'     [`is_continuous()`][distributions3::is_continuous()].
#'
#' @examples
#' library(distributions3)
#' mu <- 10
#' sigma <- 2
#' X <- LocationScaleT(mu = mu, sigma = sigma, df = 6)
#' Y <- StudentsT(6)
#' c(mean(X), mean(Y))
#' c(variance(X), variance(Y))
#' c(skewness(X), skewness(Y))
#' c(kurtosis(X), kurtosis(Y))
#'
#' c(pdf(X, 2), pdf(Y, (2 - mu) / sigma) / sigma)
#' c(log_pdf(X, 2), log_pdf(Y, (2 - mu) / sigma) - log(sigma))
#' c(cdf(X, 2), cdf(Y, (2 - mu) /sigma))
#' c(quantile(X, 0.75), sigma * quantile(Y, 0.75) + mu)
#' set.seed(42)
#' random(X, 5)
#' set.seed(42)
#' sigma * random(Y, 5) + mu
#' @export
LocationScaleT <- function(mu = 0, sigma = 1, df) {

  if (any(sigma <= 0)) {
    stop("'sigma' must be positive")
  }
  if (any(df <= 0)) {
    stop("'df' must be positive")
  }

  lengths <- unique(c(length(mu), length(sigma), length(df)))
  stopifnot(
    "parameter lengths do not match (only scalars may be recycled)" =
      length(lengths) == 1 | (length(lengths) == 2 & is.element(1, lengths))
  )

  d <- data.frame(mu = mu, sigma = sigma, df = df)
  attr(d, "parameter_names") <- c("mu", "sigma", "df")
  class(d) <- c("LocationScaleT", "distribution")
  return(d)
}

#' @export
mean.LocationScaleT <- function(x, ...) {
  rlang::check_dots_used()
  rval <- ifelse(x$df > 1, x$mu, NaN)
  return(rval)
}

#' @export
variance.LocationScaleT <- function(x, ...) {
  rval <- x$sigma ^ 2 * ifelse(x$df > 2,
                               x$df / (x$df - 2),
                               ifelse(x$df > 1, Inf, NaN)
                               )
  return(rval)
}

#' @export
skewness.LocationScaleT <- function(x, ...) {
  rval <- ifelse(x$df > 3, 0, NaN)
  return(rval)
}

#' @export
kurtosis.LocationScaleT <- function(x, ...) {
  rval <- ifelse(x$df > 4,
                 6 / (x$df - 4),
                 ifelse(x$df > 2, Inf, NaN)
                 )
  return(rval)
}

#' @export
random.LocationScaleT <- function(x, n = 1L, drop = TRUE, ...) {
  n <- distributions3::make_positive_integer(n)
  if (n == 0L) {
    return(numeric(0L))
  }
  FUN <- function(at, d) rt(n = at, df = d$df)
  rval <- distributions3::apply_dpqr(d = x, FUN = FUN, at = n, type = "random",
                                     drop = drop)
  rval <- x$sigma * rval + x$mu
  return(rval)
}

#' @export
pdf.LocationScaleT <- function(d, x, drop = TRUE, elementwise = NULL, ...) {
  FUN <- function(at, d) {
    dt(x = (at - d$mu) / d$sigma, df = d$df, ...) / d$sigma
  }
  return(distributions3::apply_dpqr(d = d, FUN = FUN, at = x, type = "density",
                                    drop = drop, elementwise = elementwise))
}

#' @export
log_pdf.LocationScaleT <- function(d, x, drop = TRUE, elementwise = NULL, ...) {
  FUN <- function(at, d) {
    dt(x = (at - d$mu) / d$sigma, df = d$df, log = TRUE, ...) - log(d$sigma)
  }
  return(distributions3::apply_dpqr(d = d, FUN = FUN, at = x, type = "logLik",
                                    drop = drop, elementwise = elementwise))
}

#' @export
cdf.LocationScaleT <- function(d, x, drop = TRUE, elementwise = NULL, ...) {
  FUN <- function(at, d) pt(q = (at - d$mu) / d$sigma, df = d$df, ...)
  return(distributions3::apply_dpqr(d = d, FUN = FUN, at = x,
                                    type = "probability", drop = drop,
                                    elementwise = elementwise))
}

#' @export
quantile.LocationScaleT <- function(x, probs, drop = TRUE, elementwise = NULL, ...) {
  FUN <- function(at, d) qt(p = at, df = d$df, ...)
  rval <- distributions3::apply_dpqr(d = x, FUN = FUN, at = probs,
                                     type = "quantile", drop = drop,
                                     elementwise = elementwise)
  rval <- x$sigma * rval + x$mu
  return(rval)
}

#' @export
support.LocationScaleT <- function(d, drop = TRUE, ...) {
  rlang::check_dots_used()
  min <- rep(-Inf, length(d))
  max <- rep(Inf, length(d))
  return(distributions3::make_support(min, max, d, drop = drop))
}

#' @exportS3Method
is_discrete.LocationScaleT <- function(d, ...) {
  rlang::check_dots_used()
  return(setNames(rep.int(FALSE, length(d)), names(d)))
}

#' @exportS3Method
is_continuous.LocationScaleT <- function(d, ...) {
  rlang::check_dots_used()
  return(setNames(rep.int(TRUE, length(d)), names(d)))
}
