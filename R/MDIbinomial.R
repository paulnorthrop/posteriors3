#' Create an MDI prior distribution for a binomial probability
#'
#' Creates a distribution with p.d.f.
#' \eqn{f_X(x) \propto x^{\alpha x} (1-x)^{\beta(1-x)}} for \eqn{0 < x < 1}.
#' The case \eqn{\alpha = \beta = 1} arises as the Maximal Data Information
#' (MDI) prior for the probability parameter \eqn{p} of a Binomial distribution.
#'
#' @param alpha,beta Non-negative shape parameters \eqn{\alpha} and \eqn{\beta}.
#'
#' @details See Tuyl et al. (2008) for further information and a comparison
#'   of the MDI prior with other prior distributions often used for Binomial
#'   data.
#'
#' @return A `MDIbinomial` object, inheriting from class `"distribution"`.
#'
#' @seealso [`posterior`] for an example where this distribution is used as a
#'   (non-conjugate) prior for the probability parameter \eqn{p} of a Binomial
#'   distribution.
#'
#' @references Tuyl, F., Gerlach, R., and Mengersen, K. (2008). A Comparison of
#'   Bayes–Laplace, Jeffreys, and Other Priors: The Case of Zero Events.
#'   *The American Statistician*, **62**(1), 40–44.
#'   \doi{10.1198/000313008X267839}
#'
#' @examples
#' P <- MDIbinomial()
#' P
#' @export
MDIbinomial <- function(alpha = 1, beta = 1) {
  if (any(alpha < 0)) {
    stop("alpha must be non-negative")
  }
  if (any(beta < 0)) {
    stop("beta must be non-negative")
  }
  stopifnot(
    "parameter lengths do not match (only scalars are allowed to be recycled)" =
      length(alpha) == length(beta) | length(alpha) == 1 | length(beta) == 1
    )
  d <- data.frame(alpha = alpha, beta = beta)
  class(d) <- c("MDIbinomial", "distribution")
  return(d)
}

#' Evaluate the probability density function of an MDI prior distribution for a
#' binomial probability
#'
#' @param d An `MDIbinomial` object created by a call to [MDIbinomial()].
#' @param x A vector of elements whose probabilities you would like to
#'   determine given the distribution `d`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param elementwise logical. Should each distribution in \code{d} be evaluated
#'   at all elements of \code{x} (\code{elementwise = FALSE}, yielding a matrix)?
#'   Or, if \code{d} and \code{x} have the same length, should the evaluation be
#'   done element by element (\code{elementwise = TRUE}, yielding a vector)? The
#'   default of \code{NULL} means that \code{elementwise = TRUE} is used if the
#'   lengths match and otherwise \code{elementwise = FALSE} is used.
#' @param ... Not used.
#'
#' @return In case of a single distribution object, either a numeric
#'   vector of length `probs` (if `drop = TRUE`, default) or a `matrix` with
#'   `length(x)` columns (if `drop = FALSE`). In case of a vectorized
#'   distribution object, a matrix with `length(x)` columns containing all
#'   possible combinations.
#' @export
#'
pdf.MDIbinomial <- function(d, x, drop = TRUE, elementwise = NULL, ...) {
  if (is.logical(elementwise) && elementwise && length(d) != length(x)) {
    warning("''elementwise'' has been set to FALSE because length(d) != length(x)")
    elementwise <- FALSE
  }
  # Set the default value of elementwise
  if (is.null(elementwise)) {
    if (length(d) == length(x)) {
      elementwise <- TRUE
    } else {
      elementwise <- FALSE
    }
  }
  # Use integrate() to evaluate the normalising constant(s)
  dMDIbinomial <- function(x, alpha, beta, const = 1) {
    val <- const * x ^ (alpha * x) * (1 - x) ^ (beta * (1 - x))
    return(val)
  }
  const_fun <- function(i) {
    if (d$alpha[i] == 0 && d$beta[i] == 0) {
      val <- 1
    } else if (d$alpha[i] == 1 && d$beta[i] == 1) {
      val <- 0.617826919366
    } else {
      val <- stats::integrate(f = dMDIbinomial, lower = 0, upper = 1,
                              alpha = d$alpha[i], beta = d$beta[i])$value
    }
    return(val)
  }
  consts <- vapply(1:length(d), const_fun, 0.0)
  FUN <- function(i, x, d, elementwise) {
    if (elementwise) {
      x_vals <- x[i]
    } else {
      x_vals <- x
    }
    return(dMDIbinomial(x = x_vals, alpha = d$alpha[i], beta = d$beta[i],
                        const= consts[i]))
  }
  rval <- sapply(1:length(d), FUN = FUN, x = x, d = d,
                 elementwise = elementwise)
  if (length(d) == 1) {
    if (drop) {
      rval <- c(rval)
    } else {
      rval <- as.matrix(rval)
    }
  }
  return(rval)
}

#' @rdname pdf.MDIbinomial
#' @export
log_pdf.MDIbinomial <- function(d, x, drop = TRUE, elementwise = NULL, ...) {
  # Call pdf.MDIbinomial
  return(log(pdf(d = d, x = x, drop = drop, elementwise = elementwise)))
}

#' Return the support of an MDI prior distribution for a binomial probability
#'
#' @param d An `MDIbinomial` object created by a call to [MDIbinomial()].
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Currently not used.
#'
#' @return In case of a single distribution object, a numeric vector of length 2
#' with the minimum and maximum value of the support (if `drop = TRUE`, default)
#' or a `matrix` with 2 columns. In case of a vectorized distribution object, a
#' matrix with 2 columns containing all minima and maxima.
#'
#' @export
support.MDIbinomial <- function(d, drop = TRUE, ...) {
  rlang::check_dots_used()
  min <- rep(0, length(d))
  max <- rep(1, length(d))
  return(make_support(min, max, d, drop = drop))
}

#' @exportS3Method
is_discrete.MDIbinomial <- function(d, ...) {
  rlang::check_dots_used()
  return(stats::setNames(rep.int(FALSE, length(d)), names(d)))
}

#' @exportS3Method
is_continuous.MDIbinomial <- function(d, ...) {
  rlang::check_dots_used()
  return(stats::setNames(rep.int(TRUE, length(d)), names(d)))
}
