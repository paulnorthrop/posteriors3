# 2 options for length(d) distributions
# 1. data is a vector and we recycle the parameter values in d - difficult!
# 2. data is a list, matrix, data frame or vector

# Create a function to check that all data values are in the relevant support?

#' Add data to a probability distribution object
#'
#' Describe
#'
#' @param d A probability distribution object such as those created by a call
#'   to [`Normal()`][distributions3::Normal()] or
#'   [`Binomial()`][distributions3::Binomial()] etc.
#' @param data A numeric vector, matrix, data frame or list providing a numeric
#'   vector of data for each of the `length(d)` distinct variable in `d`.
#'   `data` may be a matrix with `length(d)` columns, a data frame with
#'   `length(d)` variables or a list of length `length(d)`. A matrix or data
#'   frame may contain `NA` values. Only if `length(d) = 1` may `data` be a
#'   numeric vector.
#'
#' @details Additional details... Explain that all the data must be in the
#'   support of distribution `d`.
#'
#' @return An object with the same class as the input `d`, that is, inheriting
#'   from class `"distribution"`.
#'
#' @examples
#' # Note: the value of p is irrelevant
#' x <- Binomial(size = 10)
#' x <- add_data(x, 1:3)
#' y <- Beta(alpha = 1, beta = 1)
#' z <- x * y
#' z
#'
#' x <- Binomial(size = c(5, 10))
#' data <- list(1:2, 1:3)
#' x <- add_data(x, data)
#' y <- Beta(alpha = 1, beta = 1)
#' z <- x * y
#' z
#'
#' @export
add_data <- function(d, data) {
  # How many distinct variables are in d?
  nvars <- length(d)
  # Data frame
#  if (inherits(data), "data.frame") {
#
#  } else if (inherits(data), "list") {
#
#  } else if (inherits(data), "matrix") {
#
#  } else if (is.numeric(data)) {
#
#  } else {
#    stop("'data' is not of an appropriate type")
#  }
  # Check that x has the correct dimensions
  attr(d, "data") <- data
  return(d)
}
