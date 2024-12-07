#' Add data to a probability distribution object
#'
#' Add data as an attribute `"data"` to a probability distribution object,
#'   inheriting from class `"distribution"`.
#'
#' @param d A probability distribution object such as those created by a call
#'   to [`Normal()`][distributions3::Normal()] or
#'   [`Binomial()`][distributions3::Binomial()] etc.
#' @param data A numeric vector, matrix, data frame or list providing a numeric
#'   vector of data for each of the `length(d)` distinct variables in `d`.
#'   `data` may be a matrix with `length(d)` columns, a data frame with
#'   `length(d)` variables or a list of length `length(d)`. A matrix or data
#'   frame may contain `NA` values. Only if `length(d) = 1` may `data` be a
#'   numeric vector.
#'
#' @details All observations in the data must be in the support of the
#'   probability distribution `d`. See [`support()`][distributions3::support()].
#'   If necessary, the input `data` is converted to a list of length
#'   `length(d)` before assigning it to `attr(d, "data")`, for later use by
#'   [`posterior`].
#'
#' @return An object with the same class as the input `d`, that is, inheriting
#'   from class `"distribution"`, but with the extra attribute `"data"`.
#'
#' @seealso [`posterior`] for performing Bayesian inference using
#'   `"distribution"` objects.
#'
#' @examples
#' library(distributions3)
#'
#' ## One Binomial distribution, with size 10
#' ## Note: the argument p is irrelevant
#'
#' N <- Binomial(size = 10)
#' # Add the data, a numeric vector c(1, 2, 3)
#' likelihood <- add_data(N, 1:3)
#' likelihood
#' attr(likelihood, "data")
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
#' likelihood
#' attr(likelihood, "data")
#'
#' @export
add_data <- function(d, data) {
  # How many distinct variables are in d?
  nvars <- length(d)
  # Check that the data have the correct dimension(s) and make them a list
  if (inherits(data, "data.frame")) {
    if (ncol(data) != nvars) {
      stop(paste0("'data' must have length(d) = ", nvars, " columns"))
    }
    data <- as.list(data)
  } else if (inherits(data, "list")) {
    if (length(data) != nvars) {
      stop(paste0("'data' must have length ", nvars))
    }
  } else if (inherits(data, "matrix")) {
    if (ncol(data) != nvars) {
      stop(paste0("'data' must have length(d) = ", nvars, " columns"))
    }
    data <- as.list(as.data.frame(data))
  } else if (is.numeric(data)) {
    data <- list(data)
  } else {
    stop("'data' is not of an appropriate type")
  }
  # Remove any missings
  data <- lapply(data, na.omit)
  attr(d, "data") <- data
  return(d)
}
