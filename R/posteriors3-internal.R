#' Internal posteriors3 functions
#'
#' Internal posteriors3 functions
#' @details
#' These functions are not intended to be called by the user.
#' @name posteriors3-internal
#' @keywords internal
NULL

#' @keywords internal
#' @rdname posteriors3-internal
check_data <- function(x) {
  # Check that the likelihood object x has appropriate data as an attribute
  if (is.null(attr(x, "data"))) {
    stop("'x' must have a data attribute, added using add_data()")
  }
  if (!inherits(data, "list")) {
    stop("The attribute 'data' must be a list, created by add_data()")
  }
  if (length(attr(x, "data")) != length(x)) {
    stop(paste0("'data' must have length ", length(x),
                "create is using add_data()"))
  }
  # Check that all the data are in the support of the relevant random variable
  # Find the support of x
  # This is a length(d) by 2 matrix, with columns min and max
  the_support <- distributions3::support(x)
  the_data <- attr(x, "data")
  mins <- the_support[, "min"]
  maxs <- the_support[, "max"]
  check_support <- function(data, mins, maxs) {
    return(ifelse (all(data >= mins & data <= maxs), TRUE, FALSE))
  }
  valid <- mapply(check_support, the_data, mins, maxs)
  if (any(!valid)) {
    stop(paste0("The data for variable number(s): ",
                paste(which(!valid), collapse = ", "),
                "; are outside their support"))
  }
  check_integer <- function(data, discrete) {
    return(any(!is.wholenumber(data) & discrete))
  }
  discrete <- distributions3::is_discrete(x)
  not_valid <- mapply(check_integer, the_data, discrete)
  if (any(not_valid)) {
    stop(paste0("The data for variable number(s): ",
                paste(which(not_valid), collapse = ", "),
                "; are not all integers"))
  }
  return(invisible(x))
}

#' @keywords internal
#' @rdname posteriors3-internal
is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  return(abs(x - round(x)) < tol)
}
