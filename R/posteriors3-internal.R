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
  # Extract the data
  data <- attr(x, "data")
  if (!inherits(data, "list")) {
    stop("The attribute 'data' must be a list, created by add_data()")
  }
  if (length(data) != length(x)) {
    stop("'data' must have length ", length(x), "create it using add_data()")
  }
  # Check that all the data are in the support of the relevant random variable
  # Find the support of x
  # If length(x) > 1 this is a length(d) by 2 matrix, with columns min and max
  # If length(x) = 1 this is a numeric vector of length 2
  the_support <- distributions3::support(x)
  if (length(x) > 1) {
    mins <- the_support[, "min"]
    maxs <- the_support[, "max"]
  } else {
    mins <- the_support["min"]
    maxs <- the_support["max"]
  }
  check_support <- function(data, mins, maxs) {
    return(ifelse(all(data >= mins & data <= maxs), TRUE, FALSE))
  }
  valid <- mapply(check_support, data, mins, maxs)
  if (any(!valid)) {
    variable_numbers <- paste(sQuote(which(!valid)), collapse = ", ")
    msg <- sprintf(ngettext(length(which(!valid)),
                            "data for variable %s is outside its support",
                            "data for variables %s are outside their supports",
                            domain = "R-base"),
                   variable_numbers)
    stop(msg, domain = NA)
  }
  check_integer <- function(data, discrete) {
    return(any(!is.wholenumber(data) & discrete))
  }
  discrete <- distributions3::is_discrete(x)
  not_valid <- mapply(check_integer, data, discrete)
  if (any(not_valid)) {
    variable_numbers <- paste(sQuote(which(not_valid)), collapse = ", ")
    msg <- sprintf(ngettext(length(which(not_valid)),
                            "data for variable %s are not all integers",
                            "data for variables %s are not all integers",
                            domain = "R-base"),
                   variable_numbers)
    stop(msg, domain = NA)
  }
  return(invisible(x))
}

#' @keywords internal
#' @rdname posteriors3-internal
is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  return(abs(x - round(x)) < tol)
}
