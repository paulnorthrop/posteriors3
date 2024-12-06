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
  return(invisible(x))
}

#' @keywords internal
#' @rdname posteriors3-internal
check_support <- function(x) {
  return(invisible())
}
