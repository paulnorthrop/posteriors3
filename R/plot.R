#' Plot posterior and prior distributions
#'
#' Plot method for an object inheriting from class `"posterior"`.
#' Plots posterior and corresponding prior density functions.
#' Supports one-dimensional distributions, including one-dimensional marginal
#' distributions and two-dimensional distributions.
#'
#' @param x an object of class `c("name", "distribution")`, where
#'   `"name"` is the name of the distribution.
#' @param prior A logical scalar. If `prior = TRUE` then add all prior
#'   distributions to the plot. Otherwise, add no prior distributions.
#' @param likelihood A logical scalar. If `likelihood = TRUE` then add the
#'   likelihood function to the plot. Otherwise, add no likelihood.
#'   Not relevant if `margin` is supplied.
#' @param margin A numeric or character scalar giving the number or the name
#'   of the marginal variable. `attr(x, "variable_names")` can be used to view
#'   the names of the variables.
#' @param cdf A logical scalar.  In one-dimensional cases, if `cdf = TRUE` then
#'   the cumulative distribution function (c.d.f.) is plotted.  Otherwise, the
#'   probability density function (p.d.f.) is plotted. Not relevant for
#'   two-dimensional cases.
#' @param p A numeric vector.  In one-dimensional cases, if `xlim` is not
#'   passed in `...` then `p` is the fallback option for setting the range of
#'   values over which the p.d.f. or c.d.f is plotted.  See **Details**.
#'   In two-dimensional cases, `p` provides the levels of probability contours.
#'   The respective probabilities that the variable in question lies within the
#'   contour are approximately equal to the vales in `p`.
#'   The default `p = c(5, 25, 50, 75, 95)` is set internally.
#'   If `xlim` and `ylim` are passed in `...` then these are used to set the
#'   area over which the p.d.f. is contoured.  Otherwise, this is based on the
#'   smallest and largest components of `p`.
#' @param len An integer scalar.  In one-dimensional cases, the number of
#'   values at which the p.d.f or c.d.f. is evaluated to produce the plot.
#'   The larger `len` is the smoother is the curve. In two-dimensional cases,
#'   a `len` nu `len` regular grid is used to produce probability contours,
#'   using [`contour()`][graphics::contour].
#' @param legend_args A list of arguments to be passed to
#'   [`legend()`][graphics::legend()].  In particular, the argument `x`
#'   (perhaps in conjunction with `legend_args$y`) can be used to set the
#'   position of the legend.  If `legend_args$x` is not supplied then
#'   `"bottomright"` is used if `cdf = TRUE` and `"topright"` if
#'   `cdf = FALSE`.
#' @param digits The number of significant digits to be used in values of
#'   parameters displayed in the title or legend.
#'   See [`signif()`][base::signif].
#' @param ...  Further arguments to be passed to [`plot()`][graphics::plot()]
#'   and [`contour()`][graphics::contour()] such as `xlim`, `ylim`, `xlab`,
#'   `ylab`, `main`, `lwd`, `lty`, `col`, `pch`.
#' @details In one-dimensional cases, if `xlim` is passed in `...` then this
#'   determines the range of values of the variable to be plotted on the
#'   horizontal axis. If `xlim` is not passed in `...` then the range of values
#'   spans the support of the distribution, with the following proviso: if the
#'   lower (upper) endpoint of the distribution is `-Inf` (`Inf`)
#'   then the lower (upper) limit of the plotting range is set to the
#'   `p[1]`percent (`p[2]`percent) quantile of the distribution.
#'
#'   Plots of c.d.f.s are produced using calls to
#'   [`approxfun()`][stats::approxfun] and [`ecdf()`][stats::ecdf].
#' @return The input object `x` is returned. If `prior = TRUE` then the
#'   prior distribution(s) in `attr(x, "prior")` are appended to the posterior
#'   distribution(s) in `x`.
#' @seealso [`posterior`] for calculating products of functions for Bayesian
#'   Inference using `"distribution"` objects.
#' @section Examples:
#' See the examples in [`posterior`].
#' @export
plot.posterior <- function(x, prior = TRUE, likelihood = FALSE, margin = NULL,
                           cdf = FALSE, p = c(0.1, 99.9), len = 1000,
                           legend_args = list(), digits = 3, ...) {
  if (!distributions3::is_distribution(x)) {
    stop("use only with \"distribution\" objects")
  }
  # If marginal is non-NULL then attempt to extract from x a distribution
  # object relating to the required marginal posterior, and perhaps prior,
  # distributions
  marginal_variable_names <- attr(x, "variable_names")
  number_of_margins <- length(marginal_variable_names)
  if (!is.null(margin)) {
    if (number_of_margins == 1) {
      stop("'margin' is not relevant for a 1-dimensional posterior")
    }
    if (!is.numeric(margin) & !is.character(margin)) {
      stop("'margin' mus be numeric or character")
    }
    if (is.numeric(margin) & !is.element(margin, 1:number_of_margins)) {
      stop("'margin' must be in ", "1:", number_of_margins)
    }
    if (is.character(margin) & !is.element(margin, marginal_variable_names)) {
      stop_text <- paste0(marginal_variable_names, collapse = ", ")
      stop("'margin' must be in {", stop_text, "}")
    }
    if (is.numeric(margin)) {
      posterior_variable <- paste0("posterior for ",
                                   marginal_variable_names[margin])
      prior_variable <- paste0("prior for ",
                               marginal_variable_names[margin])
      variable_name <- marginal_variable_names[margin]
    }
    if (is.character(margin)) {
      posterior_variable <- paste0("posterior for ", margin)
      prior_variable <- paste0("prior for ", margin)
      variable_name <- margin
    }
    # Extract the required posterior distribution(s)
    posterior_x <- attr(x, posterior_variable)[[1]]
    # How many posterior distributions are included in x?
    n_posteriors <- length(x)
    # If required, add the corresponding prior distributions(s)
    if (prior) {
      prior_x <- attr(x, prior_variable)[[1]]
      x <- c(posterior_x, prior_x)
    }
  } else {
    # How many posterior distributions are included in x?
    n_posteriors <- length(x)
    # To add the prior distribution(s) extract them from attr(x, "prior")
    # Likewise for the likelihood function(s)
    if (prior) {
      x <- c(x, attr(x, "prior"))
    }
    if (likelihood) {
      x <- c(x, attr(x, "likelihood"))
    }
    variable_name <- attr(x, "variable_names")
  }
  #
  if (number_of_margins > 2 & is.null(margin)) {
    stop("Only 1D and 2D distributions can be plotted")
  } else if (number_of_margins == 2 & is.null(margin)) {
    if (missing(len)) {
      len <- 101
    }
    if (missing(p)) {
      p <- c(5, 25, 50, 75, 95)
    }
    plot_distribution_contours(x, prior = prior, likelihood = likelihood,
                               names = marginal_variable_names, len = len,
                               p = p, legend_args = legend_args,
                               digits = digits, ...)
    return(invisible(x))
  }
  # Extract the name of the distribution
  distn <- class(x)[1]
  # The number of parameters and their names
  np <- length(colnames(x))
  par_names <- colnames(x)
  n_distns <- length(x)
  # Create a title for the plot
  # If n_distns = 1 then place the parameter values in the title
  # If n_distns > 1 then place the parameter values in the legend
  if (n_distns > 1) {
    my_main <- paste0(distn, " (")
    if (np > 1) {
      for (i in 1:(np - 1)) {
        my_main <- paste0(my_main, par_names[i], ", ")
      }
    }
    my_main <- paste0(my_main, par_names[np], ")")
  } else {
    my_main <- paste0(distn, " (")
    # Round the parameter values to digits significant digits
    if (np > 1) {
      for (i in 1:(np - 1)) {
        my_main <- paste0(my_main, signif(as.matrix(x)[i], digits = digits),
                          ", ")
      }
    }
    my_main <- paste0(my_main, signif(as.matrix(x)[np], digits = digits), ")")
  }
  if (prior) {
    if (cdf) {
      my_main <- paste(my_main, "c.d.f.")
    } else {
      my_main <- paste(my_main, "p.d.f.")
    }
  } else {
    if (cdf) {
      my_main <- paste(my_main, "posterior c.d.f.")
    } else {
      my_main <- paste(my_main, "posterior p.d.f.")
    }
  }
  # Extract user-supplied arguments for graphics::plot()
  user_args <- list(...)
  # If xlim is supplied then use it.
  # Otherwise, use default values (but no -Inf or Inf)
  if (is.null(user_args[["xlim"]])) {
    my_xlim <- quantile(x, matrix(c(0, 1), nrow = 1), drop = FALSE)
    my_xlim <- c(min(my_xlim[, 1]), max(my_xlim[, 2]))
    my_xlim <- ifelse(
      is.finite(my_xlim),
      my_xlim,
      {
        tmp <- quantile(x, matrix(p / 100, nrow = 1), drop = FALSE)
        c(min(tmp[, 1]), max(tmp[, 2]))
      }
    )
    my_xlim <- range(my_xlim)
  } else {
    my_xlim <- user_args$xlim
  }
  # Set x and y axis labels, based on the name of the parameter
  # We include the discrete case, but this probably will not be used
  my_xlab <- variable_name
  if (cdf) {
    my_ylab <- paste0("F(", my_xlab, ")")
  } else {
    my_ylab <- paste0("f(", my_xlab, ")")
  }
  # Plot function for continuous distributions with defaults
  continuous_plot <- function(x, xvals, ..., xlab = my_xlab, ylab = my_ylab,
                              main = my_main, lwd = 2, col = my_col,
                              lty = my_lty) {
    if (cdf) {
      yvals <- t(distributions3::cdf(x, matrix(xvals, nrow = 1), drop = FALSE))
    } else {
      yvals <- t(distributions3::pdf(x, matrix(xvals, nrow = 1), drop = FALSE))
    }
    graphics::matplot(xvals, yvals, type = "l", xlab = xlab, ylab = ylab,
      axes = FALSE, lwd = lwd, lty = lty, main = main, col = col, ...)
    graphics::axis(1)
    graphics::axis(2)
    graphics::box(bty = "l")
    if (cdf) {
      graphics::abline(h = 0:1, lty = 2)
    }
    # If n_distns > 1 then add a legend
    if (n_distns > 1) {
      legend_args <- set_legend(x = x, prior = prior, likelihood = likelihood,
                                legend_args = legend_args, n_distns = n_distns,
                                n_posteriors = n_posteriors, digits = digits,
                                col = col, lwd = lwd, lty = lty)
      do.call(graphics::legend, legend_args)
    }
    return(invisible())
  }
  # Start the legend. If legend$x hasn't been supplied then set defaults
  if (is.null(legend_args[["x"]])) {
    if (cdf) {
      legend_args[["x"]] <- "bottomright"
    } else {
      legend_args[["x"]] <- "topright"
    }
  }
  xvals <- seq(my_xlim[1], my_xlim[2], length.out = len)
  #
  lty_col <- set_lty_col(prior = prior, likelihood = likelihood,
                         n_posteriors = n_posteriors)
  my_lty <- lty_col$my_lty
  my_col <- lty_col$my_col
  continuous_plot(x, xvals, ...)
  return(invisible(x))
}
