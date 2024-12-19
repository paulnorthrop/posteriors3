#' Plot posterior and prior distributions
#'
#' Plot method for an object inheriting from class `"posterior"`.
#' Plots the posterior and corresponding prior density functions.
#' The cumulative distribution functions will be plotted if `cdf = TRUE`.
#'
#' @param x an object of class `c("name", "distribution")`, where
#'   `"name"` is the name of the distribution.
#' @param prior A logical scalar. If `prior = TRUE` then include all
#'   the prior distributions in the plot. Otherwise, plot only the posterior
#'   distributions.
#' @param margin A numeric or character scalar giving the number or the name
#'   of the marginal variable. `attr(x, "variable_names")` can be used to view
#'   the names of the variables.
#' @param cdf A logical scalar.  If `cdf = TRUE` then the cumulative
#'   distribution function (c.d.f.) is plotted.  Otherwise, the probability
#'   density function (p.d.f.), for a continuous variable, or the probability
#'   mass function (p.m.f.), for a discrete variable, is plotted.
#' @param p A numeric vector.  If `xlim` is not passed in `...`
#'   then `p` is the fallback option for setting the range of values
#'   over which the p.m.f, p.d.f. or c.d.f is plotted.  See **Details**.
#' @param len An integer scalar.  If `x` is a continuous distribution
#'   object then `len` is the number of values at which the p.d.f or
#'   c.d.f. is evaluated to produce the plot.  The larger `len` is the
#'   smoother is the curve.
#' @param legend_args A list of arguments to be passed to
#'   [`legend()`][graphics::legend()].  In particular, the argument `x`
#'   (perhaps in conjunction with `legend_args$y`) can be used to set the
#'   position of the legend.  If `legend_args$x` is not supplied then
#'   `"bottomright"` is used if `cdf = TRUE` and `"topright"` if
#'   `cdf = FALSE`.
#' @param digits The number of significant digits to be used in values of
#'   parameters displayed in the title or legend.
#'   See [`signif()`][base::signif].
#' @param ...  Further arguments to be passed to [`plot()`][graphics::plot()],
#'   [`ecdf()`][stats::ecdf] and [`lines()`][graphics::lines()],
#'   such as `xlim`, `ylim`, `xlab`, `ylab`, `main`, `lwd`, `lty`, `col`, `pch`.
#' @details If `xlim` is passed in `...` then this determines the
#'   range of values of the variable to be plotted on the horizontal axis.
#'   If `xlim` is not passed in `...` then the range of values spans
#'   the support of the distribution, with the following proviso: if the
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
plot.posterior <- function(x, prior = TRUE, margin = NULL, cdf = FALSE,
                           p = c(0.1, 99.9), len = 1000, legend_args = list(),
                           digits = 3, ...) {
  if (!distributions3::is_distribution(x)) {
    stop("use only with \"distribution\" objects")
  }
  # If marginal is non-NULL then attempt to extract from x a distribution
  # object relating to the required marginal posterior, and perhaps prior,
  # distributions
  if (!is.null(margin)) {
    marginal_variable_names <- attr(x, "variable_names")
    number_of_margins <- length(marginal_variable_names)
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
    print(class(posterior_x))
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
    if (prior) {
      x <- c(x, attr(x, "prior"))
    }
    variable_name <- attr(x, "variable_names")
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
  if (cdf) {
    my_main <- paste(my_main, "c.d.f.")
  } else {
    my_main <- paste(my_main, "p.d.f.")
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
#  variable_name <- attr(x, "variable_names")
  my_xlab <- variable_name
  if (cdf) {
    my_ylab <- paste0("F(", my_xlab, ")")
  } else {
    my_ylab <- paste0("f(", my_xlab, ")")
  }
  # Function to create the legend text
  create_legend_text <- function(x, n_distns) {
    leg_text <- numeric(n_distns)
    for (i in 1:n_distns) {
      text_i <- lapply(x, "[[", i)
      # Round the parameter values to digits significant digits
      text_i <- lapply(text_i, signif, digits = digits)
      leg_text[i] <- paste0(text_i, collapse = ", ")
    }
    return(leg_text)
  }
  # Plot function for continuous distributions with defaults
  continuous_plot <- function(x, xvals, ..., xlab = my_xlab, ylab = my_ylab,
                              main = my_main, lwd = 2, col = 1:n_posteriors,
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
      if (is.null(legend_args[["legend"]])) {
        legend_args$legend <- create_legend_text(x, n_distns)
      }
      if (is.null(legend_args[["title"]])) {
        if (prior) {
          legend_args$title <- paste("posterior", "prior", sep = "        ")
        } else {
          legend_args$title <- "posterior"
        }
      }
      if (is.null(legend_args[["col"]])) {
        legend_args$col <- col
      }
      if (is.null(legend_args[["lwd"]])) {
        legend_args$lwd <- lwd
      }
      if (is.null(legend_args[["lty"]])) {
        legend_args$lty <- lty
      }
      if (is.null(legend_args[["ncol"]])) {
        if (prior) {
          legend_args$ncol <- 2
        } else {
          legend_args$ncol <- 1
        }
      }
      do.call(graphics::legend, legend_args)
    }
  }
  # Start the legend. If legend$x hasn't been supplied then set defaults.
  if (is.null(legend_args[["x"]])) {
    if (cdf) {
      legend_args[["x"]] <- "bottomright"
    } else {
      legend_args[["x"]] <- "topright"
    }
  }
  xvals <- seq(my_xlim[1], my_xlim[2], length.out = len)
  #
  if (prior) {
    my_lty <- rep(1:2, each = n_posteriors)
  } else {
    my_lty = 1
  }
  continuous_plot(x, xvals, ...)
  return(invisible(x))
}
