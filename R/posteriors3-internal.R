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

#' @keywords internal
#' @rdname posteriors3-internal
plot_distribution_contours <- function(x, prior, likelihood, names, len, p,
                                       legend_args, digits, ...) {
  # Extract the the arguments passed in ...
  dots_args <- list(...)
  # Note: the prior distributions have already been added to x
  # Extract the marginal distributions
  posterior_variable <- paste0("posterior for ", names)
  prior_variable <- paste0("prior for ", names)
  likelihood_variable <- paste0("likelihood for ", names)
  variable_name <- names
  # Find the number of posteriors (and priors)
  n_posteriors <- length(attr(x, posterior_variable[[1]])[[1]])
  # The total number of distributions (posteriors and priors)
  n_distns <- length(x)
  # Function to find the extreme quantiles of the marginals
  quantile_function <- function(i, j, variable, probs, prior = FALSE,
                                likelihood = FALSE) {
    if (prior) {
      y <- attr(x, prior_variable[[j]])[[1]][i]
    } else if (likelihood) {
      y <- attr(x, likelihood_variable[[j]])[[1]]
    } else {
      y <- attr(x, posterior_variable[[j]])[[1]][i]
    }
    return(quantile(y, probs = probs))
  }
  # Convert percentages to probabilities
  prob <- p / 100
  # Set the default posterior and prior ranges
  prob_range <- range(prob)
  # Extend the extreme probabilities a little to try to include all contours
  prob_range[1] <- prob_range[1] / 10
  prob_range[2] <- 1 - (1 - prob_range[2]) / 10
  # Set the area over which the pdf is contoured using xlim and/or ylim, if
  # these are supplied in ..., or the default based on prob
  if (!is.null(dots_args$xlim)) {
    x_range <- dots_args$xlim
  } else {
    x_range <- range(sapply(1:n_posteriors, quantile_function, j = 1,
                            probs = prob_range, prior = FALSE))
    if (prior) {
      x_prior_range <- range(sapply(1:n_posteriors, quantile_function, j = 1,
                                    probs = prob_range, prior = TRUE))
      x_range <- range(x_range, x_prior_range)
    }
    if (likelihood) {
      x_likelihood_range <- range(sapply(1, quantile_function, j = 1,
                                    probs = prob_range, likelihood = TRUE))
      x_range <- range(x_range, x_likelihood_range)
    }
  }
  if (!is.null(dots_args$ylim)) {
    y_range <- dots_args$ylim
  } else {
    y_range <- range(sapply(1:n_posteriors, quantile_function, j = 2,
                            probs = prob_range, prior = FALSE))
    if (prior) {
      y_prior_range <- range(sapply(1:n_posteriors, quantile_function, j = 2,
                                    probs = prob_range, prior = TRUE))
      y_range <- range(y_range, y_prior_range)
    }
    if (likelihood) {
      y_likelihood_range <- range(sapply(1, quantile_function, j = 2,
                                         probs = prob_range, likelihood = TRUE))
      y_range <- range(y_range, y_likelihood_range)
    }
  }
  xx <- seq(x_range[1], x_range[2], length.out = 101)
  yy <- seq(y_range[1], y_range[2], length.out = 101)
  xxyy <- as.matrix(expand.grid(xx, yy))
  # Extract the name of the distribution
  distn <- class(x)[1]
  # The number of parameters and their names
  np <- length(colnames(x))
  par_names <- colnames(x)
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
    my_main <- paste(my_main, "p.d.f.")
  } else {
    my_main <- paste(my_main, "posterior p.d.f.")
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
  # Create the contour plot of posterior and prior densities
  # Function for calculating the pdf
  pdf_fun <- function(xx, yy, distn_object) {
    pdf(distn_object, matrix(c(xx, yy), ncol = 2))
  }
  # Create a general contour_plot function
  plot_contour <- function(which_distn, add, lty, col, distn_objects, ...,
                           xlab = my_xlab, ylab = my_ylab, main = my_main,
                           lwd = my_lwd) {
    zz <- outer(xx, yy, FUN = pdf_fun,
                distn_object = distn_objects[which_distn])
    dx <- diff(xx[1:2])
    dy <- diff(yy[1:2])
    sz <- sort(zz)
    c1 <- cumsum(sz) * dx * dy
    c1 <- c1 / max(c1)
    con_levs <- suppressWarnings(sapply(prob, function(x)
      stats::approx(c1, sz, xout = 1 - x)$y))
    graphics::contour(xx, yy, zz, levels = con_levs, add = add, ann = FALSE,
                      labels = prob * 100, xlab = xlab, ylab = ylab,
                      main = main, lwd = lwd, lty = lty, col = col, ...)
    return(invisible())
  }
  # Vectors for adding to first contour plot and line types and colours
  add_vec <- c(FALSE, rep(TRUE, n_distns - 1))
  lty_col <- set_lty_col(prior = prior, likelihood = likelihood,
                         n_posteriors = n_posteriors)
  lty_vec <- lty_col$my_lty
  col_vec <- lty_col$my_col
  # Default graphics parameters
  my_xlab <- names[1]
  my_ylab <- names[2]
  my_lwd <- 2
  # Allow user-override of lty and col
  save_dots_args <- dots_args
  if (!is.null(dots_args$lty)) {
    lty_vec <- dots_args$lty
    save_dots_args["lty"] <- NULL
  }
  if (!is.null(dots_args$col)) {
    col_vec <- dots_args$col
    save_dots_args["col"] <- NULL
  }
  if (!is.null(dots_args$lwd)) {
    lwd_vec <- dots_args$lwd
  } else {
    lwd_vec <- my_lwd
  }
  MoreArgs <- c(list(distn_objects = x), save_dots_args)
  lty_vec <- lty_vec[1:n_distns]
  col_vec <- rep_len(col_vec, n_distns)
  mapply(FUN = plot_contour, which_distn = 1:n_distns, add = add_vec,
         lty = lty_vec, col = col_vec, MoreArgs = MoreArgs)
  # If n_distns > 1 then add a legend
  if (n_distns > 1) {
    legend_args <- set_legend(x = x, prior = prior, likelihood = likelihood,
                              legend_args = legend_args, n_distns = n_distns,
                              n_posteriors = n_posteriors, digits = digits,
                              col = col_vec, lwd = lwd_vec, lty = lty_vec)
    # If legend$x hasn't been supplied then set defaults
    if (is.null(legend_args[["x"]])) {
      legend_args[["x"]] <- "topright"
    }
    zeros <- length(legend_args[["legend"]]) - length(legend_args[["col"]])
    legend_args[["col"]] <- c(legend_args[["col"]], rep(0, zeros))
    legend_args[["lty"]] <- c(legend_args[["lty"]], rep(0, zeros))
    do.call(graphics::legend, legend_args)
  }
  if (likelihood) {
    likelihood_legend_pos <- switch(legend_args[["x"]],
                                    bottomright = "bottomleft",
                                    bottom = "bottomleft",
                                    bottomleft = "bottomright",
                                    left = "right",
                                    topleft = "topright",
                                    top = "topleft",
                                    topright = "topleft",
                                    right = "left",
                                    center = "left")
    graphics::legend(x = likelihood_legend_pos, legend = "likelihood",
                     lty = 3, col = ifelse(n_posteriors > 1, 8, 1),
                     lwd = lwd_vec[length(lwd_vec)])
  }
  return(invisible())
}

#' @keywords internal
#' @rdname posteriors3-internal
set_lty_col <- function(prior, likelihood, n_posteriors) {
  if (prior & !likelihood) {
    my_lty <- rep(1:2, each = n_posteriors)
    my_col <- rep(1:n_posteriors, times = n_posteriors)
  } else if (!prior & likelihood) {
    if (n_posteriors > 1) {
      my_lty <- rep(c(1, 3), each = n_posteriors)
      my_col <- c(1:n_posteriors, rep(8, n_posteriors))
      to_remove <- (n_posteriors + 2):(2 * n_posteriors)
      my_lty[to_remove] <- 0
      my_col[to_remove] <- 0
    } else {
      my_lty <- c(1, 3)
      my_col <- 1
    }
  } else if (prior & likelihood) {
    if (n_posteriors > 1) {
      my_lty <- rep(1:3, each = n_posteriors)
      my_col <- c(rep(1:n_posteriors, times = 2), rep(8, n_posteriors))
      to_remove <- (2 * (n_posteriors + 1)):(3 * n_posteriors)
      my_lty[to_remove] <- 0
      my_col[to_remove] <- 0
    } else {
      my_lty <- 1:3
      my_col <- 1
    }
  } else {
    my_lty <- 1
    my_col <- 1:n_posteriors
  }
  return(list(my_lty = my_lty, my_col = my_col))
}

#' @keywords internal
#' @rdname posteriors3-internal
set_legend <- function(x, prior, likelihood, legend_args, n_distns,
                       n_posteriors, digits, col, lwd, lty) {
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
  if (is.null(legend_args[["legend"]])) {
    legend_args$legend <- create_legend_text(x, n_distns)
    if (!prior & likelihood) {
      legend_args$legend <- c(legend_args$legend, rep("", n_posteriors - 1))
    }
    if (prior & likelihood) {
      legend_args$legend <- c(legend_args$legend, rep("", n_posteriors - 1))
    }
  }
  if (is.null(legend_args[["title"]])) {
    if (prior & !likelihood) {
      legend_args$title <- paste("posterior", "prior", sep = "        ")
    } else if (!prior & likelihood) {
      legend_args$title <- paste("posterior", "likelihood", sep = "        ")
    } else if (prior & likelihood) {
      legend_args$title <- paste("posterior", "prior", "likelihood",
                                 sep = "        ")
    } else {
      legend_args$title <- NULL
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
    legend_args$ncol <- 1
    if (prior) {
      legend_args$ncol <- legend_args$ncol + 1
    }
    if (likelihood) {
      legend_args$ncol <- legend_args$ncol + 1
    }
  }
  return(legend_args)
}
