% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.posterior}
\alias{plot.posterior}
\title{Plot posterior and prior distributions}
\usage{
\method{plot}{posterior}(
  x,
  prior = TRUE,
  margin = NULL,
  cdf = FALSE,
  p = c(0.1, 99.9),
  len = 1000,
  legend_args = list(),
  digits = 3,
  ...
)
}
\arguments{
\item{x}{an object of class \code{c("name", "distribution")}, where
\code{"name"} is the name of the distribution.}

\item{prior}{A logical scalar. If \code{prior = TRUE} then include all
the prior distributions in the plot. Otherwise, plot only the posterior
distributions.}

\item{margin}{A numeric or character scalar giving the number or the name
of the marginal variable. \code{attr(x, "variable_names")} can be used to view
the names of the variables.}

\item{cdf}{A logical scalar.  If \code{cdf = TRUE} then the cumulative
distribution function (c.d.f.) is plotted.  Otherwise, the probability
density function (p.d.f.), for a continuous variable, or the probability
mass function (p.m.f.), for a discrete variable, is plotted.}

\item{p}{A numeric vector.  If \code{xlim} is not passed in \code{...}
then \code{p} is the fallback option for setting the range of values
over which the p.m.f, p.d.f. or c.d.f is plotted.  See \strong{Details}.}

\item{len}{An integer scalar.  If \code{x} is a continuous distribution
object then \code{len} is the number of values at which the p.d.f or
c.d.f. is evaluated to produce the plot.  The larger \code{len} is the
smoother is the curve.}

\item{legend_args}{A list of arguments to be passed to
\code{\link[graphics:legend]{legend()}}.  In particular, the argument \code{x}
(perhaps in conjunction with \code{legend_args$y}) can be used to set the
position of the legend.  If \code{legend_args$x} is not supplied then
\code{"bottomright"} is used if \code{cdf = TRUE} and \code{"topright"} if
\code{cdf = FALSE}.}

\item{digits}{The number of significant digits to be used in values of
parameters displayed in the title or legend.
See \code{\link[base:Round]{signif()}}.}

\item{...}{Further arguments to be passed to \code{\link[graphics:plot.default]{plot()}},
\code{\link[stats:ecdf]{ecdf()}} and \code{\link[graphics:lines]{lines()}},
such as \code{xlim}, \code{ylim}, \code{xlab}, \code{ylab}, \code{main}, \code{lwd}, \code{lty}, \code{col}, \code{pch}.}
}
\value{
The input object \code{x} is returned. If \code{prior = TRUE} then the
prior distribution(s) in \code{attr(x, "prior")} are appended to the posterior
distribution(s) in \code{x}.
}
\description{
Plot method for an object inheriting from class \code{"posterior"}.
Plots the posterior and corresponding prior density functions.
The cumulative distribution functions will be plotted if \code{cdf = TRUE}.
}
\details{
If \code{xlim} is passed in \code{...} then this determines the
range of values of the variable to be plotted on the horizontal axis.
If \code{xlim} is not passed in \code{...} then the range of values spans
the support of the distribution, with the following proviso: if the
lower (upper) endpoint of the distribution is \code{-Inf} (\code{Inf})
then the lower (upper) limit of the plotting range is set to the
\code{p[1]}percent (\code{p[2]}percent) quantile of the distribution.

Plots of c.d.f.s are produced using calls to
\code{\link[stats:approxfun]{approxfun()}} and \code{\link[stats:ecdf]{ecdf()}}.
}
\section{Examples}{

See the examples in \code{\link{posterior}}.
}

\seealso{
\code{\link{posterior}} for calculating products of functions for Bayesian
Inference using \code{"distribution"} objects.
}
