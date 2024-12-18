% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LocationScaleT.R
\name{LocationScaleT}
\alias{LocationScaleT}
\title{Create a Location-Scale t distribution}
\usage{
LocationScaleT(mu = 0, sigma = 1, df)
}
\arguments{
\item{mu, sigma}{Parameters \eqn{\mu} (location) and \eqn{\sigma} (scale)
parameters. \code{sigma} must be positive.}

\item{df}{Degrees of freedom \eqn{\nu}. \code{df} must be positive.}
}
\value{
A \code{LocationScaleT} object, inheriting from class \code{"distribution"}.
Objects of class \code{"LocationScaleT"} have the following generic methods.
\itemize{
\item \code{\link[=mean]{mean()}}, \code{\link[distributions3:variance]{variance()}},
\code{\link[distributions3:variance]{skewness()}},
\code{\link[distributions3:variance]{kurtosis()}}.
\item \code{\link[distributions3:random]{random()}}, \code{\link[distributions3:pdf]{pdf()}},
\code{\link[distributions3:pdf]{log_pdf()}},
\code{\link[distributions3:cdf]{cdf()}},
\code{\link[distributions3:quantile.StudentsT]{quantile()}}.
\item \code{\link[distributions3:support]{support()}},
\code{\link[distributions3:is_discrete]{is_discrete()}},
\code{\link[distributions3:is_discrete]{is_continuous()}}.
}
}
\description{
Creates a t distribution with location \eqn{\mu} and scale
\eqn{\sigma} parameters, that is, \eqn{(X - \mu) / \sigma} has a Student's
t distribution.
}
\details{
See the Wikipedia page
\href{https://en.wikipedia.org/wiki/Student\%27s_t-distribution#Location-scale_t_distribution}{Location-scale t distribution}.
and \code{\link[distributions3:StudentsT]{StudentsT()}}.
}
\examples{
library(distributions3)
mu <- 10
sigma <- 2
X <- LocationScaleT(mu = mu, sigma = sigma, df = 6)
Y <- StudentsT(6)
c(mean(X), mean(Y))
c(variance(X), variance(Y))
c(skewness(X), skewness(Y))
c(kurtosis(X), kurtosis(Y))

c(pdf(X, 2), pdf(Y, (2 - mu) / sigma) / sigma)
c(log_pdf(X, 2), log_pdf(Y, (2 - mu) / sigma) - log(sigma))
c(cdf(X, 2), cdf(Y, (2 - mu) /sigma))
c(quantile(X, 0.75), sigma * quantile(Y, 0.75) + mu)
set.seed(42)
random(X, 5)
set.seed(42)
sigma * random(Y, 5) + mu
}
