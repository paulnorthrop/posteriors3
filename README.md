
<!-- README.md is generated from README.Rmd. Please edit that file -->

# posteriors3

[![Appveyor Build
status](https://ci.appveyor.com/api/projects/status/99jojhgk9t4agdmv/branch/main?svg=true)](https://ci.appveyor.com/project/paulnorthrop/posteriors3/branch/main)
[![R-CMD-check](https://github.com/paulnorthrop/posteriors3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paulnorthrop/posteriors3/actions/workflows/R-CMD-check.yaml)
[![Coverage
Status](https://codecov.io/github/paulnorthrop/posteriors3/coverage.svg?branch=master)](https://app.codecov.io/github/paulnorthrop/posteriors3?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/posteriors3)](https://cran.r-project.org/package=posteriors3)
[![Downloads
(monthly)](https://cranlogs.r-pkg.org/badges/posteriors3?color=brightgreen)](https://cran.r-project.org/package=posteriors3)
[![Downloads
(total)](https://cranlogs.r-pkg.org/badges/grand-total/posteriors3?color=brightgreen)](https://cran.r-project.org/package=posteriors3)

## Univariate Bayesian Analyses using S3 Probability Distribution Objects

Provides functions to perform univariate Bayesian analyses using the S3
probability distribution objects provided by the
[distributions3](https://cran.r-project.org/package=distributions3)
package. A multiplication generic function is provided so that a
posterior distribution object can be created from a product of a
likelihood object and a prior distribution object. Therefore, the code
mimics the mathematics that students encounter when learning about
Bayesian inference. If a conjugate prior is used then the posterior
distribution object is also an S3 probability distribution object, with
a distribution of the same type as the prior object.

## An example

We perform a univariate Bayesian analysis based on a random sample from
a Binomial distribution using a conjugate Beta prior distribution for
the probability $p$.

``` r
library(distributions3)
library(posteriors3)
```

``` r
# Create a Binomial(5, 0.2) distribution object
N <- Binomial(size = 5, 0.2)
N
#> [1] "Binomial(size = 5, p = 0.2)"

# Draw a random sample of size 10 from this distribution
data <- random(N, n = 10)
data
#>  [1] 1 0 2 0 0 0 1 0 1 0

# Add the data to the object N as an attribute "data"
likelihood <- add_data(N, data)
attr(likelihood, "data")
#> [[1]]
#>  [1] 1 0 2 0 0 0 1 0 1 0

# Set a conjugate (Jeffreys) prior distribution
prior <- Beta(alpha = 1 / 2, beta = 1 / 2)
prior
#> [1] "Beta(alpha = 0.5, beta = 0.5)"

# The posterior distribution is proportional to likelihood * prior
posterior <- likelihood * prior
#> Posterior: Binomial likelihood, Beta prior
posterior
#> [1] "Beta(alpha = 5.5, beta = 45.5)"
```

This agrees with the result that the posterior is a Beta distribution
with parameters $\alpha + \sum_{i=1}^n x_i$ and
$\beta + \sum_{i=1}^n (5 - x_i)$, where here $\alpha = \beta = 1/2$ and
$(x_1, ..., x_{10}) = (1,0,2,0,0,0,1,0,1,0)$.

## Installation

To get the current released version from CRAN:

``` r
install.packages("posteriors3")
```
