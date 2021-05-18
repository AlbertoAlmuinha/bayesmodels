
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesmodels

<img src="vignettes/logo-bayesmodels.png" width="147" height="170" align="right" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bayesmodels)](https://CRAN.R-project.org/package=bayesmodels)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/bayesmodels?color=brightgreen)](https://cran.r-project.org/package=bayesmodels)
![](http://cranlogs.r-pkg.org/badges/bayesmodels?color=brightgreen)
[![codecov](https://codecov.io/gh/AlbertoAlmuinha/bayesmodels/branch/master/graph/badge.svg?token=0OXB8WLRJX)](https://codecov.io/gh/AlbertoAlmuinha/bayesmodels)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/AlbertoAlmuinha/bayesmodels/workflows/R-CMD-check/badge.svg)](https://github.com/AlbertoAlmuinha/bayesmodels/actions)
<!-- badges: end -->

> A parsnip backend for `Bayesian` models in the `tidymodels` framework.

## Tutorials

-   [**Bayesmodels and Modeltime
    Integration**](https://albertoalmuinha.github.io/bayesmodels/articles/modeltime-integration.html):
    Learn how to integrate bayesian models with the modeltime ecosystem.

## Installation

CRAN version

``` r
install.packages("bayesmodels")
```

Development version:

``` r
# install.packages("devtools")
devtools::install_github("AlbertoAlmuinha/bayesmodels")
```

## Why Bayesmodels?

> Bayesmodels unlocks multiple bayesian models in one framework.In
> addition, it allows you to integrate these models with the Modeltime
> and the Tidymodels ecosystems.

<img src="vignettes/portada.png" width="100%" style="display: block; margin: auto;" />

In a single framework you will be able to find:

-   **Sarima**: `bayesmodels` connects to the `bayesforecast` package.

-   **Garch**: `bayesmodels` connects to the `bayesforecast` package.

-   **Random Walk (Naive)**: `bayesmodels` connects to the
    `bayesforecast` package.

-   **State Space Model**: `bayesmodels` connects to the `bayesforecast`
    and `bsts` packages.

-   **Stochastic Volatility Model**: `bayesmodels` connects to the
    `bayesforecast` package.

-   **Generalized Additive Models (GAMS)**: `bayesmodels` connects to
    the `brms` package.

-   **Adaptive Splines Surface**: `bayesmodels` connects to the `BASS`
    package.

-   **Exponential Smoothing**: `bayesmodels` connects to the `Rglt`
    package.
