
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCBC NOAS functions <img src="man/figures/hex.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/noasr)](https://CRAN.R-project.org/package=noasr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/noasr/branch/main/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/noasr?branch=main)
[![R-CMD-check](https://github.com/LCBC-UiO/noasr/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/noasr/actions)
<!-- badges: end -->

The functions in this package are intended for use on the Nephew of All
Spreadsheets (NOAS and related files) in LCBC. The functions will mostly
not work on any other data, it is an in-house package of functions.

The package can be installed using remotes:

``` r
install.packages("remotes")
remotes::install_github("LCBC-UiO/noasr", build_vignettes = TRUE)
```

The functions are now installed, and you may load them when you want to
use them. All functions are documented in standard R fashion.

The package also has a vignette, to help you get started using it.

``` r
library(noasr)
vignette("noasr")
```

The vignette and other presentations or tutorials of the package can
also be found online [here](https://lcbc-uio.github.io/noasr/)
