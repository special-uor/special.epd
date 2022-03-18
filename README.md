
<!-- README.md is generated from README.Rmd. Please edit that file -->

# special.epd: SPECIAL Research group’s version of the European Pollen Database (EPD)

<!-- S<sub>PECIAL</sub> <sub>R</sub>e<sub>search group's version of the</sub> E<sub>uropean</sub> Po<sub>llen</sub> D<sub>atabase (EPD)</sub>  -->
<!-- *S*PECIAL R*e*search group's version of the *E*uropean *Po*llen *D*atabase (EPD)  -->
<!-- **S**PECIAL R**e**search group's version of the **E**uropean **Po**llen **D**atabase (EPD)  -->
<!-- <img src="inst/images/logo.png" alt="logo" align="right" height=200px/> -->
<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-yellow.svg)](https://github.com/special-uor/special.epd)
[![R build
status](https://github.com/special-uor/special.epd/workflows/R-CMD-check/badge.svg)](https://github.com/special-uor/special.epd/actions)
[![](https://www.r-pkg.org/badges/version/special.epd?color=black)](https://cran.r-project.org/package=special.epd)
<!-- badges: end -->

The goal of `special.epd` is to provide access to the SPECIAL Research
groups’s version of the European Pollen Database (EPD).

## Installation

You can(not) install the released version of `special.epd` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("special.epd")
```

And the development version from
[GitHub](https://github.com/special-uor/special.epd) with:

``` r
# install.packages("remotes")
remotes::install_github("special-uor/special.epd", "dev")
```

## Example

#### Load tables to working environment

``` r
data("entity", package = "special.epd")
data("date_info", package = "special.epd")
data("sample", package = "special.epd")
```
