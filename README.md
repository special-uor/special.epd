
<!-- README.md is generated from README.Rmd. Please edit that file -->

# seepod

<!-- S<sub>PECIAL</sub> <sub>R</sub>e<sub>search group's version of the</sub> E<sub>uropean</sub> Po<sub>llen</sub> D<sub>atabase (EPD)</sub>  -->
<!-- *S*PECIAL R*e*search group's version of the *E*uropean *Po*llen *D*atabase (EPD)  -->

**S**PECIAL R**e**search group’s version of the **E**uropean **Po**llen
**D**atabase (EPD)
<!-- <img src="inst/images/logo.png" alt="logo" align="right" height=200px/> -->
<!-- badges: start -->
[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-yellow.svg)](https://github.com/special-uor/seepod)
[![R build
status](https://github.com/special-uor/seepod/workflows/R-CMD-check/badge.svg)](https://github.com/special-uor/seepod/actions)
[![](https://www.r-pkg.org/badges/version/seepod?color=black)](https://cran.r-project.org/package=seepod)
<!-- badges: end -->

The goal of `seepod` is to provide access to the SPECIAL Research
groups’s version of the European Pollen Database (EPD).

## Installation

You can(not) install the released version of `seepod` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("seepod")
```

And the development version from
[GitHub](https://github.com/special-uor/seepod) with:

``` r
# install.packages("remotes")
remotes::install_github("special-uor/seepod", "dev")
```

## Example

#### Load tables to working environment

``` r
data("site", package = "seepod")
data("entity", package = "seepod")
data("date_info", package = "seepod")
data("sample", package = "seepod")
```

#### Find entities link to sites

``` r
`%>%` <- seepod::`%>%`
ents <- site %>%
  dplyr::slice(1:4) %>%
  seepod::get_entities()
ents %>%
  knitr::kable()
```
