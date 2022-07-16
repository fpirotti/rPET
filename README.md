
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rPET - Physiological Equivalent Temperature

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rPET)](https://CRAN.R-project.org/package=rPET)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/fpirotti/rPET/workflows/R-CMD-check/badge.svg)](https://github.com/fpirotti/rPET/actions)
<!-- badges: end -->

The goal of rPET is to calculate Physiological Equivalent Temperature
(PET) adapted from Edouard Walther (AREP, France) and Quentin Goestchel
(ENS Paris-Saclay, France) based on: Peter Hoeppe s PET fortran code,
from the VDI Norm 3787, Blatt 2 and on : Djordje Spasic s python code.
 
Solar illumination is simulated at points in space using a 3D model 
in voxel structure and a ray-casting method.
  
<img src="man/figures/readme1.png" style="max-width: 40% !important;"/>
Figure 1. Example over a UAV lidar flight with 5000 points per square meter.

## Installation

**NOT YET AVAILABLE ON CRAN** You can install the released version of rPET from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rPET")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fpirotti/rPET")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rPET)
## basic example code
```
