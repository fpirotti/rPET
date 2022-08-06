rPET - Physiological Equivalent Temperature
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rPET)](https://CRAN.R-project.org/package=rPET)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/fpirotti/rPET/workflows/R-CMD-check/badge.svg)](https://github.com/fpirotti/rPET/actions)

<!-- badges: end -->

The goal of rPET is to calculate Physiological Equivalent Temperature
(PET) adapted from Edouard Walther (AREP, France) and Quentin Goestchel
(ENS Paris-Saclay, France) based on: Peter Hoeppe s PET fortran code,
from the VDI Norm 3787, Blatt 2 and on : Djordje Spasic s python code.

<img src="man/figures/gif_filePMV.gif" /> Solar illumination is
simulated at points in space using a 3D model in voxel structure and a
ray-casting method.

<video src="https://user-images.githubusercontent.com/1391292/181439231-4d9c09ff-c552-499a-8d95-6ea988079e55.mp4" data-canonical-src="https://user-images.githubusercontent.com/1391292/181439231-4d9c09ff-c552-499a-8d95-6ea988079e55.mp4?width=354&amp;height=488" controls="controls" muted="muted" autoplay style="max-height:640px;">
</video>

<img src="man/figures/readme1.png" style="width:300px; max-width: 400px !important;"/>
<br>**Figure 1.** Example over a UAV lidar flight with 5000 points per
square meter.

the RayShader function was taken partly from the work of
<a href="https://github.com/tylermorganwall/rayshader" target="_blank">tylermorganwal’s rayshader for R</a>
but adapted to point clouds. For more detail see [Pirotti et al.
publication](https://github.com/tylermorganwall/rayshader "Taylemoraganwal Rayshader")

## Installation

**NOT YET AVAILABLE ON CRAN** You can install the released version of
rPET from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rPET")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fpirotti/rPET")
```
