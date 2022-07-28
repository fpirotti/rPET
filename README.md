
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rPET - Physiological Equivalent Temperature

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

Solar illumination is simulated at points in space using a 3D model in
voxel structure and a ray-casting method.
# Gallery / Cool things

You can open a Mixture graph just by double clicking any texture field in the inspector with a Mixture assigned to it.
![](docs/docfx/images/MixtureOpen.gif)

[Surface Gradient](https://blogs.unity3d.com/2019/11/20/normal-map-compositing-using-the-surface-gradient-framework-in-shader-graph/) powered normal map operations.
![](docs/docfx/images/NormalBlend.gif)

Extract buffers (depth, normal, color or position) from the rendering of a prefab and use it directly in the graph (HDRP Only).
![](docs/docfx/images/SceneCapture.gif)

Fractal nodes in Mixture:
![image](https://user-images.githubusercontent.com/6877923/102915300-d8944e00-4481-11eb-8e93-f7a57c21b830.png)

Mixture Variants:

https://user-images.githubusercontent.com/6877923/115474571-03c75800-a23e-11eb-8096-8973aad5fa9f.mp4


Earth Heightmap node:

https://user-images.githubusercontent.com/6877923/123006036-64e2e780-d3b7-11eb-922e-018994b32da5.mov

https://user-images.githubusercontent.com/6877923/115474571-03c75800-a23e-11eb-8096-8973aad5fa9f.mp4
<video src="https://github.com/fpirotti/rPET/blob/master/man/figures/sim.mp4" data-canonical-src="https://github.com/fpirotti/rPET/blob/master/man/figures/sim.mp4?width=354&height=488" controls="controls" muted="muted" style="max-height:640px;">
</video>
 
<img src="man/figures/readme1.png"   style="width:100%; max-width: 400px !important;"/>
Figure 1. Example over a UAV lidar flight with 5000 points per square
meter.

the RayShader function was taken partly from the work of
<a href="https://github.com/tylermorganwall/rayshader" target="_blank">tylermorganwal’s rayshader for R</a>
but adapted to point clouds. For more detail see [Pirotti et
al. publication](https://github.com/tylermorganwall/rayshader "Taylemoraganwal Rayshader")

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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rPET)
## basic example code
```
