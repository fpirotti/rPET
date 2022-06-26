## code to prepare `DATASET` dataset goes here
library(terra)
DATASET<-terra::rast("data-raw/bolascoGapFractionTest.tif")
#terra::plot(DATASET, col=viridis::turbo(n=12), main="GAP Fraction Villa Bolasco" )
usethis::use_data(DATASET, overwrite = TRUE)
