## code to prepare `DATASET` dataset goes here
DATASET.bolasco <- list(gap.fraction = terra::wrap(terra::rast("data-raw/bolasco_GapFraction_2m.tif")),
                        dtm = terra::wrap(terra::rast("data-raw/bolasco_DTM_1m.tif")),
                        chm = terra::wrap(terra::rast("data-raw/bolasco_chm_1m.tif"))
                        )
#terra::plot(DATASET, col=viridis::turbo(n=12), main="GAP Fraction Villa Bolasco" )
usethis::use_data(DATASET.bolasco, overwrite = TRUE)
