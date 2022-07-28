## code to prepare `DATASET` dataset goes here
xyz<-  lidR::readLAS("data-raw/voxel_villabolasco_light.laz", select = "XYZ")

DATASET.bolasco <- list(gap.fraction = terra::wrap(terra::rast("data-raw/bolasco_GapFraction_2m.tif")),
                        dtm = terra::wrap(terra::rast("data-raw/bolasco_DTM_1m.tif")),
                        chm = terra::wrap(terra::rast("data-raw/bolasco_chm_1m.tif")),
                        dsm = terra::wrap(terra::rast("data-raw/bolasco_dsm_1m.tif")),
                        pointcloud = xyz@data
                        )
#terra::plot(DATASET, col=viridis::turbo(n=12), main="GAP Fraction Villa Bolasco" )
usethis::use_data(DATASET.bolasco, overwrite = TRUE)
