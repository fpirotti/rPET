#'@title Raster to Matrix
#'
#'@description Turns a raster into a matrix suitable for rayshader. From {rayshader} package
#'
#'@param raster The input raster. Either a RasterLayer object, a terra SpatRaster object, or a filename.
#'@param verbose Default `interactive()`. Will print dimensions of the resulting matrix.
#'@export
#'@examples
#'#Save montereybay as a raster and open using the filename.

raster_to_matrix = function(raster, verbose = interactive()) {
  if(is.character(raster)) {
    if(!file.exists(raster)) {
      stop("Path ", raster, " does not exist.")
    }
    raster = terra::rast(raster)
  }
  return_mat = NA
  if(inherits(raster,"SpatRaster")) {
    if(length(find.package("terra", quiet = TRUE)) > 0) {
      raster_mat = terra::as.matrix(raster)
    } else {
      stop("{terra} package required if passing SpatRaster object")
    }
    return_mat = matrix(as.numeric(terra::values(raster)),
                  nrow = terra::ncol(raster), ncol = terra::nrow(raster))
  } else {
    stop("Was not able to convert object to a matrix: double check to ensure object is either of class `character`, pointint to a raster path or `SpatRaster`.")
  }
  if(verbose) {
    print(paste0("Dimensions of matrix are: ",
                 terra::ncol(return_mat),"x" ,
                 terra::nrow(return_mat)))
  }
  if(all(dim(return_mat) == 0)) {
    stop("Was not able to convert object to a matrix: double check to ensure object is either of class `character`, pointint to a raster path or `SpatRaster`.")
  }
  return(return_mat)
}
