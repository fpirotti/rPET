#' matrix2raster
#' @description converts a matrix to a terra::rast raster valuesì
#'
#' @param matrix 2d matrix with cell values
#' @param raster Default `NULL`. If a raster is provided, CRS and
#'
#' @return raster object from {terra} library
#' @export
#'
#' @examples
#' #
matrix2raster <- function(matrix, raster=NULL) {
  if(!is.matrix(matrix)) {
    stop("Not a matrix object")
  }

  if(!is.null(raster)){
    rast<-raster
  } else {
    rast<-terra::rast(ncol=nrow(matrix), nrow=ncol(matrix),
                ext = c(0,nrow(matrix),0, ncol(matrix)),
                crs=NA)

    rast[] <-  as.numeric(matrix[nrow(matrix):1,] )
    # plot(rast)
  }

  return(rast)
}
