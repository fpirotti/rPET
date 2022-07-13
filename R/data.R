#' Villa Bolasco Gap Fraction Raster
#'
#' A raster with Gap Fraction values at 1 and 2 m resolution. Load "terra" library.
#'
#'
#' @format A list with terra raster class items:
#' \describe{
#'   \item{gap.fraction}{terra rast with gap fraction values, from 0 to 100 in percentage, 2m resolution}
#'   \item{dtm}{terra rast object with digital terrain model, i.e. terrain height above sea level, 1 m  resolution}
#'   \item{dsm}{terra rast object with digital surface model, i.e. surface height above sea level, 1 m  resolution}
#'   \item{chm}{terra rast object with normalized digital surface model (nDSM), also canopy height model,
#'   i.e.  height above terrain of objects, 1 m  resolution}
#' }
#' @source \url{https://isprs-shy.cirgeo.unipd.it/layers/}
#' @examples
#'   #dtm <- terra::rast(rPET::DATASET.bolasco$dtm)
#'   #terra::plot(dtm, col=viridis::turbo(n=12), main="DTM of Villa Bolasco" )
"DATASET.bolasco"

