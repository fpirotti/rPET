dataenv <- new.env()

dataenv$pointcloud <- NA
dataenv$dtm <- NA
dataenv$dsm <- NA
dataenv$chm <- NA
dataenv$gapfraction <- NA


#'@title Prepare Example Data
#'
#'@description Prepares data by downloading and creating an environmental variable that you
#'can access by e<-environment(rPET::prepareData) and e$dataenv
#'@param force, boolean, DEFAULT = TRUE.
#'@import lidR
#'@return boolean with TRUE on success

#'@export
#'@examples
#'#First we ray trace the Monterey Bay dataset.
prepareData = function(force=FALSE){

  if(force ||
     is.na(dataenv$pointcloud)||
     is.na(dataenv$dtm) ){

    message("Downloading Villa Bolasco Data using your Internet, should take less than a minute...")

    out <- tryCatch({
      a <- list()
      aa<-tempfile(fileext = ".laz")
      utils::download.file("https://github.com/fpirotti/rPET/raw/master/data-raw/voxel_villabolasco_light.laz", aa)
      dd<- lidR::readLAS(aa)
      dataenv$pointcloud <- dd@data[,1:3]
      file.remove(aa)
      aa<-tempfile(fileext = ".tif")
      utils::download.file("https://github.com/fpirotti/rPET/raw/master/data-raw/bolasco_DTM_1m.tif", aa)
      dataenv[["dtm"]] <- terra::wrap(terra::rast(aa))
      file.remove(aa)
      aa<-tempfile(fileext = ".tif")
      utils::download.file("https://github.com/fpirotti/rPET/raw/master/data-raw/bolasco_chm_1m.tif", aa)
      dataenv[["chm"]] <- terra::wrap(terra::rast(aa))
      file.remove(aa)
      aa<-tempfile(fileext = ".tif")
      utils::download.file("https://github.com/fpirotti/rPET/raw/master/data-raw/bolasco_dsm_1m.tif", aa)
      dataenv[["dsm"]] <- terra::wrap(terra::rast(aa))
      file.remove(aa)
      aa<-tempfile(fileext = ".tif")
      utils::download.file("https://github.com/fpirotti/rPET/raw/master/data-raw/bolasco_GapFraction_2m.tif", aa)
      dataenv[["gap.fraction"]] <- terra::wrap(terra::rast(aa))
      file.remove(aa)
      a
    },
    error = function(e){
      message("Downloading data failed!")
      message("Error Message:")
      message(e)
      return(FALSE)
    })

  } else {
    message("Dataset already available")
    return(TRUE)
  }
  return(TRUE)
}
