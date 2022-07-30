.datatable.aware <- TRUE
globals <- new.env()

globals$heightraster <- NA
globals$addressraster <- NA
globals$lengthraster <- NA
globals$voxelSpace <- NA

globals$heightmap <- NA
globals$heightmapdsm <- NA
globals$addressmap <- NA
globals$lengthmap <- NA


globals$PointCloud3D <- NA
globals$pointcloud.cellsHeightMap.m <- NA
globals$originalheightmap <- NA
globals$maxheight <- NA

#'@title Calculate Raytraced Shadow Map with two elements: a surface and a point cloud representing obstructions
#'
#'@description Calculates shadow map for a elevation matrix by propagating rays from each matrix point to the light source(s),
#' lowering the brightness at each point for each ray that intersects the surface. A lot of by-products are saved in the global
#' package environment accessible by calling it environment(rPET::ray_shade).
#'
#'@param heightrasterFile A character string with the file name of a raster grid of a Digital Terrain Model.
#' NB  it will be converted internally to a matrix, where each entry in the matrix is the elevation of ground surface at that point. All points are assumed to be evenly spaced.
#'@param PointCloud3D A three-dimensional matrix or data frame with XYZ columns
#'@param sunaltitude Default `45`. The angle, in degrees (as measured from the horizon) from which the light originates. The width of the light
#'is centered on this value and has an angular extent of 0.533 degrees, which is the angular extent of the sun. Use the `anglebreaks` argument
#'to create a softer (wider) light. This has a hard minimum/maximum of 0/90 degrees.
#'@param sunangle Default `315` (NW). The angle, in degrees, around the matrix from which the light originates. Zero degrees is North, increasing clockwise.
#'@param maxsearch Defaults to the longest possible shadow given the `sunaltitude` and `heightmap`.
#'Otherwise, this argument specifies the maximum distance that the system should propagate rays to check.
#'@param lambert Default `TRUE`. Changes the intensity of the light at each point based proportional to the
#'dot product of the ray direction and the surface normal at that point. Zeros out all values directed away from
#'the ray.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation is in units
#'of meters and the grid values are separated by 10 meters, `zscale` would be 10.
#'@param multicore Default `FALSE`. If `TRUE`, multiple cores will be used to compute the shadow matrix. By default, this uses all cores available, unless the user has
#'set `options("cores")` in which the multicore option will only use that many cores.
#'@param height Default `1.5`. A matrix of 1 and 0s, indicating which points on which the raytracer will operate.
#'@param cache_mask Default `NULL`. Height from the ground plane for which to calculate the shade.
#'@param shadow_cache Default `NULL`. The shadow matrix to be updated at the points defined by the argument `cache_mask`.
#'If present, this will only compute the raytraced shadows for those points with value `1` in the mask.
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar.
#'@param anglebreaks Default `NULL`. A vector of angle(s) in degrees (as measured from the horizon) specifying from where the light originates.
#'Use this instead of `sunaltitude` to create a softer shadow by specifying a wider light. E.g. `anglebreaks = seq(40,50,by=0.5)` creates a light
#'10 degrees wide, as opposed to the default
#'@param useClass logical Default  `TRUE`.
#' Works only if you are using a LAS/LAZ point cloud, as it will use the standard classification values
#' if available in the file (see LAS SPECIFICATION VERSION 1.4 – R6 15 August 2011 [http://www.asprs.org]{http://www.asprs.org}{target="_blank"} . Helpful if you want to have almost real time rendering and do not want the first calculation to take too much time (see rPET::solarApp() )
#'@param onlyprepare Default  `FALSE` if you only want to cache the data structure. Helpful if you want to have almost real time rendering and do not want the first calculation to take too much time (see rPET::solarApp() )
#'@param makeVoxelGrid  Default  `FALSE`  will create a voxel space with the same resolution of cubes as the 2d resolution of the `heightrasterFile` DTM grid. The result will be saved in the global environment as `globals$voxelSpace`
#'@param force Default  `FALSE`  force data preparation. If false and if data has already been prepared, it will use the prepared data.
#'@param ... Additional arguments to pass to the `makeCluster` function when `multicore=TRUE`.
#'@import foreach doParallel parallel progress terra
#'@return Matrix of light intensities at each point.
#'@seealso ray_shade()
#'@export
#'@examples
#'#First we ray trace the Monterey Bay dataset.
ray_shade = function(heightrasterFile, PointCloud3D, sunaltitude=45, sunangle=315,
                     maxsearch=NULL, lambert=TRUE, zscale=1,
                    multicore = FALSE,
                    height = 1.5,
                    cache_mask = NULL, shadow_cache=NULL, progbar=interactive(),
                    anglebreaks = NULL,
                    useClass=TRUE,
                    onlyprepare = FALSE, makeVoxelGrid=FALSE, force=FALSE, ...) {





  if(is.null(anglebreaks)) {
    anglebreaks = seq(max(0,sunaltitude-0.533/2), min(90,sunaltitude+0.533/2), length.out = 10)
  }


  if(is.character(heightrasterFile)){
    if(!file.exists(heightrasterFile)) {
      warning("Raster file", heightrasterFile, "  does not exist")
      return(NULL)
    } else {
      heightraster <- terra::rast(heightrasterFile)
    }
  } else if(inherits(heightrasterFile,"SpatRaster")){

    nlyr<-tryCatch(terra::nlyr(heightrasterFile), error = function(e) e )
    if(inherits(nlyr, "simpleError")){
      warning(nlyr$message)
      return(NULL)
    }

    heightraster <- heightrasterFile

  } else {
    warning("heightrasterFile is of class ", class(heightrasterFile)[[1]], "  and is not accepted.")
    warning("heightrasterFile parameter should be a character string with path to file or a terra::rast (Spatraster) object")
    return(NULL)
  }

    heightmap <- raster_to_matrix(heightraster)
    originalheightmap = heightmap
    heightmap =  add_padding(heightmap)

  if(force || is.na(globals$pointcloud.cellsHeightMap.m) ||
     (!identical(PointCloud3D,globals$PointCloud3D) ||
     !identical(heightmap,globals$heightmap) ) ) {


    if(force || !identical(heightmap,globals$heightmap)){
      globals$heightraster <- heightraster
      globals$heightmap <- heightmap
      globals$originalheightmap <- originalheightmap

    }

    if(force || !identical(PointCloud3D,globals$PointCloud3D)){
      globals$PointCloud3D <- PointCloud3D
    }

    if(force) message("Forcing caching your data...")
    else message("Caching your data")


    ## list of index and DSM Z value -----
    pointcloud.cellsHeightMap <- data.table::as.data.table(na.omit(cbind(id=terra::cellFromXY(heightraster, PointCloud3D[,1:2]),
                                                     Z=PointCloud3D$Z )))


    pointcloud.cellsHeightMap$nZ <- ( pointcloud.cellsHeightMap$Z -
      heightraster[pointcloud.cellsHeightMap$id][,1] )
    pointcloud.cellsHeightMap<-na.omit(pointcloud.cellsHeightMap)


    ## nZ value -----
    #pointcloud.cellsHeightMap$Z <-  pointcloud.cellsHeightMap$Z - heightraster[pointcloud.cellsHeightMap$id]

    message("1 / 4 Height map calculated...")
    #data.table::setkey(pointcloud.cellsHeightMap, nZ)
    ## remove points below terrain + height surface ----- no shade will be casted from points below
    lw <- which(pointcloud.cellsHeightMap$nZ< 0)
    if(length(lw)>0){
      message(length(lw), " points below the terrain height map! they will be removed")
      pointcloud.cellsHeightMap<-pointcloud.cellsHeightMap[-lw,]
    }

    pointcloud.cellsHeightMap$nZ<-NULL
    # dd<-data.frame(id=terra::cells(heightraster),
    #                Z=heightraster[terra::cells(heightraster)][,1] )
    # pointcloud.cellsHeightMap <- data.table::rbindlist(list(pointcloud.cellsHeightMap, dd) )



    ## order by ID and Z so that are ordered.... do we need this??? if random it is ok for checking raycasting -----
    # ord<-order(pointcloud.cellsHeightMap[,"id"], pointcloud.cellsHeightMap[,"Z"])

    message("2 / 4  Ordering data, might take a bit......")
    #ord<-order(pointcloud.cellsHeightMap$id, pointcloud.cellsHeightMap$Z)
    data.table::setorderv(pointcloud.cellsHeightMap, c("id", "Z")  )
    globals$pointcloud.cellsHeightMap.m <-  pointcloud.cellsHeightMap #pointcloud.cellsHeightMap[ order(id, Z)]
    # globals$pointcloud.cellsHeightMap.m<-as.matrix(pointcloud.cellsHeightMap)


    heightraster[is.nan(heightraster)]<-NA

    if(makeVoxelGrid){

      message("2b / 4  Making Voxel map NOT ACTIVE...")

      # pointcloud.cellsHeightMap$dZids <- as.integer(floor( pointcloud.cellsHeightMap$Z / mean(res(heightraster)) ))
      #
      #
      # dt <- data.table::data.table(na.omit(pointcloud.cellsHeightMap))
      #
      #
      # nn<-dt[, lapply( .SD, function(x, ...){length(x)}, na.rm=TRUE), by=c("id","dZids") ]
      # nndsm<-dt[, lapply( .SD, function(x, ...){max(x, na.rm=TRUE)}, na.rm=TRUE), by=c("id") ]
      # nn$N <-nn$Z
      # nn$Z <- nn$dZids*mean(res(heightraster))
      # xy <- rbind( terra::xyFromCell(heightraster, nn$id),
      #              terra::xyFromCell(heightraster, terra::cells(heightraster)) )
      #
      #
      # message("2c / 4  FINISHED Voxel map...")
      # globals$voxelSpace <- data.frame(X=xy[,1], Y=xy[,2],
      #                         Z=c( nn$Z, heightraster[terra::cells(heightraster)][,1] ),
      #                         N=c(nn$N, rep(99999, length(terra::cells(heightraster))) )
      #                         )
      # globals$heightrasterdsm <- globals$heightraster
      # globals$heightrasterdsm[nndsm$id] <- nndsm$Z
      #
      # heightmapdsm <- raster_to_matrix(globals$heightrasterdsm)
      # globals$heightmapdsm =  add_padding(heightmapdsm)

    }


    ww<-which(!duplicated(globals$pointcloud.cellsHeightMap.m$id))
    addressGrid <- heightraster
    addressGrid[!is.na(addressGrid)] <- 0
    lengthGrid <- addressGrid
    # addressGrid[] <- as.integer(addressGrid[])
    # plot(addressGrid)
    addressGrid[ globals$pointcloud.cellsHeightMap.m$id[ww] ] <- ww

    message("3 / 4  Address grid created...")

    lengthLUT <- tabulate(globals$pointcloud.cellsHeightMap.m$id)
    message("3a / 4  Address grid created...")
    llut <- which(lengthLUT!=0)
    message("3b / 4  Address grid created...")

    message("3d / 4  Address grid created...")
    lengthGrid[ llut ] <- lengthLUT[llut]

    # png("p1.png", res = 250, width = 1200, height = 1300)
    #   plot( terra::project(addressGrid, "EPSG:4326"),  col= viridis::turbo(20) )
    # dev.off()
    #
    message("4 / 4  Length grid created...")

    globals$lengthraster <- lengthGrid
    globals$addressraster <- addressGrid

    globals$lengthmap <- raster_to_matrix(lengthGrid)
    globals$addressmap <- raster_to_matrix(addressGrid)


    globals$addressmap =  add_padding(globals$addressmap)
    globals$lengthmap =  add_padding(globals$lengthmap)

    globals$maxheight <- max(globals$pointcloud.cellsHeightMap.m$Z,na.rm=TRUE)

    globals$maxsearch = (globals$maxheight - min(globals$pointcloud.cellsHeightMap.m$Z,na.rm=TRUE))/(zscale*sinpi(min(anglebreaks[anglebreaks > 0])/180))

    message("Caching finised...")

  } else {
    message("Using cached data...")
  }

  if(onlyprepare){
    message("Data has been prepared...")
    return(TRUE)
  }



  if(all(anglebreaks <= 0)) {
    return(matrix(0,nrow=nrow(globals$heightmap),ncol=ncol(globals$heightmap)))
  }
  if(!is.null(maxsearch)) {
     globals$maxsearch <- maxsearch
  }
  anglebreaks = anglebreaks[order(anglebreaks)]
  anglebreaks_rad = anglebreaks*pi/180
  sunangle_rad = pi+sunangle*pi/180


  if(is.null(cache_mask)) {
    cache_mask = matrix(1,nrow = nrow(globals$heightmap),ncol=ncol(globals$heightmap))
  } else {
    padding = matrix(0,nrow(cache_mask)+2,ncol(cache_mask)+2)
    padding[2:(nrow(padding)-1),2:(ncol(padding)-1)] = cache_mask
    cache_mask = padding
  }
  if(!multicore) {
    message("running shadowmatrix")

    shadowmatrix <- tryCatch({
      rayshade_cpp(sunangle = sunangle_rad,
                   anglebreaks = anglebreaks_rad,
                   heightmap = flipud((globals$heightmap)),
                   addressmap = flipud((globals$addressmap)),
                   lengthmap = flipud((globals$lengthmap)),
                   pointcloud = as.matrix(globals$pointcloud.cellsHeightMap.m),
                   zscale = zscale,
                   maxsearch = globals$maxsearch,
                   maxheight=globals$maxheight,
                   cache_mask = cache_mask,
                   height=height,
                   progbar = progbar)
    },
    error=function(e) {
      print(e)
      return(NA)
    })

   if(!is.matrix(shadowmatrix)){
     return(NULL)
   }

    shadowmatrix = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
    cache_mask = cache_mask[c(-1,-nrow(cache_mask)),c(-1,-ncol(cache_mask))]
    shadowmatrix[shadowmatrix<0] = 0
    if(lambert) {
      # print(shadowmatrix)
      shadowmatrix = shadowmatrix * lamb_shade(globals$originalheightmap, sunaltitude = mean(anglebreaks),
                                                         sunangle = sunangle, zscale = zscale)
    }
    if(!is.null(shadow_cache)) {
      shadow_cache[cache_mask == 1] = shadowmatrix[cache_mask == 1]
      shadowmatrix = matrix(shadow_cache,nrow=nrow(shadowmatrix),ncol=ncol(shadowmatrix))
    }
    return(shadowmatrix)
  } else {
    stop("Multicore not supported yet")
    # if(is.null(options("cores")[[1]])) {
    #   numbercores = parallel::detectCores()
    # } else {
    #   numbercores = options("cores")[[1]]
    # }
    # if(nrow(heightmap) < numbercores*16) {
    #   if(nrow(heightmap) < 4) {
    #     chunksize = 1
    #   }
    #   chunksize = 4
    # } else {
    #   chunksize = 16
    # }
    # if(nrow(heightmap) %% chunksize == 0) {
    #   number_multicore_iterations = nrow(heightmap)/chunksize
    # } else {
    #   number_multicore_iterations = floor(nrow(heightmap)/chunksize) + 1
    # }
    # itervec = rep(1,number_multicore_iterations)
    # for(i in 0:number_multicore_iterations) {
    #   itervec[i+1] = 1 + i*chunksize
    # }
    # itervec[length(itervec)] = nrow(heightmap) + 1
    # cl = parallel::makeCluster(numbercores, ...)
    #
    # doParallel::registerDoParallel(cl, cores = numbercores)
    #
    # shadowmatrixlist = tryCatch({
    #   foreach::foreach(i=1:(length(itervec)-1), .export = c("rayshade_multicore","flipud")) %dopar% {
    #     rayshade_multicore(sunangle = sunangle_rad, anglebreaks = anglebreaks_rad,
    #                        heightmap = flipud(heightmap), zscale = zscale, chunkindices = c(itervec[i],(itervec[i+1])),
    #                        maxsearch = maxsearch, cache_mask = cache_mask)
    #   }
    # }, finally = {
    #   tryCatch({
    #     parallel::stopCluster(cl)
    #   }, error = function (e) {print(e)})
    # })
    # shadowmatrix = do.call(rbind,shadowmatrixlist)
    # shadowmatrix[shadowmatrix<0] = 0
    # shadowmatrix = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
    # cache_mask = cache_mask[c(-1,-nrow(cache_mask)),c(-1,-ncol(cache_mask))]
    # if(lambert) {
    #   shadowmatrix = shadowmatrix * lamb_shade(originalheightmap, sunaltitude = mean(anglebreaks),
    #                                           sunangle = sunangle, zscale = zscale)
    # }
    # if(!is.null(shadow_cache)) {
    #   shadow_cache[cache_mask == 1] = shadowmatrix[cache_mask == 1]
    #   shadowmatrix = matrix(shadow_cache,nrow=nrow(shadowmatrix),ncol=ncol(shadowmatrix))
    # }
    # return(shadowmatrix)
  }
}

globalVariables('i')
