pkg.globals <- new.env()
pkg.globals$heightraster <- NA
pkg.globals$heightmap <- NA
pkg.globals$addressmap <- NA
pkg.globals$PointCloud3D <- NA
pkg.globals$pointcloud.cellsHeightMap.m <- NA
pkg.globals$originalheightmap <- NA
pkg.globals$maxheight <- NA

#'@title Calculate Raytraced Shadow Map with two elements: a surface and a point cloud representing obstructions
#'
#'@description Calculates shadow map for a elevation matrix by propagating rays from each matrix point to the light source(s),
#' lowering the brightness at each point for each ray that intersects the surface.
#'
#'@param heightraster A file or raster object  (regular grid) with Digital Terrain Model. NB not a two-dimensional matrix like the ray_shade function, as it will be converted internally, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
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
#'@param cache_mask Default `NULL`. A matrix of 1 and 0s, indicating which points on which the raytracer will operate.
#'@param shadow_cache Default `NULL`. The shadow matrix to be updated at the points defined by the argument `cache_mask`.
#'If present, this will only compute the raytraced shadows for those points with value `1` in the mask.
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar.
#'@param anglebreaks Default `NULL`. A vector of angle(s) in degrees (as measured from the horizon) specifying from where the light originates.
#'Use this instead of `sunaltitude` to create a softer shadow by specifying a wider light. E.g. `anglebreaks = seq(40,50,by=0.5)` creates a light
#'10 degrees wide, as opposed to the default
#'@param onlyprepare Default  `FALSE` if you only want to cache the data structure. Helpful if you want to have almost real time rendering and do not want the first calculation to take too much time (see rPET::solarApp() )
#'@param ... Additional arguments to pass to the `makeCluster` function when `multicore=TRUE`.
#'@import foreach doParallel parallel progress terra
#'@return Matrix of light intensities at each point.
#'@seealso ray_shade()
#'@export
#'@examples
#'#First we ray trace the Monterey Bay dataset.
ray_shade = function(heightraster, PointCloud3D, sunaltitude=45, sunangle=315, maxsearch=NULL, lambert=TRUE, zscale=1,
                    multicore = FALSE, cache_mask = NULL, shadow_cache=NULL, progbar=interactive(),
                    anglebreaks = NULL, onlyprepare = FALSE, ...) {





  if(is.null(anglebreaks)) {
    anglebreaks = seq(max(0,sunaltitude-0.533/2), min(90,sunaltitude+0.533/2), length.out = 10)
  }

  if(!identical(PointCloud3D,pkg.globals$PointCloud3D) ||
     !identical(heightraster,pkg.globals$heightraster) ) {

    if(!identical(heightraster,pkg.globals$heightraster)){
      pkg.globals$heightraster <- heightraster
    }

    if(!identical(PointCloud3D,pkg.globals$PointCloud3D)){
      pkg.globals$PointCloud3D <- PointCloud3D
    }
    message("Caching your data...")
    if(is.character(pkg.globals$heightraster)){
      if(!file.exists(pkg.globals$heightraster)) {
        stop("Raster file does not exist")
      }
      heightgrid <- terra::rast(pkg.globals$heightraster)
    }

    ## list of index and DSM Z value -----
    pointcloud.cellsHeightMap <- na.omit(data.frame(id=terra::cellFromXY(heightgrid, PointCloud3D[,1:2]), Z=PointCloud3D[,3]))

    ## nZ value -----
    #pointcloud.cellsHeightMap$Z <-  pointcloud.cellsHeightMap$Z - heightgrid[pointcloud.cellsHeightMap$id]
    pointcloud.cellsHeightMap <- na.omit(pointcloud.cellsHeightMap)

    message("1 / 4 Height map calculated...")

    ## remove points below terrain surface -----
    lw <- which(pointcloud.cellsHeightMap[,"Z"]<0)
    if(length(lw)>0){
      message(length(lw), " points below the terrain height map! they will be removed")
      pointcloud.cellsHeightMap<-pointcloud.cellsHeightMap[-lw,]
    }


    ## order by ID and Z so that are ordered.... do we need this??? if random it is ok for checking raycasting -----
    # ord<-order(pointcloud.cellsHeightMap[,"id"], pointcloud.cellsHeightMap[,"Z"])
    ord<-order(pointcloud.cellsHeightMap[,"id"])
    pkg.globals$pointcloud.cellsHeightMap.m<-pointcloud.cellsHeightMap[ord,]
    # pkg.globals$pointcloud.cellsHeightMap.m<-as.matrix(pointcloud.cellsHeightMap)

    message("2 / 4  Height voxel map to matrix  ordered by id only...")
    ww<-which(!duplicated(pkg.globals$pointcloud.cellsHeightMap.m[,"id"]))

    heightgrid[is.nan(heightgrid)]<-NA

    addressGrid <- heightgrid
    addressGrid[] <- NA
    lengthGrid <- addressGrid
    # addressGrid[] <- as.integer(addressGrid[])
    # plot(addressGrid)
    addressGrid[ pkg.globals$pointcloud.cellsHeightMap.m[ww, "id"] ] <- ww

    message("3 / 4  Address grid created...")

    lengthLUT <- tabulate(pkg.globals$pointcloud.cellsHeightMap.m[,"id"])
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

    pkg.globals$lengthmap <- raster_to_matrix(lengthGrid)
    pkg.globals$addressmap <- raster_to_matrix(addressGrid)

    pkg.globals$heightmap <- raster_to_matrix(heightgrid)
    pkg.globals$originalheightmap = pkg.globals$heightmap
    pkg.globals$heightmap =  add_padding(pkg.globals$heightmap)
    pkg.globals$addressmap =  add_padding(pkg.globals$addressmap)

    pkg.globals$maxheight <- max(pkg.globals$heightmap,na.rm=TRUE)

    pkg.globals$maxsearch = (max(pkg.globals$heightmap,na.rm=TRUE) - min(pkg.globals$heightmap,na.rm=TRUE))/(zscale*sinpi(min(anglebreaks[anglebreaks > 0])/180))

    message("Caching finised...")

  } else {
    message("Using cached data...")
  }

  if(onlyprepare){
    message("Data has been prepared...")
    return(NULL)
  }



  if(all(anglebreaks <= 0)) {
    return(matrix(0,nrow=nrow(pkg.globals$heightmap),ncol=ncol(pkg.globals$heightmap)))
  }
  if(!is.null(maxsearch)) {
     pkg.globals$maxsearch <- maxsearch
  }
  anglebreaks = anglebreaks[order(anglebreaks)]
  anglebreaks_rad = anglebreaks*pi/180
  sunangle_rad = pi+sunangle*pi/180


  if(is.null(cache_mask)) {
    cache_mask = matrix(1,nrow = nrow(pkg.globals$heightmap),ncol=ncol(pkg.globals$heightmap))
  } else {
    padding = matrix(0,nrow(cache_mask)+2,ncol(cache_mask)+2)
    padding[2:(nrow(padding)-1),2:(ncol(padding)-1)] = cache_mask
    cache_mask = padding
  }
  if(!multicore) {
    message("running shadowmatrix")
    shadowmatrix = rayshade_cpp(sunangle = sunangle_rad,
                                anglebreaks = anglebreaks_rad,
                                heightmap = flipud(as.matrix(pkg.globals$heightmap)),
                                addressmap = flipud(as.matrix(pkg.globals$addressmap)),
                                lengthmap = flipud(as.matrix(pkg.globals$lengthmap)),
                                pointcloud = as.matrix(pkg.globals$pointcloud.cellsHeightMap.m),
                                zscale = zscale,
                                maxsearch = pkg.globals$maxsearch,
                                maxheight=pkg.globals$maxheight,
                                cache_mask = cache_mask,
                                progbar = progbar)


    shadowmatrix = shadowmatrix[c(-1,-nrow(shadowmatrix)),c(-1,-ncol(shadowmatrix))]
    cache_mask = cache_mask[c(-1,-nrow(cache_mask)),c(-1,-ncol(cache_mask))]
    shadowmatrix[shadowmatrix<0] = 0
    if(lambert) {
      shadowmatrix = shadowmatrix * lamb_shade(pkg.globals$originalheightmap, sunaltitude = mean(anglebreaks),
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
