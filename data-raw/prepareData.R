library(terra)
library(insol)
library(lidR)
library(rPET)
library(gifski)
## read voxellized point cloud from lastools using LidR
## MUST USE a POINT CLOUD AND A DTM SURFACE!!!! convert dtm to heightmap then map all points from
## point cloud to each cell . Z vaues from point cloud get linearized to a single vector,
## and values of heightmap substitute Z to index of
## doti <- function(heightgrid, pointcloud, sunelevation, sunangle)
zscale<-1; sunelevation=90; sunangle=200; sunaltitude<-sunelevation;
xyz<-  lidR::readLAS("data-raw/voxel_villabolasco_light.laz", select = "XYZ")
pointcloud<-xyz@data
PointCloud3D <- pointcloud

heightraster<-"data-raw/bolasco_DTM_1m.tif"
Sys.Date()
Sys.Date()+1

makeplot <- function(){

  ss<-seq(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by="min")
  dd <- data.frame(insol::sunpos(insol::sunvector(insol::JD(ss), 46, 11.9701, 0)))
  dd$date <- ss
  dd2<-dd[dd$zenith<84,]
  dd3 <- dd2[as.integer(rownames(dd2))%%8==0, ]
  dd3$date <- substr(dd3$date, 1, 16)
  nn <- 0
  totnn <- nrow(dd3)

  apply( dd3, 1, function(sp){

    sunangle = as.numeric(sp[[1]])
    sunaltitude = 90 - as.numeric(sp[[2]])
    rs <- rPET::ray_shade(heightraster, PointCloud3D, sunangle = sunangle,zscale = 1,   #onlyprepare = TRUE,
                          sunaltitude = sunaltitude,
                          makeVoxelGrid = FALSE, lambert = TRUE)

    rs %>%
      # rayshader::sphere_shade() %>%
      # rayshader::add_overlay(
          # rayshader::generate_altitude_overlay(
            # rayshader::raster_to_matrix( terra::rast(rPET::DATASET.bolasco$dtm)), 0, 0) )   %>%
      rayshader::plot_map(  )
      text(100,480,sp[[3]], cex=0.8)

  })

}

tt<-terra::rast(heightraster)
save_gif(makeplot(), "gif_file.gif", ncol(tt), nrow(tt),delay = 0.1, loop = FALSE,  res = 144)

e<-environment(rPET::ray_shade)
e$globals$pointcloud.cellsHeightMap.m
rs.rast <- rPET::matrix2raster(rs)
plot(rs.rast)


summary(rs)
rs %>% rayshader::plot_map()





rs2 <- rayshader::ray_shade( heightmap = e$globals$heightmapdsm , sunangle = 100,
                             anglebreaks = seq(14,19, by=1), onlyprepare = FALSE,
                      makeVoxelGrid = FALSE, lambert = FALSE)

rs2 %>% rayshader::plot_map()
rs.rast2 <- rPET::matrix2raster(rs2)
plot(rs.rast2)


rayshader::raster_to_matrix()
summary( as.numeric(rs))
e<-environment(rPET::ray_shade)
summary(e$globals$addressmap)
plot(e$globals$addressraster)

View(e$globals$pointcloud.cellsHeightMap.m)

lasheader <- lidR::LASheader(round(e$globals$voxelSpace,2))

las <- lidR::LAS(e$globals$voxelSpace, header =  lasheader)
lidR::writeLAS(las,"voxel.laz")

write.csv(round(e$globals$voxelSpace,2), "voxel.csv" )
x <- y <- z <- seq(-2, 2, length.out = 70)
xyz <- mesh(x, y, z)
F <- with(xyz, log(x^2 + y^2 + z^2 +
                     10*(x^2 + y^2) * (y^2 + z^2) ^2))
voxel3D(PointCloud3D$X, PointCloud3D$Y, PointCloud3D$Z, F, level = 4, pch = ".", cex = 5)





e<-environment(rPET::ray_shade)
plot(e$pkg.globals$addressraster)
plot(e$pkg.globals$lengthraster)
plot(e$pkg.globals$heightraster)
e$rayshade_cpp()

sm<-e$rayshade_cpp(sunangle = 40,
             anglebreaks = c(44.4,44.7,45.0, 45.1),
             heightmap = e$flipud(as.matrix(e$pkg.globals$heightmap)),
             addressmap = e$flipud(as.matrix(e$pkg.globals$addressmap)),
             lengthmap = e$flipud(as.matrix(e$pkg.globals$lengthmap)),
             pointcloud = as.matrix(e$pkg.globals$pointcloud.cellsHeightMap.m),
             zscale = 1,
             maxsearch = e$pkg.globals$maxsearch,
             maxheight=e$pkg.globals$maxheight,
             cache_mask = NA,
             progbar = T)

heightgrid<-  terra::rast(rPET::DATASET.bolasco$dtm)
# dtm.plus <- heightgrid+1.5
heightmap <- rayshader::raster_to_matrix(heightgrid)
heightmap[2,2]
heightgrid

heightmap.l <- as.list(heightmap)
heightmap.l[[335]]

dsm.rs1 <- rayshader::ray_shade(sr)
dsm.rs2 <- rayshader::ray_shade2(sr, xyzf@data)
dsm.rs1 %>% plot_map()
dsm.rs2 %>% plot_map()




dtm.cells <- terra::cells(dtm.plus)
dtm.xyz <- cbind( terra::xyFromCell(dtm.plus,  dtm.cells), dtm.plus[dtm.cells] )
dtm.xy <- cbind(terra::xyFromCell(heightgrid,  1:terra::ncell(heightgrid) ), min(dtm.xyz[,3]) )
## raise the ground plane 2 meters

## remove points below the raised ground plane
#dtm.cells <- terra::cellFromXY(dtm.plus, pointcloud[, 1:2])
## remove voxel centers below the raised plane
#toremove <- which( dtm.plus[dtm.cells][[1]] > pointcloud[, 3] )
#pointcloud.filtered <- pointcloud[-toremove,]
pointcloud.filtered.rot <- rTLS::rotate3D(pointcloud, 90-sunelevation,0,180-sunangle)

dtm.plus.rot <- rTLS::rotate3D(dtm.xyz, 90-sunelevation,0,180-sunangle)
# dtm.xyz.rot <- rTLS::rotate3D(dtm.xyz,  90-sunelevation,0,180-sunangle  )
# dtm.xy.rot <- rTLS::rotate3D(dtm.xy, 90-sunelevation,0,180-sunangle  )


nn<-nabor::knn(dtm.plus.rot[,1:2], pointcloud.filtered.rot[,1:2], 4)

xrange <- range(dtm.xy.rot[[1]])
yrange <- range(dtm.xy.rot[[2]])
diffx <- diff(xrange)
diffy <- diff(yrange)

xres <- range(abs(diff(dtm.xy.rot[[1]])))[[1]]
yres <- range(abs(diff(dtm.xy.rot[[2]])))[[1]]


terra::rast(d2, type="xyz" )


dtm.ids <- terra::cells(dtm)
rgl::close()

cbind(terra::xyFromCell(dtm, dtm.ids), dtm.plus.ang[dtm.ids] )

vox <- VoxR::vox(dtm.xyz, 1)
VoxR::plot_voxels(  vox )
VoxR::plot_projection(VoxR::project_voxels( VoxR::vox(xyzf@data, 1) ))
VoxR::plot_projection(VoxR::project_voxels( VoxR::vox(xyzf.rot, 1) ))

# lidR::writeLAS(xyzf, "../tmp/voxel_villabolascoF.laz")

# dtm.plus.ang<-terra::project(dtm.plus, "epsg:4326" )
# dsm.ang<-terra::project(dsm, "epsg:4326" )

# dtm.ids <- terra::cells(dtm.plus.ang)
# dsm.ids <- terra::cells(dsm.ang)
#
# dtm.plus.pts <- cbind(terra::xyFromCell(dtm.plus.ang, dtm.ids), dtm.plus.ang[dtm.ids] )
#
# dsm.pts <- cbind(terra::xyFromCell(dsm.ang, dsm.ids), dsm.ang[dsm.ids] )

nn <- nabor::knn(dtm.plus.pts, dsm.pts, k=1)
