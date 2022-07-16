library(terra)
library(insol)
library(lidR)
library(rPET)

## read voxellized point cloud from lastools using LidR
## MUST USE a POINT CLOUD AND A DTM SURFACE!!!! convert dtm to heightmap then map all points from
## point cloud to each cell . Z vaues from point cloud get linearized to a single vector,
## and values of heightmap substitute Z to index of
## doti <- function(heightgrid, pointcloud, sunelevation, sunangle)
zscale<-1; sunelevation=45; sunangle=200; sunaltitude<-sunelevation;
xyz<-  lidR::readLAS("data-raw/voxel_villabolasco_fixed2DTM_light.laz", select = "XYZ")
pointcloud<-xyz@data
PointCloud3D <- pointcloud

heightraster<-"data-raw/bolasco_DTM_1m.tif"

rs <- rPET::ray_shade(heightraster, PointCloud3D, sunangle = 40, onlyprepare = F)





rs %>% rayshader::plot_map()

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
