library(terra)
library(insol)
library(lidR)
library(rPET)
library(data.table)
library(gifski)
library(foreach)
library(RPostgreSQL)
## read voxellized point cloud from lastools using LidR
## MUST USE a POINT CLOUD AND A DTM SURFACE!!!! convert dtm to heightmap then map all points from
## point cloud to each cell . Z vaues from point cloud get linearized to a single vector,
## and values of heightmap substitute Z to index of
## doti <- function(heightgrid, pointcloud, sunelevation, sunangle)
zscale<-1; sunelevation=90; sunangle=200; sunaltitude<-sunelevation;
xyz<-  lidR::readLAS("data-raw/voxel_villabolasco_light.laz", select = "XYZ")
pointcloud<-xyz@data
PointCloud3D <- pointcloud


initDB <- function(){

  con  <- tryCatch({
    for(i in dbListConnections(DBI::dbDriver("PostgreSQL"))){
      dbDisconnect(i)
    }
    con<-dbConnect(dbDriver("PostgreSQL"),
                           dbname = "bolasco", user="marika.dagostini",
                           host="postgis.docker", password="marika72Master")


  },
  error=function(cond) {
    message("Unable to connect to Database.")
  })
  return(con)
}

heightraster<-"data-raw/bolasco_DTM_1m.tif"
Sys.Date()
Sys.Date()+1

makeplot <- function(datec=Sys.Date(), cutoffZenith=84){

  vMRT<-Vectorize(rPET::calcMRT)
  vPET<-Vectorize(rPET::PETcorrected)
  if(is.na(datec)){
    ss<-seq(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by="min")
  } else {
    if( inherits(datec,"Date")){
      ss<-seq(as.POSIXct(datec), as.POSIXct(datec+1), by="min")
    } else if(is.character(datec)){
      datec<-tryCatch(as.Date(datec),
               error=function(e){
                 message(e)
                 NULL
               })
      if(is.null(datec)){
        return(NULL)
      }
      ss<-seq(as.POSIXct(datec), as.POSIXct(datec+1), by="min")
    }
  }

  dd <- data.frame(insol::sunpos(insol::sunvector(insol::JD(ss), 46, 11.9701, 0)))
  dd$date <- ss
  dd2<- data.table(dd[dd$zenith<cutoffZenith,])
  dd3 <- dd2[as.integer(rownames(dd2))%%5==0, ]
  dd3$date <- as.POSIXct(substr(dd3$date, 1, 16))

  nn <- 0
  totnn <- nrow(dd3)
  con <- initDB()
  dd <- RPostgreSQL::dbGetQuery(con,  sprintf('select * from "public"."devices_parsed_stazione_meteo" WHERE timestamp >= \'%s\'::timestamp AND timestamp <  \'%s\'::timestamp  order by timestamp  limit 100', datec, datec+1 ) )

  RPostgreSQL::dbDisconnect(con)
  dd <- data.table(dd)
  setkey(dd3, date)
  setkey(dd, timestamp)
  dd4<-dd[dd3, roll=TRUE,]
  dd5 <- as.data.frame(t(dd4))


  res <- foreach( i = 1:nrow(dd4)) %do% {

    data<-as.list(dd4[i,])
    browser()

    sunangle = as.numeric(data$azimuth)
    sunaltitude = 90 - as.numeric(data$zenith)
    rs <- rPET::ray_shade(heightraster, PointCloud3D, sunangle = sunangle,
                          zscale = 1,   #onlyprepare = TRUE,
                          sunaltitude = 40,
                          makeVoxelGrid = FALSE, lambert = FALSE)

    rs %>% rayshader::plot_map()
    rs.raster <- rPET::matrix2raster(rs)
    terra::plot(rs.raster)
    ccc<-terra::cells(rs.raster)
    values <- rs.raster[ccc]
    vMRT()
    vPET<-Vectorize(rPET::calcMRT(Ta = data$air_temperature, WV=data$wind_speed) )


    vPET<-Vectorize(rPET::PETcorrected)
    res <- vPET(Tair = 36, Tmrt =  c(21, 40, -5, 60, 30, 60),
                v_air =1, pvap = c(12, 2, 2, 21, 21, 2) )
    res["PET",]
  }


}


tt<-terra::rast(heightraster)
save_gif(makeplot(), "gif_file.gif", ncol(tt), nrow(tt),delay = 0.1, loop = FALSE,  res = 144)

e<-environment(rPET::ray_shade)
e$globals$pointcloud.cellsHeightMap.m
rs.rast <- rPET::matrix2raster(rs)
plot(rs.rast)


