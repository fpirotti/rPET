library(terra)
library(insol)
library(lidR)
library(rPET)
library(data.table)
library(gifski)
library(foreach)
library(RPostgreSQL)
library(comf)
## read voxellized point cloud from lastools using LidR
## MUST USE a POINT CLOUD AND A DTM SURFACE!!!! convert dtm to heightmap then map all points from
## point cloud to each cell . Z vaues from point cloud get linearized to a single vector,
## and values of heightmap substitute Z to index of
## doti <- function(heightgrid, pointcloud, sunelevation, sunangle)
zscale<-1; sunelevation=90; sunangle=200; sunaltitude<-sunelevation;
xyz<-  lidR::readLAS("data-raw/voxel_villabolasco_light.laz", select = "XYZ")
pointcloud<-xyz@data
PointCloud3D <- pointcloud
saveRDS(object = PointCloud3D, file="data-raw/pointcloud.rds")

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

  vMRT<-Vectorize(rPET::calcMRT2)
  vGT<-Vectorize(rPET::calcGT)
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
  dd <- RPostgreSQL::dbGetQuery(con,  sprintf('select timezone(\'Europe/Rome\', timestamp::timestamptz) as tswtz, * from "public"."devices_parsed_stazione_meteo" WHERE timestamp >= \'%s\'::timestamp AND timestamp <  \'%s\'::timestamp  order by timestamp  limit 100', datec, datec+1 ) )
  dd$timestamp <-dd$tswtz

  RPostgreSQL::dbDisconnect(con)
  dd <- data.table(dd)
  setkey(dd3, date)
  setkey(dd, timestamp)
  dd4<-dd[dd3, roll=TRUE,]
  # dd5 <- as.data.frame(t(dd4))


  res <- foreach( i = 1:nrow(dd4)) %do% {

    data<-as.list(dd4[i,])
    # browser()

    sunangle = as.numeric(data$azimuth)
    sunaltitude = 90 - as.numeric(data$zenith)
    rs <- rPET::ray_shade(heightraster, PointCloud3D, sunangle = sunangle,
                          zscale = 1,   #onlyprepare = TRUE,
                          sunaltitude = 40,
                          makeVoxelGrid = FALSE, lambert = TRUE)

    # rs %>% rayshader::plot_map()
    rs.raster <- rPET::matrix2raster(rs)
    # terra::plot(rs.raster)
    ccc<-terra::cells(rs.raster)
    values <- rs.raster[ccc]
    message("Calculating MRT")
    # mrt<-vMRT(Ta=data$air_temperature, Td=data$dew_point_temperature,
    #      WV = data$wind_speed+100, S = data$solar_radiation_wm2,
    #      Fdiff =values$lyr.1, Fd = 1-values$lyr.1, Z = data$zenith, P = data$pressure_mb  )

    mrt<- ( (data$air_temperature+273.15)^4 + 0.7*data$solar_radiation_wm2*values$lyr.1/(0.97*5.67E-8) )^0.25 -273.15

    message("Calculating PET ... will take a while")


    pet <- vPET(Tair = data$air_temperature, Tmrt =mrt, v_air = data$wind_speed, pvap = data$humidity )



    lsCond<-createCond(a = TRUE)
    lsCond$ta  <- data$air_temperature      # vector with air temperature values
    lsCond$tr  <- data$air_temperature         # vector with radiant temperature values
    lsCond$vel <- data$wind_speed  # vector with air velocities
    lsCond$rh  <- data$humidity  # vector with relative humidity values
    lsCond$tao   <- data$air_temperature
    lsCond$rho   <- data$humidity
    lsCond$met   <- 1.6
    lsCond$clo   <- 0.6

    lsCond <- as.list(data.frame(ta,tr,vel,rh,clo,met,asv))
    aa<-comf::calcPMVPPD(lsCond$ta, mrt, lsCond$vel, lsCond$rh, lsCond$clo
               , lsCond$met, getLoad=TRUE)

    bb<-comf::calcSET(ta = data$air_temperature,   tr = mrt[1:3000],   vel = data$wind_speed,
                  rh = data$humidity,
                  clo = 0.6, met = 1.4 )


    bb<-comf::calcATHBpmv2015(trm = data$air_temperature, ta = data$air_temperature,  psych=0,  tr = mrt,   vel = data$wind_speed,
                      rh = data$humidity, met = 1.4 )

    bb<-comf::calcSET(ta = data$air_temperature,   tr = mrt,   vel = data$wind_speed,
                      rh = data$humidity,
                      clo = 0.6, met = 1.4 )


    bb<- comf::calcATHBpmv2015(trm = data$air_temperature, psych = 0,  ta =  data$air_temperature,   tr = mrt,   vel = data$wind_speed,
                      rh = data$humidity,
                       met = 1.4 )



    rs.pet <- terra::deepcopy(terra::rast(heightraster) )
    rs.pet[ccc]<-pet
    rs.pet[ccc]<-bb
    rs.pet<-terra::project(rs.pet, "EPSG:4326" )
    par(mfrow=c(1,1))
    png("SET.png", width=1600, height=1600, res=300)
    terra::plot(rs.pet, col=viridis::turbo(12), range=c(-3,3))
    dev.off()
    png("SET.png", width=1600, height=1600, res=300)
     terra::plot(rs.pet, col=viridis::turbo(12), range=c(28.5,36))
    dev.off()

    png("ATHBpmv2015.png", width=1600, height=1600, res=300)
       terra::plot(rs.pet, col=viridis::turbo(12), range= c(0,8) )
    dev.off()

    rs.pet[ccc]<-aa$Lraw
    terra::plot(rs.pet)
  }


}


tt<-terra::rast(heightraster)
save_gif(makeplot(), "gif_file.gif", ncol(tt), nrow(tt),delay = 0.1, loop = FALSE,  res = 144)

e<-environment("package:rPET")


e<-environment(rPET::ray_shade)
e$globals$pointcloud.cellsHeightMap.m
rs.rast <- rPET::matrix2raster(rs)
plot(rs.rast)


