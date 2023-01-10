library(terra)
library(insol)
library(lidR)
library(rPET)
library(data.table)
library(gifski)
library(foreach)
library(RPostgreSQL)
library(comf)
library(doParallel)
## read voxellized point cloud from lastools using LidR
## MUST USE a POINT CLOUD AND A DTM SURFACE!!!! convert dtm to heightmap then map all points from
## point cloud to each cell . Z vaues from point cloud get linearized to a single vector,
## and values of heightmap substitute Z to index of
## doti <- function(heightgrid, pointcloud, sunelevation, sunangle)
zscale<-1; sunelevation=90; sunangle=200; sunaltitude<-sunelevation;
xyz<-  lidR::readLAS("data-raw/voxel_villabolasco_light.laz", select = "XYZ")
pointcloud<-xyz@data
PointCloud3D <- pointcloud
# saveRDS(object = PointCloud3D, file="data-raw/pointcloud.rds")

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

makeplot <- function(datec="2022-08-03", cutoffZenith=84){

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
  ddpyr <- RPostgreSQL::dbGetQuery(con,  sprintf('select timestamp,	device_id,	total_solar_radiation from "public"."devices_parsed_dl_pyr" WHERE timestamp >= \'%s\'::timestamp AND timestamp <  \'%s\'::timestamp  order by timestamp  limit 1000', datec, datec+1 ) )
  # ddpyr$timestamp <-dd$tswtz

  # RPostgreSQL::dbDisconnect(con)

  dd <- RPostgreSQL::dbGetQuery(con,  sprintf('select  * from "public"."devices_parsed_stazione_meteo" WHERE timestamp >= \'%s\'::timestamp AND timestamp <  \'%s\'::timestamp  order by timestamp  limit 100', datec, datec+1 ) )
  dd$timestamp <-dd$tswtz

  RPostgreSQL::dbDisconnect(con)
  dd <- data.table(dd)
  ddpyr <- data.table(ddpyr)

  ffff <- data.table::dcast(ddpyr, timestamp ~ device_id, value.var="total_solar_radiation"  )

  ffff$`dl-pyr-sn-15373`<-NULL
  ffff$`dl-pyr-sn-15375`<-NULL
  ffff$`dl-pyr-sn-15376`<-NULL

  summary(ffff)
  setkey(dd3, date)
  setkey(dd, timestamp)
  setkey(ffff, timestamp)
  dd4<-dd[dd3, roll=TRUE,]
  dd5<-na.omit(ffff[,1:2])[dd4, roll=TRUE,]
  dd6<-na.omit(ffff[,c(1,3)])[dd5, roll=TRUE,]
  dd7<-na.omit(ffff[,c(1,4)])[dd6, roll=TRUE,]

  final.climate.table<-dd7

   cs <- makeCluster(14)
   registerDoParallel(cs)
   ccc<-terra::cells(terra::rast(heightraster))

   result <- foreach( i = 1:nrow(dd4)) %dopar% {

    data<-as.list(dd4[i,])
    # browser()

    sunangle = as.numeric(data$azimuth)
    sunaltitude = 90 - as.numeric(data$zenith)
    rs <- rPET::ray_shade(heightraster, PointCloud3D, sunangle = sunangle,
                          zscale = 1,   #onlyprepare = TRUE,
                          sunaltitude =  sunaltitude, #seq(max(0,sunaltitude-1), min(90,sunaltitude+1), length.out = 20),
                          makeVoxelGrid = FALSE, progbar = FALSE,
                          lambert = TRUE)

    # rs %>% rayshader::plot_map()
    rs.raster <- rPET::matrix2raster(rs)
    return(rs.raster[ccc][,1])
    # terra::plot(rs.raster)
    ccc<-terra::cells(rs.raster)
    values <- rs.raster[ccc]
    message("Calculating MRT")
    # mrt<-vMRT(Ta=data$air_temperature, Td=data$dew_point_temperature,
    #      WV = data$wind_speed+100, S = data$solar_radiation_wm2,
    #      Fdiff =values$lyr.1, Fd = 1-values$lyr.1, Z = data$zenith, P = data$pressure_mb  )

    mrt<- ( (data$air_temperature+273.15)^4 + 0.7*data$solar_radiation_wm2*values$lyr.1/(0.97*5.67E-8) )^0.25 -273.15
    idx1<-(round(mrt, 1))
    idx<-unique(idx1 )
    idxf<-data.table::chmatch(as.character(idx1) , as.character(idx) )
    # message("Calculating PET ... will take a while")

    # rPET::PETcorrected(Tair = data$air_temperature, Tmrt =mrt, v_air = data$wind_speed, pvap = data$humidity )

    # bb<- comf::calcPMV(ta = data$air_temperature, tr = mrt,
    #                       vel = data$wind_speed, rh = data$humidity, clo = 0.6, met = 1.4 )

    bb<- comf::calc2Node(ta = data$air_temperature, tr = idx,
                       vel = data$wind_speed, rh = data$humidity,
                       clo = 0.6, met = 1.4 )


    # rs.pet <- terra::deepcopy(terra::rast(heightraster) )
     bb$pmvg
    # rs.pet[ccc]<-bb
    # rs.pet<-terra::project(rs.pet, "EPSG:4326" )
    # par(mfrow=c(1,1))
    # png("SET.png", width=1600, height=1600, res=300)
    # terra::plot(rs.pet, col=viridis::turbo(12), range=c(-3,3))
    # dev.off()
    # png("SET.png", width=1600, height=1600, res=300)
    #  terra::plot(rs.pet, col=viridis::turbo(12), range=c(28.5,36))
    # dev.off()
    #
    #
    # rs.pet[ccc]<-aa$Lraw
    # terra::plot(rs.pet)
    # rs.pet
  }


}

rs.pet.m<-sapply(result, function(bb){
   rs.pet <- (terra::rast(heightraster) )
   rs.pet[ccc]<-bb
   rs.pet
  })
rs.pet.brick<-terra::rast(rs.pet.m)
rs.pet.brick<-terra::project(rs.pet.brick, "EPSG:4326" )
terra::time(rs.pet.brick)<-dd4$tswtz
names(rs.pet.brick)<-dd4$tswtz
terra::writeRaster(rs.pet.brick, "data-raw/outputrShadow.tif", overwrite=T)
#
rs.pet.brick<-terra::rast("data-raw/outputrPET.tif")
rs.shade.brick<-terra::rast("data-raw/outputrShadow.tif")
# rs.pet.brick<-terra::project(rs.pet.brick, "EPSG:4326" )
names(rs.pet.brick)<-dd4$tswtz
names(rs.shade.brick)<-dd4$tswtz
cells<-terra::cellFromXY(object = rs.pet.brick, xy=matrix(c(11.9349170,45.6723575), nrow=1  ) )
cells2<-terra::cellFromXY(object = rs.pet.brick, xy=matrix(c(11.9344278,45.6758574), nrow=1  ) )

normalize <- function(dd){
  (dd - min(dd))/ diff(range(dd))
}

ff<-data.frame(#ComfIndex4816=  t(rs.pet.brick[cells2]),
              # SolarWatt= final.climate.table$solar_radiation_wm2,
              # Temp_Watt= final.climate.table$air_temperature,
              Model_Rad._4816=t(rs.shade.brick[cells2]),
              Pyranometer4816= final.climate.table$`dl-pyr-sn-15374`,
              Pyranometer4815= final.climate.table$`dl-pyr-sn-15377`,
              # Pyrmeter_3= final.climate.table$`dl-pyr-sn-15379`,
              # ComfIndex4815=t(rs.pet.brick[cells]),
              Model_Rad._4815=t(rs.shade.brick[cells]),
               time=as.POSIXct(names(rs.pet.brick)))
fff<-ff
for(i in names(fff)){
  if(!is.numeric(fff[[i]])) next
  fff[[i]]<-normalize(fff[[i]])
}

ff.m  <- data.table::melt(fff, id.vars = c("time" ) )
ff.m$Type <-  as.factor(substr(ff.m$variable,1,nchar("Pyranometer") ))
ff.m$Device <-  as.factor(substr(ff.m$variable,nchar("Pyranometer")+1, 11111))
library(ggplot2)
library(ggformula)


png("data-raw/plotShadePyr.png", width=2000, height=1000, res=250)
ggplot(ff.m) +
  geom_spline(aes(x =  time,
                  y = value,  colour = Device, lty=Type) ) +
  scale_color_manual(values=c("#00cc00", "blue", "black"))  +
  xlab("Time") + ylab("Normalized Values") +
  theme_light()
dev.off()

ff4816<-spline(ff$time, ff$PVM4816)
ff4815<-spline(ff$time, ff$PVM4815)
ssff4816<-spline(ff$time, ff$shade4816)
ssff4815<-spline(ff$time, ff$shade4815)
ffwatt<-spline(ff$time, ff$watt)
fftmp<-spline(ff$time, ff$temp)
times<-as.POSIXct(ff4815$x, origin = "1970-01-01")

fin<-data.frame(time=times, ffwatt=ffwatt$y,
                ff4816=ff4816$y,
                ff4815=ff4815$y,
                sff4816=ssff4816$y,
                sff4815=ssff4815$y,
                temp=fftmp$y)

png("data-raw/plotSynet.png", width=2000, height=1000, res=200)
par(mar = c(5.1, 4.1, 4.1, 6))
plot(x=fin$time, y=fin$ff4816, type="l", col="blue",
     sub="27 July 2022 - Station 4816 (green) and 4815 (blue) with solar radiation (red) W/m\u00b2 - black line is ideal comfort",
     xlab="Time", ylab="Predicted Mean Vote (PVM)", lwd=2)
lines(x=fin$time, y=fin$ff4815, col="#00cc00", lwd=2)
lines(x=fin$time, y=fin$ffwatt/100, col="red", lty=2, lwd=2)
lines(x=fin$time, y=fin$sff4815*fin$ffwatt/100, col="#00cc00", lty=2, lwd=2)
lines(x=fin$time, y=fin$sff4816*fin$ffwatt/100, col="blue", lty=2, lwd=2)
abline(h=0)
axis(4, c(0, 3, 6, 9), c(0,300,600,900) )
mtext("Solar radiation (W/m\u00b2)", side=4, line=2.5, cex.lab=1,las=3)
dev.off()


png("data-raw/plotSynet.png", width=2000, height=1000, res=200)
par(mar = c(5.1, 4.1, 4.1, 6))
plot(x=fin$time, y=fin$ff4816, type="l", col="blue",
     sub="27 July 2022 - Station 4816 (green) and 4815 (blue) with solar radiation (red) W/m\u00b2 - black line is ideal comfort",
     xlab="Time", ylab="Predicted Mean Vote (PVM)", lwd=2)
lines(x=fin$time, y=fin$ff4815, col="#00cc00", lwd=2)
lines(x=fin$time, y=fin$ffwatt/100, col="red", lty=2, lwd=2)
lines(x=fin$time, y=fin$sff4815*fin$ffwatt/100, col="#00cc00", lty=2, lwd=2)
lines(x=fin$time, y=fin$sff4816*fin$ffwatt/100, col="blue", lty=2, lwd=2)
abline(h=0)
axis(4, c(0, 3, 6, 9), c(0,300,600,900) )
mtext("Solar radiation (W/m\u00b2)", side=4, line=2.5, cex.lab=1,las=3)
dev.off()

rs.pet.brick.sum<-sum(rs.pet.brick)
rs.pet.brick.avg<-mean(rs.pet.brick)
rs.pet.brick.sd<- stdev(rs.pet.brick)
rs.pet.brick.rg<- range(rs.pet.brick)


png("man/figures/max.png", width=1600, height=1600, res=300)
plot(rs.pet.brick.rg$range_max, range=c(-1,12), col=viridis::turbo(12),
     main="Range Comfort 27 July 2022")
dev.off()

png("man/figures/avg.png", width=1600, height=1600, res=300)
plot(rs.pet.brick.avg, range=c(-4,9), col=viridis::turbo(12),
     main="Average Comfort 27 July 2022")
dev.off()

png("man/figures/sd.png", width=1600, height=1600, res=300)
plot(rs.pet.brick.sd, range=c(0,6), col=viridis::turbo(12),
     main="Std. Deviation of Comfort 27 July 2022")
dev.off()

tt<-terra::rast(heightraster)

makeplot2<-function(rs){
  if(nlyr(rs)<10){
    message("At least 10 layers are required!")
    return(NULL)
  }
  ss<- terra::spatSample(rs, 10000)
  message("Clamping")
  qq <- quantile(ss, c(0.1,0.9), na.rm=TRUE)

  nn<- terra::clamp(rs, -2, upper=10)
  # browser()
  ii<-0
  lapply(nn, function(i){
    ii<<-ii+1
    terra::plot(i, range=c(-2,10), col=viridis::turbo(12),
                main=paste( dd4$tswtz[[ii]]) )
  })
}

rs.pet.brick<-terra::rast("data-raw/outputrPET.tif")
terra::plot(rs.pet.brick[[30]], range=c(-2,10), col=viridis::turbo(12),
            main=paste( dd4$tswtz[[ii]] ))
save_gif(makeplot2(rs.pet.brick), "data-raw/gif_filePMV.gif",  delay = 0.1, loop = FALSE,  res = 144)

e<-environment("package:rPET")


e<-environment(rPET::ray_shade)
e$globals$pointcloud.cellsHeightMap.m
rs.rast <- rPET::matrix2raster(rs)
plot(rs.rast)


