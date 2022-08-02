#' solarApp
#'
#' @importFrom magrittr "%>%"
#' @return a shiny application
#' @export
#'
#' @examples
#' #rPET::solarApp()
solarApp <- function() {

  options(digits = 12)
  # options(rgl.useNULL = TRUE)
  #
  requireNamespace("RPostgreSQL", quietly = TRUE)
  requireNamespace("DBI", quietly = TRUE)
  e<-environment(rPET::prepareData)
  if(is.null(e$dataenv) ||
     !is.data.frame(e$dataenv$pointcloud)||
     !inherits(e$dataenv$dtm, "PackedSpatRaster") ){
    ret<-rPET::prepareData()
    if(!ret) return(NULL)
  }
  # rast.gap <- terra::rastpkgenv$DATASET.bolasco$gap.fraction)
  dtm <- terra::rast(e$dataenv$dtm)
  PointCloud3D <- e$dataenv$pointcloud

  RET <-  rPET::ray_shade(dtm, PointCloud3D, onlyprepare = TRUE, force = TRUE  )
  if(is.null(RET)){
    message("Leaving web app as the data preparation did not go well...")
    return(NULL)
  }

  initDB <- function(){

    con  <- tryCatch({
      drv <- DBI::dbDriver("PostgreSQL")
      RPostgreSQL::dbConnect(drv,
                             dbname = "bolasco", user="marika.dagostini",
                             host="postgis.docker", password="marika72Master")

    },
    error=function(cond) {
      message(cond)
      message("Unable to connect to Database.")
    })
    return(con)
  }


  tallVegetationMask <- NULL



  bds <-
   as.list( terra::ext(
      terra::project(
        dtm,
        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      )
    ) )


  leaflet.object <-
    leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap") %>%
    leaflet.extras::addBingTiles(
      "Satellite BING maps",
      group = "BING",
      apikey = "AjvjPYuoA4IgNeooKvodDLcxbVL1F8RdIxXUeYsb6PgiVapURz_PbbWvOxVKmNps",
      imagerySet = c("Aerial")
    ) %>%
    ## F.PIROTTI
    leaflet::addTiles(urlTemplate  = '', group = "Blank") %>%
    leaflet::addLayersControl(
      position = ("topright"),
      baseGroups = c("Blank", "OpenStreetMap", "BING"),
      overlayGroups = c("Shadow Map", "Comfort Index Map"),
      leaflet::layersControlOptions(autoZIndex = F, collapsed = F)
    ) %>%

    leaflet::showGroup("BING")   %>%

    leaflet::showGroup("Shadow Map")   %>%
    leaflet::addScaleBar("bottomright") %>%
    leaflet::addMiniMap(tiles = "OpenStreetMap",
                        toggleDisplay = TRUE,
                        position = "topright") %>%
    htmlwidgets::onRender(
      "
    function(el, x) {
      myMap = this;
      Shiny.setInputValue('leafletRendered',true, {priority: \"event\"});

    }"
    ) %>%
    # leaflet::addLegend(
    #   colors =  c(
    #       "#30123BFF",
    #       "#4686FBFF",
    #       "#1AE4B6FF",
    #       "#A2FC3CFF",
    #       "#FABA39FF",
    #       "#E4460AFF",
    #       "#7A0403FF"
    #     ) ,
    #   title = "Shadow %",
    #   labels = sprintf("%.2f",  (1 - ((1:7) / 7)) ),
    #   opacity = 1,
    #   position = "topleft",
    #   layerId = "myLegend"
    # ) %>%

    leaflet::setView(lng = 11.970140, lat = 46, zoom = 12)  %>%
    leafem::addMouseCoordinates() %>%

    leaflet::fitBounds(bds$xmin , bds$ymin, bds$xmax, bds$ymax)


  M_PI <- pi

  deg2rad <- function(x) {
    return(x * (M_PI / 180.0))

  }

  rad2deg <- function(x) {
    return(x * (180.0 / M_PI))

  }

## UI ---------
  ui <- shiny::fluidPage(

    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      div.red { background-color: red; }
      div.blue { background-color: blue; }
    "))
    ),

    shiny::fluidRow(
     shiny::sidebarPanel(
      width = 6,

      shiny::fluidRow(
        shiny::column(width = 6,
                      shinyWidgets::airDatepickerInput("bins",
                                                       "Date and Time:",
                                                       timepicker = T, value = Sys.time())
                      ),


        shiny::column(
          width = 2,
          shiny::numericInput(
            "long",
            label = "Longitude",
            min = -180.0,
            max = 180.0,
            value = 11.94
          )
        ),
        shiny::column(
          width = 2,
          shiny::numericInput(
            "lat",
            label = "Latitude",
            min = -90.0,
            max = 90.0,
            value = 45.67
          )
        ),
        shiny::column(
          width = 2,
          title="Height from terrain",
          shiny::numericInput(
            "heightFromTerrain",
            label = "Height",
            min = 0.0,
            max = 99990.0,
            value = 1.5
          )
        )

      ),

      shiny::fluidRow(

        shiny::column(
          width = 2,
          title="Sun Elevation from horizon ... 90-zenith (degrees)",
          shiny::numericInput("forceSunElev", "Zenith", NULL )
        ),
        shiny::column(
          width = 2,
          title="Sun Angle/Azimuth (degrees)",
          shiny::numericInput("forceSunAngle", "Azimuth", NULL  )
        ),
        shiny::column(
          width = 2,
          title="Air Temperature (\u00b0C)",
          shiny::numericInput("temp", "Temp.", NULL  )
        ),
        shiny::column(
          width = 2,
          title="Relative Humidity (%)",
          shiny::numericInput("hum", "Hum.", NULL  )
        ),
        shiny::column(
          width = 2,
          title="Air velocity in (m/s)",
          shiny::numericInput("wind", "Wind", NULL  )
        ),
        shiny::column(
          width = 2,
          title="Solar radiation (Watt/m2)",
          shiny::numericInput("radiation", "Rad.", NULL  )
        )
      ),

      shiny::fluidRow(

        shiny::column(
          width = 6,
          title="metabolic rate (met)",
          shiny::selectInput("met", "Metab. Rate",   choices= list("Sleeping:  (0.7 met 41 W/m\u00b2)"=0.7,
                                                                   "Reclining: (0.8 met 47 W/m\u00b2)"=0.8,
                                                                   "Seated, Writing: (1 met 58.2 W/m\u00b2)"=0.8,
                                                                   "Typing (1.1 met 64 W/m\u00b2)"=1.1,
                                                                   "Standing, relaxed, seated: (1.2 met 70 W/m\u00b2)"=1.2,
                                                                   "Walking slow: (1.4 met 81 W/m\u00b2)"=1.4,
                                                                   "Driving a car: (1.5 met 87 W/m\u00b2)"=1.5,
                                                                   "Walking medium speed (1.7 met 99 W/m\u00b2)"=1.8,
                                                                   "Walking 2mph (3.2km/h): (2.0 met 116 W/m\u00b2)"=1.8,
                                                                   "Light machine work: 2.2 met,  (128 W/m\u00b2)"=2.2,
                                                                   "Walking 3mph (4.8km/h): 2.6 met,  (151 W/m\u00b2)"=2.6,
                                                                   "House cleaning (2.7 met 157 W/m\u00b2)"=2.7,
                                                                   "Driving, heavy vehicle:(3.2 met 186 W/m\u00b2)"=3.3,
                                                                   "Dancing: (3.4 met 198 W/m\u00b2)"=3.4,
                                                                   "Walking 4mph (6.4km/h): (3.8 met 221 W/m\u00b2)"=3.8,
                                                                   "Heavy machine work: (4.0 met 233 W/m\u00b2)"=4),
                             selected=1.4 )
        ),
        shiny::column(
          width = 2,
          title="clothing (clo) from 0 to 1 ",
          shiny::numericInput("clot", "Cloth", 0.6  )
        ),
        shiny::column(
          width = 4,
          title="Select your comfort index",
          shiny::selectInput("confind", "Index", choices = list("Predicted Mean Vote (PMV)"="pmv",
                                                       "Standard Effective Temperature (SET)"="set",
                                                       "Physiological Equivalent Temperature (PET)"="pet" ),
                             selected = "pmv")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4, style="margin-top:20px;",
          title="Update physological and environmental variables",
          shiny::actionButton("updateVars", "UPDATE", NULL  )
        ),
        shiny::column( title="Color or grayscale",
                       width = 4, shinyWidgets::awesomeCheckbox("greyscale", "Greyscale", value = F)
        ),
        shiny::column( title="Changes the intensity of the light at each point proportionally to the
                       dot product of the ray direction and the surface normal at
                       that point. Zeros out all values directed away from the ray.",
          width = 4, shinyWidgets::awesomeCheckbox("lambert", "Lambertian", value = T)
        ),
        shiny::column(
          width = 4, shiny::actionButton("drawShade", "Draw shader")
        ),
        shiny::plotOutput("distPlot")
      )
    ),

    shiny::column(width = 6,
                  leaflet::leafletOutput("rastPlot2", height = "98vh")
                  )
  ))

## SERVER ----------
  server <- function(input, output, session) {
    # dsm.local <- dsm
    rast.shade <- terra::rast(e$dataenv$dtm)

    lsCond <- comf::createCond()

    sunposition <- c(0,80)

    updateData<-function(){
      con <- initDB()
      if(inherits(con, "PostgreSQLConnection")){
        dd <- RPostgreSQL::dbGetQuery(con, 'select * from "public"."devices_parsed_stazione_meteo" order by timestamp desc limit 1' )

         lsCond$ta<-dd$air_temperature
         lsCond$trm<-dd$air_temperature
         lsCond$rh<-dd$humidity
         lsCond$tao<-dd$air_temperature
         lsCond$rho<-dd$humidity
         lsCond$vel<-dd$wind_speed
         lsCond$met<-input$met
         lsCond$clo<-input$clo
         lsCond$pb <- dd$pressure_mb * 0.750062


        RPostgreSQL::dbDisconnect(con)
      }

      shinyWidgets::updateAirDateInput(session = shiny::getDefaultReactiveDomain(),
                                       inputId = "bins", value=Sys.time())
      sunposition<<-insol::sunpos(insol::sunvector(insol::JD(Sys.time()), input$lat, input$long, 0))[1,]

      shiny::updateNumericInput(inputId = "forceSunElev",value = round(sunposition[[2]]) )
      shiny::updateNumericInput(inputId = "forceSunAngle",value = round(sunposition[[1]]) )
      shiny::updateNumericInput(inputId = "temp", value = lsCond$ta )
      shiny::updateNumericInput(inputId = "hum", value = lsCond$rh )
      shiny::updateNumericInput(inputId = "wind", value = lsCond$vel )
      shiny::updateNumericInput(inputId = "radiation", value = lsCond$vel )
      shiny::updateNumericInput(inputId = "radiation", value = dd$solar_radiation_wm2 )
    }


    shiny::observeEvent( input$confind,{
      if(input$confind=="pet"){
        shinyWidgets::show_alert("Warning", paste0(toupper(input$confind), "takes quite a long time to calculate") )
      }
    })


    shiny::observeEvent( input$updateVars,{
       updateData()
    })


    shiny::observeEvent({
      input$bins
      input$lat
      input$long
      },{
        dd<-insol::sunpos(insol::sunvector(insol::JD(input$bins), input$lat, input$long, 0))
        shiny::updateNumericInput(inputId = "forceSunElev",value = round(dd[[2]]) )
        shiny::updateNumericInput(inputId = "forceSunAngle",value = round(dd[[1]]) )
        sunposition <<- dd[1,]
    })


    shiny::observeEvent({
      input$forceSunElev
      input$forceSunAngle
    },{
      sunposition <<- c(azimuth=input$forceSunAngle, zenith=input$forceSunElev)
    })

    output$rastPlot2 <- leaflet::renderLeaflet(leaflet.object)



    shiny::observeEvent(input$drawShade, {
      shiny::req(input$updateVars)

       lsCond$ta<<- input$temp
       lsCond$trm<<-input$temp
       lsCond$tr<<- input$temp
       lsCond$rh<<- input$hum
       lsCond$tao<<- input$temp
       lsCond$rho<<- input$hum
       lsCond$vel<<- input$wind
       lsCond$met<<- as.numeric(input$met)
       lsCond$clo<<-input$clot

      sp <- sunposition
      # shiny::req(input$drawShade,  sp)
      shiny::withProgress(message = "Computing rayshader...", {
        shiny::incProgress(0.1)
        rs <-
          rPET::ray_shade(
            dtm,
            PointCloud3D,
            zscale = 1,
            multicore = FALSE,
            lambert = input$lambert,
            sunangle = sp[[1]],
            sunaltitude = 90 - sp[[2]],
            progbar = FALSE,
            height = input$heightFromTerrain
          )

        shiny::incProgress(0.3, message = "Finished rayshade... drawing map")

        rast.shade[] <-  as.numeric(rs[nrow(rs):1 , ])

        # if(!input$lambert) rast.shade <- raster::as.factor(rast.shade)

        shiny::incProgress(0.36, message = "Calculating MRT")

        ccc<-terra::cells(rast.shade)
        values<-rast.shade[ccc]
        mrt<- ( (lsCond$ta+273.15)^4 + 0.7*input$radiation*values[,1]/(0.97*5.67E-8) )^0.25 -273.15
        lsCond$tr<<-mrt

        shiny::incProgress(0.36, message = paste0("Calculating ", toupper(input$confind) ) )

        print("here1")

        bb<-suppressWarnings(comf::calcPMV(ta = lsCond$ta,   tr = mrt,   vel = lsCond$ta,
                          rh = lsCond$rh,
                          clo = lsCond$clo, met = lsCond$met))

        print("here2")
        # if(input$confind=="pmv"){
        # bb<-comf::calcComfInd(lsCond, request="pmv")
        shiny::incProgress(0.4, message = paste0(" FINISHED Calculating ", toupper(input$confind) ) )
        # }
        rs.pet <- terra::deepcopy( terra::rast(rast.shade) )
        rs.pet[ccc]<-NA
        rs.pet[ccc]<- bb
        if(input$greyscale){
          cols <-  c(
            "#000000",
            "#333333",
            "#666666",
            "#999999",
            "#BBBBBB",
            "#DDDDDD",
            "#FFFFFF"
          )
        } else {
          cols <-  c(
            "#30123B",
            "#4686FB",
            "#1AE4B6",
            "#A2FC3C",
            "#FABA39",
            "#E4460A",
            "#7A0403"
          )
        }
        pal <-
          leaflet::colorNumeric(
            c(
              "#000000",
              "#333333",
              "#666666",
              "#999999",
              "#BBBBBB",
              "#DDDDDD",
              "#FFFFFF"
            ),
            raster::values(rast.shade),
            na.color = "transparent"
          )

        print("here")
        vvv<-c(-3,3 , rs.pet[ccc][,1])
        pal2 <-
          leaflet::colorNumeric(
            cols,
            vvv,
            na.color = "transparent"
          )
        leaflet::leafletProxy('rastPlot2') %>%
          leaflet::clearGroup(group = 'Shadow Map') %>%
          leaflet::clearGroup(group = 'Comfort Index Map') %>%
          leaflet::removeControl(layerId = 'myLegend') %>%

          leaflet::addRasterImage(
            raster::raster(rast.shade),
            colors = pal,
            group = "Shadow Map",
            opacity = 0.8
          ) %>%
          leaflet::addRasterImage(
            raster::raster(rs.pet),
            colors = pal2,
            group = "Comfort Index Map",
            opacity = 0.8
          ) %>%
          leaflet::addLegend(
            pal = pal2,
            title = "Comfort Index",
            values = vvv,
            opacity = 1,
            position = "topleft",
            layerId = "myLegend"
          )

      })

      # myplot <- elmat %>%
      #   rayshader::height_shade() %>%
      #   rayshader::add_shadow(rs, 0.5) %>%
      #   rayshader::plot_map()
      #
      # myplot <- rs %>%
      #   rayshader::plot_map()
      # myplot <- rayshader::plot_map(rs)

    })

    output$distPlot <- shiny::renderPlot({
      shiny::req(input$bins)

      dayafter <-
        seq(as.POSIXct(input$bins),
            length = 2,
            by = '1 day')[2]

      days = insol::JD(seq(as.POSIXct(input$bins), as.POSIXct(dayafter), by =
                             'min'))
      # message(as.POSIXct(input$bins), as.POSIXct(dayafter))
      # scatterplot3d(sunvector(juneday,45,9,0),
      #               ylim=c(-1,1),zlim=c(0,1),pch=8,color='orange')

      sp <- sunposition
         #insol::sunpos(insol::sunvector(insol::JD(input$bins), input$lat, input$long, 0))
      sps <-
        insol::sunpos(insol::sunvector((days), input$lat, input$long, 0))

      plotrix::polar.plot(
        90 - sps[, 2],
        sps[, 1],
        start = 90,
        clockwise = TRUE,
        rp.type = 'l',
        # point.symbols=20,point.col=1,cex=1,
        radial.lim = c(0, 90),
        main = 'Apparent solar path at date and position'
      )

      plotrix::polar.plot(
        90 - sp[[2]],
        sp[[1]],
        start = 90,
        clockwise = TRUE,
        rp.type = 's',
        point.symbols = 20,
        point.col = 2,
        cex = 3,
        radial.lim = c(0, 90),
        add = T
      )
      # polar.plot(90-sps[,2],sps[,1],start=90,clockwise=TRUE,rp.type='l',
      #             line.col=3,cex=2, radial.lim=c(0,90) )


    })
  }

  shiny::shinyApp(ui, server)
}

# library(rPET)
 # solarApp()
