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

  RET <- ray_shade(dtm, PointCloud3D, onlyprepare = TRUE, force = TRUE  )
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
      overlayGroups = c("ShadowMap", "ShadowMapTotal"),
      leaflet::layersControlOptions(autoZIndex = F, collapsed = F)
    ) %>%

    leaflet::showGroup("BING")   %>%

    leaflet::showGroup("ShadowMap")   %>%
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
    leaflet::addLegend(
      colors =  c(
          "#30123BFF",
          "#4686FBFF",
          "#1AE4B6FF",
          "#A2FC3CFF",
          "#FABA39FF",
          "#E4460AFF",
          "#7A0403FF"
        ) ,
      title = "Shadow %",
      labels = sprintf("%.2f",  (1 - ((1:7) / 7)) ),
      opacity = 1,
      position = "topleft",
      layerId = "myLegend"
    ) %>%

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
          title="Force Sun Elevation",
          shiny::numericInput("forceSunElev", "Zenith", NULL )
        ),
        shiny::column(
          width = 2,
          title="Force Sun Angle (Azimuth)",
          shiny::numericInput("forceSunAngle", "Azimuth", NULL  )
        ),
        shiny::column(
          width = 2,
          title="Temperature",
          shiny::numericInput("temp", "Air Temp.", NULL  )
        ),
        shiny::column(
          width = 2,
          title="Humidity",
          shiny::numericInput("hum", "Humidity", NULL  )
        ),
        shiny::column(
          width = 2,
          title="Wind Speed",
          shiny::numericInput("wind", "Wind", NULL  )
        ),
        shiny::column(
          width = 2, style="margin-top:20px;",
          title="Update variables",
          shiny::actionButton("updateVars", "UPDATE", NULL  )
        )
      ),
      shiny::fluidRow(
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

    rVals <- shiny::reactiveValues(
      sunpos = c(0,90),
      wsData = list(temp = NA,
                    hum = NA,
                    wind = NA)
    )

    updateData<-function(){
      con <- initDB()
      if(inherits(con, "PostgreSQLConnection")){
        dd <- RPostgreSQL::dbGetQuery(con, 'select * from "public"."devices_parsed_stazione_meteo" order by timestamp desc limit 1' )

        rVals$wsData <- list(temp=round(dd$air_temperature,1),
                             hum = round(dd$humidity,1),
                             wind = round(dd$wind_speed,1) )
        #
        # updateNumericInput(inputId = "temp", value = wsData$temp )
        # updateNumericInput(inputId = "hum", value = wsData$hum )
        # updateNumericInput(inputId = "wind", value = wsData$wind )

        RPostgreSQL::dbDisconnect(con)
      }
      shinyWidgets::updateAirDateInput(session = shiny::getDefaultReactiveDomain(),
                                       inputId = "bins", value=Sys.time())
      dd<-insol::sunpos(insol::sunvector(insol::JD(Sys.time()), input$lat, input$long, 0))
      shiny::updateNumericInput(inputId = "forceSunElev",value = round(dd[[2]]) )
      shiny::updateNumericInput(inputId = "forceSunAngle",value = round(dd[[1]]) )
    }


    shiny::observeEvent({
      rVals$wsData
    },{

      shiny::updateNumericInput(inputId = "temp", value = rVals$wsData$temp )
      shiny::updateNumericInput(inputId = "hum", value = rVals$wsData$hum )
      shiny::updateNumericInput(inputId = "wind", value = rVals$wsData$wind )
      # rVals$sunpos <-
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
        # rVals$sunpos <-
    })


    shiny::observeEvent({
      input$forceSunElev
      input$forceSunAngle
    },{
      rVals$sunpos <- c(azimuth=input$forceSunAngle, zenith=input$forceSunElev)
    })

    output$rastPlot2 <- leaflet::renderLeaflet(leaflet.object)

    shiny::observeEvent(rVals$sunpos, {
      print(rVals$sunpos)
      shinyjs::addClass("drawShade", "red")
    })

    shiny::observeEvent(input$drawShade, {
      sp <- rVals$sunpos
      print(sp)
      # shiny::req(input$drawShade,  sp)
      shiny::withProgress(message = "Computing rayshader...", {
        shiny::incProgress(0.1)
        rs <-
           ray_shade(
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

        if(!input$lambert) rast.shade <- raster::as.factor(rast.shade)

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
           cols,
            raster::values(rast.shade),
            na.color = "transparent"
          )

        leaflet::leafletProxy('rastPlot2') %>%
          leaflet::clearGroup(group = 'ShadowMap') %>%
          leaflet::removeControl(layerId = 'myLegend') %>%

          leaflet::addRasterImage(
            raster::raster(rast.shade),
            colors = pal,
            group = "ShadowMap",
            opacity = 0.8
          ) %>%
          leaflet::addLegend(
            colors = cols,
            title = "Shadow %",
            labels = sprintf("%.2f",  (1 - ((1:7) / 7)) ),
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

      sp <- rVals$sunpos
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
