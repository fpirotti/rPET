library(shiny)
library(shinyWidgets)
require(scatterplot3d)
require(plotrix)
require(insol)

myApp <- function(...) {

  options(digits=12)

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shinyWidgets::airDatepickerInput("bins",
                                         "Date and Time:",
                                         timepicker=T),
        shiny::numericInput("long", label = "Longitude", min = -180, max=180, value=10),
        shiny::numericInput("lat", label = "Latitude", min = -90, max=90, value = 46)
      ),

      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::plotOutput("distPlot")
      )
    )
  )


  server <- function(input, output, session) {
    output$distPlot <- shiny::renderPlot({
      shiny::req(input$bins)

      dayafter <- seq( as.POSIXct(input$bins), length=2, by='2 day' )[2]
      days = insol::JD(seq(as.POSIXct(input$bins), as.POSIXct(dayafter),by='10 min'))
      message(as.POSIXct(input$bins), as.POSIXct(dayafter))
      # scatterplot3d(sunvector(juneday,45,9,0),
      #               ylim=c(-1,1),zlim=c(0,1),pch=8,color='orange')

      sp<- insol::sunpos(insol::sunvector(insol::JD(input$bins),input$lat,input$long,0))
      sps<- insol::sunpos(insol::sunvector((days),input$lat,input$long,0))
      print(days)
      print(sps)
      plotrix::polar.plot(90-sps[,2],sps[,1],start=90,clockwise=TRUE,rp.type='s',
                 point.symbols=20,point.col=1,cex=1,radial.lim=c(0,90),
                 main='Apparent solar path at date and position')

      plotrix::polar.plot(90-sp[,2],sp[,1],start=90,clockwise=TRUE,rp.type='s',
                 point.symbols=20,point.col=2,cex=3,radial.lim=c(0,90), add=T)
      # polar.plot(90-sps[,2],sps[,1],start=90,clockwise=TRUE,rp.type='l',
      #             line.col=3,cex=2, radial.lim=c(0,90) )


    })
  }
  shiny::shinyApp(ui, server, ...)
}
