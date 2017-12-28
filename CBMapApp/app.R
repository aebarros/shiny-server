###Load Packages###
##always load plyr before dplyr##
library(shiny)
library(shinyjs)
library(leaflet)
library(shinythemes)
library(rsconnect)
library(zoo)
##source the global file with all the necessary data
source("global.R")
rm(list=ls())
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}
####Shiny####
##UI building
ui = bootstrapPage(theme = shinytheme("sandstone"),
                   div(
                     id = "app-content",
                     navbarPage("Bay Area CB Tows"),
                     tags$style(type = "text/css", "#map {height: calc(100vh - 120px) !important;}"),
                     leafletOutput("map", height = "100%"),
                     tags$div(id="cite",
                              'Data compiled for ', tags$b('Hobbs Lab Longfin Smelt Survey'), ' by Arthur Barros (2017).'
                     ),
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                   width = 200, height = "auto",
                                   h2("Month/Year Select"),
                                   sliderInput("Date","Month",
                                               min = as.Date("2015-12-01"),max =as.Date("2017-12-31"),value=as.Date("2014-12-01"),timeFormat="%b %Y")
                                   )
                   )
)
server <- function(input, output, session) {
  
  ###########Interactive Map##############################################
  #Reactive expression used to filter out by user selected variables for final data "filtered()"
  #for map generation
  sliderMonth <- reactiveValues()
  observe({
    sliderMonth$Month <- as.yearmon(input$Date)
  })
  filtered<-reactive({
    filtered.date<-data.cb[data.cb$yearmonth==sliderMonth$Month,]
  })
  
  #produces base map
  mymap1 <- reactive({
    leaflet(filtered())%>% addProviderTiles("Hydda.Full")%>%
      fitBounds(~min(longitude)-.005, ~min(latitude)-.005, ~max(longitude)+.005, ~max(latitude)+.005)
  })
  #map2 is used if no data fits selected parameters, and will be blank
  mymap2 <- reactive({
    leaflet()%>% addProviderTiles("Hydda.Full")%>%
      setView(lng = -122.40, lat = 37.85, zoom = 9)
  })
  #renders map for main panel in ui using an if function to create map fitting parameters
  output$map <- renderLeaflet({
    if(nrow(filtered())==0){mymap2()}else{mymap2()}
  })

  
  ##this function used to be an observation, switched to function
  ##in order to allow for downloading in rmarkdown
  myfun <- function(map){
    
    #call populates map with markers based on filtered() data
    addCircleMarkers(map, data = filtered(),lng = ~longitude, lat = ~latitude,radius=~5,
                     stroke=TRUE, color="black",weight=2, fillOpacity=1,
                     fillColor="red",
                     popup = ~paste("ID:", filtered()$id, "<br>",
                                    "SN:",filtered()$snumber,"<br>",
                                    "Station:", filtered()$polystn,"<br>",
                                    "Date:",filtered()$Date,"<br>",
                                    "Coordinates:", latitude,",",longitude,"<br>"))
  }
  #here is an observation using leafletProxy to take the above function and run it on our map
  #also have to have the "clear" calls here, as they won't work in the myfun
  observe({
    leafletProxy("map")%>%
      clearControls%>%
      clearMarkers()%>% myfun()
  })
  
  # MW: Stop shiny app when closing the browser
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)