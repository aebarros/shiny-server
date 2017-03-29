###Load Packages###
##always load plyr before dplyr##
library(plyr)
library(dplyr)
library(lubridate)
library(shiny)
library(reshape2)
library(leaflet)
library(shinythemes)
library(rsconnect)

##source the global file with all the necessary data
source("global.R")

####Shiny####
##UI building
ui = bootstrapPage(theme = shinytheme("sandstone"),
                     div(
                       id = "app-content",
                       navbarPage("CDFW 20mm Catch"),
                       tags$style(type = "text/css", "#map {height: calc(100vh - 120px) !important;}"),
                       leafletOutput("map", height = "100%"),
                       tags$div(id="cite",
                                'Data compiled by Arthur Barros (2017).'
                       ),
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                     width = 200, height = "auto",
                                     h2("Catch Explorer"),
                                     selectizeInput("species", "Species",
                                                    unique(as.character(data$Common.Name))),
                                     dateRangeInput("dates","Date range", start="2015-03-01", end="2015-03-31"),
                                     textOutput("DateRange"),
                                     submitButton("Submit"))
                     )
)
server <- function(input, output, session) {
  ###########Interactive Map##############################################
  #Reactive expression used to filter out by user selected variables for final data "filtered()"
  #for map generation
  filtered<-reactive({
    filtered.species<- data[data$Common.Name==input$species,]
    filtered.dates<-filtered.species[filtered.species$Date>=input$dates[1] & filtered.species$Date<=input$dates[2],]
    ##take averages of data in date range
    filtered.dates=filtered.dates%>%
      group_by_("Station","longitude","latitude","Common.Name")%>%
      summarise(CPUE=mean(CPUE,na.rm=TRUE))
    filtered.date <-  filtered.dates %>% mutate(CPUE = replace(CPUE,CPUE==0,NA))
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
    if(nrow(filtered())==0){mymap2()}else{mymap1()}
  })
  
  ##Provides date range count
  output$DateRange <- renderText({
    paste("Your date range is", 
          difftime(input$dates[2], input$dates[1], units="days"),
          "days")
  })
  
  ##this function used to be an observation, switched to function
  ##in order to allow for downloading in rmarkdown
  myfun <- function(map){
    #this "pal" produces the desired colors and bins for distinguishing CPUE
    pal<-colorBin(
      palette="Reds",
      domain=filtered()$CPUE,
      bins=c(0,.1,1,10,100,1000),
      pretty = TRUE,
      na.color="black")
    
    #next call populates map with markers based on filtered() data
    addCircleMarkers(map, data = filtered(),lng = ~longitude, lat = ~latitude,radius=~ifelse(is.na(filtered()$CPUE),2,10),
                     stroke=TRUE, color="black",weight=2, fillOpacity=1,
                     fillColor=~pal(filtered()$CPUE),
                     popup = ~paste("Catch per Minute Towed:", filtered()$CPUE, "<br>",
                                    "Station:", Station,"<br>",
                                    "Coordinates:", latitude,",",longitude,"<br>"))%>%
      addLegend("bottomleft", pal=pal, values=filtered()$CPUE, title="Catch Per Minute of Tow",
                opacity=1)
  }
  #here is an observation using leafletProxy to take the above function and run it on our map
  #also have to have the "clear" calls here, as they won't work in the myfun
  observe({
    leafletProxy("map")%>%
      clearControls%>%
      clearMarkers()%>% myfun()
  })
  
  # Stop shiny app when closing the browser
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)