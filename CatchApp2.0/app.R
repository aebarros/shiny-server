###Load Packages###
##always load plyr before dplyr##
library(shiny)
library(shinyjs)
library(leaflet)
library(shinythemes)
library(rsconnect)
##source the global file with all the necessary data
source("global.R")
##set up "water" for the water quality input select for data explorer tab

gear<- c(
  "Large Otter Trawl"="LOT",
  "Small Otter Trawl"="SOT",
  "Larval Sled"="SLS",
  "Seine"="SEINE",
  "20mm"="20mm",
  "Mysid"="Mysid",
  "Mid-Water Trawl"="MWT")


####Shiny####
##UI building
ui = bootstrapPage(theme = shinytheme("sandstone"),
                   div(
                       id = "app-content",
                   navbarPage("Bay Area/Delta Catch Data"),
                   tags$style(type = "text/css", "#map {height: calc(100vh - 120px) !important;}"),
                   leafletOutput("map", height = "100%"),
                   tags$div(id="cite",
                            'Data compiled for ', tags$b('Hobbs Lab Longfin Smelt Survey'), ' by Arthur Barros (2017).'
                   ),
                   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                 draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                 width = 200, height = "auto",
                                 h2("Catch Explorer"),
                                 selectizeInput("species", "Species",
                                                unique(as.character(data.cpue.melt$CommonName))),
                                 dateRangeInput("dates","Date range", start="2015-01-01", end="2015-01-31"),
                                 textOutput("DateRange"),
                                 selectInput("agency","Agency",
                                             c(unique(as.character(data.cpue.melt$Department)),"All")),
                                 selectInput("method","Gear Type",gear),
                                 #downloadButton('html_link','Download map'),
                                 submitButton("Submit"))
                     )
)
server <- function(input, output, session) {

  ###########Interactive Map##############################################
  #Reactive expression used to filter out by user selected variables for final data "filtered()"
  #for map generation
  filtered<-reactive({
    ifelse(input$agency=="All",filtered.agency<-data.cpue.melt,filtered.agency<-data.cpue.melt[data.cpue.melt$Department==input$agency,])
    filtered.species<- filtered.agency[filtered.agency$CommonName==input$species,]
    filtered.gear<-filtered.species[filtered.species$method==input$method,]
    filtered.dates<-filtered.gear[filtered.gear$Date>=input$dates[1] & filtered.gear$Date<=input$dates[2],]
    ##take averages of data in date range
    filtered.dates=filtered.dates%>%
      group_by_("polystn","longitude","latitude","CommonName")%>%
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
       bins=c(.001,.1,1,10,100,1000),
       pretty = TRUE,
       na.color="black")
     
     #next call populates map with markers based on filtered() data
       addCircleMarkers(map, data = filtered(),lng = ~longitude, lat = ~latitude,radius=~ifelse(is.na(filtered()$CPUE),2,10),
                        stroke=TRUE, color="black",weight=2, fillOpacity=1,
                        fillColor=~pal(filtered()$CPUE),
                        popup = ~paste("Catch per Minute Towed:", filtered()$CPUE, "<br>",
                                       "Station:", polystn,"<br>",
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
   
    # MW: Stop shiny app when closing the browser
    session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)