###Load Packages###
##always load plyr before dplyr##
library(shiny)
library(shinyjs)
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
                     navbarPage("Adult Longfin Catch"),
                     tags$style(type = "text/css", "#map {height: calc(100vh - 120px) !important;}"),
                     leafletOutput("map", height = "100%"),
                     tags$div(id="cite",
                              'Data compiled for ', tags$b('Hobbs Lab Longfin Smelt Survey'), ' by Arthur Barros (2018).'
                     ),
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                   width = 200, height = "auto",
                                   h2("Catch Explorer"),
                                   selectInput("year","Year",
                                               c(unique(as.numeric(data.longfin.clean$year)))),
                                   selectInput("month","Month",
                                               c(unique(as.character(data.longfin.clean$month)))),
                                   selectInput("age","age",
                                               c(unique(as.character(data.longfin.clean$age)),"All")),
                                   submitButton("Submit"))
                   )
)
server <- function(input, output, session) {
  
  ###########Interactive Map##############################################
  #Reactive expression used to filter out by user selected variables for final data "filtered()"
  #for map generation
  filtered<-reactive({
    ifelse(input$age=="All",filtered.age<-data.longfin.clean,filtered.age<-data.longfin.clean[data.longfin.clean$age==input$age,])
    filtered.year<-filtered.age[filtered.age$year==input$year,]
    filtered.dates<-filtered.year[filtered.year$month==input$month,]
    ##take averages of data in date range
    filtered.dates=filtered.dates%>%
      group_by_("longitude","latitude")%>%
      dplyr::summarise(Count=sum(agecount))
    filtered.dates <-  filtered.dates %>% mutate(Count = replace(Count,Count==0,NA))
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
  
  ##Provides date range count
  output$DateRange <- renderText({
    paste("Your date range is", 
          difftime(input$dates[2], input$dates[1], units="days"),
          "days")
  })
  
  ##this function used to be an observation, switched to function
  ##in order to allow for downloading in rmarkdown
  myfun <- function(map){
 
    #next call populates map with markers based on filtered() data
    addCircleMarkers(map, data = filtered(),lng = ~longitude, lat = ~latitude,radius=2,
                     stroke=TRUE, color="red",weight=10, fillOpacity=1,
                     popup = ~paste("Coordinates:", latitude,",",longitude,"<br>",
                                    "Count:", Count))
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