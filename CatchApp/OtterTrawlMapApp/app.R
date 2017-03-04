###Load Packages###
library(dplyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(reshape2)
library(leaflet)
library(DT)
library(shinythemes)
library(rmarkdown)
library(webshot)
##source the global file with all the necessary data
source("global.R")
water <- c(
  "Salinity(ppt)" = "BegSurfSalin",
  "Temperature(°C)" = "BegSurfTemp",
  "Conductivity(µS)" = "BegSurfCond",
  "DO%" = "BegSurfDO.",
  "DO mg/L" = "BegSurfDO.mg.L.")

####Shiny####
##UI building
ui = bootstrapPage(theme = shinytheme("sandstone"),
                   navbarPage("Bay Area Catch Data",
                   tabPanel("Interactive Map",
                            tags$style(type = "text/css", "html, body {width:100%;height:90%}"),
                            tags$style(type = "text/css", "#map {height: calc(100vh - 120px) !important;}"),
                   leafletOutput("map"),
                   tags$div(id="cite",
                            'Data compiled for ', tags$b('Hobbs Lab Longfin Smelt Survey'), ' by Arthur Barros (2017).'
                   ),
                   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                 draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                 width = 200, height = "auto",
                                 h2("Catch Explorer"),
                                 selectizeInput("species", "Species",
                                                unique(as.character(data.melt.average$CommonName))),
                                 dateRangeInput("dates","Date range", start="2015-01-01", end="2015-01-31"),
                                 textOutput("DateRange"),
                                 selectInput("region","Region",
                                             c(unique(as.character(data.melt.average$Bay.Region)),"All")),
                                 downloadButton('html_link','Download map'))
                   ),
                   tabPanel("Data Explorer",
                            fluidRow(
                              column(3,
                                     selectizeInput("species2", "Species",
                                                    unique(as.character(data.melt.average$CommonName))),
                                     selectInput("region2","Region",
                                                 c(unique(as.character(data.melt.average$Bay.Region)),"All")),
                                     dateRangeInput("dates2", label = h3("Date range"), start="2015-01-01", end="2015-01-31"),
                                     textOutput("DateRange2"),
                                     selectInput("water","Water Parameter",
                                                 water)
                                     ),
                              column(9,
                                     (plotOutput("plot2", height=300)),
                                     (plotOutput("plot3", height=300))
                              ))
                            )
))
server <- function(input, output, session) {
  options(shiny.sanitize.errors = FALSE)
  ###########Interactive Map##############################################
  # Reactive expression used to filter out by user selected variables for final data "filtered()"

  filtered<-reactive({
    ifelse(input$region=="All",filtered.region<-data.cpue.melt,filtered.region<-data.cpue.melt[data.cpue.melt$Bay.Region==input$region,])
    filtered.species<- filtered.region[filtered.region$CommonName==input$species,]
    filtered.dates<-filtered.species[filtered.species$Date>=input$dates[1] & filtered.species$Date<=input$dates[2],]
    ##take averages data in date range
    filtered.dates=filtered.dates%>%
      group_by_("month","Bay.Region","monthname","year","Polygon.Station","latitude","longitude","CommonName","NatInv")%>%
      summarise(CPUE=mean(CPUE,na.rm=TRUE), BegSurfSalin=mean(BegSurfSalin,na.rm=TRUE), BegSurfTemp=mean(BegSurfTemp, na.rm=TRUE), 
                BegSurfCond=mean(BegSurfCond, na.rm=TRUE),BegSurfDO.=mean(BegSurfDO.,na.rm=TRUE),
                BegSurfDO.mg.L.=mean(BegSurfDO.mg.L.,na.rm=TRUE))
    filtered.date <-  filtered.dates %>% mutate(CPUE = replace(CPUE,CPUE==0,NA))
  })
  
  #produces base map
  mymap <- reactive({
    leaflet(filtered()) %>% addProviderTiles("Hydda.Base")%>%
      fitBounds(~min(longitude)-.005, ~min(latitude)-.005, ~max(longitude)+.005, ~max(latitude)+.005)
  })
  #renders map for main panel in ui
  output$map <- renderLeaflet({
    mymap()
  })

  ##Provides errors for date range data
  output$DateRange <- renderText({
    # make sure end date later than start date
    validate(
      need(input$dates[2] > input$dates[1], "end date is earlier than start date"
      )
    )
    
    # make sure greater than 1 week difference
    validate(
      need(difftime(input$dates[2], input$dates[1], "days") > 7, "date range less the 7 days"
      )
    )
    
    paste("Your date range is", 
          difftime(input$dates[2], input$dates[1], units="days"),
          "days")
  })
  
  ##Provides errors for date range data
  output$DateRange2 <- renderText({
    # make sure end date later than start date
    validate(
      need(input$dates2[2] > input$dates2[1], "end date is earlier than start date"
      )
    )
    
    # make sure greater than 1 week difference
    validate(
      need(difftime(input$dates2[2], input$dates2[1], "days") > 7, "date range less the 7 days"
      )
    )
    
    paste("Your date range is", 
          difftime(input$dates2[2], input$dates2[1], units="days"),
          "days")
  })
 
  ##this function used to be an observation, switched to function
  ##in order to allow for downloading in rmarkdown
   myfun <- function(map){
     #this "pal" produces the desired colors and bins for distinguishing CPUE
     pal<-colorBin(
       palette="Reds",
       domain=filtered()$CPUE,
       bins=c(0,.1,1,10,100),
       pretty = TRUE,
       na.color="black")
     
     #next call populates map with markers based on filtered() data
       addCircleMarkers(map, data = filtered(),radius=~ifelse(is.na(filtered()$CPUE),2,10),
                        stroke=TRUE, color="black",weight=2, fillOpacity=1,
                        fillColor=~pal(filtered()$CPUE),
                        popup = ~paste("Catch per Minute Towed:", filtered()$CPUE, "<br>",
                                       "Station:", Polygon.Station,"<br>",
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

#next call handles the download of the pdf, starts by making an html rmarkdown document 
 output$html_link <- downloadHandler(
   filename = 'plot.pdf',
   
   content = function(file) {
     src <- normalizePath('mymap.Rmd')
     
     # temporarily switch to the temp dir, in case you do not have write
     # permission to the current working director
     ##owd <- setwd(tempdir())
     ##on.exit(setwd(owd)
     ##############AB:note, had to stop this to allow rmarkdown document to save changes
     #file.copy(src, 'mymap.Rmd')
     out <- render('mymap.Rmd',
                   html_document()
     )
     file.rename(out, 'temp.html')
     #webshot changes the rmarkdown from html to a static pdf
     webshot("temp.html", file = file, cliprect = "viewport", delay=2)
   }
 )
 ###############Data Explorer##########
 filtered2<-reactive({
   ifelse(input$region2=="All",filtered.region2<-data.cpue.melt,filtered.region2<-data.cpue.melt[data.cpue.melt$Bay.Region==input$region2,])
   filtered.species2<- filtered.region2[filtered.region2$CommonName==input$species2,]
   filtered.dates2<-filtered.species2[filtered.species2$Date>=input$dates2[1] & filtered.species2$Date<=input$dates2[2],]
   ##take averages data in date range
   filtered.dates2=filtered.dates2%>%
     group_by_("Polygon.Station","latitude","longitude","CommonName","NatInv")%>%
     summarise(CPUE=mean(CPUE,na.rm=TRUE), BegSurfSalin=mean(BegSurfSalin,na.rm=TRUE), BegSurfTemp=mean(BegSurfTemp, na.rm=TRUE), 
               BegSurfCond=mean(BegSurfCond, na.rm=TRUE),BegSurfDO.=mean(BegSurfDO.,na.rm=TRUE),
               BegSurfDO.mg.L.=mean(BegSurfDO.mg.L.,na.rm=TRUE))
   filtered.date2 <-  filtered.dates2 %>% mutate(CPUE = replace(CPUE,CPUE==0,NA))
 })
 output$plot2<-reactivePlot(function(){
   p<-ggplot(filtered2(),aes(Polygon.Station,CPUE))+
   geom_bar(stat="identity")+
     theme_minimal()+
     ggtitle("Catch")+
     xlab("station name")+
     ylab("CPUE (catch per minute tow)")+
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
   print(p)
 })

 output$plot3<-reactivePlot(function(){
   q<-ggplot(filtered2(),aes_string(x="Polygon.Station", y=input$water))+
     geom_bar(stat="identity")+
     theme_minimal()+
     ggtitle("Water Quality")+
     xlab("station name")+
     ylab(input$water)+
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
   print(q)
 })
    
    # MW: Stop shiny app when closing the browser
    session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
