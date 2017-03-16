###Load Packages###
##always load plyr before dplyr##
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(rsconnect)
library(shinythemes)
library(rmarkdown)
library(reshape2)
data.cpue.melt=readRDS("data/cleandata.rds")
water <- c(
  "Salinity(ppt)" = "BegSurfSalin",
  "Temperature(°C)" = "BegSurfTemp",
  "Conductivity(µS)" = "BegSurfCond",
  "DO%" = "BegSurfDO%",
  "DO mg/L" = "BegSurfDO(mg/L)")
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
                   navbarPage("Hobbs Lab Catch and Water Data",
                              tabPanel("Data Explorer",
                                       fluidRow(
                                         column(3,
                                                selectizeInput("species2", "Species",
                                                               unique(as.character(data.cpue.melt$CommonName))),
                                                selectInput("region2","Region",
                                                            c(unique(as.character(data.cpue.melt$Bay.Region)),"All")),
                                                dateRangeInput("dates2", "Date range", start="2015-01-01", end="2015-01-31"),
                                                selectInput("method2","Gear Type",gear),
                                                textOutput("DateRange2"),
                                                selectInput("water","Water Parameter",water),
                                                submitButton("Submit")
                                         ),
                                         column(9,
                                                (plotOutput("plot2", height=300)),
                                                (plotOutput("plot3", height=300))
                                         ))
                              )
                              ))
server <- function(input, output, session) {
  ###############Data Explorer##########
  filtered2<-reactive({
    ifelse(input$region2=="All",filtered.region2<-data.cpue.melt,filtered.region2<-data.cpue.melt[data.cpue.melt$Bay.Region==input$region2,])
    filtered.species2<- filtered.region2[filtered.region2$CommonName==input$species2,]
    filtered.gear2<-filtered.species2[filtered.species2$Method==input$method,]
    filtered.dates2<-filtered.species2[filtered.species2$Date>=input$dates2[1] & filtered.species2$Date<=input$dates2[2],]
    ##take averages data in date range
  })
  output$plot2<-reactivePlot(function(){
    p<-ggplot(filtered2(),aes(Polygon.Station,CPUE))+
      geom_boxplot()+
      theme_minimal()+
      ggtitle("Catch")+
      xlab("station name")+
      ylab("log10 CPUE (catch per minute tow)")+
      scale_y_log10()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(p)
  })
  
  output$plot3<-reactivePlot(function(){
    q<-ggplot(filtered2(),aes_string(x="Polygon.Station", y=input$water))+
      geom_boxplot()+
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