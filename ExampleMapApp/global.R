###Load Packages###
##always load plyr before dplyr##
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

#Load the data
data.catch=read.table("data/catch.txt", header= T, sep= "\t")
fishcodes=read.table("data/fishcodes.txt", header= T, sep= "\t")
data.stations=read.table("data/stations.txt", header= T, sep= "\t")
data.tows=read.table("data/towinfo.txt", header= T, sep= "\t")

#####inspect elements#
head(data.stations)
sapply(data.stations,class)
#####format stations gps#
#transform into decimal degrees
data.stations$LatS<-data.stations$LatS/3600
data.stations$LatM<-data.stations$LatM/60
data.stations$LonS<-data.stations$LonS/3600
data.stations$LonM<-data.stations$LonM/60
#add minutes and seconds together
data.stations$LatMS<-data.stations$LatM+data.stations$LatS
data.stations$LonMS<-data.stations$LonM+data.stations$LonS
#combine data
data.stations$Latitude <- paste(data.stations$LatD,data.stations$LatMS)
data.stations$Longitude <- paste(data.stations$LonD,data.stations$LonMS)
head(data.stations)
#remove spaces and first zero, replace with "."
data.stations$Latitude <- gsub(" 0.", ".", data.stations$Latitude)
data.stations$Longitude <- gsub(" 0.", ".", data.stations$Longitude)
head(data.stations)
#add "-" infront of all longitude
data.stations$negative <- rep("-",nrow(data.stations)) # make new column 
data.stations$Longitude<-paste(data.stations$negative, data.stations$Longitude)
data.stations$Longitude <- gsub(" ", "", data.stations$Longitude)
head(data.stations)
#keep columns we need#
keep<-c("Station","Latitude","Longitude")
data.stations<-data.stations[ , which(names(data.stations) %in% keep)]
head(data.stations)
#transform Lat and Long to numeric class
data.stations<-transform(data.stations, Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))

##join tables together
data<-data.catch%>%
  inner_join(data.stations)%>%
  inner_join(data.tows)%>%
  inner_join(fishcodes)
head(data)

#format dates#
sapply(data,class)
data$Date <- as.Date(data$Date , "%m/%d/%Y")

#keep columns we need#
keeps<-c("Date","Station","Latitude","Longitude","Fish.Code","Catch","Duration","Common.Name")
data<-data[ , which(names(data) %in% keeps)]
head(data)

#calculate CPUE
data$CPUE=data$Catch/data$Duration
head(data)

###Next section to calculate "All" CPUE for each tow
#reshape fish catch to wide format
datawide <- dcast(data, Station +Latitude+Longitude+Duration+ Date ~ Common.Name, value.var="CPUE",fun.aggregate=sum)
head(datawide)
#learn which column numbers apply to fish
which(colnames(datawide)=="American shad")
which(colnames(datawide)=="yellowfin goby")
#calculate All
datawide$All=rowSums(datawide[,6:71], na.rm=T)
head(datawide)
#melt back straight
data=melt(datawide,id.vars=c("Station","Date","Longitude","Latitude"), measure.vars=c(6:72),
             variable.name="Common.Name",
             value.name="CPUE")
#rename Lat and Lon for leaflet
data<-rename(data, latitude=Latitude, longitude=Longitude)
head(data)
##reorder alphabetically by common name
data=data[order(data$Common.Name),]

##debug section
debug<-data%>%
  filter(Common.Name=="All")
head(debug)
