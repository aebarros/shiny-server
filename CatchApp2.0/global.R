###Load Packages###
##always load plyr before dplyr##
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(reshape2)
library(leaflet)
library(purrr)
library(rsconnect)


hobbslab=readRDS("data/hobbslab.rds")
dataDFW=readRDS("data/dataDFW.rds")
head(hobbslab)
head(dataDFW)

#rename columns to match mapapp
dataDFW<-plyr::rename(dataDFW, c("Polygon.Station"="polystn","Method"="method"))
head(data)

########Connect both data sets
data.cpue.melt=hobbslab%>%
  rbind.fill(hobbslab,dataDFW)
head(data.cpue.melt)

#order by common name
data.cpue.melt=data.cpue.melt[order(data.cpue.melt$CommonName),]

#change NA values to 0#
data.cpue.melt$CPUE[is.na(data.cpue.melt$CPUE)]<-0

#average CPUE by month, year and station#
#####not using this anymore due to changing the date system
#data.melt.average=data.cpue.melt%>%
  #group_by_("month","Bay.Region","monthname","year","Polygon.Station","latitude","longitude","CommonName","NatInv")%>%
  #summarise(CPUE=mean(CPUE,na.rm=TRUE), BegSurfSalin=mean(BegSurfSalin,na.rm=TRUE), BegSurfTemp=mean(BegSurfTemp, na.rm=TRUE), 
            #BegSurfCond=mean(BegSurfCond, na.rm=TRUE),BegSurfDO.=mean(BegSurfDO.,na.rm=TRUE),
            #BegSurfDO.mg.L.=mean(BegSurfDO.mg.L.,na.rm=TRUE))
#data.melt.average

#now switch 0 back to na#
#data.melt.average$CPUE[data.melt.average$CPUE == 0] <- NA

#get bins for CPUE averages#
#max(data.melt.average$CPUE, na.rm=TRUE)
#data.melt.average[order(data.melt.average$CPUE, decreasing=TRUE ),]
#CPUE.bins <- hist(data.melt.average$CPUE,plot=TRUE)


###data debugging###
data.debug=data.cpue.melt%>%
  filter(Department=="CDFW")
head(data.debug)
