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


##Read Data##
data=read.table("data/CrossTabData.txt", header=T, sep="\t")
#drop lat and long fields to replace later from PolygonGPS#
head(data)
polygon.gps=read.table("data/PolygonGPS.txt", header=T, sep="\t")
species.info=read.table("data/tblSpeciesLookup.txt", header=T, sep="\t")
station.info=read.table("data/tblStationRegions.txt", header=T, sep="\t")
larval.data=readRDS("data/larvaldata.rds")
dataDFW=readRDS("data/dataDFW.rds")


##Clean Data##
#format dates#
data$Date <- as.Date(data$Date , "%m/%d/%Y")
#join data#
data=data%>%
  rbind.fill(data,larval.data)
data.location=station.info%>%
  inner_join(polygon.gps)
data=data%>%
  inner_join(data.location)
head(data)


#order dates and add month/year columns#
data[order(data$Date, decreasing=TRUE ),]
data<-data%>%
  mutate(month = format(Date, "%m"), year = format(Date, "%Y"))
#select for 2015 and 2016#
#target=c("2015","2016")
#data=data%>%
#filter(year %in% target)
head(data)

data.clean=data
#filter out inverts#
data.clean <- subset( data.clean, select = -c(AMPHIPOD, AMPHPD, ASIMUS,CALLIA, CMAENA, CMAGIS, CORBIC, CORBUL, CPRODU, CRANFR, CRANNC, CRANNM, CRANSP, DECORC, EXOPAL,HEMIGR, HEPTSTI, HETERO, HOREGO, IOBSOL, ISOPOD, JELLYF, LSTYLI, MACOMA, MCALIF, MSENHO, MUSBLE, MUSCUL, MUSSEN, MYSID, MYSIDR, MYTILS, NUDIBR, OLURID, PALAEM, PHILIN, PILEWR, POLYCH, PPRODU, RHARRI, RIDGEM, SNAILS, SPONGE, TAPJAP, TUNICA, UCINER, UNID.Clam, UNID.CRAB, UNID.NUD, UNID.LARVAL,UNID.GOBY) )
head(data.clean)

#need to turn all 0 catches to NA values to calculate CPUE properly
data.clean[data.clean == 0] <- NA

#calculate CPUE (catch per minute towed)#
#assign these to new fish range#
#the "which(colnames)" call will display in the console what columns match those 
which(colnames(data.clean)=="AMESHA")
which(colnames(data.clean)=="YELSEE")
data.cpue=data.clean
data.cpue[,47:110]=data.cpue[,47:110]/data.clean$TowDuration
head(data.cpue)

#sum up catch data for totals#
data.cpue$All=rowSums(data.cpue[,47:110], na.rm=T)
head(data.cpue)
#rearrange order#
data.cpue = data.cpue[, c(1:46,129, 47:128)]


#change month from number to month name#
data.cpue$monthname <- months(as.Date(data.cpue$Date))
head(data.cpue)

##rearrange in longform using reshape2 melt##
which(colnames(data.cpue)=="All")
which(colnames(data.cpue)=="YELSEE")
data.cpue.melt=melt(data.cpue,id.vars=c("ID","Polygon.Station","Method","Date","longitude","latitude"), measure.vars=c(47:111),
                    variable.name="FishCode",
                    value.name="CPUE")
head(data.cpue.melt)

data.cpue.melt=data.cpue.melt[order(data.cpue.melt$FishCode),]
head(data.cpue.melt)

#join in common names#
data.cpue.melt=data.cpue.melt%>%
  inner_join(species.info)
head(data.cpue.melt)

#add in research group name#
data.cpue.melt$Department <- rep("Hobbs Lab",nrow(data.cpue.melt)) # make new column 

#write to RDS to maintain classes#
saveRDS(data.cpue.melt,file="C:\\Users\\hobbs\\Desktop\\R for work\\Map app cleaning\\Hobbs Lab\\data\\hobbslab.rds")
