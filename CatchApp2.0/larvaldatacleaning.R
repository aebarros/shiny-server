library(dplyr)
library(lubridate)
library(plyr)
library(reshape2)
library(purrr)

#Import data
discard<-c("Date","Replicate")
data.catch=read.table("data/tblFishCatchInfo.txt", header= T, sep= "\t")
data.tow=read.table("data/tblTowInfo.txt", header= T, sep= "\t")
data.tow<-data.tow[ , -which(names(data.tow) %in% discard)]
data.water=read.table("data/tblWaterInfo.txt", header= T, sep= "\t")
data.water<-data.water[ , -which(names(data.water) %in% discard)]

#format TowDurations#
data.tow$TowDuration<-strptime(data.tow$TowDuration, "%m/%d/%Y %H:%M:%S")

#convert TowDuration to just numbers#
data.tow$TowDuration <- strftime(data.tow$TowDuration, format="%H:%M:%S")
data.tow$TowDuration <- gsub("00", "", data.tow$TowDuration)
data.tow$TowDuration<-gsub(":","",data.tow$TowDuration)
data.tow<-transform(data.tow, TowDuration = as.numeric(TowDuration))

#reshape fish catch to wide format
data.catch.wide <- dcast(data.catch, RecordNumber + Date + Net ~ FishCode, value.var="AdjustedLabCount",fun.aggregate=sum)
head(data.catch.wide)

#join data
data=data.catch.wide%>%
  inner_join(data.water)%>%
  inner_join(data.tow)
head(data)


#rename columns to match mapapp
data<-plyr::rename(data, c("RecordNumber"="ID","Net"="Method","Tide"="TideText", "StationName"="Polygon.Station","AvgTrawlDepth"="Depth","Tide.Height"="TideHeight","Temp.C.StartTop"="BegSurfTemp",
               "Conductivity.uS.cm.StartTop"="BegSurfCond","Salinity.ppt.StartTop"="BegSurfSalin","DOSat.Percent.StartTop"="BegSurfDO%",
               "DOSat.mg.L.StartTop"="BegSurfDO(mg/L)","Temp.C.StartBottom"="BegBotTemp","Conductivity.uS.cm.StartBottom"="BegBotCond",
               "Salinity.ppt.StartBottom"="BegBotSalin","DOSat.Percent.StartBottom"="BegBotDO%","DOSat.mg.L.StartBottom"="BegBotDO(mg/L)",
               "Temp.C.EndTop"="EndSurfTemp","Conductivity.uS.cm.EndTop"="EndSurfCond","Salinity.ppt.EndTop"="EndSurfSalin",
               "DOSat.Percent.EndTop"="EndSurfDO%","DOSat.mg.L.EndTop"="EndSurfDO(mg/L)","Temp.C.EndBottom"="EndBotTemp",
               "Conductivity.uS.cm.EndBottom"="EndBotCond","Salinity.ppt.EndBottom"="EndBotSalin","DOSat.Percent.EndBottom"="EndBotDO%",
               "DOSat.mg.L.EndBottom"="EndBotDO(mg/L)","StartSecchi.cm."="BegSecchi","EndSecchi.cm."="EndSecchi","StartNTU"="BegNTU","StartpH"="BegPH","EndpH"="EndPH"))

#remove unnecessary columns
removals<-c("StationNames","CableAngle","Latitude","Longitude","TrackNote","SalinityZone","WQStartFile","WQEndFile","SerialNumber","DELSME","LONSME/DELSME Hybrid","UNID Centrarchidae","UNID GOBY","UNID Long Skinny","UNID Non-Larval","Chl.ug.L.StartBottom","Chl.RFU.StartBottom","Chl.ug.L.StartTop","Chl.RFU.StartTop","Chl.ug.L.EndTop","Chl.RFU.EndTop","Chl.ug.L.EndBottom","Chl.RFU.EndBottom")
data<-data[ , -which(names(data) %in% removals)]
head(data)

#change date from type factor top type date
data$Date = as.Date(data$Date, format = "%m/%d/%Y")


saveRDS(data,file="C:\\Users\\Barros\\Desktop\\R\\shiny-server\\OtterTrawlMapApp\\data\\larvaldata.rds")

#debugging
debug=data%>%
  filter(Method!="SLS")%>%
  filter(Method!="Mysid")
