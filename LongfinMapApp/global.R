###Load Packages###

library(dplyr)
library(zoo)


data.longfin=readRDS("data/longfinagesgps.rds")
head(data.longfin)
data.longfin$age<-as.factor(data.longfin$age)
data.longfin<-select(data.longfin,-fl)
str(data.longfin)

#group by date,lat,long,and age and count each fish for each age per tow
#ie for this location at this date, 3 fish age0, 2 fish age2 etc
data.longfin.clean<-group_by(data.longfin, Date,lat,long,age)%>%
  summarize(agecount=n())
data.longfin.clean<-rename(data.longfin.clean,longitude=long,latitude=lat)
data.longfin.clean<-data.longfin.clean%>%
  mutate(month = format(Date, "%b"), year = format(Date, "%Y"),monthn=format(Date,"%m"))
head(data.longfin.clean)