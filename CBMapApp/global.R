###Load Packages###

library(dplyr)
library(sp)


data.cb=readRDS("data/cbdata.rds")
head(data.cb)
data.cb$latitude<-jitter(data.cb$latitude,factor=10)
data.cb$longitude<-jitter(data.cb$longitude,factor=10)
head(data.cb)
