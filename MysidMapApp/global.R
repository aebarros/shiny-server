###Load Packages###
##always load plyr before dplyr##
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(purrr)

#Load the data
data<-readRDS("data/hobbslab.rds")
head(data)
