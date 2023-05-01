library(tidyverse)

ais_wd <- '~/Downloads/GFW_AIS/' #Directory for AIS
setwd(ais_wd)

PS_AIS <- read.csv("2019PS.csv")
head(PS_AIS)
colnames(PS_AIS)
colnames(PS_AIS)[c(2,3)]<-c("Lat","Lon")

PS_AIS_5 <- PS_AIS
PS_AIS_0_25 <- PS_AIS

PS_AIS_5$Lon <- floor(PS_AIS_5$Lon)
PS_AIS_5$Lat <- floor(PS_AIS_5$Lat)
PS_AIS_5 <- aggregate(PS_AIS_5, fishing_hours ~ Lon + Lat + flag, FUN=sum)

PS_AIS_0_25$Lon <- plyr::round_any(PS_AIS_0_25$Lon, 0.25, f = floor)
PS_AIS_0_25$Lat <- plyr::round_any(PS_AIS_0_25$Lat, 0.25, f = floor)
PS_AIS_0_25 <- aggregate(PS_AIS_0_25, fishing_hours ~ Lon + Lat + flag, FUN=sum)

write_csv(PS_AIS_5, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/PS_AIS_5x5_2019.csv")

write_csv(PS_AIS_0_25, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/PS_AIS_-25x-25_2019.csv")
