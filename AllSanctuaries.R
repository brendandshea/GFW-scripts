library(tidyverse)

rm(list=ls())
setwd("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir")

#gathers all predictions into a master dataframe
fp <- read.csv("frenchpolynesia_total_preds.csv")
fp$country = "French Polynesia"
sam <- read.csv("samoa_total_preds.csv")
sam$country = "Samoa"
plw <- read.csv("palau_total_preds.csv")
plw$country = "Palau"
cok <- read.csv("cookislands_total_preds.csv")
cok$country = "Cook Islands"
fsm <- read.csv("micronesia_total_preds.csv")
fsm$country = "FSM"
ncl <- read.csv("newcaledonia_total_preds.csv")
ncl$country = "New Caledonia"
kir <- read.csv("kiribati_total_preds.csv")
kir$country = "Kiribati"
mhl <- read.csv("marshallislands_total_preds.csv")
mhl$country="Marshall Islands"

total_proj <- rbind(fp,sam,plw,cok,fsm,ncl,kir,mhl)

write_csv(total_proj, "total_preds.csv")

#summarized

#gathers all summaries into a master dataframe
fp_sum <- read.csv("frenchpolynesia_summary.csv")
fp_sum$country = "French Polynesia"
sam_sum <- read.csv("samoa_summary.csv")
sam_sum$country = "Samoa"
plw_sum <- read.csv("palau_summary.csv")
plw_sum$country = "Palau"
cok_sum<- read.csv("cookislands_summary.csv")
cok_sum$country = "Cook Islands"
fsm_sum <- read.csv("micronesia_summary.csv")
fsm_sum$country = "FSM"
ncl_sum <- read.csv("newcaledonia_summary.csv")
ncl_sum$country = "New Caledonia"
kir_sum <- read.csv("kiribati_summary.csv")
kir_sum$country = "Kiribati"
mhl_sum <- read.csv("marshallislands_summary.csv")
mhl_sum$country="Marshall Islands"

total_summary <- rbind(fp_sum,sam_sum,plw_sum,cok_sum,fsm_sum,ncl_sum,kir_sum,mhl_sum)

write_csv(total_summary, "total_summary.csv")

#mortality
totalmortality <- aggregate(data=total_summary, total~Species, FUN='sum')
totalmortality$total.upr <- aggregate(data=total_summary, total.upr~Species, FUN='sum')$total.upr
totalmortality$total.lwr <- aggregate(data=total_summary, total.lwr~Species, FUN='sum')$total.lwr

#catch
totalcatch <- aggregate(data=total_summary, catch~Species, FUN='sum')
totalcatch$catch.upr <- aggregate(data=total_summary, catch.upr~Species, FUN='sum')$catch.upr
totalcatch$catch.lwr <- aggregate(data=total_summary, catch.lwr~Species, FUN='sum')$catch.lwr

totalmortality$avg.wt=c(24.9,84.2,42.1,51.3,40.5,35.4,93.4)
totalmortality$metricton <- totalmortality$total*totalmortality$avg.wt/1000

sum(totalcatch$catch)
sum(totalcatch$catch.upr)
sum(totalcatch$catch.lwr)

sum(totalmortality$total)
sum(totalmortality$total.upr)
sum(totalmortality$total.lwr)

write_csv(totalmortality,"totalmortality.csv")
write_csv(totalcatch,"totalcatch.csv")


total_proj <- read.csv("total_preds.csv")
bysanctuary<-aggregate(total_proj, total ~ country , FUN=sum)
bysanctuary$tuna<-c(273.15,328.41,15104.61,21099.06,8837.28,66.51,252.63,82.98)
bysanctuary$rate<-bysanctuary$total/bysanctuary$tuna
