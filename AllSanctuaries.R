rm(list=ls())
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

totalmortality <- aggregate(data=total_proj, total~Species, FUN='sum')
total_proj$total.var <- total_proj$total.sd^2
totalmortalityvar <- aggregate(data=total_proj, total.var~Species, FUN='sum')
totalmortality$total.SD <- sqrt(totalmortalityvar$total.var)

totalcatch <- aggregate(data=total_proj, catch~Species, FUN='sum')
total_proj$catch.var <- total_proj$catch.sd^2
totalcatchvar <- aggregate(data=total_proj, catch.var~Species, FUN='sum')
totalcatch$catch.SD <- sqrt(totalcatchvar$catch.var)


totalmortality$avg.wt=c(24.9,84.2,42.1,51.3,40.5,35.4,93.4)
totalmortality$metricton <- totalmortality$total*totalmortality$avg.wt/1000
totalmortality$metrictonupper <- (totalmortality$total+1.96*totalmortality$total.SD)*totalmortality$avg.wt/1000


sum(totalcatch$catch)
sum(sqrt(totalcatch$catch.SD^2))
sum(totalmortality$total)
sum(sqrt(totalmortality$total.SD^2))

write_csv(totalmortality,"totalmortality.csv")
write_csv(totalcatch,"totalcatch.csv")

