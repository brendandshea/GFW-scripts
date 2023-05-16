library(tidyverse)
library(fields)
library(mgcv)
library(gratia)
library(countrycode)

rm(list=ls())
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

datMaster2 <- read.csv("RFMO-master.csv") #RFMO hook data
datMaster2 <- subset(datMaster2, year==2018)
dat <- read.csv("~/Downloads/GFW_AIS/2018LL.csv") #AIS data at 5x5 res

#re-name column
colnames(dat)[6] <- "hours"
colnames(dat)[2] <- "lat"
colnames(dat)[3] <- "lon"
colnames(dat)[4] <- "fleet"

dat$lon <- plyr::round_any(dat$lon, 5, floor)
dat$lat <- plyr::round_any(dat$lat, 5, floor)


#aggregate hook data and rename
hookdat = with(datMaster2,aggregate(hooks, by = list(lon,lat, fleet), sum))
names(hookdat) = c("lon","lat","fleet","hooks2")

#pull out two-letter country codes
hookdat$fleet = gsub("[0-9]","",hookdat$fleet, perl = T)
hookdat$fleet = gsub("EU([A-Z][A-Z][A-Z])","\\1",hookdat$fleet, perl = T)

# convert the two letter codes to three letters
hookdat$fleet <- countrycode(hookdat$fleet, origin="iso2c", destination = "iso3c", nomatch = NULL)


#aggregate AIS data and rename
aisdat = with(dat,aggregate(hours, by = list(lon,lat, fleet), sum))
names(aisdat) = c("lon","lat","fleet","hours") # not needed though
tot2 = merge(aisdat,hookdat, by=c("lon","lat","fleet"), all=TRUE)

#data clean
tot2[which(tot2==Inf)] = NA
tot2 = na.omit(tot2[,c("lon","lat","fleet","hours","hooks2")])

#plot
plot(log(hooks2)~log(hours), tot2)


#data clean
tot2$fleet = factor(tot2$fleet)
tot2 = tot2[tot2$hooks2!=0,]
tot2 = tot2[tot2$hours!=0,]

# GAMM model ####
#write model
gamm1 = gam(log(hooks2)~log(hours)+s(lat)+s(lon)+s(fleet, bs = "re"), data = tot2, method='REML', family=scat) 

#check spline wiggliness
gam.check(gamm1)

#simulate residuals and check fit
appraise(gamm1, method='simulate')

#plot
draw(gamm1)

#summarize
summary(gamm1)

#save it
saveRDS(gamm1, "hooks_gamm_2018.rds")

#### Checking strength of random effects at given point in space ####
newdata = data.frame(hours = 100, fleet = unique(tot2$fleet), lat = 0, lon = 170)
fit <- predict(gamm1, newdata = newdata, se.fit = TRUE)
newdata$pred = fit$fit
newdata$se.fit = fit$se.fit 
newdata$pred.hooks = exp(newdata$pred)
newdata = with(newdata,data.frame(newdata, lwr=pred-1.96*se.fit,upr=pred+1.96*se.fit))
newdata$phl = exp(newdata$lwr)
newdata$phu = exp(newdata$upr)
newdata$fleet = countrycode(newdata$fleet, origin="iso2c", destination = "iso3c", nomatch = NULL)
newdata = newdata[order(newdata$pred.hooks),]
pos = 1:nrow(newdata)
with(newdata,plot(pred.hooks/1000,pos, pch=16, xlim = c(min(phl/1000), max(phu/1000)), axes = F, ylab = "", xlab = "Predicted #hooks per 100 AIS hours"))
with(newdata,segments(phl/1000,pos,phu/1000,pos))
axis(1)
axis(2,at = pos, labels = as.character(newdata$fleet), las = 1)


#### Comparing model predictions to global reporting of hooks ####
dat2 = dat
dat2 = dat2[dat2$hours!=0,]
dat2$pred.hooks = exp(predict(gamm1, newdata = dat2)) 
# compare with RFMO hooks
sum(datMaster2$hooks); sum(dat2$pred.hooks, na.rm=T) #sums
sum(dat2$pred.hooks, na.rm=T) / sum(datMaster2$hooks) #ratio

#Excluding random effect
dat2 = dat
dat2 = dat2[dat2$hours!=0,]
dat2$pred.hooks = exp(predict(gamm1, newdata = dat2, exclude = "s(fleet)")) 
sum(datMaster2$hooks); sum(dat2$pred.hooks, na.rm=T) #sums
sum(dat2$pred.hooks, na.rm=T) / sum(datMaster2$hooks) #ratio

# Using global "average" fleets
dat2 = dat
dat2 = dat2[dat2$hours!=0,]
dat2$fleet = "PNG"
dat2$pred.hooks = exp(predict(gamm1, newdata = dat2)) 
sum(datMaster2$hooks); sum(dat2$pred.hooks, na.rm=T)
sum(dat2$pred.hooks, na.rm=T) / sum(datMaster2$hooks)

dat2 = dat
dat2 = dat2[dat2$hours!=0,]
dat2$fleet = "CHN"
dat2$pred.hooks = exp(predict(gamm1, newdata = dat2)) 
sum(datMaster2$hooks); sum(dat2$pred.hooks, na.rm=T)
sum(dat2$pred.hooks, na.rm=T) / sum(datMaster2$hooks)

#"low" fleet
dat2 = dat
dat2 = dat2[dat2$hours!=0,]
dat2$fleet = "VCT"
dat2$pred.hooks = exp(predict(gamm1, newdata = dat2)) 
sum(datMaster2$hooks); sum(dat2$pred.hooks, na.rm=T)
sum(dat2$pred.hooks, na.rm=T) / sum(datMaster2$hooks)

#residuals in space
tot3 <- tot2
tot3$Resid=resid(gamm1)

bbox = c(-90,90,-180,180)
ylim <- c(bbox[1], bbox[2]) 
xlim <- c(bbox[3], bbox[4])
tot3_Grid = with(tot3,mapplots::make.grid(x = lon, y = lat, z = Resid, xlim=xlim,ylim =ylim, byx=5,byy=5))
zlim = range(tot3_Grid, na.rm = TRUE)
zlim = c(-10,10)

lati = as.numeric(dimnames(tot3_Grid)[[2]])
long = as.numeric(dimnames(tot3_Grid)[[1]])

colorTable<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-3, 0, 3) / 10)
image.plot(long, lati, tot3_Grid, las = 1, ylab = "latitude", xlab = "longitude", main = "Hooks~Hours - 2019", legend.lab="Residuals", zlim = zlim, col=colorTable)
map(world, xlim = xlim, ylim = ylim, add = T, fill = T, col="black")



#### 0.25 degree predictions ####

dat <- read.csv("~/Downloads/GFW_AIS/2018LL.csv") #AIS data at 5x5 res

#re-name column
colnames(dat)[6] <- "hours"
colnames(dat)[2] <- "lat"
colnames(dat)[3] <- "lon"
colnames(dat)[4] <- "fleet"

dat$lon <- plyr::round_any(dat$lon, 0.25, floor)
dat$lat <- plyr::round_any(dat$lat, 0.25, floor)

aisdat = with(dat,aggregate(hours, by = list(lon,lat, fleet), sum))
names(aisdat) = c("lon","lat","fleet","hours") # not needed though


write_csv(aisdat, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/2018effort_*25x*25.csv")

