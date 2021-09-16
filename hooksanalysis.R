library(tidyverse)
library(fields)

hours=read.csv("effort_longline_2015.csv")
hooks=read.csv("longlineEffort2011-2017.csv")
hooks=subset(hooks, year=="2015")

#aggregate hooks data so that there is one hooks value (per fleet) per cell
hooks2=aggregate(data=hooks, hooks~lat*lon, FUN="sum")
#hooks2=aggregate(data=hooks, hooks~lat*lon*fleet, FUN="sum")

#Aggregate AIS data at 5*5 resolution
hours$roundedlat=round(hours$lat_bin/5)*5
hours$roundedlon=round(hours$lon_bin/5)*5

hours2=aggregate(data=hours, fishing_hours~roundedlat*roundedlon, FUN="sum") #removed flag
#hours2=aggregate(data=hours, fishing_hours~roundedlat*roundedlon*flag, FUN="sum")

#make variable names constant
names(hours2)[1]="lat"
names(hours2)[2]="lon"
#names(hours2)[3]="fleet"
names(hours2)[3]="hours"
#names(hours2)[4]="hours"

#merge dataframes and remove rows missing values for hooks or hours
hooks_hours=merge(hooks2,hours2, by = c("lat","lon"), all = TRUE) #removed fleet
#hooks_hours=merge(hooks2,hours2, by = c("lat","lon", "fleet"), all = TRUE)
hooks_hours=na.omit(hooks_hours[,c("hours","hooks","lat","lon")]) #removed fleet
#hooks_hours=na.omit(hooks_hours[,c("hours","hooks","lat","lon", "fleet")])

#final dataset for GLS has 433 observations (unique combinations of grid cell*fleet)
write_csv(hooks_hours, "hooks_hours_2015_allfleets.csv")

#GLS
require(nlme)
vf1Fixed <- varFixed(~1/log(hours))
hooks_model <- gls(log(hooks)~log(hours), correlation = corRatio(form =~lon+lat, nugget = TRUE), weights = vf1Fixed, data = hooks_hours) #removed "| fleet" from model after "~lon+lat"
#hooks_model <- gls(log(hooks)~log(hours), correlation = corRatio(form =~lon+lat | fleet, nugget = TRUE), weights = vf1Fixed, data = hooks_hours)

summary(hooks_model)
anova(hooks_model)
drop(hooks_model)

#residuals
plot(hooks_model)

#residuals in space
hooks_hours$Resid=resid(hooks_model)

bbox = c(-90,90,-180,180)
ylim <- c(bbox[1], bbox[2]) 
xlim <- c(bbox[3], bbox[4])
hooks_hours_Grid = with(hooks_hours,mapplots::make.grid(x = lon, y = lat, z = Resid, xlim=xlim,ylim =ylim, byx=5,byy=5))
zlim = range(hooks_hours_Grid, na.rm = TRUE)
zlim = c(-5,5)

lati = as.numeric(dimnames(hooks_hours_Grid)[[2]])
long = as.numeric(dimnames(hooks_hours_Grid)[[1]])

colorTable<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-5, 0, 5) / 10)
image.plot(long, lati, hooks_hours_Grid, las = 1, ylab = "latitude", xlab = "longitude", main = "Hooks~Hours - 2017", legend.lab="Residuals", zlim = zlim, col=colorTable)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

  
plot(density(hooks_hours$Resid))
