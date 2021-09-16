require(tidyverse)
#require(sCPUEdb) When making initial download from pelagic
#require(RPostgreSQL)
rm(list=ls())
#create connection to pelagic
#con = connectPelagic(dbuser = "brendan", dbpass = "SharkSanct")
#help(package="sCPUEdb")

####full dataset####
#fulldat = selectData(con, "select * from \"hawaiiObs\" where \"DEPART_YEAR\" between 2010 and 2017")
#dupedat <- fulldat
#write.csv(dupedat, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/fulldat.csv")
fulldat <- read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/fulldat.csv")


####data structure####
fulldat$long <- rowMeans(fulldat[,c("SET_BEGIN_LON","SET_END_LON", "HAUL_BEGIN_LON", "HAUL_END_LON")], na.rm=T)
fulldat$lat <- rowMeans(fulldat[,c("SET_BEGIN_LAT","SET_END_LAT", "HAUL_BEGIN_LAT", "HAUL_END_LAT")], na.rm=T)
fulldat$NUM_HKS_SET <- fulldat$NUM_HKS_SET %>% gsub('[[:punct:]]', '', .) # remove Punctuations
fulldat$NUM_HKS_SET <- as.numeric(fulldat$NUM_HKS_SET)
str(fulldat$FISHERY)
fulldat$FISHERY <- as.factor(fulldat$FISHERY)
str(fulldat$VESSEL_ID)
fulldat$VESSEL_ID <- as.factor(fulldat$VESSEL_ID)
str(fulldat$DECLARED_TRIP_TYPE_CODE)
fulldat$DECLARED_TRIP_TYPE_CODE <- as.factor(fulldat$DECLARED_TRIP_TYPE_CODE)
str(fulldat$DEPART_YEAR)
fulldat$DEPART_YEAR <- as.factor(fulldat$DEPART_YEAR)

####GLMs####
library(MASS)
library(pscl)
library(car)
library(glmmTMB)
library(bbmle)
library(fields)



fulldat$log_hooks=log(fulldat$NUM_HKS_SET)
fulldat<- fulldat %>% distinct()

#dataset for predictions
dummy <- data.frame(subset(fulldat, select = c("FISHERY", "DECLARED_TRIP_TYPE_CODE", "VESSEL_ID", "lat", "long", "DEPART_YEAR", "NUM_HKS_SET", "log_hooks")))
dummy$log_hooks=log(1000)
dummy$NUM_HKS_SET=1000
# Species models
{
####Blue shark####
#Simple
model.blueshark1 <- glm.nb(COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                          data=fulldat) 
#Fixed Only
model.blueshark2 <- glm.nb(COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + offset(log_hooks), 
                          data=fulldat) 

#Zero-inflated
model.blueshark3 <- zeroinfl(COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
                             offset = log_hooks,
                             data=fulldat, dist="poisson")

model.blueshark3_2 <- hurdle(COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
                             offset = log_hooks,
                             data=fulldat, dist="negbin")
#Mixed Model
model.blueshark4 <- glmmTMB(data=fulldat, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula =~0,
                            family=nbinom2)

model.blueshark4_2 <- glmmTMB(data=fulldat, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula =~0,
                            family=nbinom1)

#Zero Infl Mixed - 1 parameter
model.blueshark5 <- glmmTMB(data=fulldat, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula =~1,
                            family=nbinom2)

model.blueshark5_2 <- glmmTMB(data=fulldat, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula =~1,
                            family=nbinom1)


#Zero Infl Mixed - Full Predictors Set ## CANNOT FIT
#model.blueshark6 <- glmmTMB(data=fulldat, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                            ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                            family=nbinom2)

#model.blueshark6_2 <- glmmTMB(data=fulldat, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                            ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                            family=nbinom1)
                                       
#Hurdle model mixed
model.blueshark7 <- glmmTMB(data=fulldat, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula = ~.,
                            family=truncated_nbinom2)
#Residuals
fulldat$bluesharkResid1=resid(model.blueshark1)
fulldat$bluesharkResid2=resid(model.blueshark2)
fulldat$bluesharkResid3=resid(model.blueshark3)
fulldat$bluesharkResid3_2=resid(model.blueshark3_2)
fulldat$bluesharkResid4=residuals(model.blueshark4, type="response")
#fulldat$bluesharkResid4_2=residuals(model.blueshark4_2, type="response")
fulldat$bluesharkResid5=residuals(model.blueshark5, type="response")
fulldat$bluesharkResid5_2=residuals(model.blueshark5_2, type="response")
#fulldat$bluesharkResid6=resid(model.blueshark6)
fulldat$bluesharkResid7=residuals(model.blueshark7, type="response")




#plotting
bbox = c(min(fulldat$lat),max(fulldat$lat),min(fulldat$long),max(fulldat$long))
ylim <- c(bbox[1], bbox[2]) 
xlim <- c(bbox[3], bbox[4])


blueshark_Grid1 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati1 = as.numeric(dimnames(blueshark_Grid1)[[2]])
blueshark_long1 = as.numeric(dimnames(blueshark_Grid1)[[1]])

blueshark_Grid2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati2 = as.numeric(dimnames(blueshark_Grid2)[[2]])
blueshark_long2 = as.numeric(dimnames(blueshark_Grid2)[[1]])

blueshark_Grid3 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati3 = as.numeric(dimnames(blueshark_Grid3)[[2]])
blueshark_long3 = as.numeric(dimnames(blueshark_Grid3)[[1]])

blueshark_Grid3_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid3_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati3_2 = as.numeric(dimnames(blueshark_Grid3_2)[[2]])
blueshark_long3_2 = as.numeric(dimnames(blueshark_Grid3_2)[[1]])

blueshark_Grid4 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati4 = as.numeric(dimnames(blueshark_Grid4)[[2]])
blueshark_long4 = as.numeric(dimnames(blueshark_Grid4)[[1]])

#blueshark_Grid4_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#blueshark_lati4_2 = as.numeric(dimnames(blueshark_Grid4_2)[[2]])
#blueshark_long4_2 = as.numeric(dimnames(blueshark_Grid4_2)[[1]])

blueshark_Grid5 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati5 = as.numeric(dimnames(blueshark_Grid5)[[2]])
blueshark_long5 = as.numeric(dimnames(blueshark_Grid5)[[1]])

blueshark_Grid5_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati5_2 = as.numeric(dimnames(blueshark_Grid5_2)[[2]])
blueshark_long5_2 = as.numeric(dimnames(blueshark_Grid5_2)[[1]])

#blueshark_Grid6 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#blueshark_lati6 = as.numeric(dimnames(blueshark_Grid6)[[2]])
#blueshark_long6 = as.numeric(dimnames(blueshark_Grid6)[[1]])

blueshark_Grid6_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati6_2 = as.numeric(dimnames(blueshark_Grid6_2)[[2]])
blueshark_long6_2 = as.numeric(dimnames(blueshark_Grid6_2)[[1]])

blueshark_Grid7 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati7 = as.numeric(dimnames(blueshark_Grid7)[[2]])
blueshark_long7 = as.numeric(dimnames(blueshark_Grid7)[[1]])

blueshark_Grid7_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = bluesharkResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
blueshark_lati7_2 = as.numeric(dimnames(blueshark_Grid7_2)[[2]])
blueshark_long7_2 = as.numeric(dimnames(blueshark_Grid7_2)[[1]])

# Z limits - coerce to max and min
zlim1 = range(blueshark_Grid1, na.rm = TRUE)
zlim2 = range(blueshark_Grid2, na.rm = TRUE)
zlim3 = range(blueshark_Grid3, na.rm = TRUE)
zlim3_2 = range(blueshark_Grid3_2, na.rm = TRUE)
zlim4 = range(blueshark_Grid4, na.rm = TRUE)
#zlim4_2 = range(blueshark_Grid4_2, na.rm = TRUE)
zlim5 = range(blueshark_Grid5, na.rm = TRUE)
#zlim5_2 = range(blueshark_Grid5_2, na.rm = TRUE)
#zlim6 = range(blueshark_Grid6, na.rm = TRUE)
zlim6_2 = range(blueshark_Grid6_2, na.rm = TRUE)
zlim7 = range(blueshark_Grid7, na.rm = TRUE)
zlim7_2 = range(blueshark_Grid7_2, na.rm = TRUE)

#Zlimit colors
blueshark_zlim_fixed=c(-125,125)
blueshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-125, 0, 125) / 10)

blueshark_zlim_mixed=c(-400,400)
blueshark_colorTable_mixed<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-400,-125, 0, 125, 400) / 10)

#Plots
image.plot(blueshark_long1, blueshark_lati1, blueshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Simple Fixed Effects", 
           legend.lab="Residuals", zlim = blueshark_zlim_fixed, col=blueshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(blueshark_long2, blueshark_lati2, blueshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Fixed Effects", 
           legend.lab="Residuals", zlim = blueshark_zlim_fixed, col=blueshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(blueshark_long3, blueshark_lati3, blueshark_Grid3, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Fixed ZeroInfl", 
           legend.lab="Residuals", zlim = blueshark_zlim_fixed, col=blueshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(blueshark_long3_2, blueshark_lati3_2, blueshark_Grid3_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Fixed Hurdle", 
           legend.lab="Residuals", zlim = blueshark_zlim_fixed, col=blueshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(blueshark_long4, blueshark_lati4, blueshark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(blueshark_long4_2, blueshark_lati4_2, blueshark_Grid4_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed Effects", 
#           legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")


image.plot(blueshark_long5, blueshark_lati5, blueshark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(blueshark_long5_2, blueshark_lati5_2, blueshark_Grid5_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed 1par ZeroInfl", 
#           legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(blueshark_long6, blueshark_lati6, blueshark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(blueshark_long7, blueshark_lati7, blueshark_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed FullZeroInfl", 
           legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")


#Model comparison - fixed effects
vuong(model.blueshark1,model.blueshark2)
vuong(model.blueshark2,model.blueshark3_2) #Fixed NO ZERO best

#Model comparison - mixed models
AICtab(model.blueshark4,model.blueshark5)

summary(model.blueshark2)
Anova(model.blueshark2)

(pseudo_rsq_blueshark <- 1 - model.blueshark2$deviance / model.blueshark2$null.deviance)

pred.blueshark <- data.frame(dummy, predicted=predict(model.blueshark2, newdata=dummy, type = "response", se.fit=TRUE))

write_csv(pred.blueshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_blueshark.csv")


#### Silky shark ####
#Simple
model.silkyshark1 <- glm.nb(COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                           data=fulldat) 
#Fixed Only
model.silkyshark2 <- glm.nb(COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + offset(log_hooks), 
                           data=fulldat) 

#Zero-inflated
model.silkyshark3 <- zeroinfl(COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
                             offset = log_hooks,
                             data=fulldat, dist="negbin")
#HURDLE
model.silkyshark3_2 <- hurdle(COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
                              offset = log_hooks,
                              data=fulldat, dist="negbin")

#Mixed Model
model.silkyshark4 <- glmmTMB(data=fulldat, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula =~0,
                            family=nbinom2)

model.silkyshark4_2 <- glmmTMB(data=fulldat, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                             ziformula =~0,
                             family=nbinom1)

#Zero Infl Mixed - 1 parameter ##CANNOT FIT
model.silkyshark5 <- glmmTMB(data=fulldat, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula =~1,
                            family=nbinom2)

model.silkyshark5_2 <- glmmTMB(data=fulldat, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula =~1,
                            family=nbinom1)

#Zero Infl Mixed - Full Predictors Set
model.silkyshark6 <- glmmTMB(data=fulldat, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                            family=nbinom2)

model.silkyshark6_2 <- glmmTMB(data=fulldat, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                             ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                             family=nbinom1)

#HURDLE
model.silkyshark7 <- glmmTMB(data=fulldat, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                             ziformula = ~. ,
                             family=truncated_nbinom2)

#model.silkyshark7_2 <- glmmTMB(data=fulldat, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                             ziformula = ~. ,
#                             family=truncated_nbinom1)

#Residuals
fulldat$silkysharkResid1=resid(model.silkyshark1)
fulldat$silkysharkResid2=resid(model.silkyshark2)
#fulldat$silkysharkResid3=resid(model.silkyshark3)
fulldat$silkysharkResid3_2=resid(model.silkyshark3_2)
fulldat$silkysharkResid4=resid(model.silkyshark4)
fulldat$silkysharkResid4_2=resid(model.silkyshark4_2)
fulldat$silkysharkResid5=resid(model.silkyshark5)
fulldat$silkysharkResid5_2=resid(model.silkyshark5_2)
fulldat$silkysharkResid6=resid(model.silkyshark6)
fulldat$silkysharkResid6_2=resid(model.silkyshark6_2)
fulldat$silkysharkResid7=resid(model.silkyshark7)
#fulldat$silkysharkResid7_2=resid(model.silkyshark7_2)

#plotting
bbox = c(min(fulldat$lat),max(fulldat$lat),min(fulldat$long),max(fulldat$long))
ylim <- c(bbox[1], bbox[2]) 
xlim <- c(bbox[3], bbox[4])


silkyshark_Grid1 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati1 = as.numeric(dimnames(silkyshark_Grid1)[[2]])
silkyshark_long1 = as.numeric(dimnames(silkyshark_Grid1)[[1]])

silkyshark_Grid2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati2 = as.numeric(dimnames(silkyshark_Grid2)[[2]])
silkyshark_long2 = as.numeric(dimnames(silkyshark_Grid2)[[1]])

#silkyshark_Grid3 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
#silkyshark_lati3 = as.numeric(dimnames(silkyshark_Grid3)[[2]])
#silkyshark_long3 = as.numeric(dimnames(silkyshark_Grid3)[[1]])

silkyshark_Grid3_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid3_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati3_2 = as.numeric(dimnames(silkyshark_Grid3_2)[[2]])
silkyshark_long3_2 = as.numeric(dimnames(silkyshark_Grid3_2)[[1]])

silkyshark_Grid4 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati4 = as.numeric(dimnames(silkyshark_Grid4)[[2]])
silkyshark_long4 = as.numeric(dimnames(silkyshark_Grid4)[[1]])

silkyshark_Grid4_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati4_2 = as.numeric(dimnames(silkyshark_Grid4_2)[[2]])
silkyshark_long4_2 = as.numeric(dimnames(silkyshark_Grid4_2)[[1]])

silkyshark_Grid5 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati5 = as.numeric(dimnames(silkyshark_Grid5)[[2]])
silkyshark_long5 = as.numeric(dimnames(silkyshark_Grid5)[[1]])

silkyshark_Grid5_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati5_2 = as.numeric(dimnames(silkyshark_Grid5_2)[[2]])
silkyshark_long5_2 = as.numeric(dimnames(silkyshark_Grid5_2)[[1]])

silkyshark_Grid6 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati6 = as.numeric(dimnames(silkyshark_Grid6)[[2]])
silkyshark_long6 = as.numeric(dimnames(silkyshark_Grid6)[[1]])

silkyshark_Grid6_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati6_2 = as.numeric(dimnames(silkyshark_Grid6_2)[[2]])
silkyshark_long6_2 = as.numeric(dimnames(silkyshark_Grid6_2)[[1]])

silkyshark_Grid7 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati7 = as.numeric(dimnames(silkyshark_Grid7)[[2]])
silkyshark_long7 = as.numeric(dimnames(silkyshark_Grid7)[[1]])

#silkyshark_Grid7_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = silkysharkResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#silkyshark_lati7_2 = as.numeric(dimnames(silkyshark_Grid7_2)[[2]])
#silkyshark_long7_2 = as.numeric(dimnames(silkyshark_Grid7_2)[[1]])

# Z limits - coerce to max and min
zlim1 = range(silkyshark_Grid1, na.rm = TRUE)
zlim2 = range(silkyshark_Grid2, na.rm = TRUE)
#zlim3 = range(silkyshark_Grid3, na.rm = TRUE)
zlim3_2 = range(silkyshark_Grid3_2, na.rm = TRUE)
zlim4 = range(silkyshark_Grid4, na.rm = TRUE)
zlim4_2 = range(silkyshark_Grid4_2, na.rm = TRUE)
zlim5 = range(silkyshark_Grid5, na.rm = TRUE)
zlim5_2 = range(silkyshark_Grid5_2, na.rm = TRUE)
zlim6 = range(silkyshark_Grid6, na.rm = TRUE)

#Zlimit colors
silkyshark_zlim_fixed=c(-130,130)
silkyshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-130, 0, 130) / 10)
silkyshark_zlim_mixed=c(-130,130)
silkyshark_colorTable_mixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-130, 0, 130) / 10)

#silkyshark_zlim_mixed=c(-105,105)
#silkyshark_colorTable_mixed<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-400,-125, 0, 125, 400) / 10)

#Plots
image.plot(silkyshark_long1, silkyshark_lati1, silkyshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Simple Fixed Effects", 
           legend.lab="Residuals", zlim = silkyshark_zlim_fixed, col=silkyshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long2, silkyshark_lati2, silkyshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Fixed Effects", 
           legend.lab="Residuals", zlim = silkyshark_zlim_fixed, col=silkyshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(silkyshark_long3, silkyshark_lati3, silkyshark_Grid3, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Fixed ZeroInfl", 
#           legend.lab="Residuals", zlim = silkyshark_zlim_fixed, col=silkyshark_colorTable_fixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long3_2, silkyshark_lati3_2, silkyshark_Grid3_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Fixed Hurdle", 
           legend.lab="Residuals", zlim = silkyshark_zlim_fixed, col=silkyshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(silkyshark_long4, silkyshark_lati4, silkyshark_Grid4, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed Effects", 
#           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long4_2, silkyshark_lati4_2, silkyshark_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long5, silkyshark_lati5, silkyshark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed ZeroInfl", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long5_2, silkyshark_lati5_2, silkyshark_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed ZeroInfl", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long6_2, silkyshark_lati6_2, silkyshark_Grid6_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed FullZeroInfl", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long7, silkyshark_lati7, silkyshark_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed FullZeroInfl", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(silkyshark_long7_2, silkyshark_lati7_2, silkyshark_Grid7_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#Model comparison - fixed effects
vuong(model.silkyshark1,model.silkyshark2)
vuong(model.silkyshark2,model.silkyshark3_2) #Hurdle model best

#Model comparison - mixed models
AICtab(model.silkyshark4, model.silkyshark4_2, 
       model.silkyshark5, model.silkyshark5_2,
       model.silkyshark6, model.silkyshark6_2,
       model.silkyshark7) #Full predict zero-inflate best of 2, best overall

summary(model.silkyshark6)
Anova(model.silkyshark6)

summary(model.silkyshark3_2)
extractAIC(model.silkyshark3_2)
Anova(model.silkyshark3_2)

(pseudo_rsq_silkyshark <- 1 - model.silkyshark6$deviance / model.silkyshark6$null.deviance)

# Fixed predict - ~7 observations way high
pred.silkyshark2 <- data.frame(dummy, predicted=predict(model.silkyshark2, newdata=dummy, type = "response", se.fit=TRUE))

# Full Zero Mixed - Too much time
dummy_silky<-dummy
silky_vessel_intercepts <- ranef(model.silkyshark6)
silky_vessel_itnercepts.cond <- silky_vessel_intercepts$cond$VESSEL_ID
silky_vessel_intercepts.zi <- silky_vessel_intercepts$zi$VESSEL_ID
dummy_silky$VESSEL_ID <- "FV470308"

pred.silkyshark <- data.frame(dummy, predicted=predict(model.silkyshark6, newdata=dummy, type = "response", se.fit=TRUE, re.form=NA))

write_csv(pred.silkyshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_silkyshark.csv")
#### Unidentified Mako ####
#Simple
model.unidmakoshark1 <- glm.nb(COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                            data=fulldat) 
#Fixed Only
model.unidmakoshark2 <- glm.nb(COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + offset(log_hooks), 
                            data=fulldat) 

#Zero-inflated #NaNs
model.unidmakoshark3 <- zeroinfl(COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
                              offset = log_hooks,
                              data=fulldat, dist="negbin")

#Hurdle
#model.unidmakoshark3_2 <- hurdle(COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
#                                 offset = log_hooks,
#                                 data=fulldat, dist="negbin")

#Mixed Model
model.unidmakoshark4 <- glmmTMB(data=fulldat, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                             ziformula =~0,
                             family=nbinom2)

model.unidmakoshark4_2 <- glmmTMB(data=fulldat, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula =~0,
                                family=nbinom1)

#Zero Infl Mixed - 1 parameter #CANNOT FIT
model.unidmakoshark5 <- glmmTMB(data=fulldat, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                             ziformula =~1,
                             family=nbinom2)

#Zero Infl Mixed - Full Predictors Set  #CANNOT FIT
model.unidmakoshark6 <- glmmTMB(data=fulldat, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                             ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                             family=nbinom2)

#Residuals
fulldat$unidmakosharkResid1=resid(model.unidmakoshark1)
fulldat$unidmakosharkResid2=resid(model.unidmakoshark2)
fulldat$unidmakosharkResid3=resid(model.unidmakoshark3)
fulldat$unidmakosharkResid4=resid(model.unidmakoshark4)
#fulldat$unidmakosharkResid5=resid(model.unidmakoshark5)
#fulldat$unidmakosharkResid6=resid(model.unidmakoshark6)

#plotting
bbox = c(min(fulldat$lat),max(fulldat$lat),min(fulldat$long),max(fulldat$long))
ylim <- c(bbox[1], bbox[2]) 
xlim <- c(bbox[3], bbox[4])


unidmakoshark_Grid1 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati1 = as.numeric(dimnames(unidmakoshark_Grid1)[[2]])
unidmakoshark_long1 = as.numeric(dimnames(unidmakoshark_Grid1)[[1]])

unidmakoshark_Grid2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati2 = as.numeric(dimnames(unidmakoshark_Grid2)[[2]])
unidmakoshark_long2 = as.numeric(dimnames(unidmakoshark_Grid2)[[1]])

unidmakoshark_Grid3 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati3 = as.numeric(dimnames(unidmakoshark_Grid3)[[2]])
unidmakoshark_long3 = as.numeric(dimnames(unidmakoshark_Grid3)[[1]])

unidmakoshark_Grid4 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati4 = as.numeric(dimnames(unidmakoshark_Grid4)[[2]])
unidmakoshark_long4 = as.numeric(dimnames(unidmakoshark_Grid4)[[1]])

#unidmakoshark_Grid5 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
#unidmakoshark_lati5 = as.numeric(dimnames(unidmakoshark_Grid5)[[2]])
#unidmakoshark_long5 = as.numeric(dimnames(unidmakoshark_Grid5)[[1]])

#unidmakoshark_Grid6 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#unidmakoshark_lati6 = as.numeric(dimnames(unidmakoshark_Grid6)[[2]])
#unidmakoshark_long6 = as.numeric(dimnames(unidmakoshark_Grid6)[[1]])

# Z limits - coerce to max and min
zlim1 = range(unidmakoshark_Grid1, na.rm = TRUE)
zlim2 = range(unidmakoshark_Grid2, na.rm = TRUE)
zlim3 = range(unidmakoshark_Grid3, na.rm = TRUE)
zlim4 = range(unidmakoshark_Grid4, na.rm = TRUE)
#zlim5 = range(unidmakoshark_Grid5, na.rm = TRUE)
#zlim6 = range(unidmakoshark_Grid6, na.rm = TRUE)

#Zlimit colors
unidmakoshark_zlim_fixed=c(-60,60)
unidmakoshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-60, 0, 60) / 10)
unidmakoshark_zlim_mixed=c(-15,15)
unidmakoshark_colorTable_mixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-15, 0, 15) / 10)

#unidmakoshark_zlim_mixed=c(-105,105)
#unidmakoshark_colorTable_mixed<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-400,-125, 0, 125, 400) / 10)

#Plots
image.plot(unidmakoshark_long1, unidmakoshark_lati1, unidmakoshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "UnID Mako Sharks - Simple Fixed Effects", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long2, unidmakoshark_lati2, unidmakoshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "UnID Mako Sharks - Fixed Effects", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long3, unidmakoshark_lati3, unidmakoshark_Grid3, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "UnID Mako Sharks - Fixed ZeroInfl", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_fixed, col=unidmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long4, unidmakoshark_lati4, unidmakoshark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "UnID Mako Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(unidmakoshark_long5, unidmakoshark_lati5, unidmakoshark_Grid5, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "UnID Mako Sharks - Mixed ZeroInfl", 
#           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(unidmakoshark_long6, unidmakoshark_lati6, unidmakoshark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "UnID Mako Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

# Mixed effects model best based on residuals
#Model comparison - fixed effects
vuong(model.unidmakoshark1,model.unidmakoshark2)
vuong(model.unidmakoshark2,model.unidmakoshark3) #Fixed, no zero inflate best of 3

#Model comparison - mixed models
AICtab(model.unidmakoshark4,model.unidmakoshark6) #Full predict zero-inflate best of 2, best overall

summary(model.unidmakoshark6)
Anova(model.unidmakoshark6)

(pseudo_rsq_unidmakoshark <- 1 - model.unidmakoshark6$deviance / model.unidmakoshark6$null.deviance)

# Fixed  predict - 
#pred.unidmakoshark2 <- data.frame(dummy, predicted=predict(model.unidmakoshark3, newdata=dummy, type = "response", se.fit=TRUE))

#  Mixed 
pred.unidmakoshark <- data.frame(dummy, predicted=predict(model.unidmakoshark4, newdata=dummy, type = "response", se.fit=TRUE, re.form=NA))


#### Shortfin mako ####
#Simple
model.shortfinmakoshark1 <- glm.nb(COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                               data=fulldat) 
#Fixed Only
model.shortfinmakoshark2 <- glm.nb(COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + offset(log_hooks), 
                               data=fulldat) 

#Zero-inflated #Fitting errors
#model.shortfinmakoshark3 <- zeroinfl(COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
#                                 offset = log_hooks,
#                                 data=fulldat, dist="negbin")

model.shortfinmakoshark3_2 <- hurdle(COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
                                     offset = log_hooks,
                                     data=fulldat, dist="negbin")

#Mixed Model
model.shortfinmakoshark4 <- glmmTMB(data=fulldat, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula =~0,
                                family=nbinom2)

model.shortfinmakoshark4_2 <- glmmTMB(data=fulldat, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                    ziformula =~0,
                                    family=nbinom1)

#Zero Infl Mixed - 1 parameter 
model.shortfinmakoshark5 <- glmmTMB(data=fulldat, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula =~1,
                                family=nbinom2)

model.shortfinmakoshark5_2 <- glmmTMB(data=fulldat, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                    ziformula =~1,
                                    family=nbinom1)

#Zero Infl Mixed - Full Predictors Set
#model.shortfinmakoshark6 <- glmmTMB(data=fulldat, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                family=nbinom2)

model.shortfinmakoshark6_2 <- glmmTMB(data=fulldat, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                    ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                    family=nbinom1)

#Hurdle mixed
model.shortfinmakoshark7 <- glmmTMB(data=fulldat, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                    ziformula = ~. ,
                                    family=truncated_nbinom2)

#Residuals
fulldat$shortfinmakosharkResid1=resid(model.shortfinmakoshark1)
fulldat$shortfinmakosharkResid2=resid(model.shortfinmakoshark2)
#fulldat$shortfinmakosharkResid3=resid(model.shortfinmakoshark3)
fulldat$shortfinmakosharkResid3_2=resid(model.shortfinmakoshark3_2)
fulldat$shortfinmakosharkResid4=resid(model.shortfinmakoshark4)
fulldat$shortfinmakosharkResid4_2=resid(model.shortfinmakoshark4_2)
fulldat$shortfinmakosharkResid5=resid(model.shortfinmakoshark5)
fulldat$shortfinmakosharkResid5_2=resid(model.shortfinmakoshark5_2)
#fulldat$shortfinmakosharkResid6=resid(model.shortfinmakoshark6)
fulldat$shortfinmakosharkResid7=resid(model.shortfinmakoshark7)

#plotting
bbox = c(min(fulldat$lat),max(fulldat$lat),min(fulldat$long),max(fulldat$long))
ylim <- c(bbox[1], bbox[2]) 
xlim <- c(bbox[3], bbox[4])


shortfinmakoshark_Grid1 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati1 = as.numeric(dimnames(shortfinmakoshark_Grid1)[[2]])
shortfinmakoshark_long1 = as.numeric(dimnames(shortfinmakoshark_Grid1)[[1]])

shortfinmakoshark_Grid2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati2 = as.numeric(dimnames(shortfinmakoshark_Grid2)[[2]])
shortfinmakoshark_long2 = as.numeric(dimnames(shortfinmakoshark_Grid2)[[1]])

#shortfinmakoshark_Grid3 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
#shortfinmakoshark_lati3 = as.numeric(dimnames(shortfinmakoshark_Grid3)[[2]])
#shortfinmakoshark_long3 = as.numeric(dimnames(shortfinmakoshark_Grid3)[[1]])

shortfinmakoshark_Grid3_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid3_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati3_2 = as.numeric(dimnames(shortfinmakoshark_Grid3_2)[[2]])
shortfinmakoshark_long3_2 = as.numeric(dimnames(shortfinmakoshark_Grid3_2)[[1]])

shortfinmakoshark_Grid4 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati4 = as.numeric(dimnames(shortfinmakoshark_Grid4)[[2]])
shortfinmakoshark_long4 = as.numeric(dimnames(shortfinmakoshark_Grid4)[[1]])

shortfinmakoshark_Grid4_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati4_2 = as.numeric(dimnames(shortfinmakoshark_Grid4_2)[[2]])
shortfinmakoshark_long4_2 = as.numeric(dimnames(shortfinmakoshark_Grid4_2)[[1]])

shortfinmakoshark_Grid5 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati5 = as.numeric(dimnames(shortfinmakoshark_Grid5)[[2]])
shortfinmakoshark_long5 = as.numeric(dimnames(shortfinmakoshark_Grid5)[[1]])

shortfinmakoshark_Grid5_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati5_2 = as.numeric(dimnames(shortfinmakoshark_Grid5_2)[[2]])
shortfinmakoshark_long5_2 = as.numeric(dimnames(shortfinmakoshark_Grid5_2)[[1]])

#shortfinmakoshark_Grid6 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#shortfinmakoshark_lati6 = as.numeric(dimnames(shortfinmakoshark_Grid6)[[2]])
#shortfinmakoshark_long6 = as.numeric(dimnames(shortfinmakoshark_Grid6)[[1]])

shortfinmakoshark_Grid7 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati7 = as.numeric(dimnames(shortfinmakoshark_Grid7)[[2]])
shortfinmakoshark_long7 = as.numeric(dimnames(shortfinmakoshark_Grid7)[[1]])

# Z limits - coerce to max and min
zlim1 = range(shortfinmakoshark_Grid1, na.rm = TRUE)
zlim2 = range(shortfinmakoshark_Grid2, na.rm = TRUE)
#zlim3 = range(shortfinmakoshark_Grid3, na.rm = TRUE)
zlim3_2 = range(shortfinmakoshark_Grid3_2, na.rm = TRUE)
zlim4 = range(shortfinmakoshark_Grid4, na.rm = TRUE)
zlim4_2 = range(shortfinmakoshark_Grid4_2, na.rm = TRUE)
zlim5 = range(shortfinmakoshark_Grid5, na.rm = TRUE)
zlim5_2 = range(shortfinmakoshark_Grid5_2, na.rm = TRUE)
#zlim6 = range(shortfinmakoshark_Grid6, na.rm = TRUE)
zlim7 = range(shortfinmakoshark_Grid7, na.rm = TRUE)

#Zlimit colors
shortfinmakoshark_zlim_fixed=c(-80,80)
shortfinmakoshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-60, 0, 60) / 10)

shortfinmakoshark_zlim_mixed=c(-110,110)
shortfinmakoshark_colorTable_mixed<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-110,-80, 0, 80, 110) / 10)

#Plots
image.plot(shortfinmakoshark_long1, shortfinmakoshark_lati1, shortfinmakoshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Simple Fixed Effects", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long2, shortfinmakoshark_lati2, shortfinmakoshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Fixed Effects", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(shortfinmakoshark_long3, shortfinmakoshark_lati3, shortfinmakoshark_Grid3, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Fixed ZeroInfl", 
#           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_fixed, col=shortfinmakoshark_colorTable_fixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long3_2, shortfinmakoshark_lati3_2, shortfinmakoshark_Grid3_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Fixed ZeroInfl", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_fixed, col=shortfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long4, shortfinmakoshark_lati4, shortfinmakoshark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long4_2, shortfinmakoshark_lati4_2, shortfinmakoshark_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long5, shortfinmakoshark_lati5, shortfinmakoshark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Mixed ZeroInfl", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long5_2, shortfinmakoshark_lati5_2, shortfinmakoshark_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Mixed ZeroInfl", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(shortfinmakoshark_long6, shortfinmakoshark_lati6, shortfinmakoshark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long7, shortfinmakoshark_lati7, shortfinmakoshark_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako Sharks - Mixed ZeroInfl", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

# Fixed hurdle model best based on residuals
#Model comparison - fixed effects
vuong(model.shortfinmakoshark1,model.shortfinmakoshark2)
vuong(model.shortfinmakoshark2,model.shortfinmakoshark3_2) #Fixed, no zero inflate best of 3

summary(model.shortfinmakoshark2)
extractAIC(model.shortfinmakoshark4)

#Model comparison - mixed models
AICtab(model.shortfinmakoshark1, model.shortfinmakoshark2,
       model.shortfinmakoshark3_2, model.shortfinmakoshark4,
       model.shortfinmakoshark4_2, model.shortfinmakoshark5, 
       model.shortfinmakoshark5_2, model.shortfinmakoshark7) #Full predict zero-inflate best of 2, best overall

summary(model.shortfinmakoshark2)
Anova(model.shortfinmakoshark2)

(pseudo_rsq_shortfinmakoshark <- 1 - model.shortfinmakoshark6$deviance / model.shortfinmakoshark6$null.deviance)

# Fixed  predict - 
pred.shortfinmakoshark2 <- data.frame(dummy, predicted=predict(model.shortfinmakoshark2, newdata=dummy, type = "response", se.fit=TRUE))

#  Mixed 
pred.shortfinmakoshark <- data.frame(dummy, predicted=predict(model.shortfinmakoshark4, newdata=dummy, type = "response", se.fit=TRUE, re.form=NA))

write_csv(pred.shortfinmakoshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_shortfinmakoshark.csv")
write_csv(pred.shortfinmakoshark2, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_shortfinmakoshark2.csv")


#### Longfin mako ####
#Simple
model.longfinmakoshark1 <- glm.nb(COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                   data=fulldat) 
#Fixed Only
model.longfinmakoshark2 <- glm.nb(COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + offset(log_hooks), 
                                   data=fulldat) 

#Zero-inflated 
model.longfinmakoshark3 <- zeroinfl(COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
                                 offset = log_hooks,
                                 data=fulldat, dist="negbin")

#Hurdle #cannot fit
#model.longfinmakoshark3_2 <- hurdle(COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
#                                     offset = log_hooks,
#                                     data=fulldat, dist="negbin")

#Mixed Model
model.longfinmakoshark4 <- glmmTMB(data=fulldat, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                    ziformula =~0,
                                    family=nbinom2)

model.longfinmakoshark4_2 <- glmmTMB(data=fulldat, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                      ziformula =~0,
                                      family=nbinom1)

#Zero Infl Mixed - 1 parameter 
model.longfinmakoshark5 <- glmmTMB(data=fulldat, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                    ziformula =~1,
                                    family=nbinom2)

model.longfinmakoshark5_2 <- glmmTMB(data=fulldat, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                      ziformula =~1,
                                      family=nbinom1)

#Zero Infl Mixed - Full Predictors Set
#model.longfinmakoshark6 <- glmmTMB(data=fulldat, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                family=nbinom2)

#model.longfinmakoshark6_2 <- glmmTMB(data=fulldat, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                      ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                      family=nbinom1)

#Hurdle mixed
#model.longfinmakoshark7 <- glmmTMB(data=fulldat, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                    ziformula = ~. ,
#                                    family=truncated_nbinom2)

#Residuals
fulldat$longfinmakosharkResid1=resid(model.longfinmakoshark1)
fulldat$longfinmakosharkResid2=resid(model.longfinmakoshark2)
fulldat$longfinmakosharkResid3=resid(model.longfinmakoshark3)
#fulldat$longfinmakosharkResid3_2=resid(model.longfinmakoshark3_2)
fulldat$longfinmakosharkResid4=resid(model.longfinmakoshark4)
fulldat$longfinmakosharkResid4_2=resid(model.longfinmakoshark4_2)
fulldat$longfinmakosharkResid5=resid(model.longfinmakoshark5)
fulldat$longfinmakosharkResid5_2=resid(model.longfinmakoshark5_2)
#fulldat$longfinmakosharkResid6=resid(model.longfinmakoshark6)
#fulldat$longfinmakosharkResid6_2=resid(model.longfinmakoshark6_2)
#fulldat$longfinmakosharkResid7=resid(model.longfinmakoshark7)

#plotting
bbox = c(min(fulldat$lat),max(fulldat$lat),min(fulldat$long),max(fulldat$long))
ylim <- c(bbox[1], bbox[2]) 
xlim <- c(bbox[3], bbox[4])


longfinmakoshark_Grid1 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati1 = as.numeric(dimnames(longfinmakoshark_Grid1)[[2]])
longfinmakoshark_long1 = as.numeric(dimnames(longfinmakoshark_Grid1)[[1]])

longfinmakoshark_Grid2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati2 = as.numeric(dimnames(longfinmakoshark_Grid2)[[2]])
longfinmakoshark_long2 = as.numeric(dimnames(longfinmakoshark_Grid2)[[1]])

longfinmakoshark_Grid3 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati3 = as.numeric(dimnames(longfinmakoshark_Grid3)[[2]])
longfinmakoshark_long3 = as.numeric(dimnames(longfinmakoshark_Grid3)[[1]])

#longfinmakoshark_Grid3_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid3_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#longfinmakoshark_lati3_2 = as.numeric(dimnames(longfinmakoshark_Grid3_2)[[2]])
#longfinmakoshark_long3_2 = as.numeric(dimnames(longfinmakoshark_Grid3_2)[[1]])

longfinmakoshark_Grid4 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati4 = as.numeric(dimnames(longfinmakoshark_Grid4)[[2]])
longfinmakoshark_long4 = as.numeric(dimnames(longfinmakoshark_Grid4)[[1]])

longfinmakoshark_Grid4_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati4_2 = as.numeric(dimnames(longfinmakoshark_Grid4_2)[[2]])
longfinmakoshark_long4_2 = as.numeric(dimnames(longfinmakoshark_Grid4_2)[[1]])

longfinmakoshark_Grid5 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati5 = as.numeric(dimnames(longfinmakoshark_Grid5)[[2]])
longfinmakoshark_long5 = as.numeric(dimnames(longfinmakoshark_Grid5)[[1]])

longfinmakoshark_Grid5_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati5_2 = as.numeric(dimnames(longfinmakoshark_Grid5_2)[[2]])
longfinmakoshark_long5_2 = as.numeric(dimnames(longfinmakoshark_Grid5_2)[[1]])

#longfinmakoshark_Grid6 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#longfinmakoshark_lati6 = as.numeric(dimnames(longfinmakoshark_Grid6)[[2]])
#longfinmakoshark_long6 = as.numeric(dimnames(longfinmakoshark_Grid6)[[1]])

#longfinmakoshark_Grid6_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#longfinmakoshark_lati6_2 = as.numeric(dimnames(longfinmakoshark_Grid6_2)[[2]])
#longfinmakoshark_long6_2 = as.numeric(dimnames(longfinmakoshark_Grid6_2)[[1]])

#longfinmakoshark_Grid7 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
#longfinmakoshark_lati7 = as.numeric(dimnames(longfinmakoshark_Grid7)[[2]])
#longfinmakoshark_long7 = as.numeric(dimnames(longfinmakoshark_Grid7)[[1]])

# Z limits - coerce to max and min
zlim1 = range(longfinmakoshark_Grid1, na.rm = TRUE)
zlim2 = range(longfinmakoshark_Grid2, na.rm = TRUE)
zlim3 = range(longfinmakoshark_Grid3, na.rm = TRUE)
#zlim3_2 = range(longfinmakoshark_Grid3_2, na.rm = TRUE)
zlim4 = range(longfinmakoshark_Grid4, na.rm = TRUE)
zlim4_2 = range(longfinmakoshark_Grid4_2, na.rm = TRUE)
zlim5 = range(longfinmakoshark_Grid5, na.rm = TRUE)
zlim5_2 = range(longfinmakoshark_Grid5_2, na.rm = TRUE)
#zlim6 = range(longfinmakoshark_Grid6, na.rm = TRUE)
#zlim6_2 = range(longfinmakoshark_Grid6_2, na.rm = TRUE)
#zlim7 = range(longfinmakoshark_Grid7, na.rm = TRUE)

#Zlimit colors
longfinmakoshark_zlim_fixed=c(-50,50)
longfinmakoshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

longfinmakoshark_zlim_mixed=c(-10,10)
longfinmakoshark_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-10, 0, 10) / 10)
#longfinmakoshark_colorTable_mixed<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-110,-80, 0, 80, 110) / 10)


#Plots
image.plot(longfinmakoshark_long1, longfinmakoshark_lati1, longfinmakoshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Simple Fixed Effects", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long2, longfinmakoshark_lati2, longfinmakoshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Fixed Effects", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long3, longfinmakoshark_lati3, longfinmakoshark_Grid3, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Fixed ZeroInfl", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_fixed, col=longfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(longfinmakoshark_long3_2, longfinmakoshark_lati3_2, longfinmakoshark_Grid3_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Fixed Hurdle", 
#           legend.lab="Residuals", zlim = longfinmakoshark_zlim_fixed, col=longfinmakoshark_colorTable_fixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long4, longfinmakoshark_lati4, longfinmakoshark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Mixed Effects2", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long4_2, longfinmakoshark_lati4_2, longfinmakoshark_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Mixed Effects1", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long5, longfinmakoshark_lati5, longfinmakoshark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Mixed ZeroInfl2", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long5_2, longfinmakoshark_lati5_2, longfinmakoshark_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Mixed ZeroInfl1", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(longfinmakoshark_long6, longfinmakoshark_lati6, longfinmakoshark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Mixed FullZeroInfl2", 
#           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(longfinmakoshark_long6_2, longfinmakoshark_lati6_2, longfinmakoshark_Grid6_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Mixed FullZeroInfl1", 
#           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(longfinmakoshark_long7, longfinmakoshark_lati7, longfinmakoshark_Grid7, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "longfin Mako Sharks - Mixed Hurdle", 
#           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")


#Model comparison - fixed effects
vuong(model.longfinmakoshark1,model.longfinmakoshark2)
vuong(model.longfinmakoshark2,model.longfinmakoshark3_2) #Fixed, no zero inflate best of 3

summary(model.longfinmakoshark2)
extractAIC(model.longfinmakoshark4)

#Model comparison - mixed models
AICtab(model.longfinmakoshark1, model.longfinmakoshark2,
       model.longfinmakoshark3, #model.longfinmakoshark3_2,
       model.longfinmakoshark4, model.longfinmakoshark4_2, 
       model.longfinmakoshark5, model.longfinmakoshark5_2#,
       #model.longfinmakoshark6, model.longfinmakoshark6_2,
       #model.longfinmakoshark7
       ) #Full predict zero-inflate best of 2, best overall

summary(model.longfinmakoshark4)
Anova(model.longfinmakoshark4)

pred.longfinmakoshark <- data.frame(dummy, predicted=predict(model.longfinmakoshark4, newdata=dummy, type = "response", se.fit=TRUE, re.form=NA))
write_csv(pred.longfinmakoshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_longfinmakoshark.csv")

#### All Threshers ####

fulldat$COUNT_THRESHER_SHARK <- fulldat$COUNT_BIGEYE_THRESHER_SHARK + fulldat$COUNT_PELAGIC_THRESHER_SHARK + fulldat$COUNT_COMMON_THRESHER_SHARK + fulldat$COUNT_UNID_THRESHER_SHARK

#Simple
model.allthreshershark1 <- glm.nb(COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                  data=fulldat) 
#Fixed Only
model.allthreshershark2 <- glm.nb(COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + offset(log_hooks), 
                                  data=fulldat) 

#Zero-inflated 
#model.allthreshershark3 <- zeroinfl(COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
#                                    offset = log_hooks,
#                                    data=fulldat, dist="negbin")

#Hurdle
model.allthreshershark3_2 <- hurdle(COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE | lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE, 
                                     offset = log_hooks,
                                     data=lldat, dist="negbin")

#Mixed Model
model.allthreshershark4 <- glmmTMB(data=fulldat, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~0,
                                   family=nbinom2)

model.allthreshershark4_2 <- glmmTMB(data=fulldat, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                     ziformula =~0,
                                     family=nbinom1)

#Zero Infl Mixed - 1 parameter 
model.allthreshershark5 <- glmmTMB(data=fulldat, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~1,
                                   family=nbinom2)

model.allthreshershark5_2 <- glmmTMB(data=fulldat, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                     ziformula =~1,
                                     family=nbinom1)

#Zero Infl Mixed - Full Predictors Set
#model.allthreshershark6 <- glmmTMB(data=fulldat, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                family=nbinom2)

model.allthreshershark6_2 <- glmmTMB(data=fulldat, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                      ziformula =~lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                      family=nbinom1)

#Hurdle mixed
model.allthreshershark7 <- glmmTMB(data=fulldat, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
                                    ziformula = ~. ,
                                    family=truncated_nbinom2)

#model.allthreshershark7_2 <- glmmTMB(data=fulldat, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + FISHERY + DECLARED_TRIP_TYPE_CODE + (1|VESSEL_ID) + offset(log_hooks),
#                                   ziformula = ~. ,
#                                   family=truncated_nbinom1)

#Residuals
fulldat$allthreshersharkResid1=resid(model.allthreshershark1)
fulldat$allthreshersharkResid2=resid(model.allthreshershark2)
#fulldat$allthreshersharkResid3=resid(model.allthreshershark3)
fulldat$allthreshersharkResid3_2=resid(model.allthreshershark3_2)
fulldat$allthreshersharkResid4=resid(model.allthreshershark4)
fulldat$allthreshersharkResid4_2=resid(model.allthreshershark4_2)
fulldat$allthreshersharkResid5=resid(model.allthreshershark5)
fulldat$allthreshersharkResid5_2=resid(model.allthreshershark5_2)
#fulldat$allthreshersharkResid6=resid(model.allthreshershark6)
#fulldat$allthreshersharkResid6_2=resid(model.allthreshershark6_2)
fulldat$allthreshersharkResid7=resid(model.allthreshershark7)
#fulldat$allthreshersharkResid7_2=resid(model.allthreshershark7_2)

#plotting
bbox = c(min(fulldat$lat),max(fulldat$lat),min(fulldat$long),max(fulldat$long))
ylim <- c(bbox[1], bbox[2]) 
xlim <- c(bbox[3], bbox[4])


allthreshershark_Grid1 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthreshershark_lati1 = as.numeric(dimnames(allthreshershark_Grid1)[[2]])
allthreshershark_long1 = as.numeric(dimnames(allthreshershark_Grid1)[[1]])

allthreshershark_Grid2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthreshershark_lati2 = as.numeric(dimnames(allthreshershark_Grid2)[[2]])
allthreshershark_long2 = as.numeric(dimnames(allthreshershark_Grid2)[[1]])

#allthreshershark_Grid3 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
#allthreshershark_lati3 = as.numeric(dimnames(allthreshershark_Grid3)[[2]])
#allthreshershark_long3 = as.numeric(dimnames(allthreshershark_Grid3)[[1]])

allthreshershark_Grid3_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid3_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthreshershark_lati3_2 = as.numeric(dimnames(allthreshershark_Grid3_2)[[2]])
allthreshershark_long3_2 = as.numeric(dimnames(allthreshershark_Grid3_2)[[1]])

allthreshershark_Grid4 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthreshershark_lati4 = as.numeric(dimnames(allthreshershark_Grid4)[[2]])
allthreshershark_long4 = as.numeric(dimnames(allthreshershark_Grid4)[[1]])

allthreshershark_Grid4_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthreshershark_lati4_2 = as.numeric(dimnames(allthreshershark_Grid4_2)[[2]])
allthreshershark_long4_2 = as.numeric(dimnames(allthreshershark_Grid4_2)[[1]])

allthreshershark_Grid5 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthreshershark_lati5 = as.numeric(dimnames(allthreshershark_Grid5)[[2]])
allthreshershark_long5 = as.numeric(dimnames(allthreshershark_Grid5)[[1]])

allthreshershark_Grid5_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthreshershark_lati5_2 = as.numeric(dimnames(allthreshershark_Grid5_2)[[2]])
allthreshershark_long5_2 = as.numeric(dimnames(allthreshershark_Grid5_2)[[1]])

#allthreshershark_Grid6 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#allthreshershark_lati6 = as.numeric(dimnames(allthreshershark_Grid6)[[2]])
#allthreshershark_long6 = as.numeric(dimnames(allthreshershark_Grid6)[[1]])

#allthreshershark_Grid6_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#allthreshershark_lati6_2 = as.numeric(dimnames(allthreshershark_Grid6_2)[[2]])
#allthreshershark_long6_2 = as.numeric(dimnames(allthreshershark_Grid6_2)[[1]])

allthreshershark_Grid7 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthreshershark_lati7 = as.numeric(dimnames(allthreshershark_Grid7)[[2]])
allthreshershark_long7 = as.numeric(dimnames(allthreshershark_Grid7)[[1]])

#allthreshershark_Grid7_2 = with(fulldat,mapplots::make.grid(x = long, y = lat, z = allthreshersharkResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#allthreshershark_lati7_2 = as.numeric(dimnames(allthreshershark_Grid7_2)[[2]])
#allthreshershark_long7_2 = as.numeric(dimnames(allthreshershark_Grid7_2)[[1]])

# Z limits - coerce to max and min
zlim1 = range(allthreshershark_Grid1, na.rm = TRUE)
zlim2 = range(allthreshershark_Grid2, na.rm = TRUE)
#zlim3 = range(allthreshershark_Grid3, na.rm = TRUE)
zlim3_2 = range(allthreshershark_Grid3_2, na.rm = TRUE)
zlim4 = range(allthreshershark_Grid4, na.rm = TRUE)
zlim4_2 = range(allthreshershark_Grid4_2, na.rm = TRUE)
zlim5 = range(allthreshershark_Grid5, na.rm = TRUE)
zlim5_2 = range(allthreshershark_Grid5_2, na.rm = TRUE)
#zlim6 = range(allthreshershark_Grid6, na.rm = TRUE)
#zlim6_2 = range(allthreshershark_Grid6_2, na.rm = TRUE)
zlim7 = range(allthreshershark_Grid7, na.rm = TRUE)
#zlim7_2 = range(allthreshershark_Grid7_2, na.rm = TRUE)

#Zlimit colors
allthreshershark_zlim_fixed=c(-250,250)
allthreshershark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-250, 0, 250) / 10)

allthreshershark_zlim_mixed=c(-7500,7500)
#allthreshershark_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-10, 0, 10) / 10)
allthreshershark_colorTable_mixed<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-7500,-250, 0, 250, 7500) / 10)


#Plots
image.plot(allthreshershark_long1, allthreshershark_lati1, allthreshershark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Simple Fixed Effects", 
           legend.lab="Residuals", zlim = allthreshershark_zlim_fixed, col=allthreshershark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthreshershark_long2, allthreshershark_lati2, allthreshershark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Fixed Effects", 
           legend.lab="Residuals", zlim = allthreshershark_zlim_fixed, col=allthreshershark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(allthreshershark_long3, allthreshershark_lati3, allthreshershark_Grid3, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Fixed ZeroInfl", 
#           legend.lab="Residuals", zlim = allthreshershark_zlim_fixed, col=allthreshershark_colorTable_fixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthreshershark_long3_2, allthreshershark_lati3_2, allthreshershark_Grid3_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Fixed Hurdle", 
           legend.lab="Residuals", zlim = allthreshershark_zlim_fixed, col=allthreshershark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthreshershark_long4, allthreshershark_lati4, allthreshershark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Mixed Effects2", 
           legend.lab="Residuals", zlim = allthreshershark_zlim_mixed, col=allthreshershark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthreshershark_long4_2, allthreshershark_lati4_2, allthreshershark_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Mixed Effects1", 
           legend.lab="Residuals", zlim = allthreshershark_zlim_mixed, col=allthreshershark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthreshershark_long5, allthreshershark_lati5, allthreshershark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Mixed ZeroInfl2", 
           legend.lab="Residuals", zlim = allthreshershark_zlim_mixed, col=allthreshershark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthreshershark_long5_2, allthreshershark_lati5_2, allthreshershark_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Mixed ZeroInfl1", 
           legend.lab="Residuals", zlim = allthreshershark_zlim_mixed, col=allthreshershark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(allthreshershark_long6, allthreshershark_lati6, allthreshershark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Mixed FullZeroInfl2", 
#           legend.lab="Residuals", zlim = allthreshershark_zlim_mixed, col=allthreshershark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(allthreshershark_long6_2, allthreshershark_lati6_2, allthreshershark_Grid6_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Mixed FullZeroInfl1", 
#           legend.lab="Residuals", zlim = allthreshershark_zlim_mixed, col=allthreshershark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthreshershark_long7, allthreshershark_lati7, allthreshershark_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Thresher Sharks - Mixed Hurdle", 
           legend.lab="Residuals", zlim = allthreshershark_zlim_mixed, col=allthreshershark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")


#Model comparison - fixed effects
vuong(model.allthreshershark1,model.allthreshershark2)
vuong(model.allthreshershark2,model.allthreshershark3_2) #Fixed, no zero inflate best of 3

summary(model.allthreshershark2)
extractAIC(model.allthreshershark4)

#Model comparison - mixed models
AICtab(model.allthreshershark1, model.allthreshershark2,
       #model.allthreshershark3, 
       model.allthreshershark3_2,
       model.allthreshershark4, model.allthreshershark4_2, 
       model.allthreshershark5, model.allthreshershark5_2,
       #model.allthreshershark6, model.allthreshershark6_2,
       model.allthreshershark7
) #Full predict zero-inflate best of 2, best overall

summary(model.allthreshershark4)
Anova(model.allthreshershark4)

pred.allthreshershark <- data.frame(dummy, predicted=predict(model.allthreshershark4, newdata=dummy, type = "response", se.fit=TRUE, re.form=NA))
write_csv(pred.allthreshershark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_allthreshershark.csv")




model.allthresher <- glm.nb(COUNT_THRESHER ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                 data=fulldat) 

summary(model.allthresher)
Anova(model.allthresher)
plot(model.allthresher)

(pseudo_rsq_allthresher <- 1 - model.allthresher$deviance / model.allthresher$null.deviance)

pred.allthresher <- data.frame(dummy, predicted=predict(model.allthresher, newdata=dummy, type = "response", se.fit=TRUE))

library(DHARMa)
allthresher_mod2_resid<-simulateResiduals(model.allthreshershark2)
allthresher_mod4_resid<-simulateResiduals(model.allthreshershark4, group=fulldat$VESSEL_ID)
allthresher_mod7_resid<-simulateResiduals(model.allthreshershark7, group = fulldat$VESSEL_ID)

testOutliers(allthresher_mod4_resid)
plot(allthresher_mod2_resid)
plot(allthresher_mod4_resid)
plot(allthresher_mod7_resid)

#### Unidentified Thresher - not used ####
model.unidthresher <- glm.nb(COUNT_UNID_THRESHER_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                               data=fulldat) 
  
summary(model.unidthresher)
Anova(model.unidthresher)
plot(model.unidthresher)
  
(pseudo_rsq_unidthresher <- 1 - model.unidthresher$deviance / model.unidthresher$null.deviance)
  
pred.unidthresher <- data.frame(dummy, predicted=predict(model.unidthresher, newdata=dummy, type = "response", se.fit=TRUE))

#### Bigeye thresher - not used ####
model.bigeyethresher <- glm.nb(COUNT_BIGEYE_THRESHER_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                           data=fulldat) 

summary(model.bigeyethresher)
Anova(model.bigeyethresher)
plot(model.bigeyethresher)

(pseudo_rsq_bigeyethresher <- 1 - model.bigeyethresher$deviance / model.bigeyethresher$null.deviance)

pred.bigeyethresher <- data.frame(dummy, predicted=predict(model.bigeyethresher, newdata=dummy, type = "response", se.fit=TRUE))
#### Pelagic Thresher - not used) ####
model.pelagicthresher <- glm.nb(COUNT_PELAGIC_THRESHER_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                           data=fulldat) 

summary(model.pelagicthresher)
Anova(model.pelagicthresher)
plot(model.pelagicthresher)

(pseudo_rsq_pelagicthresher <- 1 - model.pelagicthresher$deviance / model.pelagicthresher$null.deviance)

pred.pelagicthresher <- data.frame(dummy, predicted=predict(model.pelagicthresher, newdata=dummy, type = "response", se.fit=TRUE))
pred.pelagicthresher$truepredict = exp(pred.pelagicthresher$predicted.fit)
#### Common Thresher - not used ####
model.commonthresher <- glm.nb(COUNT_COMMON_THRESHER_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                           data=fulldat) 

summary(model.commonthresher)
Anova(model.commonthresher)
plot(model.commonthresher)

(pseudo_rsq_commonthresher <- 1 - model.commonthresher$deviance / model.commonthresher$null.deviance)

pred.commonthresher <- data.frame(dummy, predicted=predict(model.commonthresher, newdata=dummy, type = "response", se.fit=TRUE))
pred.commonthresher$truepredict = exp(pred.commonthresher$predicted.fit)
#### All Hammers ####
fulldat$COUNT_HAMMER <- fulldat$COUNT_UNID_HAMMERHEAD_SHARK + fulldat$COUNT_SCALLOPED_HAMMERHEAD + fulldat$COUNT_SMOOTH_HAMMERHEAD_SHARK
model.allhammer <- glm.nb(COUNT_HAMMER ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                              data=fulldat) 
  
summary(model.allhammer)
Anova(model.allhammer)
plot(model.allhammer)
  
(pseudo_rsq_allhammer <- 1 - model.allhammer$deviance / model.allhammer$null.deviance)
  
pred.allhammer <- data.frame(dummy, predicted=predict(model.allhammer, newdata=dummy, type = "response", se.fit=TRUE))
#Zero-inflated
model.allhammer2 <- zeroinfl(COUNT_HAMMER ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, offset = log_hooks,
  data=fulldat, dist="negbin")
pred.allhammer2 <- data.frame(dummy, predicted=predict(model.allhammer2, newdata=dummy, type = "response", se.fit=TRUE))
  
vuong(model.allhammer,model.allhammer2)
#### Oceanic whitetip ####
model.oceanicwhitetip <- glm.nb(COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                             data=fulldat) 
  
summary(model.oceanicwhitetip)
Anova(model.oceanicwhitetip)
plot(model.oceanicwhitetip)
  
(pseudo_rsq_oceanicwhitetip <- 1 - model.oceanicwhitetip$deviance / model.oceanicwhitetip$null.deviance)
  
pred.oceanicwhitetip <- data.frame(dummy, predicted=predict(model.oceanicwhitetip, newdata=dummy, type = "response", se.fit=TRUE))
#### All other sharks ####
fulldat$ALLOTHERSHARKS <- rowSums(fulldat[,61:79])
model.allothersharks <- glm.nb(ALLOTHERSHARKS~ lat + long + DEPART_YEAR + offset(log_hooks), 
                          data=fulldat) 

summary(model.allothersharks)
Anova(model.allothersharks)
plot(model.allothersharks)
(pseudo_rsq_allothersharks <- 1 - model.allothersharks$deviance / model.allothersharks$null.deviance)

pred.allothersharks <- data.frame(dummy, predicted=predict(model.allothersharks, newdata=dummy, type = "response", se.fit=TRUE))
}

#### CPUE dataframe ####
# Hammers use zero-inflated
HawAmS_CPUEs <- data.frame(dummy, 
                    blueshark = pred.blueshark$predicted.fit, blueshark.se = pred.blueshark$predicted.se.fit,
                    silkyshark = pred.silkyshark$predicted.fit, silkyshark.se=pred.silkyshark$predicted.se.fit,
                    #unidmako = predict(model.unidmako, newdata=dummy, type = "response", se.fit=TRUE),
                    shortfinmako = pred.shortfinmakoshark$predicted.fit, shortfinmako.se=pred.shortfinmakoshark$predicted.se.fit,
                    longfinmako = pred.longfinmakoshark$predicted.fit, longfinmako.se=pred.longfinmakoshark$predicted.se.fit)#,
                    allthresher = predict(model.allthresher, newdata=dummy, type = "response", se.fit=TRUE),
                    allhammer = predict(model.allhammer2, newdata=dummy, type = "response", se.fit=TRUE),
                    oceanicwhitetip =  predict(model.oceanicwhitetip, newdata=dummy, type = "response", se.fit=TRUE),
                    allothersharks = predict(model.allothersharks, newdata=dummy, type = "response", se.fit=TRUE))

write_csv(HawAmS_CPUEs, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/HawAmS_CPUEs.csv")

#### Plot spatial distribution ####

HawAms_df <- aggregate() # need to get length of species

bbox1 = c(-35,35,120,180)
ylim1 <- c(bbox1[1], bbox1[2]) 
xlim1 <- c(bbox1[3], bbox1[4])
hawaii_cpue_Grid1 = with(CPUEs,mapplots::make.grid(x = long, y = lat, z = Species, xlim=xlim1,ylim =ylim1, byx=5,byy=5))
zlim1 = range(hawaii_cpue_Grid1, na.rm = TRUE)
zlim1 = c(0,8)

lati1 = as.numeric(dimnames(hawaii_cpue_Grid1)[[2]])
long1 = as.numeric(dimnames(hawaii_cpue_Grid1)[[1]])

image.plot(long1, lati1, hawaii_cpue_Grid1, las = 1, ylab = "Latitude", xlab = "Longitude", main = "WCPFC CPUE Coverage", legend.lab="# of Species", zlim = c(0,8))
maps::map("world", xlim = xlim1, ylim = ylim1, add = TRUE, fill = TRUE, col="black")
plot(NCL_shp, add=TRUE, col="gray")
plot(PLW_shp, add=TRUE, col="gray")
plot(FSM_shp, add=TRUE, col="gray")
plot(MHL_shp, add=TRUE, col="gray")
plot(KIR_shp, add=TRUE, col="gray")
plot(WSM_shp, add=TRUE, col="gray")
image.plot(long1, lati1, species_cpue_Grid1, las = 1, zlim = zlim1, col=viridis(6), add=TRUE)
plot(NCL_shp, add=TRUE)
plot(PLW_shp, add=TRUE)
plot(FSM_shp, add=TRUE)
plot(MHL_shp, add=TRUE)
plot(KIR_shp, add=TRUE)
plot(WSM_shp, add=TRUE)

bbox2 = c(-35,35,-180,-130)
ylim2 <- c(bbox2[1], bbox2[2]) 
xlim2 <- c(bbox2[3], bbox2[4])
hawaii_cpue_Grid = with(CPUEs,mapplots::make.grid(x =long, y = lat, z = Species, xlim=xlim2,ylim =ylim2, byx=1,byy=1))
zlim2 = c(0,11)

lati2 = as.numeric(dimnames(hawaii_cpue_Grid)[[2]])
long2 = as.numeric(dimnames(hawaii_cpue_Grid)[[1]])

image.plot(long2, lati2, hawaii_cpue_Grid, las = 1, ylab = "latitude", xlab = "longitude", main = "Hawaii/Samoa Observer CPUEs", legend.lab="# of Species", zlim = zlim2, col=viridis(6))
maps::map("world", xlim = xlim2, ylim = ylim2, add = TRUE, fill = TRUE, col="black")
plot(PYF_shp, add=TRUE, col="gray")
plot(KIR_shp, add=TRUE, col="gray")
plot(WSM_shp, add=TRUE, col="gray")
plot(COK_shp, add=TRUE, col="gray")
image.plot(long2, lati2, hawaii_cpue_Grid, las = 1, zlim = zlim2, col=viridis(6), add=TRUE)
plot(PYF_shp, add=TRUE)
plot(KIR_shp, add=TRUE)
plot(WSM_shp, add=TRUE)
plot(COK_shp, add=TRUE)

#### Aggregated nominal CPUEs - sum hooks and catches for each 5*5 cell and calculate ####
#2017
dat <- subset(fulldat, DEPART_YEAR==2017)
dat$roundedlat=round(dat$lat)
dat$roundedlon=round(dat$long)

#dat <- dat[,c(15, 48:79, 155:156)] #for using raw lat/long
dat <- dat[,c(15, 48:79, 157:158)] #for using rounded lat/long

dat2 <- dat %>% group_by(roundedlat,roundedlon) %>%
  summarise_each(funs(sum))

dat2$blueshark_cpue <- dat2$COUNT_BLUE_SHARK/dat2$NUM_HKS_SET*1000
dat2$UnID_mako_cpue <- dat2$COUNT_UNID_MAKO_SHARK/dat2$NUM_HKS_SET*1000
dat2$shortfinmako_cpue <- dat2$COUNT_SHORTFIN_MAKO_SHARK/dat2$NUM_HKS_SET*1000
dat2$longfinmako_cpue <- dat2$COUNT_LONGFIN_MAKO_SHARK/dat2$NUM_HKS_SET*1000
dat2$UnID_thresher_cpue <- dat2$COUNT_UNID_THRESHER_SHARK/dat2$NUM_HKS_SET*1000
dat2$pelagicthresher_cpue <- dat2$COUNT_PELAGIC_THRESHER_SHARK/dat2$NUM_HKS_SET*1000
dat2$bigeyethresher_cpue <- dat2$COUNT_BIGEYE_THRESHER_SHARK/dat2$NUM_HKS_SET*1000
dat2$commonthresher_cpue <- dat2$COUNT_COMMON_THRESHER_SHARK/dat2$NUM_HKS_SET*1000
dat2$UnID_hammerhead_cpue <- dat2$COUNT_UNID_HAMMERHEAD_SHARK/dat2$NUM_HKS_SET*1000
dat2$scallopedhammerhead_cpue <- dat2$COUNT_SCALLOPED_HAMMERHEAD/dat2$NUM_HKS_SET*1000
dat2$smoothhammerhead_cpue <- dat2$COUNT_SMOOTH_HAMMERHEAD_SHARK/dat2$NUM_HKS_SET*1000
dat2$oceanicwhitetip_cpue <- dat2$COUNT_OCEANIC_WHITETIP_SHARK/dat2$NUM_HKS_SET*1000
dat2$silkyshark_cpue <- dat2$COUNT_SILKY_SHARK/dat2$NUM_HKS_SET*1000
dat2$cookiecutter_cpue <- dat2$COUNT_COOKIE_CUTTER_SHARK/dat2$NUM_HKS_SET*1000
dat2$crocodileshark_cpue <- dat2$COUNT_CROCODILE_SHARK/dat2$NUM_HKS_SET*1000
dat2$velvetdogfish_cpue <- dat2$COUNT_VELVET_DOGFISH/dat2$NUM_HKS_SET*1000
dat2$tigershark_cpue <- dat2$COUNT_TIGER_SHARK/dat2$NUM_HKS_SET*1000
dat2$bigeyesandtiger_cpue <- dat2$COUNT_BIGEYE_SANDTIGER_SHARK/dat2$NUM_HKS_SET*1000
dat2$whiteshark_cpue <- dat2$COUNT_WHITE_SHARK/dat2$NUM_HKS_SET*1000
dat2$whaleshark_cpue <- dat2$COUNT_WHALE_SHARK/dat2$NUM_HKS_SET*1000
dat2$sandbarshark_cpue <- dat2$COUNT_SANDBAR_SHARK/dat2$NUM_HKS_SET*1000
dat2$salmonshark_cpue <- dat2$COUNT_SALMON_SHARK/dat2$NUM_HKS_SET*1000
dat2$baskingshark_cpue <- dat2$COUNT_BASKING_SHARK/dat2$NUM_HKS_SET*1000
dat2$bignoseshark_cpue <- dat2$COUNT_BIGNOSE_SHARK/dat2$NUM_HKS_SET*1000
dat2$blacktipshark_cpue <- dat2$COUNT_BLACKTIP_SHARK/dat2$NUM_HKS_SET*1000
dat2$blacktipreefshark_cpue <- dat2$COUNT_BLACKTIP_REEF_SHARK/dat2$NUM_HKS_SET*1000
dat2$duskyshark_cpue <- dat2$COUNT_DUSKY_SHARK/dat2$NUM_HKS_SET*1000
dat2$galapagosshark_cpue <- dat2$COUNT_GALAPAGOS_SHARK/dat2$NUM_HKS_SET*1000
dat2$grayreefshark_cpue <- dat2$COUNT_GRAY_REEF_SHARK/dat2$NUM_HKS_SET*1000
dat2$megamouthshark_cpue <- dat2$COUNT_MEGAMOUTH_SHARK/dat2$NUM_HKS_SET*1000
dat2$other_ID_shark_cpue <- dat2$COUNT_OTHER_IDENTIFIED_SHARK/dat2$NUM_HKS_SET*1000
dat2$other_UnID_shark_cpue <- dat2$COUNT_UNID_SHARK/dat2$NUM_HKS_SET*1000

masterdat <- dat2[,c(1:3,36:67)]

masterdat2 <- masterdat %>% group_by(roundedlat,roundedlon) %>%
  summarise_each(funs(mean))

mean(masterdat2$blueshark_cpue)
mean(masterdat$blueshark_cpue)

#marmap to extract depths and assign to each lat/long
