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

fulldat$log_hooks=log(fulldat$NUM_HKS_SET)
fulldat<- fulldat %>% distinct()

## Split datasets
hawaii<-subset(fulldat, FISHERY == "HI")
amsam<-subset(fulldat, FISHERY == "AS")

####GLMs####
library(MASS)
library(pscl)
library(car)
library(glmmTMB)
library(bbmle)
library(fields)
library(DHARMa)


#dataset for predictions
dummy.amsam <- data.frame(subset(amsam, select = c("VESSEL_ID", "lat", "long", "DEPART_YEAR", "NUM_HKS_SET", "log_hooks")))
dummy.amsam$log_hooks=log(1000)
dummy.amsam$NUM_HKS_SET=1000
# Species models
{
  ####Blue shark####
  #Simple
mod.amsam.blueshark1 <- glm.nb(COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                             data=amsam) 
#spline lat and long
  
  #Zero-inflated
mod.amsam.blueshark2 <- zeroinfl(COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
                               offset = log_hooks,
                               data=amsam, dist="poisson")

#Hurdle  
mod.amsam.blueshark3 <- hurdle(COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
                               offset = log_hooks,
                               data=amsam, dist="negbin")
  #Mixed Model
mod.amsam.blueshark4 <- glmmTMB(data=amsam, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                              ziformula =~0,
                              family=nbinom2)
  
mod.amsam.blueshark4_2 <- glmmTMB(data=amsam, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula =~0,
                                family=nbinom1)
  
  #Zero Infl Mixed - 1 parameter
mod.amsam.blueshark5 <- glmmTMB(data=amsam, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                              ziformula =~1,
                              family=nbinom2)
  
mod.amsam.blueshark5_2 <- glmmTMB(data=amsam, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula =~1,
                                family=nbinom1)
  
  
  #Zero Infl Mixed - Full Predictors Set ## CANNOT FIT
#mod.amsam.blueshark6 <- glmmTMB(data=amsam, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            family=nbinom2)
  
#mod.amsam.blueshark6_2 <- glmmTMB(data=amsam, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
#                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                              family=nbinom1)
  
  #Hurdle model mixed
mod.amsam.blueshark7 <- glmmTMB(data=amsam, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                              ziformula = ~.,
                              family=truncated_nbinom2)

mod.amsam.blueshark7_2 <- glmmTMB(data=amsam, COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula = ~.,
                                family=truncated_nbinom1)

  #Residuals
  amsam$bluesharkResid1=resid(mod.amsam.blueshark1)
  amsam$bluesharkResid2=resid(mod.amsam.blueshark2)
  amsam$bluesharkResid3=resid(mod.amsam.blueshark3)
  amsam$bluesharkResid4=resid(mod.amsam.blueshark4, type="response")
  amsam$bluesharkResid4_2=residuals(mod.amsam.blueshark4_2, type="response")
  amsam$bluesharkResid5=residuals(mod.amsam.blueshark5, type="response")
  amsam$bluesharkResid5_2=residuals(mod.amsam.blueshark5_2, type="response")
  #amsam$bluesharkResid6=residuals(mod.amsam.blueshark6, type="response")
  #amsam$bluesharkResid6=resid(mod.amsam.blueshark6_2, type="response")
  amsam$bluesharkResid7=residuals(mod.amsam.blueshark7, type="response")
  amsam$bluesharkResid7_2=residuals(mod.amsam.blueshark7, type="response")
  
  #plotting
  bbox = c(min(amsam$lat),max(amsam$lat),min(amsam$long),max(amsam$long))
  ylim <- c(bbox[1], bbox[2]) 
  xlim <- c(bbox[3], bbox[4])
  
  #Season as a sine/cosine function of month or ordinal day
  
  #SST - NOAA
  #Depths from marmap
  #Distance from land
  #Use 
  
  blueshark_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati1 = as.numeric(dimnames(blueshark_Grid1)[[2]])
  blueshark_long1 = as.numeric(dimnames(blueshark_Grid1)[[1]])
  
  blueshark_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati2 = as.numeric(dimnames(blueshark_Grid2)[[2]])
  blueshark_long2 = as.numeric(dimnames(blueshark_Grid2)[[1]])
  
  blueshark_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati3 = as.numeric(dimnames(blueshark_Grid3)[[2]])
  blueshark_long3 = as.numeric(dimnames(blueshark_Grid3)[[1]])
  
  blueshark_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati4 = as.numeric(dimnames(blueshark_Grid4)[[2]])
  blueshark_long4 = as.numeric(dimnames(blueshark_Grid4)[[1]])
  
  blueshark_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati4_2 = as.numeric(dimnames(blueshark_Grid4_2)[[2]])
  blueshark_long4_2 = as.numeric(dimnames(blueshark_Grid4_2)[[1]])
  
  blueshark_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati5 = as.numeric(dimnames(blueshark_Grid5)[[2]])
  blueshark_long5 = as.numeric(dimnames(blueshark_Grid5)[[1]])
  
  blueshark_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati5_2 = as.numeric(dimnames(blueshark_Grid5_2)[[2]])
  blueshark_long5_2 = as.numeric(dimnames(blueshark_Grid5_2)[[1]])
  
  #blueshark_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #blueshark_lati6 = as.numeric(dimnames(blueshark_Grid6)[[2]])
  #blueshark_long6 = as.numeric(dimnames(blueshark_Grid6)[[1]])
  
  #blueshark_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #blueshark_lati6_2 = as.numeric(dimnames(blueshark_Grid6_2)[[2]])
  #blueshark_long6_2 = as.numeric(dimnames(blueshark_Grid6_2)[[1]])
  
  blueshark_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati7 = as.numeric(dimnames(blueshark_Grid7)[[2]])
  blueshark_long7 = as.numeric(dimnames(blueshark_Grid7)[[1]])
  
  blueshark_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = bluesharkResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  blueshark_lati7_2 = as.numeric(dimnames(blueshark_Grid7_2)[[2]])
  blueshark_long7_2 = as.numeric(dimnames(blueshark_Grid7_2)[[1]])
  
  # Z limits - coerce to max and min
  zlim1 = range(blueshark_Grid1, na.rm = TRUE)
  zlim2 = range(blueshark_Grid2, na.rm = TRUE)
  zlim3 = range(blueshark_Grid3, na.rm = TRUE)
  zlim4 = range(blueshark_Grid4, na.rm = TRUE)
  zlim4_2 = range(blueshark_Grid4_2, na.rm = TRUE)
  zlim5 = range(blueshark_Grid5, na.rm = TRUE)
  zlim5_2 = range(blueshark_Grid5_2, na.rm = TRUE)
  #zlim6 = range(blueshark_Grid6, na.rm = TRUE)
  #zlim6_2 = range(blueshark_Grid6_2, na.rm = TRUE)
  zlim7 = range(blueshark_Grid7, na.rm = TRUE)
  zlim7_2 = range(blueshark_Grid7_2, na.rm = TRUE)
  
  #Zlimit colors
  blueshark_zlim_fixed=c(-75,75)
  blueshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-75, 0, 75) / 10)
  
  blueshark_zlim_mixed=c(-75,75)
  blueshark_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-75, 0, 75) / 10)
  
  blueshark_zlim_mixed2=c(-150,150)
  blueshark_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -75, 0, 75, 150) / 10)
  
  #Plots
  image.plot(blueshark_long1, blueshark_lati1, blueshark_Grid1, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Fixed Effects", 
             legend.lab="Residuals", zlim = blueshark_zlim_fixed, col=blueshark_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(blueshark_long2, blueshark_lati2, blueshark_Grid2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Fixed Zero-Inflate", 
             legend.lab="Residuals", zlim = blueshark_zlim_fixed, col=blueshark_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(blueshark_long3, blueshark_lati3, blueshark_Grid3, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Fixed Hurdle", 
             legend.lab="Residuals", zlim = blueshark_zlim_fixed, col=blueshark_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(blueshark_long4, blueshark_lati4, blueshark_Grid4, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed Effects", 
             legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(blueshark_long4_2, blueshark_lati4_2, blueshark_Grid4_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed Effects", 
             legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

  image.plot(blueshark_long5, blueshark_lati5, blueshark_Grid5, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed 1par ZeroInfl", 
             legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(blueshark_long5_2, blueshark_lati5_2, blueshark_Grid5_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed 1par ZeroInfl", 
             legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(blueshark_long6, blueshark_lati6, blueshark_Grid6, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(blueshark_long6_2, blueshark_lati6_2, blueshark_Grid6_2, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = blueshark_zlim_mixed, col=blueshark_colorTable_mixed)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
             
  image.plot(blueshark_long7, blueshark_lati7, blueshark_Grid7, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed FullZeroInfl", 
             legend.lab="Residuals", zlim = blueshark_zlim_mixed2, col=blueshark_colorTable_mixed2)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(blueshark_long7_2, blueshark_lati7_2, blueshark_Grid7_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Blue Sharks - Mixed FullZeroInfl", 
             legend.lab="Residuals", zlim = blueshark_zlim_mixed2, col=blueshark_colorTable_mixed2)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #Model comparison - fixed effects
  vuong(mod.amsam.blueshark1,mod.amsam.blueshark2)
  vuong(mod.amsam.blueshark1,mod.amsam.blueshark3) #Fixed hurdle best
  
  #Model comparison - all models
  AICtab(mod.amsam.blueshark1,
         mod.amsam.blueshark2,
         mod.amsam.blueshark3,
         mod.amsam.blueshark4,
         mod.amsam.blueshark4_2,
         mod.amsam.blueshark5,
         mod.amsam.blueshark5_2,
         #mod.amsam.blueshark6,
         #mod.amsam.blueshark6_2,
         mod.amsam.blueshark7,
         mod.amsam.blueshark7_2,
         sort=T)
  
#residuals
amsam.blueshark1.resid<-simulateResiduals(mod.amsam.blueshark1)
amsam.blueshark4.resid<-simulateResiduals(mod.amsam.blueshark4, group=amsam$VESSEL_ID)
plot(amsam.blueshark1.resid)
plot(amsam.blueshark4.resid)

plot(resid(mod.amsam.blueshark1), main="Fixed Effects")  
plot(resid(mod.amsam.blueshark3), main="Fixed Effects Hurdle")

plot(x = predict(mod.amsam.blueshark1, type="response"),
     y = resid(mod.amsam.blueshark1), main="Fixed Effects", 
     xlab = "Predicted Value", ylim=c(-15,15))
plot(x = predict(mod.amsam.blueshark3, type="response"),
     y = resid(mod.amsam.blueshark3), main="Fixed Effects Hurdle", 
     xlab = "Predicted Value", ylim=c(-15,15))
plot(x = predict(mod.amsam.blueshark4, type="response"),
     y = resid(mod.amsam.blueshark4), main="Mixed Effect", 
     xlab = "Predicted Value", ylim=c(-15,15))

qqplot()
summary(residuals(mod.amsam.blueshark1))
summary(residuals(mod.amsam.blueshark3))
summary(residuals(mod.amsam.blueshark4))


plot(x=predict(mod.amsam.blueshark1, type="response"),
     y=residuals(mod.amsam.blueshark3, type="pearson"),
     main="Fixed Effects") 
plot(x=predict(mod.amsam.blueshark3, type="response"),
     y=residuals(mod.amsam.blueshark3, type="pearson"),
     main="Fixed Effects Hurdle") 
plot(x=predict(mod.amsam.blueshark4, type="response"),
     y=residuals(mod.amsam.blueshark4, type="pearson"),
     main="Random Effect") 



summary(mod.amsam.blueshark3)
Anova(mod.amsam.blueshark3)

summary(mod.amsam.blueshark4)
Anova(mod.amsam.blueshark4)

pred.blueshark <- data.frame(dummy.amsam, predicted=predict(mod.amsam.blueshark3, newdata=dummy.amsam, type = "response", se.fit=TRUE))
pred.blueshark <- data.frame(dummy.amsam, predicted=predict(mod.amsam.blueshark4, newdata=dummy.amsam, type = "response", se.fit=TRUE))


write_csv(pred.blueshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_blueshark.csv")
  
  
  #### Silky shark ####
#Fixed effects
mod.amsam.silkyshark1 <- glm.nb(COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                               data=amsam) 
#Zero-inflated
mod.amsam.silkyshark2 <- zeroinfl(COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
                                 offset = log_hooks,
                                 data=amsam, dist="poisson")
#Hurdle  
mod.amsam.silkyshark3 <- hurdle(COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
                               offset = log_hooks,
                               data=amsam, dist="negbin")
#Mixed Model
mod.amsam.silkyshark4 <- glmmTMB(data=amsam, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula =~0,
                                family=nbinom2)

mod.amsam.silkyshark4_2 <- glmmTMB(data=amsam, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                  ziformula =~0,
                                  family=nbinom1)

#Zero Infl Mixed - 1 parameter
mod.amsam.silkyshark5 <- glmmTMB(data=amsam, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula =~1,
                                family=nbinom2)

mod.amsam.silkyshark5_2 <- glmmTMB(data=amsam, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
                                  ziformula =~1,
                                  family=nbinom1)


#Zero Infl Mixed - Full Predictors Set
#mod.amsam.silkyshark6 <- glmmTMB(data=amsam, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            family=nbinom2)

#mod.amsam.silkyshark6_2 <- glmmTMB(data=amsam, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
#                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                              family=nbinom1)

#Hurdle model mixed
mod.amsam.silkyshark7 <- glmmTMB(data=amsam, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                ziformula = ~.,
                                family=truncated_nbinom2)

#mod.amsam.silkyshark7_2 <- glmmTMB(data=amsam, COUNT_SILKY_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                                  ziformula = ~.,
#                                  family=truncated_nbinom1)

#Residuals
amsam$silkysharkResid1=resid(mod.amsam.silkyshark1)
amsam$silkysharkResid2=resid(mod.amsam.silkyshark2)
amsam$silkysharkResid3=resid(mod.amsam.silkyshark3)
amsam$silkysharkResid4=resid(mod.amsam.silkyshark4, type="response")
amsam$silkysharkResid4_2=residuals(mod.amsam.silkyshark4_2, type="response")
amsam$silkysharkResid5=residuals(mod.amsam.silkyshark5, type="response")
amsam$silkysharkResid5_2=residuals(mod.amsam.silkyshark5_2, type="response")
#amsam$silkysharkResid6=residuals(mod.amsam.silkyshark6, type="response")
#amsam$silkysharkResid6=resid(mod.amsam.silkyshark6_2, type="response")
amsam$silkysharkResid7=residuals(mod.amsam.silkyshark7, type="response")
#amsam$silkysharkResid7_2=residuals(mod.amsam.silkyshark7, type="response")

silkyshark_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati1 = as.numeric(dimnames(silkyshark_Grid1)[[2]])
silkyshark_long1 = as.numeric(dimnames(silkyshark_Grid1)[[1]])

silkyshark_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati2 = as.numeric(dimnames(silkyshark_Grid2)[[2]])
silkyshark_long2 = as.numeric(dimnames(silkyshark_Grid2)[[1]])

silkyshark_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati3 = as.numeric(dimnames(silkyshark_Grid3)[[2]])
silkyshark_long3 = as.numeric(dimnames(silkyshark_Grid3)[[1]])

silkyshark_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati4 = as.numeric(dimnames(silkyshark_Grid4)[[2]])
silkyshark_long4 = as.numeric(dimnames(silkyshark_Grid4)[[1]])

silkyshark_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati4_2 = as.numeric(dimnames(silkyshark_Grid4_2)[[2]])
silkyshark_long4_2 = as.numeric(dimnames(silkyshark_Grid4_2)[[1]])

silkyshark_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati5 = as.numeric(dimnames(silkyshark_Grid5)[[2]])
silkyshark_long5 = as.numeric(dimnames(silkyshark_Grid5)[[1]])

silkyshark_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati5_2 = as.numeric(dimnames(silkyshark_Grid5_2)[[2]])
silkyshark_long5_2 = as.numeric(dimnames(silkyshark_Grid5_2)[[1]])

#silkyshark_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#silkyshark_lati6 = as.numeric(dimnames(silkyshark_Grid6)[[2]])
#silkyshark_long6 = as.numeric(dimnames(silkyshark_Grid6)[[1]])

#silkyshark_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#silkyshark_lati6_2 = as.numeric(dimnames(silkyshark_Grid6_2)[[2]])
#silkyshark_long6_2 = as.numeric(dimnames(silkyshark_Grid6_2)[[1]])

silkyshark_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
silkyshark_lati7 = as.numeric(dimnames(silkyshark_Grid7)[[2]])
silkyshark_long7 = as.numeric(dimnames(silkyshark_Grid7)[[1]])

#silkyshark_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = silkysharkResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#silkyshark_lati7_2 = as.numeric(dimnames(silkyshark_Grid7_2)[[2]])
#silkyshark_long7_2 = as.numeric(dimnames(silkyshark_Grid7_2)[[1]])

# Z limits - coerce to max and min
zlim1 = range(silkyshark_Grid1, na.rm = TRUE)
zlim2 = range(silkyshark_Grid2, na.rm = TRUE)
zlim3 = range(silkyshark_Grid3, na.rm = TRUE)
zlim4 = range(silkyshark_Grid4, na.rm = TRUE)
zlim4_2 = range(silkyshark_Grid4_2, na.rm = TRUE)
zlim5 = range(silkyshark_Grid5, na.rm = TRUE)
zlim5_2 = range(silkyshark_Grid5_2, na.rm = TRUE)
#zlim6 = range(silkyshark_Grid6, na.rm = TRUE)
#zlim6_2 = range(silkyshark_Grid6_2, na.rm = TRUE)
zlim7 = range(silkyshark_Grid7, na.rm = TRUE)
#zlim7_2 = range(silkyshark_Grid7_2, na.rm = TRUE)



#Zlimit colors
silkyshark_zlim_fixed=c(-50,50)
silkyshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

silkyshark_zlim_mixed=c(-50,50)
silkyshark_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

silkyshark_zlim_mixed2=c(-150,150)
silkyshark_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -50, 0, 50, 150) / 10)

#Plots
image.plot(silkyshark_long1, silkyshark_lati1, silkyshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Fixed Effects", 
           legend.lab="Residuals", zlim = silkyshark_zlim_fixed, col=silkyshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long2, silkyshark_lati2, silkyshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Fixed Zero-Inflate", 
           legend.lab="Residuals", zlim = silkyshark_zlim_fixed, col=silkyshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long3, silkyshark_lati3, silkyshark_Grid3, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Fixed Hurdle", 
           legend.lab="Residuals", zlim = silkyshark_zlim_fixed, col=silkyshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long4, silkyshark_lati4, silkyshark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long4_2, silkyshark_lati4_2, silkyshark_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long5, silkyshark_lati5, silkyshark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long5_2, silkyshark_lati5_2, silkyshark_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(silkyshark_long6, silkyshark_lati6, silkyshark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(silkyshark_long6_2, silkyshark_lati6_2, silkyshark_Grid6_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = silkyshark_zlim_mixed, col=silkyshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(silkyshark_long7, silkyshark_lati7, silkyshark_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed FullZeroInfl", 
           legend.lab="Residuals", zlim = silkyshark_zlim_mixed2, col=silkyshark_colorTable_mixed2)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(silkyshark_long7_2, silkyshark_lati7_2, silkyshark_Grid7_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Silky Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = silkyshark_zlim_mixed2, col=silkyshark_colorTable_mixed2)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#Model comparison - fixed effects
vuong(mod.amsam.silkyshark1,mod.amsam.silkyshark2)
vuong(mod.amsam.silkyshark1,mod.amsam.silkyshark3) #Fixed hurdle best

#Model comparison - all models
AICtab(mod.amsam.silkyshark1,
       mod.amsam.silkyshark2,
       mod.amsam.silkyshark3,
       mod.amsam.silkyshark4,
       mod.amsam.silkyshark4_2,
       mod.amsam.silkyshark5,
       mod.amsam.silkyshark5_2,
       #mod.amsam.silkyshark6,
       #mod.amsam.silkyshark6_2,
       mod.amsam.silkyshark7,
       #mod.amsam.silkyshark7_2,
       sort=T)

#residuals
amsam.silkyshark7.resid<-simulateResiduals(mod.amsam.silkyshark7, group=amsam$VESSEL_ID)
amsam.silkyshark4.resid<-simulateResiduals(mod.amsam.silkyshark4, group=amsam$VESSEL_ID)

plot(amsam.silkyshark7.resid)
plot(amsam.silkyshark4.resid)

plot(resid(mod.amsam.silkyshark7), main="Mixed Effects Hurdle")  
plot(resid(mod.amsam.silkyshark4), main="Mixed Effects")

plot(x = predict(mod.amsam.silkyshark4, type="response"),
     y = resid(mod.amsam.silkyshark4), main="Mixed Effects", 
     xlab = "Predicted Value")
plot(x = predict(mod.amsam.silkyshark7, type="response"),
     y = resid(mod.amsam.silkyshark7), main="Mixed Effect Hurdle", 
     xlab = "Predicted Value")

summary(residuals(mod.amsam.silkyshark7))
summary(residuals(mod.amsam.silkyshark4))

plot(x=predict(mod.amsam.silkyshark1, type="response"),
     y=residuals(mod.amsam.silkyshark3, type="pearson"),
     main="Fixed Effects") 
plot(x=predict(mod.amsam.silkyshark3, type="response"),
     y=residuals(mod.amsam.silkyshark3, type="pearson"),
     main="Fixed Effects Hurdle") 
plot(x=predict(mod.amsam.silkyshark4, type="response"),
     y=residuals(mod.amsam.silkyshark4, type="pearson"),
     main="Random Effect") 

#Significance
summary(mod.amsam.silkyshark7)
Anova(mod.amsam.silkyshark7)

pred.silkyshark <- data.frame(dummy.amsam, predicted=predict(mod.amsam.silkyshark7, newdata=dummy.amsam, type = "response", se.fit=TRUE))

write_csv(pred.silkyshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_silkyshark.csv")

  #### Unidentified Mako #### 
#### DON"T USE ONLY 1 for Samoa
#Fixed effects
mod.amsam.unidmakoshark1 <- glm.nb(COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                data=amsam) 
#Zero-inflated
mod.amsam.unidmakoshark2 <- zeroinfl(COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
                                  offset = log_hooks,
                                  data=amsam, dist="poisson")
#Hurdle  
mod.amsam.unidmakoshark3 <- hurdle(COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
                                offset = log_hooks,
                                data=amsam, dist="negbin")
#Mixed Model
mod.amsam.unidmakoshark4 <- glmmTMB(data=amsam, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula =~0,
                                 family=nbinom2)

mod.amsam.unidmakoshark4_2 <- glmmTMB(data=amsam, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~0,
                                   family=nbinom1)

#Zero Infl Mixed - 1 parameter
mod.amsam.unidmakoshark5 <- glmmTMB(data=amsam, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula =~1,
                                 family=nbinom2)

mod.amsam.unidmakoshark5_2 <- glmmTMB(data=amsam, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~1,
                                   family=nbinom1)


#Zero Infl Mixed - Full Predictors Set
#mod.amsam.unidmakoshark6 <- glmmTMB(data=amsam, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            family=nbinom2)

#mod.amsam.unidmakoshark6_2 <- glmmTMB(data=amsam, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
#                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                              family=nbinom1)

#Hurdle model mixed
mod.amsam.unidmakoshark7 <- glmmTMB(data=amsam, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula = ~.,
                                 family=truncated_nbinom2)

#mod.amsam.unidmakoshark7_2 <- glmmTMB(data=amsam, COUNT_UNID_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                                  ziformula = ~.,
#                                  family=truncated_nbinom1)

#Residuals
amsam$unidmakosharkResid1=resid(mod.amsam.unidmakoshark1)
amsam$unidmakosharkResid2=resid(mod.amsam.unidmakoshark2)
amsam$unidmakosharkResid3=resid(mod.amsam.unidmakoshark3)
amsam$unidmakosharkResid4=resid(mod.amsam.unidmakoshark4, type="response")
amsam$unidmakosharkResid4_2=residuals(mod.amsam.unidmakoshark4_2, type="response")
amsam$unidmakosharkResid5=residuals(mod.amsam.unidmakoshark5, type="response")
amsam$unidmakosharkResid5_2=residuals(mod.amsam.unidmakoshark5_2, type="response")
#amsam$unidmakosharkResid6=residuals(mod.amsam.unidmakoshark6, type="response")
#amsam$unidmakosharkResid6=resid(mod.amsam.unidmakoshark6_2, type="response")
amsam$unidmakosharkResid7=residuals(mod.amsam.unidmakoshark7, type="response")
#amsam$unidmakosharkResid7_2=residuals(mod.amsam.unidmakoshark7, type="response")

unidmakoshark_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati1 = as.numeric(dimnames(unidmakoshark_Grid1)[[2]])
unidmakoshark_long1 = as.numeric(dimnames(unidmakoshark_Grid1)[[1]])

unidmakoshark_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati2 = as.numeric(dimnames(unidmakoshark_Grid2)[[2]])
unidmakoshark_long2 = as.numeric(dimnames(unidmakoshark_Grid2)[[1]])

unidmakoshark_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati3 = as.numeric(dimnames(unidmakoshark_Grid3)[[2]])
unidmakoshark_long3 = as.numeric(dimnames(unidmakoshark_Grid3)[[1]])

unidmakoshark_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati4 = as.numeric(dimnames(unidmakoshark_Grid4)[[2]])
unidmakoshark_long4 = as.numeric(dimnames(unidmakoshark_Grid4)[[1]])

unidmakoshark_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati4_2 = as.numeric(dimnames(unidmakoshark_Grid4_2)[[2]])
unidmakoshark_long4_2 = as.numeric(dimnames(unidmakoshark_Grid4_2)[[1]])

unidmakoshark_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati5 = as.numeric(dimnames(unidmakoshark_Grid5)[[2]])
unidmakoshark_long5 = as.numeric(dimnames(unidmakoshark_Grid5)[[1]])

unidmakoshark_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati5_2 = as.numeric(dimnames(unidmakoshark_Grid5_2)[[2]])
unidmakoshark_long5_2 = as.numeric(dimnames(unidmakoshark_Grid5_2)[[1]])

#unidmakoshark_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#unidmakoshark_lati6 = as.numeric(dimnames(unidmakoshark_Grid6)[[2]])
#unidmakoshark_long6 = as.numeric(dimnames(unidmakoshark_Grid6)[[1]])

#unidmakoshark_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#unidmakoshark_lati6_2 = as.numeric(dimnames(unidmakoshark_Grid6_2)[[2]])
#unidmakoshark_long6_2 = as.numeric(dimnames(unidmakoshark_Grid6_2)[[1]])

unidmakoshark_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
unidmakoshark_lati7 = as.numeric(dimnames(unidmakoshark_Grid7)[[2]])
unidmakoshark_long7 = as.numeric(dimnames(unidmakoshark_Grid7)[[1]])

#unidmakoshark_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = unidmakosharkResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#unidmakoshark_lati7_2 = as.numeric(dimnames(unidmakoshark_Grid7_2)[[2]])
#unidmakoshark_long7_2 = as.numeric(dimnames(unidmakoshark_Grid7_2)[[1]])

# Z limits - coerce to max and min
zlim1 = range(unidmakoshark_Grid1, na.rm = TRUE)
zlim2 = range(unidmakoshark_Grid2, na.rm = TRUE)
zlim3 = range(unidmakoshark_Grid3, na.rm = TRUE)
zlim4 = range(unidmakoshark_Grid4, na.rm = TRUE)
zlim4_2 = range(unidmakoshark_Grid4_2, na.rm = TRUE)
zlim5 = range(unidmakoshark_Grid5, na.rm = TRUE)
zlim5_2 = range(unidmakoshark_Grid5_2, na.rm = TRUE)
#zlim6 = range(unidmakoshark_Grid6, na.rm = TRUE)
#zlim6_2 = range(unidmakoshark_Grid6_2, na.rm = TRUE)
zlim7 = range(unidmakoshark_Grid7, na.rm = TRUE)
#zlim7_2 = range(unidmakoshark_Grid7_2, na.rm = TRUE)



#Zlimit colors
unidmakoshark_zlim_fixed=c(-50,50)
unidmakoshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

unidmakoshark_zlim_mixed=c(-50,50)
unidmakoshark_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

unidmakoshark_zlim_mixed2=c(-150,150)
unidmakoshark_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -50, 0, 50, 150) / 10)

#Plots
image.plot(unidmakoshark_long1, unidmakoshark_lati1, unidmakoshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Fixed Effects", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_fixed, col=unidmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long2, unidmakoshark_lati2, unidmakoshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Fixed Zero-Inflate", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_fixed, col=unidmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long3, unidmakoshark_lati3, unidmakoshark_Grid3, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Fixed Hurdle", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_fixed, col=unidmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long4, unidmakoshark_lati4, unidmakoshark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long4_2, unidmakoshark_lati4_2, unidmakoshark_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Mixed Effects", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long5, unidmakoshark_lati5, unidmakoshark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long5_2, unidmakoshark_lati5_2, unidmakoshark_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(unidmakoshark_long6, unidmakoshark_lati6, unidmakoshark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(unidmakoshark_long6_2, unidmakoshark_lati6_2, unidmakoshark_Grid6_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed, col=unidmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(unidmakoshark_long7, unidmakoshark_lati7, unidmakoshark_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Mixed FullZeroInfl", 
           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed2, col=unidmakoshark_colorTable_mixed2)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(unidmakoshark_long7_2, unidmakoshark_lati7_2, unidmakoshark_Grid7_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Unidentified Mako Sharks - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = unidmakoshark_zlim_mixed2, col=unidmakoshark_colorTable_mixed2)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#Model comparison - fixed effects
vuong(mod.amsam.unidmakoshark1,mod.amsam.unidmakoshark2)
vuong(mod.amsam.unidmakoshark1,mod.amsam.unidmakoshark3) #Fixed hurdle best

#Model comparison - all models
AICtab(mod.amsam.unidmakoshark1,
       mod.amsam.unidmakoshark2,
       mod.amsam.unidmakoshark3,
       mod.amsam.unidmakoshark4,
       mod.amsam.unidmakoshark4_2,
       mod.amsam.unidmakoshark5,
       mod.amsam.unidmakoshark5_2,
       #mod.amsam.unidmakoshark6,
       #mod.amsam.unidmakoshark6_2,
       mod.amsam.unidmakoshark7,
       #mod.amsam.unidmakoshark7_2,
       sort=T)

library(DHARMa)
amsam.unidmakoshark7.resid<-simulateResiduals(mod.amsam.unidmakoshark1, group=amsam$VESSEL_ID)
amsam.unidmakoshark4.resid<-simulateResiduals(mod.amsam.unidmakoshark4, group=amsam$VESSEL_ID)

plot(amsam.unidmakoshark7.resid)
plot(amsam.unidmakoshark4.resid)

plot(resid(mod.amsam.unidmakoshark7), main="Mixed Effects Hurdle")  
plot(resid(mod.amsam.unidmakoshark4), main="Mixed Effects")

plot(x = predict(mod.amsam.unidmakoshark3, type="response"),
     y = resid(mod.amsam.unidmakoshark3), main="Fixed Effects Hurdle", 
     xlab = "Predicted Value", ylim=c(-15,15))
plot(x = predict(mod.amsam.unidmakoshark4, type="response"),
     y = resid(mod.amsam.unidmakoshark4), main="Mixed Effects", 
     xlab = "Predicted Value", ylim=c(-15,15))
plot(x = predict(mod.amsam.unidmakoshark7, type="response"),
     y = resid(mod.amsam.unidmakoshark7), main="Mixed Effect Hurdle", 
     xlab = "Predicted Value", ylim=c(-15,15))

summary(residuals(mod.amsam.unidmakoshark1))
summary(residuals(mod.amsam.unidmakoshark3))
summary(residuals(mod.amsam.unidmakoshark4))

plot(x=predict(mod.amsam.unidmakoshark1, type="response"),
     y=residuals(mod.amsam.unidmakoshark3, type="pearson"),
     main="Fixed Effects") 
plot(x=predict(mod.amsam.unidmakoshark3, type="response"),
     y=residuals(mod.amsam.unidmakoshark3, type="pearson"),
     main="Fixed Effects Hurdle") 
plot(x=predict(mod.amsam.unidmakoshark4, type="response"),
     y=residuals(mod.amsam.unidmakoshark4, type="pearson"),
     main="Random Effect") 

#Significance
summary(mod.amsam.unidmakoshark4)
Anova(mod.amsam.unidmakoshark4)

pred.unidmakoshark <- data.frame(dummy.amsam, predicted=predict(mod.amsam.unidmakoshark4, newdata=dummy.amsam, type = "response", se.fit=TRUE))

write_csv(pred.unidmakoshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_unidmakoshark.csv")

  #### Shortfin mako ####
#Fixed effects
mod.amsam.shortfinmakoshark1 <- glm.nb(COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                data=amsam) 
#Zero-inflated
mod.amsam.shortfinmakoshark2 <- zeroinfl(COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
                                  offset = log_hooks,
                                  data=amsam, dist="poisson")
#Hurdle  
mod.amsam.shortfinmakoshark3 <- hurdle(COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
                                offset = log_hooks,
                                data=amsam, dist="negbin")
#Mixed Model
mod.amsam.shortfinmakoshark4 <- glmmTMB(data=amsam, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula =~0,
                                 family=nbinom2)

mod.amsam.shortfinmakoshark4_2 <- glmmTMB(data=amsam, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~0,
                                   family=nbinom1)

#Zero Infl Mixed - 1 parameter
mod.amsam.shortfinmakoshark5 <- glmmTMB(data=amsam, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula =~1,
                                 family=nbinom2)

mod.amsam.shortfinmakoshark5_2 <- glmmTMB(data=amsam, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~1,
                                   family=nbinom1)


#Zero Infl Mixed - Full Predictors Set
#mod.amsam.shortfinmakoshark6 <- glmmTMB(data=amsam, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            family=nbinom2)

#mod.amsam.shortfinmakoshark6_2 <- glmmTMB(data=amsam, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
#                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                              family=nbinom1)

#Hurdle model mixed
#mod.amsam.shortfinmakoshark7 <- glmmTMB(data=amsam, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                                 ziformula = ~.,
#                                 family=truncated_nbinom2)

#mod.amsam.shortfinmakoshark7_2 <- glmmTMB(data=amsam, COUNT_SHORTFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                                  ziformula = ~.,
#                                  family=truncated_nbinom1)

#Residuals
amsam$shortfinmakosharkResid1=resid(mod.amsam.shortfinmakoshark1)
amsam$shortfinmakosharkResid2=resid(mod.amsam.shortfinmakoshark2)
amsam$shortfinmakosharkResid3=resid(mod.amsam.shortfinmakoshark3)
amsam$shortfinmakosharkResid4=resid(mod.amsam.shortfinmakoshark4, type="response")
amsam$shortfinmakosharkResid4_2=residuals(mod.amsam.shortfinmakoshark4_2, type="response")
amsam$shortfinmakosharkResid5=residuals(mod.amsam.shortfinmakoshark5, type="response")
amsam$shortfinmakosharkResid5_2=residuals(mod.amsam.shortfinmakoshark5_2, type="response")
#amsam$shortfinmakosharkResid6=residuals(mod.amsam.shortfinmakoshark6, type="response")
#amsam$shortfinmakosharkResid6=resid(mod.amsam.shortfinmakoshark6_2, type="response")
#amsam$shortfinmakosharkResid7=residuals(mod.amsam.shortfinmakoshark7, type="response")
#amsam$shortfinmakosharkResid7_2=residuals(mod.amsam.shortfinmakoshark7, type="response")

shortfinmakoshark_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati1 = as.numeric(dimnames(shortfinmakoshark_Grid1)[[2]])
shortfinmakoshark_long1 = as.numeric(dimnames(shortfinmakoshark_Grid1)[[1]])

shortfinmakoshark_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati2 = as.numeric(dimnames(shortfinmakoshark_Grid2)[[2]])
shortfinmakoshark_long2 = as.numeric(dimnames(shortfinmakoshark_Grid2)[[1]])

shortfinmakoshark_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati3 = as.numeric(dimnames(shortfinmakoshark_Grid3)[[2]])
shortfinmakoshark_long3 = as.numeric(dimnames(shortfinmakoshark_Grid3)[[1]])

shortfinmakoshark_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati4 = as.numeric(dimnames(shortfinmakoshark_Grid4)[[2]])
shortfinmakoshark_long4 = as.numeric(dimnames(shortfinmakoshark_Grid4)[[1]])

shortfinmakoshark_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati4_2 = as.numeric(dimnames(shortfinmakoshark_Grid4_2)[[2]])
shortfinmakoshark_long4_2 = as.numeric(dimnames(shortfinmakoshark_Grid4_2)[[1]])

shortfinmakoshark_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati5 = as.numeric(dimnames(shortfinmakoshark_Grid5)[[2]])
shortfinmakoshark_long5 = as.numeric(dimnames(shortfinmakoshark_Grid5)[[1]])

shortfinmakoshark_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
shortfinmakoshark_lati5_2 = as.numeric(dimnames(shortfinmakoshark_Grid5_2)[[2]])
shortfinmakoshark_long5_2 = as.numeric(dimnames(shortfinmakoshark_Grid5_2)[[1]])

#shortfinmakoshark_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#shortfinmakoshark_lati6 = as.numeric(dimnames(shortfinmakoshark_Grid6)[[2]])
#shortfinmakoshark_long6 = as.numeric(dimnames(shortfinmakoshark_Grid6)[[1]])

#shortfinmakoshark_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#shortfinmakoshark_lati6_2 = as.numeric(dimnames(shortfinmakoshark_Grid6_2)[[2]])
#shortfinmakoshark_long6_2 = as.numeric(dimnames(shortfinmakoshark_Grid6_2)[[1]])

#shortfinmakoshark_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
#shortfinmakoshark_lati7 = as.numeric(dimnames(shortfinmakoshark_Grid7)[[2]])
#shortfinmakoshark_long7 = as.numeric(dimnames(shortfinmakoshark_Grid7)[[1]])

#shortfinmakoshark_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = shortfinmakosharkResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#shortfinmakoshark_lati7_2 = as.numeric(dimnames(shortfinmakoshark_Grid7_2)[[2]])
#shortfinmakoshark_long7_2 = as.numeric(dimnames(shortfinmakoshark_Grid7_2)[[1]])

# Z limits - coerce to max and min
zlim1 = range(shortfinmakoshark_Grid1, na.rm = TRUE)
zlim2 = range(shortfinmakoshark_Grid2, na.rm = TRUE)
zlim3 = range(shortfinmakoshark_Grid3, na.rm = TRUE)
zlim4 = range(shortfinmakoshark_Grid4, na.rm = TRUE)
zlim4_2 = range(shortfinmakoshark_Grid4_2, na.rm = TRUE)
zlim5 = range(shortfinmakoshark_Grid5, na.rm = TRUE)
zlim5_2 = range(shortfinmakoshark_Grid5_2, na.rm = TRUE)
#zlim6 = range(shortfinmakoshark_Grid6, na.rm = TRUE)
#zlim6_2 = range(shortfinmakoshark_Grid6_2, na.rm = TRUE)
#zlim7 = range(shortfinmakoshark_Grid7, na.rm = TRUE)
#zlim7_2 = range(shortfinmakoshark_Grid7_2, na.rm = TRUE)



#Zlimit colors
shortfinmakoshark_zlim_fixed=c(-50,50)
shortfinmakoshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

shortfinmakoshark_zlim_mixed=c(-50,50)
shortfinmakoshark_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

shortfinmakoshark_zlim_mixed2=c(-150,150)
shortfinmakoshark_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -50, 0, 50, 150) / 10)

#Plots
image.plot(shortfinmakoshark_long1, shortfinmakoshark_lati1, shortfinmakoshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Fixed Effects", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_fixed, col=shortfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long2, shortfinmakoshark_lati2, shortfinmakoshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Fixed Zero-Inflate", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_fixed, col=shortfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long3, shortfinmakoshark_lati3, shortfinmakoshark_Grid3, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Fixed Hurdle", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_fixed, col=shortfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long4, shortfinmakoshark_lati4, shortfinmakoshark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Mixed Effects", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long4_2, shortfinmakoshark_lati4_2, shortfinmakoshark_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Mixed Effects", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long5, shortfinmakoshark_lati5, shortfinmakoshark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long5_2, shortfinmakoshark_lati5_2, shortfinmakoshark_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(shortfinmakoshark_long6, shortfinmakoshark_lati6, shortfinmakoshark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(shortfinmakoshark_long6_2, shortfinmakoshark_lati6_2, shortfinmakoshark_Grid6_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed, col=shortfinmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(shortfinmakoshark_long7, shortfinmakoshark_lati7, shortfinmakoshark_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Mixed FullZeroInfl", 
           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed2, col=shortfinmakoshark_colorTable_mixed2)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(shortfinmakoshark_long7_2, shortfinmakoshark_lati7_2, shortfinmakoshark_Grid7_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Shortfin Mako - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = shortfinmakoshark_zlim_mixed2, col=shortfinmakoshark_colorTable_mixed2)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#Model comparison - fixed effects
vuong(mod.amsam.shortfinmakoshark1,mod.amsam.shortfinmakoshark2)
vuong(mod.amsam.shortfinmakoshark1,mod.amsam.shortfinmakoshark3) #Fixed hurdle best

#Model comparison - all models
AICtab(mod.amsam.shortfinmakoshark1,
       mod.amsam.shortfinmakoshark2,
       mod.amsam.shortfinmakoshark3,
       mod.amsam.shortfinmakoshark4,
       mod.amsam.shortfinmakoshark4_2,
       mod.amsam.shortfinmakoshark5,
       mod.amsam.shortfinmakoshark5_2,
       #mod.amsam.shortfinmakoshark6,
       #mod.amsam.shortfinmakoshark6_2,
       #mod.amsam.shortfinmakoshark7,
       #mod.amsam.shortfinmakoshark7_2,
       sort=T)

# residuals
amsam.shortfinmakoshark7.resid<-simulateResiduals(mod.amsam.shortfinmakoshark7, group=amsam$VESSEL_ID)
amsam.shortfinmakoshark4.resid<-simulateResiduals(mod.amsam.shortfinmakoshark4, group=amsam$VESSEL_ID)

plot(amsam.shortfinmakoshark7.resid)
plot(amsam.shortfinmakoshark4.resid)

plot(resid(mod.amsam.shortfinmakoshark7), main="Mixed Effects Hurdle")  
plot(resid(mod.amsam.shortfinmakoshark4), main="Mixed Effects")

plot(x = predict(mod.amsam.shortfinmakoshark4, type="response"),
     y = resid(mod.amsam.shortfinmakoshark4), main="Mixed Effects", 
     xlab = "Predicted Value")

plot(x = predict(mod.amsam.shortfinmakoshark7, type="response"),
     y = resid(mod.amsam.shortfinmakoshark7), main="Mixed Effects Hurdle", 
     xlab = "Predicted Value")

summary(residuals(mod.amsam.shortfinmakoshark4))

#Significance
summary(mod.amsam.shortfinmakoshark4)
Anova(mod.amsam.shortfinmakoshark4)

pred.shortfinmakoshark <- data.frame(dummy.amsam, predicted=predict(mod.amsam.shortfinmakoshark4, newdata=dummy.amsam, type = "response", se.fit=TRUE))

write_csv(pred.shortfinmakoshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_shortfinmakoshark.csv")

  #### Longfin mako ####
# Not enough data --> n=22
#Fixed effects
#mod.amsam.longfinmakoshark1 <- glm.nb(COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
#                                data=amsam) 
#Zero-inflated
#mod.amsam.longfinmakoshark2 <- zeroinfl(COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
#                                  offset = log_hooks,
#                                  data=amsam, dist="poisson")
#Hurdle  
mod.amsam.longfinmakoshark3 <- hurdle(COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
                                offset = log_hooks,
                                data=amsam, dist="negbin")
#Mixed Model
mod.amsam.longfinmakoshark4 <- glmmTMB(data=amsam, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula =~0,
                                 family=nbinom2)

mod.amsam.longfinmakoshark4_2 <- glmmTMB(data=amsam, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~0,
                                   family=nbinom1)

#Zero Infl Mixed - 1 parameter
mod.amsam.longfinmakoshark5 <- glmmTMB(data=amsam, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula =~1,
                                 family=nbinom2)

mod.amsam.longfinmakoshark5_2 <- glmmTMB(data=amsam, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~1,
                                   family=nbinom1)


#Zero Infl Mixed - Full Predictors Set
#mod.amsam.longfinmakoshark6 <- glmmTMB(data=amsam, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            family=nbinom2)

#mod.amsam.longfinmakoshark6_2 <- glmmTMB(data=amsam, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
#                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                              family=nbinom1)

#Hurdle model mixed
mod.amsam.longfinmakoshark7 <- glmmTMB(data=amsam, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula = ~.,
                                 family=truncated_nbinom2)

#mod.amsam.longfinmakoshark7_2 <- glmmTMB(data=amsam, COUNT_LONGFIN_MAKO_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                                  ziformula = ~.,
#                                  family=truncated_nbinom1)

#Residuals
amsam$longfinmakosharkResid1=resid(mod.amsam.longfinmakoshark1)
amsam$longfinmakosharkResid2=resid(mod.amsam.longfinmakoshark2)
amsam$longfinmakosharkResid3=resid(mod.amsam.longfinmakoshark3)
amsam$longfinmakosharkResid4=resid(mod.amsam.longfinmakoshark4, type="response")
amsam$longfinmakosharkResid4_2=residuals(mod.amsam.longfinmakoshark4_2, type="response")
amsam$longfinmakosharkResid5=residuals(mod.amsam.longfinmakoshark5, type="response")
amsam$longfinmakosharkResid5_2=residuals(mod.amsam.longfinmakoshark5_2, type="response")
#amsam$longfinmakosharkResid6=residuals(mod.amsam.longfinmakoshark6, type="response")
#amsam$longfinmakosharkResid6=resid(mod.amsam.longfinmakoshark6_2, type="response")
amsam$longfinmakosharkResid7=residuals(mod.amsam.longfinmakoshark7, type="response")
#amsam$longfinmakosharkResid7_2=residuals(mod.amsam.longfinmakoshark7, type="response")

longfinmakoshark_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati1 = as.numeric(dimnames(longfinmakoshark_Grid1)[[2]])
longfinmakoshark_long1 = as.numeric(dimnames(longfinmakoshark_Grid1)[[1]])

longfinmakoshark_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati2 = as.numeric(dimnames(longfinmakoshark_Grid2)[[2]])
longfinmakoshark_long2 = as.numeric(dimnames(longfinmakoshark_Grid2)[[1]])

longfinmakoshark_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati3 = as.numeric(dimnames(longfinmakoshark_Grid3)[[2]])
longfinmakoshark_long3 = as.numeric(dimnames(longfinmakoshark_Grid3)[[1]])

longfinmakoshark_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati4 = as.numeric(dimnames(longfinmakoshark_Grid4)[[2]])
longfinmakoshark_long4 = as.numeric(dimnames(longfinmakoshark_Grid4)[[1]])

longfinmakoshark_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati4_2 = as.numeric(dimnames(longfinmakoshark_Grid4_2)[[2]])
longfinmakoshark_long4_2 = as.numeric(dimnames(longfinmakoshark_Grid4_2)[[1]])

longfinmakoshark_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati5 = as.numeric(dimnames(longfinmakoshark_Grid5)[[2]])
longfinmakoshark_long5 = as.numeric(dimnames(longfinmakoshark_Grid5)[[1]])

longfinmakoshark_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati5_2 = as.numeric(dimnames(longfinmakoshark_Grid5_2)[[2]])
longfinmakoshark_long5_2 = as.numeric(dimnames(longfinmakoshark_Grid5_2)[[1]])

#longfinmakoshark_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#longfinmakoshark_lati6 = as.numeric(dimnames(longfinmakoshark_Grid6)[[2]])
#longfinmakoshark_long6 = as.numeric(dimnames(longfinmakoshark_Grid6)[[1]])

#longfinmakoshark_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#longfinmakoshark_lati6_2 = as.numeric(dimnames(longfinmakoshark_Grid6_2)[[2]])
#longfinmakoshark_long6_2 = as.numeric(dimnames(longfinmakoshark_Grid6_2)[[1]])

longfinmakoshark_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
longfinmakoshark_lati7 = as.numeric(dimnames(longfinmakoshark_Grid7)[[2]])
longfinmakoshark_long7 = as.numeric(dimnames(longfinmakoshark_Grid7)[[1]])

#longfinmakoshark_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = longfinmakosharkResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#longfinmakoshark_lati7_2 = as.numeric(dimnames(longfinmakoshark_Grid7_2)[[2]])
#longfinmakoshark_long7_2 = as.numeric(dimnames(longfinmakoshark_Grid7_2)[[1]])

# Z limits - coerce to max and min
zlim1 = range(longfinmakoshark_Grid1, na.rm = TRUE)
zlim2 = range(longfinmakoshark_Grid2, na.rm = TRUE)
zlim3 = range(longfinmakoshark_Grid3, na.rm = TRUE)
zlim4 = range(longfinmakoshark_Grid4, na.rm = TRUE)
zlim4_2 = range(longfinmakoshark_Grid4_2, na.rm = TRUE)
zlim5 = range(longfinmakoshark_Grid5, na.rm = TRUE)
zlim5_2 = range(longfinmakoshark_Grid5_2, na.rm = TRUE)
#zlim6 = range(longfinmakoshark_Grid6, na.rm = TRUE)
#zlim6_2 = range(longfinmakoshark_Grid6_2, na.rm = TRUE)
zlim7 = range(longfinmakoshark_Grid7, na.rm = TRUE)
#zlim7_2 = range(longfinmakoshark_Grid7_2, na.rm = TRUE)



#Zlimit colors
longfinmakoshark_zlim_fixed=c(-50,50)
longfinmakoshark_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

longfinmakoshark_zlim_mixed=c(-50,50)
longfinmakoshark_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

longfinmakoshark_zlim_mixed2=c(-150,150)
longfinmakoshark_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -50, 0, 50, 150) / 10)

#Plots
image.plot(longfinmakoshark_long1, longfinmakoshark_lati1, longfinmakoshark_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Fixed Effects", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_fixed, col=longfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long2, longfinmakoshark_lati2, longfinmakoshark_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Fixed Zero-Inflate", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_fixed, col=longfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long3, longfinmakoshark_lati3, longfinmakoshark_Grid3, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Fixed Hurdle", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_fixed, col=longfinmakoshark_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long4, longfinmakoshark_lati4, longfinmakoshark_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Mixed Effects", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long4_2, longfinmakoshark_lati4_2, longfinmakoshark_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Mixed Effects", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long5, longfinmakoshark_lati5, longfinmakoshark_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long5_2, longfinmakoshark_lati5_2, longfinmakoshark_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(longfinmakoshark_long6, longfinmakoshark_lati6, longfinmakoshark_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(longfinmakoshark_long6_2, longfinmakoshark_lati6_2, longfinmakoshark_Grid6_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed, col=longfinmakoshark_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(longfinmakoshark_long7, longfinmakoshark_lati7, longfinmakoshark_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Mixed FullZeroInfl", 
           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed2, col=longfinmakoshark_colorTable_mixed2)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(longfinmakoshark_long7_2, longfinmakoshark_lati7_2, longfinmakoshark_Grid7_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "Longfin Mako - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = longfinmakoshark_zlim_mixed2, col=longfinmakoshark_colorTable_mixed2)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#Model comparison - fixed effects
vuong(mod.amsam.longfinmakoshark1,mod.amsam.longfinmakoshark2)
vuong(mod.amsam.longfinmakoshark1,mod.amsam.longfinmakoshark3) #Fixed hurdle best

#Model comparison - all models
AICtab(mod.amsam.longfinmakoshark1,
       mod.amsam.longfinmakoshark2,
       mod.amsam.longfinmakoshark3,
       mod.amsam.longfinmakoshark4,
       mod.amsam.longfinmakoshark4_2,
       mod.amsam.longfinmakoshark5,
       mod.amsam.longfinmakoshark5_2,
       #mod.amsam.longfinmakoshark6,
       #mod.amsam.longfinmakoshark6_2,
       mod.amsam.longfinmakoshark7,
       #mod.amsam.longfinmakoshark7_2,
       sort=T)

library(DHARMa)
amsam.longfinmakoshark7.resid<-simulateResiduals(mod.amsam.longfinmakoshark1, group=amsam$VESSEL_ID)
amsam.longfinmakoshark4.resid<-simulateResiduals(mod.amsam.longfinmakoshark4, group=amsam$VESSEL_ID)

plot(amsam.longfinmakoshark7.resid)
plot(amsam.longfinmakoshark4.resid)

plot(resid(mod.amsam.longfinmakoshark7), main="Mixed Effects Hurdle")  
plot(resid(mod.amsam.longfinmakoshark4), main="Mixed Effects")

plot(x = predict(mod.amsam.longfinmakoshark3, type="response"),
     y = resid(mod.amsam.longfinmakoshark3), main="Fixed Effects Hurdle", 
     xlab = "Predicted Value", ylim=c(-15,15))
plot(x = predict(mod.amsam.longfinmakoshark4, type="response"),
     y = resid(mod.amsam.longfinmakoshark4), main="Mixed Effects", 
     xlab = "Predicted Value", ylim=c(-15,15))
plot(x = predict(mod.amsam.longfinmakoshark7, type="response"),
     y = resid(mod.amsam.longfinmakoshark7), main="Mixed Effect Hurdle", 
     xlab = "Predicted Value", ylim=c(-15,15))

summary(residuals(mod.amsam.longfinmakoshark1))
summary(residuals(mod.amsam.longfinmakoshark3))
summary(residuals(mod.amsam.longfinmakoshark4))

plot(x=predict(mod.amsam.longfinmakoshark1, type="response"),
     y=residuals(mod.amsam.longfinmakoshark3, type="pearson"),
     main="Fixed Effects") 
plot(x=predict(mod.amsam.longfinmakoshark3, type="response"),
     y=residuals(mod.amsam.longfinmakoshark3, type="pearson"),
     main="Fixed Effects Hurdle") 
plot(x=predict(mod.amsam.longfinmakoshark4, type="response"),
     y=residuals(mod.amsam.longfinmakoshark4, type="pearson"),
     main="Random Effect") 

#Significance
summary(mod.amsam.longfinmakoshark4)
Anova(mod.amsam.longfinmakoshark4)

pred.longfinmakoshark <- data.frame(dummy.amsam, predicted=predict(mod.amsam.longfinmakoshark4, newdata=dummy.amsam, type = "response", se.fit=TRUE))

write_csv(pred.longfinmakoshark, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_longfinmakoshark.csv")

  #### All Threshers ####
  
  amsam$COUNT_THRESHER_SHARK <- amsam$COUNT_BIGEYE_THRESHER_SHARK + amsam$COUNT_PELAGIC_THRESHER_SHARK + amsam$COUNT_COMMON_THRESHER_SHARK + amsam$COUNT_UNID_THRESHER_SHARK
  
#Fixed effects
mod.amsam.allthresher1 <- glm.nb(COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                data=amsam) 
#Zero-inflated
mod.amsam.allthresher2 <- zeroinfl(COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
                                  offset = log_hooks,
                                  data=amsam, dist="negbin")
#Hurdle  
mod.amsam.allthresher3 <- hurdle(COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
                                offset = log_hooks,
                                data=amsam, dist="negbin")
#Mixed Model
mod.amsam.allthresher4 <- glmmTMB(data=amsam, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula =~0,
                                 family=nbinom2)

mod.amsam.allthresher4_2 <- glmmTMB(data=amsam, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~0,
                                   family=nbinom1)

#Zero Infl Mixed - 1 parameter
mod.amsam.allthresher5 <- glmmTMB(data=amsam, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                 ziformula =~1,
                                 family=nbinom2)

#mod.amsam.allthresher5_2 <- glmmTMB(data=amsam, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
#                                   ziformula =~1,
#                                   family=nbinom1)


#Zero Infl Mixed - Full Predictors Set
#mod.amsam.allthresher6 <- glmmTMB(data=amsam, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                            family=nbinom2)

mod.amsam.allthresher6_2 <- glmmTMB(data=amsam, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                              family=nbinom1)

#Hurdle model mixed
#mod.amsam.allthresher7 <- glmmTMB(data=amsam, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                                 ziformula = ~.,
#                                 family=truncated_nbinom2)

#mod.amsam.allthresher7_2 <- glmmTMB(data=amsam, COUNT_THRESHER_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
#                                  ziformula = ~.,
#                                  family=truncated_nbinom1)

#Residuals
amsam$allthresherResid1=resid(mod.amsam.allthresher1)
amsam$allthresherResid2=resid(mod.amsam.allthresher2)
amsam$allthresherResid3=resid(mod.amsam.allthresher3)
amsam$allthresherResid4=resid(mod.amsam.allthresher4, type="response")
amsam$allthresherResid4_2=residuals(mod.amsam.allthresher4_2, type="response")
amsam$allthresherResid5=residuals(mod.amsam.allthresher5, type="response")
#amsam$allthresherResid5_2=residuals(mod.amsam.allthresher5_2, type="response")
#amsam$allthresherResid6=residuals(mod.amsam.allthresher6, type="response")
amsam$allthresherResid6_2=resid(mod.amsam.allthresher6_2, type="response")
#amsam$allthresherResid7=residuals(mod.amsam.allthresher7, type="response")
#amsam$allthresherResid7_2=residuals(mod.amsam.allthresher7, type="response")

allthresher_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthresher_lati1 = as.numeric(dimnames(allthresher_Grid1)[[2]])
allthresher_long1 = as.numeric(dimnames(allthresher_Grid1)[[1]])

allthresher_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthresher_lati2 = as.numeric(dimnames(allthresher_Grid2)[[2]])
allthresher_long2 = as.numeric(dimnames(allthresher_Grid2)[[1]])

allthresher_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthresher_lati3 = as.numeric(dimnames(allthresher_Grid3)[[2]])
allthresher_long3 = as.numeric(dimnames(allthresher_Grid3)[[1]])

allthresher_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthresher_lati4 = as.numeric(dimnames(allthresher_Grid4)[[2]])
allthresher_long4 = as.numeric(dimnames(allthresher_Grid4)[[1]])

allthresher_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthresher_lati4_2 = as.numeric(dimnames(allthresher_Grid4_2)[[2]])
allthresher_long4_2 = as.numeric(dimnames(allthresher_Grid4_2)[[1]])

allthresher_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthresher_lati5 = as.numeric(dimnames(allthresher_Grid5)[[2]])
allthresher_long5 = as.numeric(dimnames(allthresher_Grid5)[[1]])

#allthresher_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#allthresher_lati5_2 = as.numeric(dimnames(allthresher_Grid5_2)[[2]])
#allthresher_long5_2 = as.numeric(dimnames(allthresher_Grid5_2)[[1]])

#allthresher_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
#allthresher_lati6 = as.numeric(dimnames(allthresher_Grid6)[[2]])
#allthresher_long6 = as.numeric(dimnames(allthresher_Grid6)[[1]])

allthresher_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
allthresher_lati6_2 = as.numeric(dimnames(allthresher_Grid6_2)[[2]])
allthresher_long6_2 = as.numeric(dimnames(allthresher_Grid6_2)[[1]])

#allthresher_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
#allthresher_lati7 = as.numeric(dimnames(allthresher_Grid7)[[2]])
#allthresher_long7 = as.numeric(dimnames(allthresher_Grid7)[[1]])

#allthresher_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allthresherResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
#allthresher_lati7_2 = as.numeric(dimnames(allthresher_Grid7_2)[[2]])
#allthresher_long7_2 = as.numeric(dimnames(allthresher_Grid7_2)[[1]])

# Z limits - coerce to max and min
zlim1 = range(allthresher_Grid1, na.rm = TRUE)
zlim2 = range(allthresher_Grid2, na.rm = TRUE)
zlim3 = range(allthresher_Grid3, na.rm = TRUE)
zlim4 = range(allthresher_Grid4, na.rm = TRUE)
zlim4_2 = range(allthresher_Grid4_2, na.rm = TRUE)
zlim5 = range(allthresher_Grid5, na.rm = TRUE)
#zlim5_2 = range(allthresher_Grid5_2, na.rm = TRUE)
#zlim6 = range(allthresher_Grid6, na.rm = TRUE)
zlim6_2 = range(allthresher_Grid6_2, na.rm = TRUE)
#lim7 = range(allthresher_Grid7, na.rm = TRUE)
#zlim7_2 = range(allthresher_Grid7_2, na.rm = TRUE)



#Zlimit colors
allthresher_zlim_fixed=c(-50,50)
allthresher_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

allthresher_zlim_mixed=c(-50,50)
allthresher_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)

allthresher_zlim_mixed2=c(-150,150)
allthresher_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -50, 0, 50, 150) / 10)

#Plots
image.plot(allthresher_long1, allthresher_lati1, allthresher_Grid1, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Threshers - Fixed Effects", 
           legend.lab="Residuals", zlim = allthresher_zlim_fixed, col=allthresher_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthresher_long2, allthresher_lati2, allthresher_Grid2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Threshers - Fixed Zero-Inflate", 
           legend.lab="Residuals", zlim = allthresher_zlim_fixed, col=allthresher_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthresher_long3, allthresher_lati3, allthresher_Grid3, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Threshers - Fixed Hurdle", 
           legend.lab="Residuals", zlim = allthresher_zlim_fixed, col=allthresher_colorTable_fixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthresher_long4, allthresher_lati4, allthresher_Grid4, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Threshers - Mixed Effects", 
           legend.lab="Residuals", zlim = allthresher_zlim_mixed, col=allthresher_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthresher_long4_2, allthresher_lati4_2, allthresher_Grid4_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Threshers - Mixed Effects", 
           legend.lab="Residuals", zlim = allthresher_zlim_mixed, col=allthresher_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthresher_long5, allthresher_lati5, allthresher_Grid5, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Threshers - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = allthresher_zlim_mixed, col=allthresher_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthresher_long5_2, allthresher_lati5_2, allthresher_Grid5_2, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Threshers - Mixed 1par ZeroInfl", 
           legend.lab="Residuals", zlim = allthresher_zlim_mixed, col=allthresher_colorTable_mixed)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(allthresher_long6, allthresher_lati6, allthresher_Grid6, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "All Threshers - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = allthresher_zlim_mixed, col=allthresher_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(allthresher_long6_2, allthresher_lati6_2, allthresher_Grid6_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "All Threshers - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = allthresher_zlim_mixed, col=allthresher_colorTable_mixed)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

image.plot(allthresher_long7, allthresher_lati7, allthresher_Grid7, las = 1, 
           ylab = "latitude", xlab = "longitude", main = "All Threshers - Mixed FullZeroInfl", 
           legend.lab="Residuals", zlim = allthresher_zlim_mixed2, col=allthresher_colorTable_mixed2)
maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#image.plot(allthresher_long7_2, allthresher_lati7_2, allthresher_Grid7_2, las = 1, 
#           ylab = "latitude", xlab = "longitude", main = "All Threshers - Mixed FullZeroInfl", 
#           legend.lab="Residuals", zlim = allthresher_zlim_mixed2, col=allthresher_colorTable_mixed2)
#maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")

#Model comparison - fixed effects
vuong(mod.amsam.allthresher1,mod.amsam.allthresher2)
vuong(mod.amsam.allthresher1,mod.amsam.allthresher3) #Fixed hurdle best

#Model comparison - all models
AICtab(mod.amsam.allthresher1,
       mod.amsam.allthresher2,
       mod.amsam.allthresher3,
       mod.amsam.allthresher4,
       mod.amsam.allthresher4_2,
       mod.amsam.allthresher5,
       #mod.amsam.allthresher5_2,
       #mod.amsam.allthresher6,
       mod.amsam.allthresher6_2,
       #mod.amsam.allthresher7,
       #mod.amsam.allthresher7_2,
       sort=T)

# residuals
amsam.allthresher6_2.resid<-simulateResiduals(mod.amsam.allthresher6_2, group=amsam$VESSEL_ID)
amsam.allthresher4.resid<-simulateResiduals(mod.amsam.allthresher4, group=amsam$VESSEL_ID)

plot(amsam.allthresher6_2.resid)
plot(amsam.allthresher4.resid)

plot(resid(mod.amsam.allthresher6_2), main="Mixed Effects ZI QP")  
plot(resid(mod.amsam.allthresher4), main="Mixed Effects")

plot(x = predict(mod.amsam.allthresher4, type="response"),
     y = resid(mod.amsam.allthresher4), main="Mixed Effects", 
     xlab = "Predicted Value")
plot(x = predict(mod.amsam.allthresher6_2, type="response"),
     y = resid(mod.amsam.allthresher6_2), main="Mixed Effect ZI QP", 
     xlab = "Predicted Value")

summary(residuals(mod.amsam.allthresher6_2))
summary(residuals(mod.amsam.allthresher4))

#Significance
summary(mod.amsam.allthresher6_2)
Anova(mod.amsam.allthresher6_2)

pred.allthresher <- data.frame(dummy.amsam, predicted=predict(mod.amsam.allthresher, newdata=dummy.amsam, type = "response", se.fit=TRUE))

write_csv(pred.allthresher, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_allthresher.csv")

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
  # DON'T USE - move to other sharks
  amsam$COUNT_HAMMER <- amsam$COUNT_UNID_HAMMERHEAD_SHARK + amsam$COUNT_SCALLOPED_HAMMERHEAD + amsam$COUNT_SMOOTH_HAMMERHEAD_SHARK
  
  #Fixed effects
  mod.amsam.allhammer1 <- glm.nb(COUNT_HAMMER ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                  data=amsam) 
  #Zero-inflated
  mod.amsam.allhammer2 <- zeroinfl(COUNT_HAMMER ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
                                    offset = log_hooks,
                                    data=amsam, dist="poisson")
  #Hurdle  
  mod.amsam.allhammer3 <- hurdle(COUNT_HAMMER ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
                                  offset = log_hooks,
                                  data=amsam, dist="negbin")
  #Mixed Model
  mod.amsam.allhammer4 <- glmmTMB(data=amsam, COUNT_HAMMER ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~0,
                                   family=nbinom2)
  
  mod.amsam.allhammer4_2 <- glmmTMB(data=amsam, COUNT_HAMMER ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                     ziformula =~0,
                                     family=nbinom1)
  
  #Zero Infl Mixed - 1 parameter
  mod.amsam.allhammer5 <- glmmTMB(data=amsam, COUNT_HAMMER ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~1,
                                   family=nbinom2)
  
  mod.amsam.allhammer5_2 <- glmmTMB(data=amsam, COUNT_HAMMER ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
                                     ziformula =~1,
                                     family=nbinom1)
  
  
  #Zero Infl Mixed - Full Predictors Set
  #mod.amsam.allhammer6 <- glmmTMB(data=amsam, COUNT_HAMMER ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                            family=nbinom2)
  
  #mod.amsam.allhammer6_2 <- glmmTMB(data=amsam, COUNT_HAMMER ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
  #                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                              family=nbinom1)
  
  #Hurdle model mixed
  mod.amsam.allhammer7 <- glmmTMB(data=amsam, COUNT_HAMMER ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula = ~.,
                                   family=truncated_nbinom2)
  
  #mod.amsam.allhammer7_2 <- glmmTMB(data=amsam, COUNT_HAMMER ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                                  ziformula = ~.,
  #                                  family=truncated_nbinom1)
  
  #Residuals
  amsam$allhammerResid1=resid(mod.amsam.allhammer1)
  amsam$allhammerResid2=resid(mod.amsam.allhammer2)
  amsam$allhammerResid3=resid(mod.amsam.allhammer3)
  amsam$allhammerResid4=resid(mod.amsam.allhammer4, type="response")
  amsam$allhammerResid4_2=residuals(mod.amsam.allhammer4_2, type="response")
  amsam$allhammerResid5=residuals(mod.amsam.allhammer5, type="response")
  amsam$allhammerResid5_2=residuals(mod.amsam.allhammer5_2, type="response")
  #amsam$allhammerResid6=residuals(mod.amsam.allhammer6, type="response")
  #amsam$allhammerResid6=resid(mod.amsam.allhammer6_2, type="response")
  amsam$allhammerResid7=residuals(mod.amsam.allhammer7, type="response")
  #amsam$allhammerResid7_2=residuals(mod.amsam.allhammer7, type="response")
  
  allhammer_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allhammer_lati1 = as.numeric(dimnames(allhammer_Grid1)[[2]])
  allhammer_long1 = as.numeric(dimnames(allhammer_Grid1)[[1]])
  
  allhammer_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allhammer_lati2 = as.numeric(dimnames(allhammer_Grid2)[[2]])
  allhammer_long2 = as.numeric(dimnames(allhammer_Grid2)[[1]])
  
  allhammer_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allhammer_lati3 = as.numeric(dimnames(allhammer_Grid3)[[2]])
  allhammer_long3 = as.numeric(dimnames(allhammer_Grid3)[[1]])
  
  allhammer_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allhammer_lati4 = as.numeric(dimnames(allhammer_Grid4)[[2]])
  allhammer_long4 = as.numeric(dimnames(allhammer_Grid4)[[1]])
  
  allhammer_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allhammer_lati4_2 = as.numeric(dimnames(allhammer_Grid4_2)[[2]])
  allhammer_long4_2 = as.numeric(dimnames(allhammer_Grid4_2)[[1]])
  
  allhammer_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allhammer_lati5 = as.numeric(dimnames(allhammer_Grid5)[[2]])
  allhammer_long5 = as.numeric(dimnames(allhammer_Grid5)[[1]])
  
  allhammer_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allhammer_lati5_2 = as.numeric(dimnames(allhammer_Grid5_2)[[2]])
  allhammer_long5_2 = as.numeric(dimnames(allhammer_Grid5_2)[[1]])
  
  #allhammer_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #allhammer_lati6 = as.numeric(dimnames(allhammer_Grid6)[[2]])
  #allhammer_long6 = as.numeric(dimnames(allhammer_Grid6)[[1]])
  
  #allhammer_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #allhammer_lati6_2 = as.numeric(dimnames(allhammer_Grid6_2)[[2]])
  #allhammer_long6_2 = as.numeric(dimnames(allhammer_Grid6_2)[[1]])
  
  allhammer_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allhammer_lati7 = as.numeric(dimnames(allhammer_Grid7)[[2]])
  allhammer_long7 = as.numeric(dimnames(allhammer_Grid7)[[1]])
  
  #allhammer_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allhammerResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #allhammer_lati7_2 = as.numeric(dimnames(allhammer_Grid7_2)[[2]])
  #allhammer_long7_2 = as.numeric(dimnames(allhammer_Grid7_2)[[1]])
  
  # Z limits - coerce to max and min
  zlim1 = range(allhammer_Grid1, na.rm = TRUE)
  zlim2 = range(allhammer_Grid2, na.rm = TRUE)
  zlim3 = range(allhammer_Grid3, na.rm = TRUE)
  zlim4 = range(allhammer_Grid4, na.rm = TRUE)
  zlim4_2 = range(allhammer_Grid4_2, na.rm = TRUE)
  zlim5 = range(allhammer_Grid5, na.rm = TRUE)
  zlim5_2 = range(allhammer_Grid5_2, na.rm = TRUE)
  #zlim6 = range(allhammer_Grid6, na.rm = TRUE)
  #zlim6_2 = range(allhammer_Grid6_2, na.rm = TRUE)
  zlim7 = range(allhammer_Grid7, na.rm = TRUE)
  #zlim7_2 = range(allhammer_Grid7_2, na.rm = TRUE)
  
  
  
  #Zlimit colors
  allhammer_zlim_fixed=c(-50,50)
  allhammer_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)
  
  allhammer_zlim_mixed=c(-50,50)
  allhammer_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)
  
  allhammer_zlim_mixed2=c(-150,150)
  allhammer_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -50, 0, 50, 150) / 10)
  
  #Plots
  image.plot(allhammer_long1, allhammer_lati1, allhammer_Grid1, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Hammers - Fixed Effects", 
             legend.lab="Residuals", zlim = allhammer_zlim_fixed, col=allhammer_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allhammer_long2, allhammer_lati2, allhammer_Grid2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Hammers - Fixed Zero-Inflate", 
             legend.lab="Residuals", zlim = allhammer_zlim_fixed, col=allhammer_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allhammer_long3, allhammer_lati3, allhammer_Grid3, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Hammers - Fixed Hurdle", 
             legend.lab="Residuals", zlim = allhammer_zlim_fixed, col=allhammer_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allhammer_long4, allhammer_lati4, allhammer_Grid4, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Hammers - Mixed Effects", 
             legend.lab="Residuals", zlim = allhammer_zlim_mixed, col=allhammer_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allhammer_long4_2, allhammer_lati4_2, allhammer_Grid4_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Hammers - Mixed Effects", 
             legend.lab="Residuals", zlim = allhammer_zlim_mixed, col=allhammer_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allhammer_long5, allhammer_lati5, allhammer_Grid5, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Hammers - Mixed 1par ZeroInfl", 
             legend.lab="Residuals", zlim = allhammer_zlim_mixed, col=allhammer_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allhammer_long5_2, allhammer_lati5_2, allhammer_Grid5_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Hammers - Mixed 1par ZeroInfl", 
             legend.lab="Residuals", zlim = allhammer_zlim_mixed, col=allhammer_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(allhammer_long6, allhammer_lati6, allhammer_Grid6, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "All Hammers - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = allhammer_zlim_mixed, col=allhammer_colorTable_mixed)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(allhammer_long6_2, allhammer_lati6_2, allhammer_Grid6_2, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "All Hammers - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = allhammer_zlim_mixed, col=allhammer_colorTable_mixed)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allhammer_long7, allhammer_lati7, allhammer_Grid7, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Hammers - Mixed FullZeroInfl", 
             legend.lab="Residuals", zlim = allhammer_zlim_mixed2, col=allhammer_colorTable_mixed2)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(allhammer_long7_2, allhammer_lati7_2, allhammer_Grid7_2, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "All Hammers - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = allhammer_zlim_mixed2, col=allhammer_colorTable_mixed2)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #Model comparison - fixed effects
  vuong(mod.amsam.allhammer1,mod.amsam.allhammer2)
  vuong(mod.amsam.allhammer1,mod.amsam.allhammer3) #Fixed hurdle best
  
  #Model comparison - all models
  AICtab(mod.amsam.allhammer1,
         mod.amsam.allhammer2,
         mod.amsam.allhammer3,
         mod.amsam.allhammer4,
         mod.amsam.allhammer4_2,
         mod.amsam.allhammer5,
         mod.amsam.allhammer5_2,
         #mod.amsam.allhammer6,
         #mod.amsam.allhammer6_2,
         mod.amsam.allhammer7,
         #mod.amsam.allhammer7_2,
         sort=T)
  
  library(DHARMa)
  amsam.allhammer7.resid<-simulateResiduals(mod.amsam.allhammer7, group=amsam$VESSEL_ID)
  amsam.allhammer4.resid<-simulateResiduals(mod.amsam.allhammer4, group=amsam$VESSEL_ID)
  
  plot(amsam.allhammer7.resid)
  plot(amsam.allhammer4.resid)
  
  plot(resid(mod.amsam.allhammer7), main="Mixed Effects Hurdle")  
  plot(resid(mod.amsam.allhammer4), main="Mixed Effects")
  
  plot(x = predict(mod.amsam.allhammer4, type="response"),
       y = resid(mod.amsam.allhammer4), main="Mixed Effects", 
       xlab = "Predicted Value")
  plot(x = predict(mod.amsam.allhammer7, type="response"),
       y = resid(mod.amsam.allhammer7), main="Mixed Effect Hurdle", 
       xlab = "Predicted Value")
  
  summary(residuals(mod.amsam.allhammer7))
  summary(residuals(mod.amsam.allhammer4))
  
  plot(x=predict(mod.amsam.allhammer1, type="response"),
       y=residuals(mod.amsam.allhammer3, type="pearson"),
       main="Fixed Effects") 
  plot(x=predict(mod.amsam.allhammer3, type="response"),
       y=residuals(mod.amsam.allhammer3, type="pearson"),
       main="Fixed Effects Hurdle") 
  plot(x=predict(mod.amsam.allhammer4, type="response"),
       y=residuals(mod.amsam.allhammer4, type="pearson"),
       main="Random Effect") 
  
  #Significance
  summary(mod.amsam.allhammer7)
  Anova(mod.amsam.allhammer7)
  
  pred.allhammer <- data.frame(dummy.amsam, predicted=predict(mod.amsam.allhammer7, newdata=dummy.amsam, type = "response", se.fit=TRUE))
  
  write_csv(pred.allhammer, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_allhammer.csv")
  #### Oceanic whitetip ####
  #Fixed effects
  mod.amsam.oceanicwhitetip1 <- glm.nb(COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                  data=amsam) 
  #Zero-inflated
  mod.amsam.oceanicwhitetip2 <- zeroinfl(COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
                                    offset = log_hooks,
                                    data=amsam, dist="poisson")
  #Hurdle  
  mod.amsam.oceanicwhitetip3 <- hurdle(COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
                                  offset = log_hooks,
                                  data=amsam, dist="negbin")
  #Mixed Model
  mod.amsam.oceanicwhitetip4 <- glmmTMB(data=amsam, COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~0,
                                   family=nbinom2)
  
  mod.amsam.oceanicwhitetip4_2 <- glmmTMB(data=amsam, COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                     ziformula =~0,
                                     family=nbinom1)
  
  #Zero Infl Mixed - 1 parameter
  mod.amsam.oceanicwhitetip5 <- glmmTMB(data=amsam, COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~1,
                                   family=nbinom2)
  
  mod.amsam.oceanicwhitetip5_2 <- glmmTMB(data=amsam, COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
                                     ziformula =~1,
                                     family=nbinom1)
  
  
  #Zero Infl Mixed - Full Predictors Set
  #mod.amsam.oceanicwhitetip6 <- glmmTMB(data=amsam, COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                            family=nbinom2)
  
  #mod.amsam.oceanicwhitetip6_2 <- glmmTMB(data=amsam, COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
  #                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                              family=nbinom1)
  
  #Hurdle model mixed
  mod.amsam.oceanicwhitetip7 <- glmmTMB(data=amsam, COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula = ~.,
                                   family=truncated_nbinom2)
  
  #mod.amsam.oceanicwhitetip7_2 <- glmmTMB(data=amsam, COUNT_OCEANIC_WHITETIP_SHARK ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                                  ziformula = ~.,
  #                                  family=truncated_nbinom1)
  
  #Residuals
  amsam$oceanicwhitetipResid1=resid(mod.amsam.oceanicwhitetip1)
  amsam$oceanicwhitetipResid2=resid(mod.amsam.oceanicwhitetip2)
  amsam$oceanicwhitetipResid3=resid(mod.amsam.oceanicwhitetip3)
  amsam$oceanicwhitetipResid4=resid(mod.amsam.oceanicwhitetip4, type="response")
  amsam$oceanicwhitetipResid4_2=residuals(mod.amsam.oceanicwhitetip4_2, type="response")
  amsam$oceanicwhitetipResid5=residuals(mod.amsam.oceanicwhitetip5, type="response")
  amsam$oceanicwhitetipResid5_2=residuals(mod.amsam.oceanicwhitetip5_2, type="response")
  #amsam$oceanicwhitetipResid6=residuals(mod.amsam.oceanicwhitetip6, type="response")
  #amsam$oceanicwhitetipResid6=resid(mod.amsam.oceanicwhitetip6_2, type="response")
  amsam$oceanicwhitetipResid7=residuals(mod.amsam.oceanicwhitetip7, type="response")
  #amsam$oceanicwhitetipResid7_2=residuals(mod.amsam.oceanicwhitetip7, type="response")
  
  oceanicwhitetip_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
  oceanicwhitetip_lati1 = as.numeric(dimnames(oceanicwhitetip_Grid1)[[2]])
  oceanicwhitetip_long1 = as.numeric(dimnames(oceanicwhitetip_Grid1)[[1]])
  
  oceanicwhitetip_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  oceanicwhitetip_lati2 = as.numeric(dimnames(oceanicwhitetip_Grid2)[[2]])
  oceanicwhitetip_long2 = as.numeric(dimnames(oceanicwhitetip_Grid2)[[1]])
  
  oceanicwhitetip_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
  oceanicwhitetip_lati3 = as.numeric(dimnames(oceanicwhitetip_Grid3)[[2]])
  oceanicwhitetip_long3 = as.numeric(dimnames(oceanicwhitetip_Grid3)[[1]])
  
  oceanicwhitetip_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
  oceanicwhitetip_lati4 = as.numeric(dimnames(oceanicwhitetip_Grid4)[[2]])
  oceanicwhitetip_long4 = as.numeric(dimnames(oceanicwhitetip_Grid4)[[1]])
  
  oceanicwhitetip_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  oceanicwhitetip_lati4_2 = as.numeric(dimnames(oceanicwhitetip_Grid4_2)[[2]])
  oceanicwhitetip_long4_2 = as.numeric(dimnames(oceanicwhitetip_Grid4_2)[[1]])
  
  oceanicwhitetip_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
  oceanicwhitetip_lati5 = as.numeric(dimnames(oceanicwhitetip_Grid5)[[2]])
  oceanicwhitetip_long5 = as.numeric(dimnames(oceanicwhitetip_Grid5)[[1]])
  
  oceanicwhitetip_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  oceanicwhitetip_lati5_2 = as.numeric(dimnames(oceanicwhitetip_Grid5_2)[[2]])
  oceanicwhitetip_long5_2 = as.numeric(dimnames(oceanicwhitetip_Grid5_2)[[1]])
  
  #oceanicwhitetip_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #oceanicwhitetip_lati6 = as.numeric(dimnames(oceanicwhitetip_Grid6)[[2]])
  #oceanicwhitetip_long6 = as.numeric(dimnames(oceanicwhitetip_Grid6)[[1]])
  
  #oceanicwhitetip_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #oceanicwhitetip_lati6_2 = as.numeric(dimnames(oceanicwhitetip_Grid6_2)[[2]])
  #oceanicwhitetip_long6_2 = as.numeric(dimnames(oceanicwhitetip_Grid6_2)[[1]])
  
  oceanicwhitetip_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
  oceanicwhitetip_lati7 = as.numeric(dimnames(oceanicwhitetip_Grid7)[[2]])
  oceanicwhitetip_long7 = as.numeric(dimnames(oceanicwhitetip_Grid7)[[1]])
  
  #oceanicwhitetip_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = oceanicwhitetipResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #oceanicwhitetip_lati7_2 = as.numeric(dimnames(oceanicwhitetip_Grid7_2)[[2]])
  #oceanicwhitetip_long7_2 = as.numeric(dimnames(oceanicwhitetip_Grid7_2)[[1]])
  
  # Z limits - coerce to max and min
  zlim1 = range(oceanicwhitetip_Grid1, na.rm = TRUE)
  zlim2 = range(oceanicwhitetip_Grid2, na.rm = TRUE)
  zlim3 = range(oceanicwhitetip_Grid3, na.rm = TRUE)
  zlim4 = range(oceanicwhitetip_Grid4, na.rm = TRUE)
  zlim4_2 = range(oceanicwhitetip_Grid4_2, na.rm = TRUE)
  zlim5 = range(oceanicwhitetip_Grid5, na.rm = TRUE)
  zlim5_2 = range(oceanicwhitetip_Grid5_2, na.rm = TRUE)
  #zlim6 = range(oceanicwhitetip_Grid6, na.rm = TRUE)
  #zlim6_2 = range(oceanicwhitetip_Grid6_2, na.rm = TRUE)
  zlim7 = range(oceanicwhitetip_Grid7, na.rm = TRUE)
  #zlim7_2 = range(oceanicwhitetip_Grid7_2, na.rm = TRUE)
  
  
  
  #Zlimit colors
  oceanicwhitetip_zlim_fixed=c(-50,50)
  oceanicwhitetip_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)
  
  oceanicwhitetip_zlim_mixed=c(-50,50)
  oceanicwhitetip_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)
  
  oceanicwhitetip_zlim_mixed2=c(-150,150)
  oceanicwhitetip_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -50, 0, 50, 150) / 10)
  
  #Plots
  image.plot(oceanicwhitetip_long1, oceanicwhitetip_lati1, oceanicwhitetip_Grid1, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Fixed Effects", 
             legend.lab="Residuals", zlim = oceanicwhitetip_zlim_fixed, col=oceanicwhitetip_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(oceanicwhitetip_long2, oceanicwhitetip_lati2, oceanicwhitetip_Grid2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Fixed Zero-Inflate", 
             legend.lab="Residuals", zlim = oceanicwhitetip_zlim_fixed, col=oceanicwhitetip_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(oceanicwhitetip_long3, oceanicwhitetip_lati3, oceanicwhitetip_Grid3, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Fixed Hurdle", 
             legend.lab="Residuals", zlim = oceanicwhitetip_zlim_fixed, col=oceanicwhitetip_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(oceanicwhitetip_long4, oceanicwhitetip_lati4, oceanicwhitetip_Grid4, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Mixed Effects", 
             legend.lab="Residuals", zlim = oceanicwhitetip_zlim_mixed, col=oceanicwhitetip_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(oceanicwhitetip_long4_2, oceanicwhitetip_lati4_2, oceanicwhitetip_Grid4_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Mixed Effects", 
             legend.lab="Residuals", zlim = oceanicwhitetip_zlim_mixed, col=oceanicwhitetip_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(oceanicwhitetip_long5, oceanicwhitetip_lati5, oceanicwhitetip_Grid5, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Mixed 1par ZeroInfl", 
             legend.lab="Residuals", zlim = oceanicwhitetip_zlim_mixed, col=oceanicwhitetip_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(oceanicwhitetip_long5_2, oceanicwhitetip_lati5_2, oceanicwhitetip_Grid5_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Mixed 1par ZeroInfl", 
             legend.lab="Residuals", zlim = oceanicwhitetip_zlim_mixed, col=oceanicwhitetip_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(oceanicwhitetip_long6, oceanicwhitetip_lati6, oceanicwhitetip_Grid6, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = oceanicwhitetip_zlim_mixed, col=oceanicwhitetip_colorTable_mixed)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(oceanicwhitetip_long6_2, oceanicwhitetip_lati6_2, oceanicwhitetip_Grid6_2, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = oceanicwhitetip_zlim_mixed, col=oceanicwhitetip_colorTable_mixed)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(oceanicwhitetip_long7, oceanicwhitetip_lati7, oceanicwhitetip_Grid7, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Mixed FullZeroInfl", 
             legend.lab="Residuals", zlim = oceanicwhitetip_zlim_mixed2, col=oceanicwhitetip_colorTable_mixed2)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(oceanicwhitetip_long7_2, oceanicwhitetip_lati7_2, oceanicwhitetip_Grid7_2, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "Oceanic Whitetip - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = oceanicwhitetip_zlim_mixed2, col=oceanicwhitetip_colorTable_mixed2)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #Model comparison - fixed effects
  vuong(mod.amsam.oceanicwhitetip1,mod.amsam.oceanicwhitetip2)
  vuong(mod.amsam.oceanicwhitetip1,mod.amsam.oceanicwhitetip3) #Fixed hurdle best
  
  #Model comparison - all models
  AICtab(mod.amsam.oceanicwhitetip1,
         mod.amsam.oceanicwhitetip2,
         mod.amsam.oceanicwhitetip3,
         mod.amsam.oceanicwhitetip4,
         mod.amsam.oceanicwhitetip4_2,
         mod.amsam.oceanicwhitetip5,
         mod.amsam.oceanicwhitetip5_2,
         #mod.amsam.oceanicwhitetip6,
         #mod.amsam.oceanicwhitetip6_2,
         mod.amsam.oceanicwhitetip7,
         #mod.amsam.oceanicwhitetip7_2,
         sort=T)
  
# residuals
  amsam.oceanicwhitetip4.resid<-simulateResiduals(mod.amsam.oceanicwhitetip4, group=amsam$VESSEL_ID)
  
  plot(amsam.oceanicwhitetip4.resid)
  
  plot(resid(mod.amsam.oceanicwhitetip7), main="Mixed Effects Hurdle")  
  plot(resid(mod.amsam.oceanicwhitetip4), main="Mixed Effects")
  
  plot(x = predict(mod.amsam.oceanicwhitetip4, type="response"),
       y = resid(mod.amsam.oceanicwhitetip4), main="Mixed Effects", 
       xlab = "Predicted Value")
  plot(x = predict(mod.amsam.oceanicwhitetip7, type="response"),
       y = resid(mod.amsam.oceanicwhitetip7), main="Mixed Effect Hurdle", 
       xlab = "Predicted Value")
  
  summary(residuals(mod.amsam.oceanicwhitetip7))
  summary(residuals(mod.amsam.oceanicwhitetip4))
  
  plot(x=predict(mod.amsam.oceanicwhitetip1, type="response"),
       y=residuals(mod.amsam.oceanicwhitetip3, type="pearson"),
       main="Fixed Effects") 
  plot(x=predict(mod.amsam.oceanicwhitetip3, type="response"),
       y=residuals(mod.amsam.oceanicwhitetip3, type="pearson"),
       main="Fixed Effects Hurdle") 
  plot(x=predict(mod.amsam.oceanicwhitetip4, type="response"),
       y=residuals(mod.amsam.oceanicwhitetip4, type="pearson"),
       main="Random Effect") 
  
  #Significance
  summary(mod.amsam.oceanicwhitetip4)
  Anova(mod.amsam.oceanicwhitetip4)
  
  pred.oceanicwhitetip <- data.frame(dummy.amsam, predicted=predict(mod.amsam.oceanicwhitetip4, newdata=dummy.amsam, type = "response", se.fit=TRUE))
  
  write_csv(pred.oceanicwhitetip, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_oceanicwhitetip.csv")
  #### All other sharks ####
  amsam$ALLOTHERSHARKS <- rowSums(amsam[,c(51,56:58,61:79)])
  #Fixed effects
  mod.amsam.allothersharks1 <- glm.nb(ALLOTHERSHARKS ~ lat + long + DEPART_YEAR + offset(log_hooks), 
                                  data=amsam) 
  #Zero-inflated
  #mod.amsam.allothersharks2 <- zeroinfl(ALLOTHERSHARKS ~ lat + long + DEPART_YEAR| lat + long + DEPART_YEAR, 
  #                                  offset = log_hooks,
  #                                  data=amsam, dist="negbin")
  #Hurdle  
  #mod.amsam.allothersharks3 <- hurdle(ALLOTHERSHARKS ~ lat + long + DEPART_YEAR | lat + long + DEPART_YEAR, 
  #                                offset = log_hooks,
  #                                data=amsam, dist="negbin")
  #Mixed Model
  mod.amsam.allothersharks4 <- glmmTMB(data=amsam, ALLOTHERSHARKS ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                   ziformula =~0,
                                   family=nbinom2)
  
  mod.amsam.allothersharks4_2 <- glmmTMB(data=amsam, ALLOTHERSHARKS ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
                                     ziformula =~0,
                                     family=nbinom1)
  
  #Zero Infl Mixed - 1 parameter
  #mod.amsam.allothersharks5 <- glmmTMB(data=amsam, ALLOTHERSHARKS ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                                 ziformula =~1,
  #                                 family=nbinom2)
  
  #mod.amsam.allothersharks5_2 <- glmmTMB(data=amsam, ALLOTHERSHARKS ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
  #                                   ziformula =~1,
  #                                   family=nbinom1)
  
  
  #Zero Infl Mixed - Full Predictors Set
  #mod.amsam.allothersharks6 <- glmmTMB(data=amsam, ALLOTHERSHARKS ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                            ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                            family=nbinom2)
  
  #mod.amsam.allothersharks6_2 <- glmmTMB(data=amsam, ALLOTHERSHARKS ~ lat + long + DEPART_YEAR  + (1|VESSEL_ID) + offset(log_hooks),
  #                              ziformula =~lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                              family=nbinom1)
  
  #Hurdle model mixed
  #mod.amsam.allothersharks7 <- glmmTMB(data=amsam, ALLOTHERSHARKS ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                                 ziformula = ~.,
  #                                 family=truncated_nbinom2)
  
  #mod.amsam.allothersharks7_2 <- glmmTMB(data=amsam, ALLOTHERSHARKS ~ lat + long + DEPART_YEAR + (1|VESSEL_ID) + offset(log_hooks),
  #                                  ziformula = ~.,
  #                                  family=truncated_nbinom1)
  
  #Residuals
  amsam$allothersharksResid1=resid(mod.amsam.allothersharks1)
  ##amsam$allothersharksResid2=resid(mod.amsam.allothersharks2)
  #amsam$allothersharksResid3=resid(mod.amsam.allothersharks3)
  amsam$allothersharksResid4=resid(mod.amsam.allothersharks4, type="response")
  amsam$allothersharksResid4_2=residuals(mod.amsam.allothersharks4_2, type="response")
  #amsam$allothersharksResid5=residuals(mod.amsam.allothersharks5, type="response")
  #amsam$allothersharksResid5_2=residuals(mod.amsam.allothersharks5_2, type="response")
  #amsam$allothersharksResid6=residuals(mod.amsam.allothersharks6, type="response")
  #amsam$allothersharksResid6=resid(mod.amsam.allothersharks6_2, type="response")
  #amsam$allothersharksResid7=residuals(mod.amsam.allothersharks7, type="response")
  #amsam$allothersharksResid7_2=residuals(mod.amsam.allothersharks7, type="response")
  
  allothersharks_Grid1 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid1, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allothersharks_lati1 = as.numeric(dimnames(allothersharks_Grid1)[[2]])
  allothersharks_long1 = as.numeric(dimnames(allothersharks_Grid1)[[1]])
  
  allothersharks_Grid2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allothersharks_lati2 = as.numeric(dimnames(allothersharks_Grid2)[[2]])
  allothersharks_long2 = as.numeric(dimnames(allothersharks_Grid2)[[1]])
  
  allothersharks_Grid3 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid3, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allothersharks_lati3 = as.numeric(dimnames(allothersharks_Grid3)[[2]])
  allothersharks_long3 = as.numeric(dimnames(allothersharks_Grid3)[[1]])
  
  allothersharks_Grid4 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid4, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allothersharks_lati4 = as.numeric(dimnames(allothersharks_Grid4)[[2]])
  allothersharks_long4 = as.numeric(dimnames(allothersharks_Grid4)[[1]])
  
  allothersharks_Grid4_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid4_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allothersharks_lati4_2 = as.numeric(dimnames(allothersharks_Grid4_2)[[2]])
  allothersharks_long4_2 = as.numeric(dimnames(allothersharks_Grid4_2)[[1]])
  
  allothersharks_Grid5 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid5, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allothersharks_lati5 = as.numeric(dimnames(allothersharks_Grid5)[[2]])
  allothersharks_long5 = as.numeric(dimnames(allothersharks_Grid5)[[1]])
  
  allothersharks_Grid5_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid5_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allothersharks_lati5_2 = as.numeric(dimnames(allothersharks_Grid5_2)[[2]])
  allothersharks_long5_2 = as.numeric(dimnames(allothersharks_Grid5_2)[[1]])
  
  #allothersharks_Grid6 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid6, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #allothersharks_lati6 = as.numeric(dimnames(allothersharks_Grid6)[[2]])
  #allothersharks_long6 = as.numeric(dimnames(allothersharks_Grid6)[[1]])
  
  #allothersharks_Grid6_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid6_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #allothersharks_lati6_2 = as.numeric(dimnames(allothersharks_Grid6_2)[[2]])
  #allothersharks_long6_2 = as.numeric(dimnames(allothersharks_Grid6_2)[[1]])
  
  allothersharks_Grid7 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid7, xlim=xlim,ylim =ylim, byx=1,byy=1))
  allothersharks_lati7 = as.numeric(dimnames(allothersharks_Grid7)[[2]])
  allothersharks_long7 = as.numeric(dimnames(allothersharks_Grid7)[[1]])
  
  #allothersharks_Grid7_2 = with(amsam,mapplots::make.grid(x = long, y = lat, z = allothersharksResid7_2, xlim=xlim,ylim =ylim, byx=1,byy=1))
  #allothersharks_lati7_2 = as.numeric(dimnames(allothersharks_Grid7_2)[[2]])
  #allothersharks_long7_2 = as.numeric(dimnames(allothersharks_Grid7_2)[[1]])
  
  # Z limits - coerce to max and min
  zlim1 = range(allothersharks_Grid1, na.rm = TRUE)
  zlim2 = range(allothersharks_Grid2, na.rm = TRUE)
  zlim3 = range(allothersharks_Grid3, na.rm = TRUE)
  zlim4 = range(allothersharks_Grid4, na.rm = TRUE)
  zlim4_2 = range(allothersharks_Grid4_2, na.rm = TRUE)
  zlim5 = range(allothersharks_Grid5, na.rm = TRUE)
  zlim5_2 = range(allothersharks_Grid5_2, na.rm = TRUE)
  #zlim6 = range(allothersharks_Grid6, na.rm = TRUE)
  #zlim6_2 = range(allothersharks_Grid6_2, na.rm = TRUE)
  zlim7 = range(allothersharks_Grid7, na.rm = TRUE)
  #zlim7_2 = range(allothersharks_Grid7_2, na.rm = TRUE)
  
  
  
  #Zlimit colors
  allothersharks_zlim_fixed=c(-50,50)
  allothersharks_colorTable_fixed<- designer.colors(500, c( "dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)
  
  allothersharks_zlim_mixed=c(-50,50)
  allothersharks_colorTable_mixed<- designer.colors(500, c("dark red","gray", "dark blue"), x = c(-50, 0, 50) / 10)
  
  allothersharks_zlim_mixed2=c(-150,150)
  allothersharks_colorTable_mixed2<- designer.colors(500, c("yellow", "dark red","gray", "dark blue", "darkgreen"), x = c(-150, -50, 0, 50, 150) / 10)
  
  #Plots
  image.plot(allothersharks_long1, allothersharks_lati1, allothersharks_Grid1, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Fixed Effects", 
             legend.lab="Residuals", zlim = allothersharks_zlim_fixed, col=allothersharks_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allothersharks_long2, allothersharks_lati2, allothersharks_Grid2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Fixed Zero-Inflate", 
             legend.lab="Residuals", zlim = allothersharks_zlim_fixed, col=allothersharks_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allothersharks_long3, allothersharks_lati3, allothersharks_Grid3, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Fixed Hurdle", 
             legend.lab="Residuals", zlim = allothersharks_zlim_fixed, col=allothersharks_colorTable_fixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allothersharks_long4, allothersharks_lati4, allothersharks_Grid4, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Mixed Effects", 
             legend.lab="Residuals", zlim = allothersharks_zlim_mixed, col=allothersharks_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allothersharks_long4_2, allothersharks_lati4_2, allothersharks_Grid4_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Mixed Effects", 
             legend.lab="Residuals", zlim = allothersharks_zlim_mixed, col=allothersharks_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allothersharks_long5, allothersharks_lati5, allothersharks_Grid5, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Mixed 1par ZeroInfl", 
             legend.lab="Residuals", zlim = allothersharks_zlim_mixed, col=allothersharks_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allothersharks_long5_2, allothersharks_lati5_2, allothersharks_Grid5_2, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Mixed 1par ZeroInfl", 
             legend.lab="Residuals", zlim = allothersharks_zlim_mixed, col=allothersharks_colorTable_mixed)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(allothersharks_long6, allothersharks_lati6, allothersharks_Grid6, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = allothersharks_zlim_mixed, col=allothersharks_colorTable_mixed)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(allothersharks_long6_2, allothersharks_lati6_2, allothersharks_Grid6_2, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = allothersharks_zlim_mixed, col=allothersharks_colorTable_mixed)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  image.plot(allothersharks_long7, allothersharks_lati7, allothersharks_Grid7, las = 1, 
             ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Mixed FullZeroInfl", 
             legend.lab="Residuals", zlim = allothersharks_zlim_mixed2, col=allothersharks_colorTable_mixed2)
  maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #image.plot(allothersharks_long7_2, allothersharks_lati7_2, allothersharks_Grid7_2, las = 1, 
  #           ylab = "latitude", xlab = "longitude", main = "All Other Sharks - Mixed FullZeroInfl", 
  #           legend.lab="Residuals", zlim = allothersharks_zlim_mixed2, col=allothersharks_colorTable_mixed2)
  #maps::map("world", xlim = xlim, ylim = ylim, add = TRUE, fill = TRUE, col="black")
  
  #Model comparison - fixed effects
  vuong(mod.amsam.allothersharks1,mod.amsam.allothersharks2)
  vuong(mod.amsam.allothersharks1,mod.amsam.allothersharks3) #Fixed hurdle best
  
  #Model comparison - all models
  AICtab(mod.amsam.allothersharks1,
         #mod.amsam.allothersharks2,
         #mod.amsam.allothersharks3,
         mod.amsam.allothersharks4,
         mod.amsam.allothersharks4_2,
         #mod.amsam.allothersharks5,
         #mod.amsam.allothersharks5_2,
         #mod.amsam.allothersharks6,
         #mod.amsam.allothersharks6_2,
         #mod.amsam.allothersharks7,
         #mod.amsam.allothersharks7_2,
         sort=T)
  
# residual plots
  amsam.allothersharks4.resid<-simulateResiduals(mod.amsam.allothersharks4, group=amsam$VESSEL_ID)
  

  plot(amsam.allothersharks4.resid)

  plot(resid(mod.amsam.allothersharks4), main="Mixed Effects")
  
  plot(x = predict(mod.amsam.allothersharks4, type="response"),
       y = resid(mod.amsam.allothersharks4), main="Mixed Effects", 
       xlab = "Predicted Value")

  summary(residuals(mod.amsam.allothersharks4))
  
  plot(x=predict(mod.amsam.allothersharks4, type="response"),
       y=residuals(mod.amsam.allothersharks4, type="pearson"),
       main="Random Effect") 
  
  #Significance
  summary(mod.amsam.allothersharks4)
  Anova(mod.amsam.allothersharks4)
  
  pred.allothersharks <- data.frame(dummy.amsam, predicted=predict(mod.amsam.allothersharks4, newdata=dummy.amsam, type = "response", se.fit=TRUE))
  
  write_csv(pred.allothersharks, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/pred_allothersharks.csv")
  
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
