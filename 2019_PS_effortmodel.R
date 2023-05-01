library(tidyverse)
library(mgcv)
library(gratia)
library(countrycode)

rm(list=ls())

WD <- '~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir' #primary WD
setwd(WD) #Set WD
PSdat <- read.csv("purseseine_raw.csv") #RFMO effort data from WCPFC
head(PSdat)

#Change lat/long to numeric
PSdat$Lat_text <- gsub("[[:digit:]]", "", PSdat$lat_short)
PSdat$Lat <- as.numeric(gsub("[^0-9.-]", "", PSdat$lat_short))
PSdat$Lat <- ifelse(PSdat$Lat_text == "S", PSdat$Lat*-1, PSdat$Lat)
PSdat$Lon_text <- gsub("[[:digit:]]", "", PSdat$lon_short)
PSdat$Lon <- as.numeric(gsub("[^0-9.-]", "", PSdat$lon_short))
PSdat$Lon <- ifelse(PSdat$Lon_text == "W", PSdat$Lon*-1, PSdat$Lon)
range(PSdat$Lat)

colnames(PSdat)
colnames(PSdat)[3] <- "flag"

PSdat <- subset(PSdat, yy == 2019)

# convert the two letter codes to three letters
PSdat$flag <- countrycode(PSdat$flag, origin="iso2c", destination = "iso3c", nomatch = NULL)

#Add sets and simplify
PSdat$sets <- PSdat$sets_afad + PSdat$sets_dfad + PSdat$sets_log + PSdat$sets_oth + PSdat$sets_una
PSdat <- PSdat[,c("Lat","Lon","flag", "sets")]

#plot coverage vs EEZs
PSdat_plot <- PSdat
PSdat_plot$maplon <- ifelse(PSdat_plot$Lon < 0, PSdat_plot$Lon + 360, PSdat_plot$Lon)


#antimeridian center world map
world <- map_data("world2")

WCPFC_2019_PS <- ggplot(PSdat_plot) +
  geom_tile(aes(x=maplon, y=Lat, fill=sets)) +
  geom_polygon(data = fsm_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = pyf_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = cok_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = ncl_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = plw_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = wsm_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = mhl_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = kir_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  coord_cartesian(xlim=c(120,260), ylim=c(-50,50))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/WCPFC_2019_PS.png", 
       WCPFC_2019_PS, width = 12, height = 8, dpi=300)

#### AIS data
PS_AIS <- read.csv("PS_AIS_1x1_2019.csv")
colnames(PS_AIS)

PS_AIS$log_hours <- log(PS_AIS$fishing_hours)
PS_AIS$log_hours <- ifelse(PS_AIS$log_hours == -Inf, NA, PS_AIS$log_hours)
PS_AIS.nona <- PS_AIS %>% drop_na("log_hours")


PS_AIS_2019 <- ggplot(data=PS_AIS.nona) +
  geom_tile(aes(x=ifelse(Lon<0, Lon+360, Lon), y=Lat, fill=log(fishing_hours)), alpha=0.4) +
  geom_polygon(data = fsm_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = pyf_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = cok_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = ncl_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = plw_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = wsm_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = mhl_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = kir_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  coord_cartesian(xlim=c(120,260), ylim=c(-50,50))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/PS_AIS_2019.png", 
       PS_AIS_2019, width = 12, height = 8, dpi=300)


totPS = merge(PS_AIS,PSdat, by=c("Lon","Lat","flag"), all=TRUE)

head(totPS)



#data clean
totPS = na.omit(totPS[,c("Lon","Lat","flag","fishing_hours","sets")])

#plot
plot(log(sets)~log(fishing_hours), totPS)
plot(sets~fishing_hours, totPS)

colnames(totPS)[4] <- "hours"
#data clean
totPS$flag = factor(totPS$flag)
totPS = totPS[totPS$sets!=0,]
totPS = totPS[totPS$hours!=0,]

totPS$Lon2 <- ifelse(totPS$Lon < 0, totPS$Lon + 360, totPS$Lon)

# GAMM model ####
#write model
gamm1 = gam(log(sets)~log(hours)+s(Lat)+s(Lon2)+s(flag, bs = "re"), data = totPS, method='REML') 

#check spline wiggliness
gam.check(gamm1)

#simulate residuals and check fit
appraise(gamm1, method='simulate')

#plot
draw(gamm1)

#summarize
summary(gamm1)

#save it
saveRDS(gamm1, "PS_gamm_2019.rds")

#### Checking strength of random effects at given point in space ####
newdata = data.frame(hours = 100, flag = unique(totPS$flag), Lat = 0, Lon2 = 170)
fit <- predict(gamm1, newdata = newdata, se.fit = TRUE)
newdata$pred = fit$fit
newdata$se.fit = fit$se.fit 
newdata = with(newdata,data.frame(newdata, lwr=pred-1.96*se.fit,upr=pred+1.96*se.fit))
newdata$pred.sets<-exp(newdata$pred)
newdata$pupr<-exp(newdata$upr)
newdata$plwr<-exp(newdata$lwr)
newdata$flag = countrycode(newdata$flag, origin="iso2c", destination = "iso3c", nomatch = NULL)
newdata = newdata[order(newdata$pred.sets),]
pos = 1:nrow(newdata)
with(newdata,plot(pred.sets,pos, pch=16, xlim = c(0, max(pupr)), axes = F, ylab = "", xlab = "Predicted #sets per 100 AIS hours"))
with(newdata,segments(plwr,pos,pupr,pos))
axis(1)
axis(2,at = pos, labels = as.character(newdata$flag), las = 1)

#residuals in space
tot3 <- totPS
tot3$Resid=resid(gamm1)

world<-world <- map_data("world2")

residuals_PS_2019 <- ggplot(tot3) +
  stat_summary_2d(aes(x = Lon2+0.5, y = Lat+0.5, z = Resid), alpha = 0.6, binwidth=1) + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_fill_viridis(name="Residuals", option='D') +
  coord_cartesian(xlim=c(120,240), ylim=c(-25,25)) +
  geom_polygon(data = fsm_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = pyf_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = cok_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = ncl_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = plw_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = wsm_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = mhl_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_polygon(data = kir_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/residuals_PS_2019.png", 
       residuals_PS_2019, width = 12, height = 8, dpi=300)
