library(tidyverse)
library(fields)
library(stringr)
library(mgcv)
library(MASS)
library(car)
library(gratia)

rm(list=ls())

setwd("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir")
data <- read.csv("WCPFC_bycatch.csv")

data <- subset(data, Species.Category == "SHK")
colnames(data)[1] = "Year"
colnames(data)[3] = "Lat"
colnames(data)[4] = "Long"
colnames(data)[6] = "Species"
colnames(data)[8] = "Captures"
colnames(data)[9] = "Nominal_CPUE"
colnames(data)[10] = "Mortalities"
colnames(data)[11] = "Morality Rate"
data$hooks=data$Captures/(data$Nominal_CPUE/1000)
data$log_hooks <- log(data$hooks)

data$Long2 <- ifelse(data$Long<0, 180 + (data$Long+180), data$Long)
species.list <- unique(data$Species)
species.list <- as.data.frame(species.list)

x.range <- range(data$Long2)
y.range <- range(data$Lat)
data.grd <- expand.grid("Lon"=seq(x.range[1]-2.5,x.range[2]+2.5,0.25),
                        "Lat"=seq(y.range[1]-2.5,y.range[2]+2.5,0.25))

captures <- data.frame("Lon"=data$Long2,
                       "Lat"=data$Lat,
                       "Captures"=data$Captures)
captures$Captures=1
t<-data.frame("Lon"=152.5,"Lat"=-12.5,"Captures"=1)
captures<-rbind(captures,t)

library(raster)
library(sf)
test.raster <- rasterFromXYZ(captures)
poly <- rasterToPolygons(test.raster, n=4, na.rm=T)
poly2 <- rgeos::gUnaryUnion(poly,id=poly$Captures)
poly3 <- st_as_sf(poly2)
plot(poly)
plot(poly2)
plot(poly3)

library(smoothr)
area_thresh <- units::set_units(1000, km^2)
finalpoly <- fill_holes(poly3, threshold=area_thresh)


plot(finalpoly, col='black')

ggplot(poly3) +
  geom_sf(color="red") +
  geom_tile(data=data, aes(x=Long2,y=Lat,fill=Captures)) +
  scale_y_continuous(breaks=seq(-50,50,5))  

data.grd <- st_as_sf(data.grd, coords = c("Lon","Lat"))

predsgrid <- data.grd %>% 
  sf::st_join(finalpoly, join = st_intersects, left=F) %>% # use a spatial join
  bind_cols(st_coordinates(.) %>% as.data.frame())

predsgrid2 <- fortify(predsgrid)
ggplot(predsgrid2, aes(x=X,y=Y)) +
  geom_point()
  


predsgrid<-as.data.frame(predsgrid)
preds.df <- predsgrid[,c(1:2)]
colnames(preds.df)[1] = "Long2"
colnames(preds.df)[2] = "Lat"
preds.df$Year = 2019
preds.df$log_hooks = log(1000)

WCPFC_thresher_data <- subset(data, Species=="BIGEYE THRESHER SHARK" | Species == "PELAGIC THRESHER SHARK" | Species=="THRESHER SHARKS NEI" | Species == "THRESHER SHARK (VULPINUS)")
WCPFC_blueshark_data <- subset(data, Species=="BLUE SHARK")
WCPFC_hammerhead_data <- subset(data, Species=="GREAT HAMMERHEAD" | Species=="HAMMERHEAD SHARKS NEI" |
                                  Species == "SCALLOPED HAMMERHEAD" | Species == "SMOOTH HAMMERHEAD")
WCPFC_mako_data <- subset(data, Species=="SHORTFIN MAKO" | Species=="LONGFIN MAKO" | Species =="MAKO SHARKS")
WCPFC_oceanicwhitetip_data <- subset(data, Species=="OCEANIC WHITETIP SHARK")
WCPFC_silky_data <- subset(data, Species=="SILKY SHARK")
WCPFC_othersharks_data <- subset(data, Species == "PORBEAGLE SHARK" | Species == "OTHERS" | Species == "WHALE SHARK")

world <- map_data("world2")

#### thresher ####
threshers <- aggregate(data=WCPFC_thresher_data, Captures ~ Lat + Long2 + Year, FUN='sum')
ths_hooks <- aggregate(data=WCPFC_thresher_data, hooks ~ Lat + Long2 + Year, FUN='mean')
threshers <- merge(threshers, ths_hooks, by=c("Lat", "Long2", "Year"), all=T)
threshers$log_hooks <- log(threshers$hooks)

mod.thresher.wcpfc <- gam(data=threshers, 
                                Captures ~ s(Long2) + s(Lat, k=13) + ti(Long2,Lat, k=9) + Year + 
                                  offset(log_hooks),
                                family=nb, method="ML")

plot(predict(mod.thresher.wcpfc)~mod.thresher.wcpfc$fitted.values)
gratia::draw(mod.thresher.wcpfc)
mod.thresher.wcpfc$sp

appraise(mod.thresher.wcpfc, method = "simulate")
gam.check(mod.thresher.wcpfc)
summary(mod.thresher.wcpfc)
anova(mod.thresher.wcpfc)

plot(x = predict(mod.thresher.wcpfc, type="response"),
     y = mod.thresher.wcpfc$residuals, 
     xlab = "Predicted Value")

acf(residuals(mod.thresher.wcpfc))

plot(mod.thresher.wcpfc, all.terms=F, residuals=T)

pred.df.thresher <- preds.df
preds_thresher = predict(mod.thresher.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_thresher_log = predict(mod.thresher.wcpfc, newdata=preds.df, se.fit=T)
pred.df.thresher$cpue <- preds_thresher$fit
pred.df.thresher$cpuefit <- preds_thresher$se.fit
pred.df.thresher$log_cpue <- preds_thresher_log$fit
pred.df.thresher$log_cpuefit <- preds_thresher_log$se.fit

tsh_cpue <- ggplot(pred.df.thresher, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, size=1)) +
  ggtitle(alopias_title)

tsh_cpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/tsh_cpue.png", tsh_cpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.thresher, "thresherCPUE.csv", row.names = F)
pred.df.thresher <- read.csv('thresherCPUE.csv')
#### Blue Sharks #### 
mod.blueshark.wcpfc <- gam(data=WCPFC_blueshark_data, 
                           Captures ~ s(Lat, k=16) + s(Long2) + ti(Long2,Lat, k=10) + Year + 
                             offset(log_hooks),
                           family=nb, method='ML')

plot(mod.blueshark.wcpfc)
gratia::draw(mod.blueshark.wcpfc)

appraise(mod.blueshark.wcpfc, method="simulate")
gam.check(mod.blueshark.wcpfc)
summary(mod.blueshark.wcpfc)
anova(mod.blueshark.wcpfc)

plot(x = predict(mod.blueshark.wcpfc, type="response"),
     y = mod.blueshark.wcpfc$residuals, 
     xlab = "Predicted Value")
abline(0,0,col='red')

acf(residuals(mod.blueshark.wcpfc))

plot(mod.blueshark.wcpfc, all.terms=F, residuals=T)

pred.df.blueshark <- preds.df
preds_blueshark = predict(mod.blueshark.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_blueshark_log = predict(mod.blueshark.wcpfc, newdata=preds.df, se.fit=T)
pred.df.blueshark$cpue <- preds_blueshark$fit
pred.df.blueshark$cpuefit <- preds_blueshark$se.fit
pred.df.blueshark$log_cpue <- preds_blueshark_log$fit
pred.df.blueshark$log_cpuefit <- preds_blueshark_log$se.fit

BSHcpue <- ggplot(pred.df.blueshark, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, size=1)) +
  ggtitle("Blue Shark")

BSHcpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/BSHcpue.png", BSHcpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.blueshark, "bluesharkCPUE.csv", row.names = F)
pred.df.blueshark <- read.csv("bluesharkCPUE.csv")
#### Hammerheads ####
hammers <- aggregate(data=WCPFC_hammerhead_data, Captures ~ Lat + Long2 + Year, FUN='sum')
sphyr_hooks <- aggregate(data=WCPFC_hammerhead_data, hooks ~ Lat + Long2 + Year, FUN='mean')
hammers <- merge(hammers, sphyr_hooks, by=c("Lat", "Long2", "Year"), all=T)
hammers$log_hooks <- log(hammers$hooks)


mod.hammerhead.wcpfc <- gam(data=hammers, 
                            Captures ~ s(Lat) +s(Long2) +ti(Long2,Lat, k=7) + Year + 
                              offset(log_hooks),
                            family=nb, method="ML")

draw(mod.hammerhead.wcpfc)
plot(mod.hammerhead.wcpfc)
appraise(mod.hammerhead.wcpfc, method="simulate")
gam.check(mod.hammerhead.wcpfc)

plot(x = predict(mod.hammerhead.wcpfc, type="response"),
     y = mod.hammerhead.wcpfc$residuals, 
     xlab = "Predicted Value")
abline(0,0,col='red')

summary(mod.hammerhead.wcpfc)
anova(mod.hammerhead.wcpfc)

acf(residuals(mod.hammerhead.wcpfc))

plot(mod.hammerhead.wcpfc, all.terms=F, residuals=T)

pred.df.hammerhead <- preds.df
preds_hammerhead = predict(mod.hammerhead.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_hammerhead_log = predict(mod.hammerhead.wcpfc, newdata=preds.df, se.fit=T)
pred.df.hammerhead$cpue <- preds_hammerhead$fit
pred.df.hammerhead$cpuefit <- preds_hammerhead$se.fit
pred.df.hammerhead$log_cpue <- preds_hammerhead_log$fit
pred.df.hammerhead$log_cpuefit <- preds_hammerhead_log$se.fit

hammercpue <- ggplot(pred.df.hammerhead, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, size=1))+
  ggtitle(sphyrnas_title)

hammercpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/hammercpue.png", hammercpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.hammerhead, "hammerheadCPUE.csv", row.names = F)
pred.df.hammerhead <- read.csv('hammerheadCPUE.csv')
#### Silky Shark ####
mod.silkyshark.wcpfc <- gam(data=WCPFC_silky_data, 
                            Captures ~ s(Lat, k=13) + s(Long2) +ti(Long2,Lat, k=10) + Year + 
                              offset(log_hooks),
                            family=nb, mnethod='ML')

draw(mod.silkyshark.wcpfc)
plot(mod.silkyshark.wcpfc)
appraise(mod.silkyshark.wcpfc, method='simulate')
gam.check(mod.silkyshark.wcpfc)

plot(x = predict(mod.silkyshark.wcpfc, type="response"),
     y = mod.silkyshark.wcpfc$residuals, 
     xlab = "Predicted Value")
abline(0,0,col='red')

summary(mod.silkyshark.wcpfc)
anova(mod.silkyshark.wcpfc)

acf(residuals(mod.silkyshark.wcpfc))

plot(mod.silkyshark.wcpfc, all.terms=F, residuals=T)

pred.df.silkyshark <- preds.df
preds_silkyshark = predict(mod.silkyshark.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_silkyshark_log = predict(mod.silkyshark.wcpfc, newdata=preds.df, se.fit=T)
pred.df.silkyshark$cpue <- preds_silkyshark$fit
pred.df.silkyshark$cpuefit <- preds_silkyshark$se.fit
pred.df.silkyshark$log_cpue <- preds_silkyshark_log$fit
pred.df.silkyshark$log_cpuefit <- preds_silkyshark_log$se.fit

FALcpue <- ggplot(pred.df.silkyshark, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, size=1)) +
  ggtitle("Silky Shark")

FALcpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/FALcpue.png", FALcpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.silkyshark, "silkysharkCPUE.csv", row.names = F)
pred.df.silkyshark <- read.csv('silkysharkCPUE.csv')
#### Mako #### 
makos <- aggregate(data=WCPFC_mako_data, Captures ~ Lat + Long2 + Year, FUN='sum')
mak_hooks <- aggregate(data=WCPFC_mako_data, hooks ~ Lat + Long2 + Year, FUN='mean')
makos <- merge(makos, mak_hooks, by=c("Lat", "Long2", "Year"), all=T)
makos$log_hooks <- log(makos$hooks)

mod.mako.wcpfc <- gam(data=makos, 
                              Captures ~ s(Lat, k=14) + s(Long2, k=10) + ti(Long2,Lat, k=9) + Year + 
                                offset(log_hooks),
                              family=nb)

gam.check(mod.mako.wcpfc)
draw(mod.mako.wcpfc)
appraise(mod.mako.wcpfc, method = 'simulate')
summary(mod.mako.wcpfc)

plot(mod.mako.wcpfc)

plot(x = predict(mod.mako.wcpfc, type="response"),
     y = mod.mako.wcpfc$residuals, 
     xlab = "Predicted Value")


anova(mod.mako.wcpfc)

acf(residuals(mod.mako.wcpfc))

plot(mod.mako.wcpfc, all.terms=F, residuals=T)

pred.df.mako <- preds.df
preds_mako = predict(mod.mako.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_mako_log = predict(mod.mako.wcpfc, newdata=preds.df, se.fit=T)
pred.df.mako$cpue <- preds_mako$fit
pred.df.mako$cpuefit <- preds_mako$se.fit
pred.df.mako$log_cpue <- preds_mako_log$fit
pred.df.mako$log_cpuefit <- preds_mako_log$se.fit

mak_cpue <- ggplot(pred.df.mako, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, size=1)) +
  ggtitle(isurus_title)

mak_cpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/mak_cpue.png", mak_cpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.mako, "makoCPUE.csv", row.names = F)
pred.df.mako <- read.csv('makoCPUE.csv')
#### Oceanic whitetip ####
mod.oceanicwhitetip.wcpfc <- gam(data=WCPFC_oceanicwhitetip_data, 
                                 Captures ~ s(Lat, k=13) + s(Long2) + ti(Long2,Lat, k=8) + Year + 
                                   offset(log_hooks),
                                 family=nb, method='ML')
gam.check(mod.oceanicwhitetip.wcpfc)
appraise(mod.oceanicwhitetip.wcpfc, method='simulate')
draw(mod.oceanicwhitetip.wcpfc)
summary(mod.oceanicwhitetip.wcpfc)

plot(mod.oceanicwhitetip.wcpfc)

plot(x = predict(mod.oceanicwhitetip.wcpfc, type="response"),
     y = mod.oceanicwhitetip.wcpfc$residuals, 
     xlab = "Predicted Value")
abline(0,0,col='red')

summary(mod.oceanicwhitetip.wcpfc)
anova(mod.oceanicwhitetip.wcpfc)

acf(residuals(mod.oceanicwhitetip.wcpfc))

plot(mod.oceanicwhitetip.wcpfc, all.terms=F, residuals=T)

pred.df.oceanicwhitetip <- preds.df
preds_oceanicwhitetip = predict(mod.oceanicwhitetip.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_oceanicwhitetip_log = predict(mod.oceanicwhitetip.wcpfc, newdata=preds.df, se.fit=T)
pred.df.oceanicwhitetip$cpue <- preds_oceanicwhitetip$fit
pred.df.oceanicwhitetip$cpuefit <- preds_oceanicwhitetip$se.fit
pred.df.oceanicwhitetip$log_cpue <- preds_oceanicwhitetip_log$fit
pred.df.oceanicwhitetip$log_cpuefit <- preds_oceanicwhitetip_log$se.fit

OCScpue <- ggplot(pred.df.oceanicwhitetip, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, size=1)) +
  ggtitle("Oceanic Whitetip")

OCScpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/OCScpue.png", OCScpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.oceanicwhitetip, "oceanicwhitetipCPUE.csv", row.names = F)
pred.df.oceanicwhitetip <- read.csv('oceanicwhitetipCPUE.csv')
#### Other Sharks ####
others <- aggregate(data=WCPFC_othersharks_data, Captures ~ Lat + Long2 + Year, FUN='sum')
other_hooks <- aggregate(data=WCPFC_othersharks_data, hooks ~ Lat + Long2 + Year, FUN='mean')
others <- merge(others, other_hooks, by=c("Lat", "Long2", "Year"), all=T)
others$log_hooks <- log(others$hooks)

mod.othersharks.wcpfc <- gam(data=others, 
                             Captures ~ s(Lat, k=15) + s(Long2, k=11) + ti(Long2,Lat,k=9) + Year + 
                               offset(log_hooks),
                             family=nb, method='ML')
gam.check(mod.othersharks.wcpfc)
appraise(mod.othersharks.wcpfc, method='simulate')
draw(mod.othersharks.wcpfc)
summary(mod.othersharks.wcpfc)

plot(mod.othersharks.wcpfc)

plot(x = predict(mod.othersharks.wcpfc, type="response"),
     y = mod.othersharks.wcpfc$residuals, 
     xlab = "Predicted Value")
abline(0,0,col='red')

summary(mod.othersharks.wcpfc)
anova(mod.othersharks.wcpfc)

acf(residuals(mod.othersharks.wcpfc))

plot(mod.othersharks.wcpfc, all.terms=F, residuals=T)

pred.df.othersharks <- preds.df
preds_othersharks = predict(mod.othersharks.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_othersharks_log = predict(mod.othersharks.wcpfc, newdata=preds.df, se.fit=T)
pred.df.othersharks$cpue <- preds_othersharks$fit
pred.df.othersharks$cpuefit <- preds_othersharks$se.fit
pred.df.othersharks$log_cpue <- preds_othersharks_log$fit
pred.df.othersharks$log_cpuefit <- preds_othersharks_log$se.fit

othercpue <- ggplot(pred.df.othersharks, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, size=1))+
  ggtitle("Other Sharks")

othercpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/othercpue.png", othercpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.othersharks, "othersharksCPUE.csv", row.names = F)
pred.df.othersharks <- read.csv("othersharksCPUE.csv")
#### Combine data ####

WCPFC_CPUEs <- merge(pred.df.blueshark, pred.df.silkyshark, by=c("Year", "Lat", "Long2", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[5]<- "blueshark.cpue"
colnames(WCPFC_CPUEs)[6]<- "blueshark.cpue.se"
colnames(WCPFC_CPUEs)[7]<- "blueshark.logcpue"
colnames(WCPFC_CPUEs)[8]<- "blueshark.logcpue.se"
colnames(WCPFC_CPUEs)[9]<- "silkyshark.cpue"
colnames(WCPFC_CPUEs)[10]<- "silkyshark.cpue.se"
colnames(WCPFC_CPUEs)[11]<- "silkyshark.logcpue"
colnames(WCPFC_CPUEs)[12]<- "silkyshark.logcpue.se"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.df.mako, by=c("Year", "Lat", "Long2", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[13]<- "mako.cpue"
colnames(WCPFC_CPUEs)[14]<- "mako.cpue.se"
colnames(WCPFC_CPUEs)[15]<- "mako.logcpue"
colnames(WCPFC_CPUEs)[16]<- "mako.logcpue.se"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.df.thresher, by=c("Year", "Lat", "Long2", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[17]<- "thresher.cpue"
colnames(WCPFC_CPUEs)[18]<- "thresher.cpue.se"
colnames(WCPFC_CPUEs)[19]<- "thresher.logcpue"
colnames(WCPFC_CPUEs)[20]<- "thresher.logcpue.se"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.df.oceanicwhitetip, by=c("Year", "Lat", "Long2", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[21]<- "oceanicwhitetip.cpue"
colnames(WCPFC_CPUEs)[22]<- "oceanicwhitetip.cpue.se"
colnames(WCPFC_CPUEs)[23]<- "oceanicwhitetip.logcpue"
colnames(WCPFC_CPUEs)[24]<- "oceanicwhitetip.logcpue.se"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.df.hammerhead, by=c("Year", "Lat", "Long2", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[25]<- "hammerhead.cpue"
colnames(WCPFC_CPUEs)[26]<- "hammerhead.cpue.se"
colnames(WCPFC_CPUEs)[27]<- "hammerhead.logcpue"
colnames(WCPFC_CPUEs)[28]<- "hammerhead.logcpue.se"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.df.othersharks, by=c("Year", "Lat", "Long2", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[29]<- "othersharks.cpue"
colnames(WCPFC_CPUEs)[30]<- "othersharks.cpue.se"
colnames(WCPFC_CPUEs)[31]<- "othersharks.logcpue"
colnames(WCPFC_CPUEs)[32]<- "othersharks.logcpue.se"

write.csv(WCPFC_CPUEs, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/WCPFC CPUEs.csv", row.names = F)

#Figure for supplementals ####

library(ggpubr)

FigS2 <- ggarrange(BSHcpue,FALcpue,tsh_cpue,mak_cpue,OCScpue,hammercpue,othercpue,
                      ncol=4, nrow=2, common.legend = T, legend = 'bottom') 

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/FigS2.png", FigS2,
       width=10, height=5, dpi=300, bg='white')
