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

#titles for genera
alopias_title <- expression(paste(italic("Alopias")," spp."))
isurus_title <- expression(paste(italic("Isurus")," spp."))
sphyrnas_title <- expression(paste(italic("Sphyrnas")," spp."))

data <- subset(data, Species.Category == "SHK")
colnames(data)[1] = "Year"
colnames(data)[3] = "Lat"
colnames(data)[4] = "Long"
colnames(data)[6] = "Species"
colnames(data)[8] = "Captures"
colnames(data)[9] = "Nominal_CPUE"
colnames(data)[10] = "Mortalities"
colnames(data)[11] = "Mortality Rate"
data$hooks=round(data$Captures/(data$Nominal_CPUE/1000))
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

fullgrid<-aggregate(data, hooks~Lat+Long2+Year, FUN=mean)
fullgrid$log_hooks <- log(fullgrid$hooks)

#### thresher ####
threshers <- aggregate(data=WCPFC_thresher_data, Captures ~ Lat + Long2 + Year, FUN='sum')
threshers <- merge(fullgrid,threshers,by=c("Lat","Long2","Year"), all=T)
threshers$Captures <- ifelse(is.na(threshers$Captures),0, threshers$Captures)

mod.thresher.wcpfc <- gam(data=threshers, 
                                Captures ~ s(Long2) + s(Lat, k=13) + ti(Long2,Lat, k=9) + Year + 
                                  offset(log_hooks),
                                family=nb, method="REML")

thresher_diag <- appraise(mod.thresher.wcpfc, method = "simulate")
thresher_diag
draw(mod.thresher.wcpfc)
gam.check(mod.thresher.wcpfc)
mod.thresher.wcpfc$sp

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
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1)) +
  ggtitle(alopias_title)

tsh_cpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/tsh_cpue.png", tsh_cpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.thresher, "thresherCPUE.csv", row.names = F)
pred.df.thresher <- read.csv('thresherCPUE.csv')
#### Blue Sharks #### 
blueshark <- WCPFC_blueshark_data[,c("Lat","Long2","Year","Captures")]
blueshark <- merge(fullgrid,blueshark,by=c("Lat","Long2","Year"), all=T)
blueshark$Captures <- ifelse(is.na(blueshark$Captures),0, blueshark$Captures)


mod.blueshark.wcpfc <- gam(data=blueshark, 
                           Captures ~ s(Lat, k=14) + s(Long2) + ti(Long2,Lat, k=8) + Year + 
                             offset(log_hooks),
                           family=nb, method='REML')

bsh_diag <- appraise(mod.blueshark.wcpfc, method = "simulate")
bsh_diag 
draw(mod.blueshark.wcpfc)
gam.check(mod.blueshark.wcpfc)
mod.blueshark.wcpfc$sp

summary(mod.blueshark.wcpfc)
anova(mod.blueshark.wcpfc)

plot(x = predict(mod.blueshark.wcpfc, type="response"),
     y = mod.blueshark.wcpfc$residuals, 
     xlab = "Predicted Value")

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
hammers <- merge(fullgrid,hammers,by=c("Lat","Long2","Year"), all=T)
hammers$Captures <- ifelse(is.na(hammers$Captures),0, hammers$Captures)

mod.hammer.wcpfc <- gam(data=hammers, 
                          Captures ~ s(Long2) + s(Lat, k=13) + ti(Long2,Lat, k=10) + Year + 
                            offset(log_hooks),
                          family=nb, method="REML")

hammer_diag <- appraise(mod.hammer.wcpfc, method = "simulate")
hammer_diag
draw(mod.hammer.wcpfc)
gam.check(mod.hammer.wcpfc)
mod.hammer.wcpfc$sp

summary(mod.hammer.wcpfc)
anova(mod.hammer.wcpfc)

plot(x = predict(mod.hammer.wcpfc, type="response"),
     y = mod.hammer.wcpfc$residuals, 
     xlab = "Predicted Value")

acf(residuals(mod.hammer.wcpfc))
plot(mod.hammer.wcpfc, all.terms=F, residuals=T)

pred.df.hammer <- preds.df
preds_hammer = predict(mod.hammer.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_hammer_log = predict(mod.hammer.wcpfc, newdata=preds.df, se.fit=T)
pred.df.hammer$cpue <- preds_hammer$fit
pred.df.hammer$cpuefit <- preds_hammer$se.fit
pred.df.hammer$log_cpue <- preds_hammer_log$fit
pred.df.hammer$log_cpuefit <- preds_hammer_log$se.fit

hammer_cpue <- ggplot(pred.df.hammer, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1)) +
  ggtitle(sphyrnas_title)

hammer_cpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/hammer_cpue.png", hammer_cpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.hammer, "hammerCPUE.csv", row.names = F)
pred.df.hammer <- read.csv('hammerCPUE.csv')

#### Silky Shark ####
silkyshark <- WCPFC_silky_data[,c("Lat","Long2","Year","Captures")]
silkyshark <- merge(fullgrid,silkyshark,by=c("Lat","Long2","Year"), all=T)
silkyshark$Captures <- ifelse(is.na(silkyshark$Captures),0, silkyshark$Captures)

mod.silkyshark.wcpfc <- gam(data=silkyshark, 
                           Captures ~ s(Lat, k=17) + s(Long2, k=11) + ti(Long2,Lat, k=12) + Year + 
                             offset(log_hooks),
                           family=nb, method='REML')

silky_diag <- appraise(mod.silkyshark.wcpfc, method = "simulate")
silky_diag 
draw(mod.silkyshark.wcpfc)
gam.check(mod.silkyshark.wcpfc)
mod.silkyshark.wcpfc$sp

summary(mod.silkyshark.wcpfc)
anova(mod.silkyshark.wcpfc)

plot(x = predict(mod.silkyshark.wcpfc, type="response"),
     y = mod.silkyshark.wcpfc$residuals, 
     xlab = "Predicted Value")

acf(residuals(mod.silkyshark.wcpfc))
plot(mod.silkyshark.wcpfc, all.terms=F, residuals=T)

pred.df.silkyshark <- preds.df
preds_silkyshark = predict(mod.silkyshark.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_silkyshark_log = predict(mod.silkyshark.wcpfc, newdata=preds.df, se.fit=T)
pred.df.silkyshark$cpue <- preds_silkyshark$fit
pred.df.silkyshark$cpuefit <- preds_silkyshark$se.fit
pred.df.silkyshark$log_cpue <- preds_silkyshark_log$fit
pred.df.silkyshark$log_cpuefit <- preds_silkyshark_log$se.fit

silkycpue <- ggplot(pred.df.silkyshark, aes(x=Long2,y=Lat)) +
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

silkycpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/silkycpue.png", silkycpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.silkyshark, "silkysharkCPUE.csv", row.names = F)
pred.df.silkyshark <- read.csv("silkysharkCPUE.csv")
#### Mako #### 
makos <- aggregate(data=WCPFC_mako_data, Captures ~ Lat + Long2 + Year, FUN='sum')
makos <- merge(fullgrid,makos,by=c("Lat","Long2","Year"), all=T)
makos$Captures <- ifelse(is.na(makos$Captures),0, makos$Captures)

mod.mako.wcpfc <- gam(data=makos, 
                        Captures ~ s(Long2) + s(Lat, k=13) + ti(Long2,Lat, k=10) + Year + 
                          offset(log_hooks),
                        family=nb, method="REML")

mako_diag <- appraise(mod.mako.wcpfc, method = "simulate")
mako_diag
draw(mod.mako.wcpfc)
gam.check(mod.mako.wcpfc)
mod.mako.wcpfc$sp

summary(mod.mako.wcpfc)
anova(mod.mako.wcpfc)

plot(x = predict(mod.mako.wcpfc, type="response"),
     y = mod.mako.wcpfc$residuals, 
     xlab = "Predicted Value")

acf(residuals(mod.mako.wcpfc))
plot(mod.mako.wcpfc, all.terms=F, residuals=T)

pred.df.mako <- preds.df
preds_mako = predict(mod.mako.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_mako_log = predict(mod.mako.wcpfc, newdata=preds.df, se.fit=T)
pred.df.mako$cpue <- preds_mako$fit
pred.df.mako$cpuefit <- preds_mako$se.fit
pred.df.mako$log_cpue <- preds_mako_log$fit
pred.df.mako$log_cpuefit <- preds_mako_log$se.fit

mako_cpue <- ggplot(pred.df.mako, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1)) +
  ggtitle(isurus_title)

mako_cpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/mako_cpue.png", mako_cpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.mako, "makoCPUE.csv", row.names = F)
pred.df.mako <- read.csv('makoCPUE.csv')

#### Oceanic whitetip ####
oceanicwhitetip <- WCPFC_oceanicwhitetip_data[,c("Lat","Long2","Year","Captures")]
oceanicwhitetip <- merge(fullgrid,oceanicwhitetip,by=c("Lat","Long2","Year"), all=T)
oceanicwhitetip$Captures <- ifelse(is.na(oceanicwhitetip$Captures),0, oceanicwhitetip$Captures)

mod.oceanicwhitetip.wcpfc <- gam(data=oceanicwhitetip, 
                            Captures ~ s(Lat, k=17) + s(Long2, k=11) + ti(Long2,Lat, k=12) + Year + 
                              offset(log_hooks),
                            family=nb, method='REML')

ocs_diag <- appraise(mod.oceanicwhitetip.wcpfc, method = "simulate")
ocs_diag
draw(mod.oceanicwhitetip.wcpfc)
gam.check(mod.oceanicwhitetip.wcpfc)
mod.oceanicwhitetip.wcpfc$sp

summary(mod.oceanicwhitetip.wcpfc)
anova(mod.oceanicwhitetip.wcpfc)

plot(x = predict(mod.oceanicwhitetip.wcpfc, type="response"),
     y = mod.oceanicwhitetip.wcpfc$residuals, 
     xlab = "Predicted Value")

acf(residuals(mod.oceanicwhitetip.wcpfc))
plot(mod.oceanicwhitetip.wcpfc, all.terms=F, residuals=T)

pred.df.oceanicwhitetip <- preds.df
preds_oceanicwhitetip = predict(mod.oceanicwhitetip.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_oceanicwhitetip_log = predict(mod.oceanicwhitetip.wcpfc, newdata=preds.df, se.fit=T)
pred.df.oceanicwhitetip$cpue <- preds_oceanicwhitetip$fit
pred.df.oceanicwhitetip$cpuefit <- preds_oceanicwhitetip$se.fit
pred.df.oceanicwhitetip$log_cpue <- preds_oceanicwhitetip_log$fit
pred.df.oceanicwhitetip$log_cpuefit <- preds_oceanicwhitetip_log$se.fit

oceanicwhitetipcpue <- ggplot(pred.df.oceanicwhitetip, aes(x=Long2,y=Lat)) +
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

oceanicwhitetipcpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/oceanicwhitetipcpue.png", oceanicwhitetipcpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.oceanicwhitetip, "oceanicwhitetipCPUE.csv", row.names = F)
pred.df.oceanicwhitetip <- read.csv("oceanicwhitetipCPUE.csv")
#### Other Sharks ####
others <- aggregate(data=WCPFC_othersharks_data, Captures ~ Lat + Long2 + Year, FUN='sum')
others <- merge(fullgrid,others,by=c("Lat","Long2","Year"), all=T)
others$Captures <- ifelse(is.na(others$Captures),0, others$Captures)

mod.other.wcpfc <- gam(data=others, 
                          Captures ~ s(Long2, k=12) + s(Lat, k=14) + ti(Long2,Lat, k=11) + Year + 
                            offset(log_hooks),
                          family=nb, method="REML")

other_diag <- appraise(mod.other.wcpfc, method = "simulate")
draw(mod.other.wcpfc)
gam.check(mod.other.wcpfc)
mod.other.wcpfc$sp

summary(mod.other.wcpfc)
anova(mod.other.wcpfc)

plot(x = predict(mod.other.wcpfc, type="response"),
     y = mod.other.wcpfc$residuals, 
     xlab = "Predicted Value")

acf(residuals(mod.other.wcpfc))
plot(mod.other.wcpfc, all.terms=F, residuals=T)

pred.df.other <- preds.df
preds_other = predict(mod.other.wcpfc, newdata=preds.df, type="response", se.fit=T)
preds_other_log = predict(mod.other.wcpfc, newdata=preds.df, se.fit=T)
pred.df.other$cpue <- preds_other$fit
pred.df.other$cpuefit <- preds_other$se.fit
pred.df.other$log_cpue <- preds_other_log$fit
pred.df.other$log_cpuefit <- preds_other_log$se.fit

other_cpue <- ggplot(pred.df.other, aes(x=Long2,y=Lat)) +
  geom_raster(aes(fill=log_cpue)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  geom_polygon(data = pyf_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  scale_fill_viridis(name="Log CPUE", alpha=0.85, option="H",limits=c(-10.5,4.5),breaks=c(seq(-10,4,2))) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1)) +
  ggtitle("Other Sharks")

other_cpue
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/other_cpue.png", other_cpue,
       width = 8, height = 6, dpi=300, bg="white")

write.csv(pred.df.other, "otherCPUE.csv", row.names = F)
pred.df.other <- read.csv('otherCPUE.csv')
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
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.df.hammer, by=c("Year", "Lat", "Long2", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[25]<- "hammerhead.cpue"
colnames(WCPFC_CPUEs)[26]<- "hammerhead.cpue.se"
colnames(WCPFC_CPUEs)[27]<- "hammerhead.logcpue"
colnames(WCPFC_CPUEs)[28]<- "hammerhead.logcpue.se"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.df.other, by=c("Year", "Lat", "Long2", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[29]<- "othersharks.cpue"
colnames(WCPFC_CPUEs)[30]<- "othersharks.cpue.se"
colnames(WCPFC_CPUEs)[31]<- "othersharks.logcpue"
colnames(WCPFC_CPUEs)[32]<- "othersharks.logcpue.se"

write.csv(WCPFC_CPUEs, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/WCPFC CPUEs.csv", row.names = F)

#Figure for supplementals ####

library(ggpubr)

FigS2 <- ggarrange(BSHcpue,silkycpue,tsh_cpue,mako_cpue,oceanicwhitetipcpue,hammer_cpue,other_cpue,
                      ncol=4, nrow=2, common.legend = T, legend = 'bottom') 

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/FigS2.png", FigS2,
       width=10, height=5, dpi=300, bg='white')


ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/bsh_diag.png", bsh_diag,
       width=10, height=6, dpi=300, bg='white')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/silky_diag.png", silky_diag,
       width=10, height=6, dpi=300, bg='white')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/thresher_diag.png", thresher_diag,
       width=10, height=6, dpi=300, bg='white')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/mako_diag.png", mako_diag,
       width=10, height=6, dpi=300, bg='white')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/ocs_diag.png", ocs_diag,
       width=10, height=6, dpi=300, bg='white')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/hammer_diag.png", hammer_diag,
       width=10, height=6, dpi=300, bg='white')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/other_diag.png", other_diag,
       width=10, height=6, dpi=300, bg='white')

#CPUE variation

world <- map_data("world2")

blueshark_resid<-ggplot(pred.df.blueshark) +
  geom_tile(aes(x=Long2,y=Lat, fill=log_cpuefit)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="CPUE Standard Deviation\n(Log Scale)", alpha=0.85) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1))+
  ggtitle("Blue Shark")

thresher_resid<-ggplot(pred.df.thresher) +
  geom_tile(aes(x=Long2,y=Lat, fill=log_cpuefit)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="CPUE Standard Deviation\n(Log Scale)", alpha=0.85) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1))+
  ggtitle("Thresher Shark")

hammer_resid<-ggplot(pred.df.hammer) +
  geom_tile(aes(x=Long2,y=Lat, fill=log_cpuefit)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="CPUE Standard Deviation\n(Log Scale)", alpha=0.85) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1))+
  ggtitle("Hammerhead Shark")

silkyshark_resid<-ggplot(pred.df.silkyshark) +
  geom_tile(aes(x=Long2,y=Lat, fill=log_cpuefit)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="CPUE Standard Deviation\n(Log Scale)", alpha=0.85) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1))+
  ggtitle("Silky Shark")

mako_resid<-ggplot(pred.df.mako) +
  geom_tile(aes(x=Long2,y=Lat, fill=log_cpuefit)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="CPUE Standard Deviation\n(Log Scale)", alpha=0.85) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1))+
  ggtitle("Mako Shark")

oceanicwhitetip_resid<-ggplot(pred.df.oceanicwhitetip) +
  geom_tile(aes(x=Long2,y=Lat, fill=log_cpuefit)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="CPUE Standard Deviation\n(Log Scale)", alpha=0.85) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1))+
  ggtitle("Oceanic Whitetip")

other_resid<-ggplot(pred.df.other) +
  geom_tile(aes(x=Long2,y=Lat, fill=log_cpuefit)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_y_continuous(name="Latitude", breaks=c(seq(-50, 50, 25))) +
  scale_x_continuous(name="Longitude", breaks=c(seq(130, 230, 25)), labels = c(130,155,180,-155,-130)) +
  coord_cartesian(xlim=c(122,233), ylim=c(-54,49), expand=F) +
  scale_fill_viridis(name="CPUE Standard Deviation\n(Log Scale)", alpha=0.85) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey93"),
        panel.border = element_rect(colour='black', fill=NA, linewidth = 1))+
  ggtitle("Other Sharks")

residualsCPUE <- ggarrange(blueshark_resid,silkyshark_resid,thresher_resid,mako_resid,
                           oceanicwhitetip_resid,hammer_resid,other_resid,
                           nrow=2, ncol=4, common.legend = T, legend='bottom')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/CPUEresid.png", residualsCPUE,
       width=10, height=6, dpi=300, bg='white')

