library(tidyverse)
library(raster)
library(sf)
library(smoothr)
library(RColorBrewer)

#Stock delineations
#Here I am taking the raw catch data from the WCPFC, at 5 degree res
#And will find every cell where a species HAS occurred, to delineate stock boudnaries

#data
setwd("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir")
dat <- read.csv("WCPFC_raw.csv")
dat$lat <- dat$lat_short
dat$lon <- dat$lon_short
dat <- dat %>%
  mutate_at(vars(lat, lon),
            funs(as.numeric(gsub("[NE]$", "", gsub("^(.*)[WS]$", "-\\1", .)))))
dat$maplon <- ifelse(dat$lon < 0, dat$lon + 360, dat$lon)
dat$lat=dat$lat+2.5
dat$maplon=dat$maplon+2.5

world <- map_data("world2")

my_pal <- rcartocolor::carto_pal(n = 12, name = "Bold")[c(3,4,1,7)] 

#### Blue sharks ####
#data wrangle
blueshark <- aggregate(data=dat, bsh_n~lat+maplon, FUN='sum')
blueshark$bsh_n <- ifelse(blueshark$bsh_n == 0, NA, blueshark$bsh_n)
blueshark <- na.omit(blueshark)

#separate stocks stock
bsh_north <- subset(blueshark, lat >= 0)
bsh_south <- subset(blueshark, lat < 0)

#arrange so that long (x) is first, lat(y) is second, and catch (z) is third for 'rasterfromXYZ()', and create raster
bsh_north <-bsh_north [,c(2,1,3)] 
bsh.rasterN <- rasterFromXYZ(bsh_north )

area_thresh <- units::set_units(1000, km^2) #assign threshold to fill internal holes in polygon (empty pixels)

bsh_polyN <- bsh.rasterN %>% rasterToPolygons(n=4, na.rm=T) #raster to SpatialPolygonsDataFrame
bsh_polyN <- rgeos::gUnaryUnion(bsh_polyN, id=bsh_polyN$Captures) %>% #Join individual pixel polygons to one larger polygon
  st_as_sf() %>% #convert to spatial frame
  fill_holes(threshold=area_thresh) #fill internal holes

plot(bsh_polyN, col='black')

bsh_poly_dfN <- as(bsh_polyN, Class="Spatial") #convert to SpatialPolygonsDataFrame for area measurement
bsh_areaN <- area(bsh_poly_dfN) / 1000000
sum(bsh_areaN) #38585660 


#### southern stock ####
#arrange so that long (x) is first, lat(y) is second, and catch (z) is third for 'rasterfromXYZ()', and create raster
bsh_south<-bsh_south[,c(2,1,3)] 
bsh.rasterS <- rasterFromXYZ(bsh_south)

bsh_polyS <- bsh.rasterS %>% rasterToPolygons(n=4, na.rm=T) #raster to SpatialPolygonsDataFrame
bsh_polyS <- rgeos::gUnaryUnion(bsh_polyS, id=bsh_polyS$Captures) %>% #Join individual pixel polygons to one larger polygon
  st_as_sf() %>% #convert to spatial frame
  fill_holes(threshold=area_thresh) #fill internal holes

plot(bsh_polyS, col='black')

bsh_poly_dfS <- as(bsh_polyS, Class="Spatial") #convert to SpatialPolygonsDataFrame for area measurement
bsh_areaS <- area(bsh_poly_dfS) / 1000000
sum(bsh_areaS) #36025997

#plot
bsh_stock_s <- ggplot()+
  geom_sf(data=bsh_polyS, color=my_pal[1], size=1, fill=my_pal[1], alpha=0.5)+
  #geom_tile(data=blueshark, aes(x=maplon,y=lat, fill=bsh_n), alpha=0.95) +
  scale_x_continuous(name="Longitude", limits=c(115,230), breaks=seq(120,220, by=20), labels=c(120,140,160,180,-160,-140)) +     
  scale_y_continuous(name="Latitude", limits=c(-55,45), breaks=seq(-40,40, by=20)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(color='black', fill=NA, size=0.5),
        axis.title = element_blank())+
  ggtitle("Blue Shark - South")

bsh_stock_s
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/bsh_stock_s.png", bsh_stock_s,
       width = 8, height = 6, dpi=300, bg="white")

bsh_stock_n <- ggplot()+
  geom_sf(data=bsh_polyN, color=my_pal[1], size=1, fill=my_pal[1], alpha=0.5)+
  #geom_tile(data=blueshark, aes(x=maplon,y=lat, fill=bsh_n), alpha=0.95) +
  scale_x_continuous(name="Longitude", limits=c(115,230), breaks=seq(120,220, by=20), labels=c(120,140,160,180,-160,-140)) +     
  scale_y_continuous(name="Latitude", limits=c(-55,45), breaks=seq(-40,40, by=20)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(color='black', fill=NA, size=0.5),
        axis.title = element_blank())+
  ggtitle("Blue Shark - North")

bsh_stock_n
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/bsh_stock_n.png", bsh_stock_n,
       width = 8, height = 6, dpi=300, bg="white")

sum(bsh_areaS) + sum(bsh_areaN) #74611657

#silky shark
silkyshark <- aggregate(data=dat, fal_n~lat+maplon, FUN='sum')
silkyshark$fal_n <- ifelse(silkyshark$fal_n == 0, NA, silkyshark$fal_n)
silkyshark <- na.omit(silkyshark)

#arrange so that long (x) is first, lat(y) is second, and catch (z) is third for 'rasterfromXYZ()', and create raster
silkyshark<-silkyshark[,c(2,1,3)] 
fal.raster <- rasterFromXYZ(silkyshark)

area_thresh <- units::set_units(1000, km^2) #assign threshold to fill internal holes in polygon (empty pixels)

fal_poly <- fal.raster %>% rasterToPolygons(n=4, na.rm=T) #raster to SpatialPolygonsDataFrame
fal_poly <- rgeos::gUnaryUnion(fal_poly, id=fal_poly$Captures) %>% #Join individual pixel polygons to one larger polygon
  st_as_sf() %>% #convert to spatial frame
  fill_holes(threshold=area_thresh) #fill internal holes

plot(fal_poly, col='black') 

#manually force cells that weren't auto-filled and repeat
correct_fal <- data.frame("maplon" = c(152.5,157.5,152.5),
                       "lat" = c(22.5,22.5,27.5),
                       "fal_n" = 100)

correct_fal<-rbind(correct_fal,silkyshark)
fal.raster <- rasterFromXYZ(correct_fal)
fal_poly <- fal.raster %>% rasterToPolygons(n=4, na.rm=T) #raster to SpatialPolygonsDataFrame
fal_poly <- rgeos::gUnaryUnion(fal_poly, id=fal_poly$Captures) %>% #Join individual pixel polygons to one larger polygon
  st_as_sf() %>% #convert to spatial frame
  fill_holes(threshold=area_thresh) #fill internal holes

plot(fal_poly, col='black') 

fal_poly_df <- as(fal_poly, Class="Spatial") #convert to SpatialPolygonsDataFrame for area measurement
fal_areas <- area(fal_poly_df) / 1000000
sum(fal_areas) #59,689,229

#plot
fal_stock <- ggplot(fal_poly)+
  geom_sf(color=my_pal[2], size=1.5, fill=my_pal[2], alpha=0.5)+
  #geom_tile(data=silkyshark, aes(x=maplon,y=lat, fill=fal_n), alpha=0.95) +
  scale_x_continuous(limits=c(115,230), breaks=seq(120,220, by=20), labels=c(120,140,160,180,-160,-140))+
  scale_y_continuous(limits=c(-55,45), breaks=seq(-40,40, by=20)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region))  +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(color='black', fill=NA, size=0.5),
        axis.title = element_blank())+
  ggtitle("Silky Shark")

fal_stock
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/fal_stock.png", fal_stock,
       width = 8, height = 6, dpi=300, bg="white")
#mak shark
mako <- aggregate(data=dat, mak_n~lat+maplon, FUN='sum')
mako$mak_n <- ifelse(mako$mak_n == 0, NA, mako$mak_n)
mako <- na.omit(mako)

mako <- subset(mako, lat >= 0)
#arrange so that long (x) is first, lat(y) is second, and catch (z) is third for 'rasterfromXYZ()', and create raster
mako<-mako[,c(2,1,3)] 
mak.raster <- raster::rasterFromXYZ(mako)

area_thresh <- units::set_units(1000, km^2) #assign threshold to fill internal holes in polygon (empty pixels)

mak_poly <- mak.raster %>% raster::rasterToPolygons(n=4, na.rm=T) #raster to SpatialPolygonsDataFrame
mak_poly <- rgeos::gUnaryUnion(mak_poly, id=mak_poly$Captures) %>% #Join individual pixel polygons to one larger polygon
  st_as_sf() %>% #convert to spatial frame
  smoothr::fill_holes(threshold=area_thresh) #fill internal holes

plot(mak_poly, col='black') 

mak_poly_df <- as(mak_poly, Class="Spatial") #convert to SpatialPolygonsDataFrame for area measurement
mak_areas <- raster::area(mak_poly_df) / 1000000
sum(mak_areas) #38027072

#plot
mak_stock <- ggplot(mak_poly)+
  geom_sf(color=my_pal[3], size=1.5, fill=my_pal[3], alpha=0.5)+
  #geom_tile(data=mako, aes(x=maplon,y=lat, fill=mak_n), alpha=0.95) +
  scale_x_continuous(limits=c(115,230), breaks=seq(120,220, by=20), labels=c(120,140,160,180,-160,-140))+
  scale_y_continuous(limits=c(-55,45), breaks=seq(-40,40, by=20)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(color='black', fill=NA, size=0.5),
        axis.title = element_blank()) +
  ggtitle(isurus_title)

mak_stock
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/mak_stock.png", mak_stock,
       width = 8, height = 6, dpi=300, bg="white")
#ocs shark
ocs <- aggregate(data=dat, ocs_n~lat+maplon, FUN='sum')
ocs$ocs_n <- ifelse(ocs$ocs_n == 0, NA, ocs$ocs_n)
ocs <- na.omit(ocs)

#arrange so that long (x) is first, lat(y) is second, and catch (z) is third for 'rasterfromXYZ()', and create raster
ocs<-ocs[,c(2,1,3)] 
ocs.raster <- rasterFromXYZ(ocs)

ocs_poly <- ocs.raster %>% rasterToPolygons(n=4, na.rm=T) #raster to SpatialPolygonsDataFrame
ocs_poly <- rgeos::gUnaryUnion(ocs_poly, id=ocs_poly$Captures) %>% #Join individual pixel polygons to one larger polygon
  st_as_sf() %>% #convert to spatial frame
  fill_holes(threshold=area_thresh) #fill internal holes

plot(ocs_poly, col='black')

#manually connect lone cell with closest cell and repeat
corrects_ocs <- data.frame("maplon" = c(157.5,227.5), 
                           "lat" = c(37.5,-27.5), "ocs_n" = 100)
corrects_ocs <- rbind(corrects_ocs, ocs)
ocs.raster <- rasterFromXYZ(corrects_ocs)
ocs_poly <- ocs.raster %>% rasterToPolygons(n=4, na.rm=T) #raster to SpatialPolygonsDataFrame
ocs_poly <- rgeos::gUnaryUnion(ocs_poly, id=ocs_poly$Captures) %>% #Join individual pixel polygons to one larger polygon
  st_as_sf() %>% #convert to spatial frame
  fill_holes(threshold=area_thresh) #fill internal holes

plot(ocs_poly, col='black')

ocs_poly_df <- as(ocs_poly, Class="Spatial") #convert to SpatialPolygonsDataFrame for area measurement
ocs_areas <- area(ocs_poly_df) / 1000000
sum(ocs_areas) #57,632,913

#plot
ocs_stock <- ggplot(ocs_poly)+
  geom_sf(color=my_pal[4], size=1.5, fill=my_pal[4],alpha=0.5)+
  #geom_tile(data=ocs, aes(x=maplon,y=lat, fill=ocs_n), alpha=0.95) +
  scale_x_continuous(limits=c(115,230), breaks=seq(120,220, by=20), labels=c(120,140,160,180,-160,-140))+
  scale_y_continuous(limits=c(-55,45), breaks=seq(-40,40, by=20)) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region))  +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(color='black', fill=NA, size=0.5),
        axis.title = element_blank()) +
  ggtitle("Oceanic Whitetip")
ocs_stock
ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/ocs_stock.png", ocs_stock,
       width = 8, height = 6, dpi=300, bg="white")
