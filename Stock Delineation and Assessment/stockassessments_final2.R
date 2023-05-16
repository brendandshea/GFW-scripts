library(tidyverse)
library(sf)
library(rgdal)
library(tools)
library(maptools)

sanctarea = 17126627
sanct_s = 9950035

#kiribati, micronesia straddle equator

### Polygons for EEZs ####
#EEZ shapefiles
path.eez.world <- ('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/')
eez.world <- "eez_v10.shp"

eez.global <- readOGR(dsn = path.eez.world, 
                      layer = file_path_sans_ext(eez.world))

# Extract the sanctuary EEZs
KIR_shp <- eez.global[eez.global@data$ISO_Ter1== "KIR", ]
MHL_shp <- eez.global[eez.global@data$ISO_Ter1== "MHL", ]
PLW_shp <- eez.global[eez.global@data$ISO_Ter1== "PLW", ]
WSM_shp <- eez.global[eez.global@data$ISO_Ter1== "WSM", ]
FSM_shp <- eez.global[eez.global@data$ISO_Ter1== "FSM", ]
PYF_shp <- eez.global[eez.global@data$ISO_Ter1== "PYF", ]
COK_shp <- eez.global[eez.global@data$ISO_Ter1== "COK", ]
NCL_shp <- eez.global[eez.global@data$ISO_Ter1== "NCL", ]



#Split Kiribati and FSM at equatorKIR_shp@bbox

####Kiribati####
KIR_shp@bbox #check overall bbox
#South
library(rgeos)
x<-c(-180,180,180,-180,-180) #setting coordinates of polygon to intersect to only include south
y<-c(-13.83833,-13.83833,0,0,-13.83833) #y coordinates - need to include starting coordinates as end as well

#language for rgeos readWKT
ply<-paste0(noquote(
  paste(x,y,collapse=",")))
ply<-paste0("POLYGON ((",ply,"))")
ply<-paste0('readWKT("',ply,'")')

ply<-eval(parse(text=ply))

proj4string(ply) <- CRS("+proj=longlat +datum=WGS84 +no_defs") #match CRS

KIR_S <- gIntersection(KIR_shp,ply) #intersect southern polygon with EEZ shapefile to get southern portion

KIR_area_s <- raster::area(KIR_S)/1000000 # take area

x<-c(-180,180,180,-180,-180) #setting coordinates of polygon to intersect to only include north
y<-c(0,0,7.879056,7.879056,0)#y coordinates - need to include starting coordinates as end as well

#language for rgeos readWKT
ply<-paste0(noquote(
  paste(x,y,collapse=",")))
ply<-paste0("POLYGON ((",ply,"))")
ply<-paste0('readWKT("',ply,'")')

ply<-eval(parse(text=ply))

proj4string(ply) <- CRS("+proj=longlat +datum=WGS84 +no_defs") #match CRS

KIR_n <- gIntersection(KIR_shp,ply)#intersection

KIR_area_n <- raster::area(KIR_n)/1000000 #take area

####FSM####
FSM_shp@bbox #check overall bbox
#South
library(rgeos)
x<-c(135,166,166,135,135) #setting coordinates of polygon to intersect to only include south
y<-c(-2,-2,0,0,-2) #y coordinates - need to include starting coordinates as end as well

#language for rgeos readWKT
ply<-paste0(noquote(
  paste(x,y,collapse=",")))
ply<-paste0("POLYGON ((",ply,"))")
ply<-paste0('readWKT("',ply,'")')

ply<-eval(parse(text=ply))

proj4string(ply) <- CRS("+proj=longlat +datum=WGS84 +no_defs") #match CRS

FSM_shp <- FSM_shp %>% gBuffer(byid=TRUE, width=0)
FSM_S <- gIntersection(FSM_shp,ply) #intersect southern polygon with EEZ shapefile to get southern portion

FSM_area_s <- raster::area(FSM_S)/1000000 # take area

x<-c(135,166,166,135,135) #setting coordinates of polygon to intersect to only include north
y<-c(0,0,14,14,0)#y coordinates - need to include starting coordinates as end as well

#language for rgeos readWKT
ply<-paste0(noquote(
  paste(x,y,collapse=",")))
ply<-paste0("POLYGON ((",ply,"))")
ply<-paste0('readWKT("',ply,'")')

ply<-eval(parse(text=ply))

proj4string(ply) <- CRS("+proj=longlat +datum=WGS84 +no_defs") #match CRS

FSM_n <- gIntersection(FSM_shp,ply) #intersection

FSM_area_n <- raster::area(FSM_n)/1000000 #take area

wsm_area <- raster::area(WSM_shp)/1000000 
pyf_area <-  raster::area(PYF_shp)/1000000
cok_area <-  raster::area(COK_shp)/1000000 
ncl_area <- raster::area(NCL_shp)/1000000
plw_area <- raster::area(PLW_shp)/1000000
mhl_area <- raster::area(MHL_shp)/1000000
fsm_total_area <- raster::area(FSM_shp)/1000000
kir_total_area <- raster::area(KIR_shp)/1000000
  
sanct_s <- sum(KIR_area_s, wsm_area, pyf_area, cok_area,ncl_area,FSM_area_s)
sanct_n <- sum(plw_area,FSM_area_n, KIR_area_n, mhl_area)
sanct_total <- sum(wsm_area,pyf_area,cok_area,ncl_area,plw_area,mhl_area,fsm_total_area,kir_total_area)

#blue shark
total_preds<-read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/total_summary.csv")  
blueshark <- subset(total_preds, Species == "Blue Shark")

#Detailed preds for splitting north
micronesia_preds<-read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/micronesia_total_preds.csv")  
micronesia_blue <- subset(micronesia_preds, Species == "Blue Shark")
micronesisa_north <- subset(micronesia_blue, Lat >= 0)
micronesia_south <- subset(micronesia_blue, Lat < 0)

micronesia_reduce_rate <- sum(micronesisa_north$total)/sum(micronesia_blue$total) # 99.9 of catch from northern portion

kiribati_preds<-read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/kiribati_total_preds.csv")  
kiribati_blue <- subset(kiribati_preds, Species == "Blue Shark")
kiribati_north <- subset(kiribati_blue, Lat >= 0)
kiribati_south <- subset(kiribati_blue, Lat < 0)

kiribati_reduce_rate <- sum(kiribati_north$total)/sum(kiribati_blue$total) # of catch from northern portion

bsh_north <- subset(blueshark, country=="Kiribati" | country=="FSM" | country=="Marshall Islands" | country=="Palau")
bsh_north[2,11:13] <- bsh_north[2,11:13]*micronesia_reduce_rate 
bsh_north[3,11:13] <- bsh_north[3,11:13]*kiribati_reduce_rate 
bsh_north$mt <- bsh_north$total*24.9/1000

bsh_south <- subset(blueshark, country=="Kiribati" | country=="FSM" | country=="New Caledonia" | country=="Cook Islands" | country=="Samoa" | country == "French Polynesia")
bsh_south[4,11:13] <- bsh_south[4,11:13]*(1-micronesia_reduce_rate) 
bsh_south[6,11:13] <- bsh_south[6,11:13]*(1-kiribati_reduce_rate)
bsh_south$mt <- bsh_south$total*24.9/1000

#north
bsh_stockarea=38585660
bsh_msy = 99927*1000 #kg
bsh_wt <- 24.9 #kg
sMSY = bsh_msy/bsh_wt/bsh_stockarea*1000

sum(bsh_north$mt)
catch = sum(bsh_north$mt)*1000/24.9
sanctarea = sanct_n
sHarvest <- catch/sanctarea*1000

(sMSY-sHarvest)/sMSY*100 #95.98%
100-(sMSY-sHarvest)/sMSY*100 #4.02%

#south

bsh_stockarea=36025997
bsh_msy = 13234*1000 #kg
bsh_wt <- 24.9 #kg
sMSY = bsh_msy/bsh_wt/bsh_stockarea*1000

sum(bsh_south$mt)
catch = sum(bsh_south$mt)*1000/24.9
sanctarea = sanct_s
sHarvest <- catch/sanctarea*1000

(sMSY-sHarvest)/sMSY*100 #94.57%
100-(sMSY-sHarvest)/sMSY*100 #5.43%

#silky shark
silky <- subset(total_preds, Species=="Silky Shark")
silky$mt <- silky$total*35.4/1000

silky_stockarea=59689229
silky_msy = 12162*1000 #kg
silky_wt <- 35.4 #kg
sMSY = silky_msy/silky_wt/silky_stockarea*1000

sum(silky$mt)
catch = sum(silky$mt)*1000/35.4
sanctarea = sanct_total
sHarvest <- catch/sanctarea*1000

(sMSY-sHarvest)/sMSY*100 #58.51%
100-(sMSY-sHarvest)/sMSY*100 #41.49%

#shortfin mako
#MSY is 3127

mako <- subset(total_preds, Species == "Mako Shark")

#Detailed preds for splitting north
micronesia_preds<-read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/micronesia_total_preds.csv")  
micronesia_mako <- subset(micronesia_preds, Species == "Mako")
micronesisa_north <- subset(micronesia_mako, Lat >= 0)
micronesia_south <- subset(micronesia_mako, Lat < 0)

micronesia_reduce_rate <- sum(micronesisa_north$total)/sum(micronesia_mako$total) # 99.9 of catch from northern portion

kiribati_preds<-read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/kiribati_total_preds.csv")  
kiribati_mako <- subset(kiribati_preds, Species == "Mako")
kiribati_north <- subset(kiribati_mako, Lat >= 0)
kiribati_south <- subset(kiribati_mako, Lat < 0)

kiribati_reduce_rate <- sum(kiribati_north$total)/sum(kiribati_mako$total) # of catch from northern portion

mako_north <- subset(mako, country=="Kiribati" | country=="FSM" | country=="Marshall Islands" | country=="Palau")
mako_north[2,11:13] <- mako_north[2,11:13]*micronesia_reduce_rate 
mako_north[3,11:13] <- mako_north[3,11:13]*kiribati_reduce_rate 
mako_north$mt <- mako_north$total*31.3/1000

mako_south <- subset(mako, country=="Kiribati" | country=="FSM" | country=="New Caledonia" | country=="Cook Islands" | country=="Samoa" | country == "French Polynesia")
mako_south[4,11:13] <- mako_south[4,11:13]*(1-micronesia_reduce_rate) 
mako_south[6,11:13] <- mako_south[6,11:13]*(1-kiribati_reduce_rate)
mako_south$mt <- mako_south$total*31.3/1000

#north

mako_stockarea=38027072
mako_msy = 3127*1000 #kg
mako_wt <- 31.3 #kg
sMSY = mako_msy/mako_wt/mako_stockarea*1000

catch = sum(mako_north$mt)*1000/31.3
sanctarea = sanct_n
sHarvest <- catch/sanctarea*1000

(sMSY-sHarvest)/sMSY*100 #88.40%
100-(sMSY-sHarvest)/sMSY*100 #11.60%

#south
sum(mako_south$mt)
sum(mako_north$mt)

#oceanic whitetip shark
#MSY is 7055

oceanicwhitetip <- subset(total_preds, Species=="Oceanic Whitetip")
oceanicwhitetip$mt <- oceanicwhitetip$total*51.3/1000


ocs_stockarea=57585206
ocs_msy = 7055*1000 #kg
ocs_wt <- 51.3 #kg
sMSY = ocs_msy/ocs_wt/ocs_stockarea*1000

catch = sum(oceanicwhitetip$mt)*1000/51.3
sanctarea = sanct_total
sHarvest <- catch/sanctarea*1000

100-(sMSY-sHarvest)/sMSY*100 #10.89%
(sMSY-sHarvest)/sMSY*100 #89.11%
