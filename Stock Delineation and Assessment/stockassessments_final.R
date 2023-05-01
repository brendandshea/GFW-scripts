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
plot(FSM_S)

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
plot(FSM_n)

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
total_preds<-read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/total_preds.csv")  
blueshark <- subset(total_preds, Species == "Blue Shark")
blueshark$total.var <- blueshark$total.sd^2
north <- subset(blueshark, Lat >= 0)
south <- subset(blueshark, Lat <0 )

bsh_north <- aggregate(data=north, total~country, FUN='sum')
bsh_north.var <- aggregate(data=north, total.var~country, FUN='sum')
bsh_north$total.sd <- sqrt(bsh_north.var$total.var)
bsh_north$mt <- bsh_north$total*24.9/1000

bsh_south <- aggregate(data=south, total~country, FUN='sum')
bsh_south.var <- aggregate(data=south, total.var~country, FUN='sum')
bsh_south$total.sd <- sqrt(bsh_south.var$total.var)
bsh_south$mt <- bsh_south$total*24.9/1000

#north
stockarea=38585660 
MSY = 99927
sMSY = MSY/stockarea*100000

catch = sum(bsh_north$mt)
sanctarea = sanct_n
sHarvest <- catch/sanctarea*100000

(sMSY-sHarvest)/sMSY*100 #95.99%
100-(sMSY-sHarvest)/sMSY*100 #4.01%

#south
stockarea=36025997
MSY = 13234 #median grid estiamte
sMSY = MSY/stockarea*100000

catch = sum(bsh_south$mt)
sanctarea = sanct_s
sHarvest <- catch/sanctarea*100000

(sMSY-sHarvest)/sMSY*100 #94.60%
100-(sMSY-sHarvest)/sMSY*100 #5.40%

#silky shark
stockarea=59689229
MSY = 12162
sMSY = MSY/stockarea*100000

catch = 1427.60664
sanctarea = sanct_total
sHarvest <- catch*100000/sanctarea

(sMSY-sHarvest)/sMSY*100 #59.49%
100-(sMSY-sHarvest)/sMSY*100 #40.51%

#shortfin mako
#MSY is 3127

total_preds<-read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/total_preds.csv")  
mako <- subset(total_preds, Species == "Mako")
mako$total.var <- mako$total.sd^2
north <- subset(mako, Lat >= 0)
south <- subset(mako, Lat < 0)

mako_north <- aggregate(data=north, total~country, FUN='sum')
mako_north.var <- aggregate(data=north, total.var~country, FUN='sum')
mako_north$total.sd <- sqrt(mako_north.var$total.var)
mako_north$mt <- mako_north$total*31.3/1000
stockarea=38027072
MSY = 3127
sMSY = MSY/stockarea*100000

catch = sum(mako_north$mt)
sanctarea = sanct_n
sHarvest <- catch*100000/sanctarea

(sMSY-sHarvest)/sMSY*100 #85.62%
100-(sMSY-sHarvest)/sMSY*100 #14.38%

mako_south <- aggregate(data=south, total~country, FUN='sum')
mako_south.var <- aggregate(data=south, total.var~country, FUN='sum')
mako_south$total.sd <- sqrt(mako_south.var$total.var)
mako_south$mt <- mako_south$total*31.3/1000
sum(mako_south$mt)
sum(mako_north$mt)
sum(mako_north$total)*31.3/1000
sum(mako_south$total)*31.3/1000
sum(mako$total)*31.3/1000

#oceanic whitetip shark
#MSY is 7055
stockarea=57585206
MSY = 6052
sMSY = MSY/stockarea*100000

catch = 279.654
sanctarea = sanct_total
sHarvest <- catch*100000/sanctarea

100-(sMSY-sHarvest)/sMSY*100 #15.38%
(sMSY-sHarvest)/sMSY*100 #84.62%
