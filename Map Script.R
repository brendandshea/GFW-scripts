library(marmap)
library(tidyverse)
library(sf)
library(rgdal)
library(tools)
library(maptools)

#### Polygons for EEZs ####
#EEZ shapefiles
path.eez.world <- ("/Volumes/Storage/Papers/GFW/World_EEZ_v10_20180221/")
eez.world <- "eez_v10.shp"

eez.global <- readOGR(dsn = path.eez.world, 
                      layer = file_path_sans_ext(eez.world))

# Extract the sanctuary EEZs
KIR_shp <- eez.global[eez.global@data$ISO_Ter1== "KIR", ]
MHL_shp <- eez.global[eez.global@data$ISO_Ter1== "MHL", ]
PLW_shp <- eez.global[eez.global@data$ISO_Ter1== "PLW", ]
MDV_shp <- eez.global[eez.global@data$ISO_Ter1== "MDV", ]
WSM_shp <- eez.global[eez.global@data$ISO_Ter1== "WSM", ]
FSM_shp <- eez.global[eez.global@data$ISO_Ter1== "FSM", ]
PYF_shp <- eez.global[eez.global@data$ISO_Ter1== "PYF", ]
COK_shp <- eez.global[eez.global@data$ISO_Ter1== "COK", ]
NCL_shp <- eez.global[eez.global@data$ISO_Ter1== "NCL", ]

# Fortify the shapefile data:
kir_shp <- fortify(kir_shp) 
mhl_shp <- fortify(mhl_shp) 
plw_shp <- fortify(plw_shp) 
mdv_shp <- fortify(mdv_shp) 
wsm_shp <- fortify(wsm_shp) 
fsm_shp <- fortify(FSM_shp) 
pyf_shp <- fortify(PYF_shp) 
cok_shp <- fortify(COK_shp) 
ncl_shp <- fortify(NCL_shp) 

#correct longitude for antimeridian center
kir_shp$maplong <- ifelse(kir_shp$long<0, kir_shp$long+360, kir_shp$long)
mhl_shp$maplong <- ifelse(mhl_shp$long<0, mhl_shp$long+360, mhl_shp$long)
mdv_shp$maplong <- ifelse(mdv_shp$long<0, mdv_shp$long+360, mdv_shp$long)
plw_shp$maplong <- ifelse(plw_shp$long<0, plw_shp$long+360, plw_shp$long)
wsm_shp$maplong <- ifelse(wsm_shp$long<0, wsm_shp$long+360, wsm_shp$long)
fsm_shp$maplong <- ifelse(fsm_shp$long<0, fsm_shp$long+360, fsm_shp$long)
pyf_shp$maplong <- ifelse(pyf_shp$long<0, pyf_shp$long+360, pyf_shp$long)
cok_shp$maplong <- ifelse(cok_shp$long<0, cok_shp$long+360, cok_shp$long)
ncl_shp$maplong <- ifelse(ncl_shp$long<0, ncl_shp$long+360, ncl_shp$long)


####Bathymetric map ####
#Get marmap bathymetry data for antimeridian center for Pacific shark sanctuaries
bathy <- getNOAA.bathy(65, -130, -35, 25, resolution = 3, antimeridian = TRUE, keep=TRUE)

st_as_sf(bathy)

# World map shapefile
data(wrld_simpl)
landmasses <- fortify(wrld_simpl)

#correct for antimeridian
landmasses$maplong<-ifelse(landmasses$long<0, landmasses$long+360, landmasses$long)

# this was an attempt to get arround scale correction problems
#landmasses$maplong<- ifelse(landmasses$maplong < 65, 65, landmasses$maplong)
#landmasses$maplong<- ifelse(landmasses$maplong > 230, 230, landmasses$maplong)

#landmasses$lat <- ifelse(landmasses$lat < -35 , -35, landmasses$lat)
#landmasses$lat <- ifelse(landmasses$lat > 25, 25, landmasses$lat)

#### Plot ####

map.layer <- geom_polygon(data = landmasses, aes(x = maplong, y = lat, group = group), color="black",
                         fill = "black", size = 0.1) 


worldmap <-  autoplot(bathy, geom=c('tile'), mapping=NULL, coast = TRUE) +
    geom_polygon(data = kir_shp, aes(x = maplong, y = lat, group = group), color="black",
            fill = "seagreen1", alpha=0.3, size = 0.25) +
    geom_polygon(data = mhl_shp, aes(x = maplong, y = lat, group = group), color="black",
               fill = "lightgoldenrod1", alpha=0.3, size = 0.25) +
    geom_polygon(data = plw_shp, aes(x = maplong, y = lat, group = group), color="black",
               fill = "darkorchid4", alpha=0.3, size = 0.25) +
    geom_polygon(data = mdv_shp, aes(x = maplong, y = lat, group = group), color="black",
               fill = "orange1", alpha=0.3, size = 0.25) +
    geom_polygon(data = wsm_shp, aes(x = maplong, y = lat, group = group), color="black",
               fill = "salmon2", alpha=0.3, size = 0.25) +
    geom_polygon(data = fsm_shp, aes(x = maplong, y = lat, group = group), color="black",
               fill = "hotpink2", alpha=0.3, size = 0.25) +
    geom_polygon(data = pyf_shp, aes(x = maplong, y = lat, group = group), color="black",
               fill = "goldenrod1", alpha=0.3, size = 0.25) +
    geom_polygon(data = cok_shp, aes(x = maplong, y = lat, group = group), color="black",
               fill = "plum2", alpha=0.3, size = 0.25) +
    geom_polygon(data = ncl_shp, aes(x = maplong, y = lat, group = group), color="black",
               fill = "olivedrab1", alpha=0.3, size = 0.25) +
  theme(legend.position = 'none') +
  labs(x="Longitude", y="Latitude") 

worldmap
worldmapfull <- worldmap + map.layer

data_full_range <- ggplot_build(worldmapfull)$data[[2]]
data_full_range <- data_full_range[data_full_range$x >= 65, ]
data_full_range <- data_full_range[data_full_range$x <= 230, ]
data_full_range <- data_full_range[data_full_range$y >= -35, ]
data_full_range <- data_full_range[data_full_range$y <= 25, ]

sanctuarymap <- worldmap+geom_polygon(data = data_full_range, aes(x = x, y = y, group = group), color="black",
                      fill = "black", size = 0.1)+
  annotate(geom="text", label="Kiribati", x=190, y=3, color="white", size=4)+
  annotate(geom="text", label="French Polynesia", x=220, y=-33, color="white", size=4)+
  annotate(geom="text", label="Cook Islands", x=192, y=-25, color="white", size=4)+
  annotate(geom="text", label="Samoa", x=182, y=-15, color="white", size=4)+
  annotate(geom="text", label="New Caledonia", x=167, y=-28, color="white", size=4)+
  annotate(geom="text", label="Marshall Islands", x=170, y=19, color="white", size=4)+
  annotate(geom="text", label="Micronesia", x=147, y=14, color="white", size=4)+
  annotate(geom="text", label="Palau", x=129, y=11, color="white", size=4)+
  annotate(geom="text", label="Maldives", x=75, y=-5, color="white", size=4) 

ggsave("sanctuarymap.pdf", sanctuarymap, useDingbats = FALSE, width = 12, height = 8)
