library(tidyverse)
library(rgdal)
library(tools)
library(maptools)
library(sf)
library(viridis)

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#EEZ shapefiles
path.eez.world <- ('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/')
eez.world <- "eez_v10.shp"

eez.global <- readOGR(dsn = path.eez.world, 
                      layer = file_path_sans_ext(eez.world))

MHL_shp <- eez.global[eez.global@data$ISO_Ter1== "MHL", ]
mhl_shp <- fortify(MHL_shp) 

# read in AIS data
effort2019 <- read.csv("2019effort_*25x*25.csv")

#EEZs for filtering
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select MI EEZ
marshall <- eezs %>% 
  filter(ISO_Ter1 == 'MHL' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

#Fishing hours
library(gfwr)
library(tidyverse)

key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJWaXJnaW5pYSBUZWNoIEZlcnJldHRpIExhYiIsInVzZXJJZCI6MTk4NzgsImFwcGxpY2F0aW9uTmFtZSI6IlZpcmdpbmlhIFRlY2ggRmVycmV0dGkgTGFiIiwiaWQiOjExNywidHlwZSI6InVzZXItYXBwbGljYXRpb24ifSwiaWF0IjoxNjYwNzU0MjY2LCJleHAiOjE5NzYxMTQyNjYsImF1ZCI6ImdmdyIsImlzcyI6ImdmdyJ9.e4nWtHsEV57JimuKMtY9aS1zF2GrntrlTBg7fpjeiHl7STMQUPA1JNQHRekP4LsletdmLkT5So4hfaRLkE0JwXDUN1A6XpymggTv5ekNU4qJF6LlgtzovyZsP74SZt9ZtB0AH2_xLcMW130b6SxPFAwGwGVS_pY4EHebt3jO-YV-Qzb0iPJ3uqifTPzOnRGu8BV2j-LvFCD9WJrenBYv2OtBA3rj2Irgv2SC64RikPA2eKEt1ACgfy-XVk4VSXlZgqN8GLHjG7GQ0zVcNwuZ2QoGIGS3hHUlARl_5mTF7eN0M4jQHjhFs6QnVvYXbPrUZxaNLZGTn8TDwGRiN2qLPtyx80TDzNgBJaHE3uZ_d9VEkNHFmDHuaS9f2j9neCR-KizHFud67D7XD7Ro8nvtmK-cnGDvl0jJ6owFkKYeqRuRgbXiNaJlN-cJtv4LlheY_ZUC7nGLtKjAJnDIDyiqVHzG5FhTYFoBEzrB1iQSDelnzodqPip1MvhBHb6qpcr4"

region_json = '{"geojson":{"type":"Polygon","coordinates":[[[150, 25],[150,-5],[180,-5],[180,25]]]}}'
(code_eez <- get_region_id(region_name="Marshall", region_source = 'eez', key=key))
#MI is 8318

dat <- get_raster(spatial_resolution = 'low', #0.1 degree res
                  temporal_resolution = 'yearly',
                  group_by = 'flagAndGearType',
                  date_range = '2019-01-01,2020-01-01',
                  region = 8318, #codes for Marshall Islands
                  region_source = 'eez',
                  key = key)

colnames(dat)[4] <- "Fleet"
colnames(dat)[6] <- "hours"

dat2 <- subset(dat, Geartype == "drifting_longlines")
dat2$Fleet <- ifelse(is.na(dat2$Fleet), "Unknown", dat2$Fleet)

library(maps)
world <- map_data("world")

mhl_fishing_plot <- ggplot(data=dat2, aes(x=Lon, y=Lat)) +
  geom_tile(aes(fill=Fleet, alpha=hours)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(breaks=seq(160,180,by=5))+
  scale_y_continuous(breaks=seq(5,15,by=5)) +
  coord_cartesian(xlim=c(156,180), ylim=c(1,19), expand=F) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour="black", size=1, fill=NA),
        legend.position = "bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=7),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        legend.key.size = unit(0.7, "lines"))+
  scale_alpha(range=c(0.3,1)) +
  guides(alpha="none",
         fill=guide_legend(override.aes = list(size=2)))

mhl_fishing_plot

ggsave("~/Desktop/GFWAIS.png", mhl_fishing_plot,
       width = 8, height = 6, dpi=300, bg="white")

#hooks predictions
marshall_proj <- read.csv("marshallislands2019.csv")
marshall_proj$thooks = marshall_proj$hooks/1000

marshall_hooks_map <- ggplot(data=marshall_proj, aes(x=lon, y=lat)) +
  geom_tile(aes(fill=thooks)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(breaks=seq(160,180,by=5))+
  scale_y_continuous(breaks=seq(5,15,by=5)) +
  coord_cartesian(xlim=c(156,180), ylim=c(1,19), expand=F) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour="black", size=1, fill=NA),
        legend.position = "bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))+
  scale_fill_viridis(name="1000s of Hooks",option="mako", begin=0.9,end=0.2)

marshall_hooks_map
ggsave("~/Desktop/testhooks.png", marshall_hooks_map,
       width = 8, height = 6, dpi=300, bg="white")


#rawhooks map
datMaster2 <- read.csv("RMFO2019longline5deg.csv") #RFMO hook data
#aggregate hook data and rename
hookdat = with(datMaster2,aggregate(hooks2, by = list(lon,lat), sum))
names(hookdat) = c("lon","lat","hooks2")

hookdat$thooks <- hookdat$hooks2/1000
hookdat$thooks <- ifelse(hookdat$thooks >10000, 10000, hookdat$thooks)
hookdat$lon <- ifelse(hookdat$lon==-180, 180, hookdat$lon)
hookdat$lon = hookdat$lon+2.5
hookdat$lat = hookdat$lat+2.5
marshall_rfmo_map <- ggplot(data=hookdat, aes(x=lon, y=lat)) +
  geom_tile(aes(fill=thooks)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(breaks=seq(160,180,by=5))+
  scale_y_continuous(breaks=seq(5,15,by=5)) +
  coord_cartesian(xlim=c(156,180), ylim=c(1,19), expand=F) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour="black", size=1, fill=NA),
        legend.position = "bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))+
  scale_fill_viridis(name="1000s of \n Hooks",option="B", begin=1, end=0.1)

marshall_rfmo_map

ggsave("~/Desktop/marshall_rfmo.png", 
       marshall_rfmo_map,  width = 8, height = 6, dpi=300, bg="white")

#CPUEs
master_cpue <- read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/WCPFC CPUEs.csv")
master_cpue$Long <- ifelse(master_cpue$Long2 > 180, master_cpue$Long2 - 360, master_cpue$Long2)
master_cpue$total <- rowSums(master_cpue[ ,c(5,9,13,17,21,25,29)], na.rm=T)
master_cpue$log.total <- log(master_cpue$total)

cpues <- master_cpue[,c(2,3,34)]

cpues$plotting <- ifelse(cpues$total > 3, 3, cpues$total)
cpues$Long <- ifelse(cpues$Long == -180, 180, cpues$Long)


marshall_cpue_map <- ggplot(data=cpues, aes(x=Long2, y=Lat)) +
  geom_raster(aes(fill=plotting)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(breaks=seq(160,180,by=5))+
  scale_y_continuous(breaks=seq(5,15,by=5)) +
  coord_cartesian(xlim=c(156,180), ylim=c(1,19), expand=F) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour="black", size=1, fill=NA),
        legend.position = "bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))+
  scale_fill_viridis(name="Shark CPUE \n(Individuals/1000 hooks)",option="turbo")

ggsave("~/Desktop/testCPUE.png", marshall_cpue_map, width=8, height=6, dpi=320, bg='white')

marshall_cpue_map


#### Total catch ####
catch_mort <- read.csv("marshallislands_total_preds.csv")
catches <- aggregate(data=catch_mort, catch~Lat+Long, FUN='sum')

marshall_catch_map <- ggplot(data=catches, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=catch)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(breaks=seq(160,180,by=5))+
  scale_y_continuous(breaks=seq(5,15,by=5)) +
  coord_cartesian(xlim=c(156,180), ylim=c(1,19), expand=F) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour="black", size=1, fill=NA),
        legend.position = "bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))+
  scale_fill_viridis(name="Total Catch \n(Individuals)",option="rocket", begin=0.9, end=0.2)

ggsave("~/Desktop/testcatch.png", marshall_catch_map, width=8, height=6, dpi=320, bg='white')

#### Total mortality ####
mortality <- aggregate(data=catch_mort, total~Lat+Long, FUN='sum')

marshall_mortality_map <- ggplot(data=mortality, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(breaks=seq(160,180,by=5))+
  scale_y_continuous(breaks=seq(5,15,by=5)) +
  coord_cartesian(xlim=c(156,180), ylim=c(1,19), expand=F) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour="black", size=1, fill=NA),
        legend.position = "bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))+
  scale_fill_viridis(name="Mortality \n(Individuals)",option="plasma", begin=0.9,end=0.2) 

ggsave("~/Desktop/testmortality.png", marshall_mortality_map, width=8, height=6, dpi=320, bg='white')


library(ggpubr)
Fig2 <- ggarrange(mhl_fishing_plot,marshall_rfmo_map, marshall_hooks_map,marshall_cpue_map,marshall_catch_map,marshall_mortality_map,
                  ncol=3,nrow=2, labels="AUTO", align='hv')

Fig2

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Fig2marshall.png", 
       Fig2,  width = 10, height = 6, dpi=300, bg="white")

