library(tidyverse) 
library(sf) 

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#### Isolate AIS data to sanctuary only ####
# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select palau EEZ
palau <- eezs %>% 
  filter(ISO_Ter1 == 'PLW' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
palau_bbox <- sf::st_bbox(palau)

# read in AIS data
effort2019 <- read.csv("2019effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_palau_2019 <- st_as_sf(effort2019,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(palau))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
palau_effort_2019 <- effort_longline_sf_palau_2019 %>% 
  sf::st_join(palau, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'PLW') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
palau2019 <- palau_effort_2019

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
palau2019$fleet[is.na(palau2019$fleet)] <- "Unknown"

palau2019$fishing_hours_1000s=palau2019$fishing_hours/1000
sum(palau2019$fishing_hours)#72842.52
rate_palau = sum(palau2019$fishing_hours)/604289 #0.1199468 hrs/km2


#### Hook projections ####
# data clean
colnames(palau2019)[3] <- "hours"
colnames(palau2019)[5] <- "lon"
colnames(palau2019)[6] <- "lat"
palau2019 <- subset(palau2019, hours > 0)
sum(palau2019$hours)

palau2019$log_hours <- log(palau2019$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm.rds")
palau_preds <- predict(gamm1, newdata=palau2019, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(palau2019$fleet), lat = 6, lon = 133)
fit <- predict(gamm1, newdata = newdata, se.fit = TRUE)
newdata$pred = fit$fit
newdata$se.fit = fit$se.fit 
newdata$pred.hooks = exp(newdata$pred)
newdata = with(newdata,data.frame(newdata, lwr=pred-1.96*se.fit,upr=pred+1.96*se.fit))
newdata$phl = exp(newdata$lwr)
newdata$phu = exp(newdata$upr)
library(countrycode)
newdata$fleet = countrycode(newdata$fleet, origin="iso2c", destination = "iso3c", nomatch = NULL)
newdata = newdata[order(newdata$pred.hooks),]
pos = 1:nrow(newdata)
with(newdata,plot(pred.hooks/1000,pos, pch=16, xlim = c(min(phl/1000), max(phu/1000)), axes = F, ylab = "", xlab = "Predicted #hooks per 100 AIS hours"))
with(newdata,segments(phl/1000,pos,phu/1000,pos))
axis(1)
axis(2,at = pos, labels = as.character(newdata$fleet), las = 1)

palau2019$fleet2 <- palau2019$fleet
#CHN is average
palau2019$fleet <- ifelse(palau2019$fleet == "Unknown" | palau2019$fleet == "ALB", "CHN", palau2019$fleet)

palau_preds <- predict(gamm1, newdata=palau2019, se.fit=T)
palau2019$log_hooks <- palau_preds$fit
palau2019$log_hooks.se <- palau_preds$se.fit

palau2019$hooks <- exp(palau2019$log_hooks)
sum(palau2019$hooks, na.rm=T) #21000452 hooks
21000452 / 604289 #34.75233 hooks/km2


#save
write.csv(palau2019, "palau2019.csv", row.names = F) 

#### plotting ####
# Hours
#aggregate across fleets
palau_2019 <- aggregate(data=palau_effort_2019, fishing_hours~cell_ll_lon+cell_ll_lat+fleet, FUN='sum')
palau_2019 <- subset(palau_2019, fishing_hours != 0)
palau_2019$log_fishing_hours=ifelse(palau_2019$fishing_hours < 10, 0, log10(palau_2019$fishing_hours))
range(palau_2019$fishing_hours)

colnames(palau_2019)[3] <- "Fleet"

world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

palau2019plot <- palau_2019 %>%
  ggplot() +
  geom_raster(aes(x = cell_ll_lon, y = cell_ll_lat, fill = Fleet, alpha=log_fishing_hours)) +
  geom_sf(data = world_shp, 
          fill = '#374a6d', 
          color = '#0A1738',
          size = 0.1) +
  geom_sf(data = eezs,
          color = '#374a6d',
          alpha = 0.2,
          fill = NA,
          size = 0.1) +theme(legend.position="bottom", legend.box = "horizontal") +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  coord_sf(xlim=c(125,145), ylim=c(0,12)) +
  guides(alpha="none") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_line(color="gray", size=0.3),
        panel.grid.minor = element_line(color="gray", size=0.3))

palau2019plot

scale_fill_gradientn(
    "Fishing Hours",
    na.value = NA,
    limits = c(0,3),
    colours = heat.colors(12, rev=T), # Linear Green
    labels = c("0", "10", "100", "1000"),
    values = scales::rescale(c(0, 1))) +
  labs(fill  = 'Fishing hours (log scale)',       title = 'Palau'
  ) +
  guides(fill = guide_colourbar(barwidth = 10))
  

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/palaulonglines2019.png", palau2019plot, width = 12, height = 8)



#Hooks plotting
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
palau_ais <- read.csv("palau2019.csv")
palau_proj <- palau_ais
palau_proj$thooks = palau_proj$hooks/1000
dev.off()
world <- map_data("world")
palau_map <- ggplot(data=palau_proj, aes(x=lon, y=lat)) +
  geom_tile(aes(fill=thooks)) +
  scale_fill_gradient(name="Thousands of Longline \nHooks Deployed", low="khaki1", high="red",
                     breaks=c(0,50,100,150,200),
                     labels=c(0,50,100,150,200),
                     limits=c(0,200)) +
  geom_polygon(data = plw_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(129,138), breaks=seq(130,135,by=5)) +
  scale_y_continuous(limits=c(1,13), breaks=seq(5,10,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Palau")


palau_map