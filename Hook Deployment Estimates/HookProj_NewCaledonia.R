library(tidyverse) 
library(sf) 

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#### Isolate AIS data to sanctuary only ####
# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select newcaledonia EEZ
newcaledonia <- eezs %>% 
  filter(ISO_Ter1 == 'NCL' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
newcaledonia_bbox <- sf::st_bbox(newcaledonia)

# read in AIS data
effort2019 <- read.csv("2019effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_newcaledonia_2019 <- st_as_sf(effort2019,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(newcaledonia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
newcaledonia_effort_2019 <- effort_longline_sf_newcaledonia_2019 %>% 
  sf::st_join(newcaledonia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'NCL') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
newcaledonia2019 <- newcaledonia_effort_2019

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
newcaledonia2019$fleet[is.na(newcaledonia2019$fleet)] <- "Unknown"

newcaledonia2019$fishing_hours_1000s=newcaledonia2019$fishing_hours/1000
sum(newcaledonia2019$fishing_hours)#35761.89
rate_newcaledonia = sum(newcaledonia2019$fishing_hours)/1245000 #0.02872441 hrs/km2

#### Hook projections ####
# data clean
colnames(newcaledonia2019)[3] <- "hours"
colnames(newcaledonia2019)[5] <- "lon"
colnames(newcaledonia2019)[6] <- "lat"
newcaledonia2019 <- subset(newcaledonia2019, hours > 0)
sum(newcaledonia2019$hours)

newcaledonia2019$log_hours <- log(newcaledonia2019$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm.rds")
newcaledonia_preds <- predict(gamm1, newdata=newcaledonia2019, se.fit=T)

newcaledonia2019$log_hooks <- newcaledonia_preds$fit
newcaledonia2019$log_hooks.se <- newcaledonia_preds$se.fit

newcaledonia2019$hooks <- exp(newcaledonia2019$log_hooks)
sum(newcaledonia2019$hooks, na.rm=T) #7966385 hooks
7966385/1245000 #6.398703 hooks/km2

#save
write.csv(newcaledonia2019, "newcaledonia2019.csv", row.names = F) 

#### plotting ####
# Hours

#aggregate across fleets
newcaledonia_2019 <- aggregate(data=newcaledonia_effort_2019, fishing_hours~cell_ll_lon+cell_ll_lat, FUN='sum')
newcaledonia_2019 <- subset(newcaledonia_2019, fishing_hours != 0)
newcaledonia_2019$log_fishing_hours=ifelse(newcaledonia_2019$fishing_hours < 10, 0, log10(newcaledonia_2019$fishing_hours))
range(newcaledonia_2019$fishing_hours)

world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

newcaledonia2019plot <- newcaledonia_2019 %>%
  ggplot() +
  geom_raster(aes(x = cell_ll_lon, y = cell_ll_lat, fill = log_fishing_hours)) +
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
  scale_fill_gradientn(
    "Fishing Hours",
    na.value = NA,
    limits = c(0,3),
    colours = heat.colors(12, rev=T), # Linear Green
    labels = c("0", "10", "100", "1000"),
    values = scales::rescale(c(0, 1))) +
  labs(fill  = 'Fishing hours (log scale)',       title = 'newcaledonia'
  ) +
  guides(fill = guide_colourbar(barwidth = 10)) +
  coord_sf(xlim=c(128,138), ylim=c(0,13))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/newcaledonialonglines2019.png", newcaledonia2019plot, width = 12, height = 8)

#### Hooks plot ####
#Hooks plotting
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
newcaledonia_ais <- read.csv("newcaledonia2019.csv")
newcaledonia_proj <- newcaledonia_ais
newcaledonia_proj$thooks = newcaledonia_proj$hooks/1000
dev.off()
world <- map_data("world")
newcaledonia_map <- ggplot(data=newcaledonia_proj, aes(x=lon, y=lat)) +
  geom_tile(aes(fill=thooks)) +
  scale_fill_gradient(name="Thousands of Longline \nHooks Deployed", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = ncl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6)+ 
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(155,175), breaks=seq(155,175,by=5)) +
  scale_y_continuous(limits=c(-27,-14), breaks=seq(-25,15,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="New Caledonia")


newcaledonia_map

