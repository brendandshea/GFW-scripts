library(tidyverse) 
library(sf) 

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#### Isolate AIS data to sanctuary only ####
# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select frenchpolynesia EEZ
frenchpolynesia <- eezs %>% 
  filter(ISO_Ter1 == 'PYF' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
frenchpolynesia_bbox <- sf::st_bbox(frenchpolynesia)

# read in AIS data
effort2019 <- read.csv("2019effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_frenchpolynesia_2019 <- st_as_sf(effort2019,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(frenchpolynesia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
frenchpolynesia_effort_2019 <- effort_longline_sf_frenchpolynesia_2019 %>% 
  sf::st_join(frenchpolynesia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'PYF') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
frenchpolynesia2019 <- frenchpolynesia_effort_2019

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
frenchpolynesia2019$fleet[is.na(frenchpolynesia2019$fleet)] <- "Unknown"

frenchpolynesia2019$fishing_hours_1000s=frenchpolynesia2019$fishing_hours/1000
sum(frenchpolynesia2019$fishing_hours)#25922.1
rate_frenchpolynesia = sum(frenchpolynesia2019$fishing_hours)/4767242 #0.0054375


#### Hook projections ####
# data clean
colnames(frenchpolynesia2019)[3] <- "hours"
colnames(frenchpolynesia2019)[5] <- "lon"
colnames(frenchpolynesia2019)[6] <- "lat"
frenchpolynesia2019 <- subset(frenchpolynesia2019, hours > 0)
sum(frenchpolynesia2019$hours)

frenchpolynesia2019$log_hours <- log(frenchpolynesia2019$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm.rds")
frenchpolynesia_preds <- predict(gamm1, newdata=frenchpolynesia2019, se.fit=T)
frenchpolynesia2019$log_hooks <- frenchpolynesia_preds$fit
frenchpolynesia2019$log_hooks.se <- frenchpolynesia_preds$se.fit

frenchpolynesia2019$hooks <- exp(frenchpolynesia2019$log_hooks)
sum(frenchpolynesia2019$hooks, na.rm=T) #25162147 hooks
25162147/4767242 #5.278135 hooks/km2

#save
write.csv(frenchpolynesia2019, "frenchpolynesia2019.csv", row.names = F) 

#### plotting ####
# Hours

#aggregate across fleets
frenchpolynesia_2019 <- aggregate(data=frenchpolynesia_effort_2019, fishing_hours~cell_ll_lon+cell_ll_lat, FUN='sum')
frenchpolynesia_2019 <- subset(frenchpolynesia_2019, fishing_hours != 0)
frenchpolynesia_2019$log_fishing_hours=ifelse(frenchpolynesia_2019$fishing_hours < 10, 0, log10(frenchpolynesia_2019$fishing_hours))
range(frenchpolynesia_2019$fishing_hours)


frenchpolynesia2019plot <- frenchpolynesia_2019 %>%
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
  labs(fill  = 'Fishing hours (log scale)',       title = 'frenchpolynesia'
  ) +
  guides(fill = guide_colourbar(barwidth = 10)) +
  coord_sf(xlim=c(128,138), ylim=c(0,13))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/frenchpolynesialonglines2019.png", frenchpolynesia2019plot, width = 12, height = 8)

#### Hooks Plot ####
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
frenchpolynesia_ais <- read.csv("frenchpolynesia2019.csv")
frenchpolynesia_proj <- frenchpolynesia_ais
frenchpolynesia_proj$thooks = frenchpolynesia_proj$hooks/1000
dev.off()
world <- map_data("world")
frenchpolynesia_map <- ggplot(data=frenchpolynesia_proj, aes(x=lon, y=lat)) +
  geom_tile(aes(fill=thooks)) +
  scale_fill_gradient(name="Thousands of Longline \nHooks Deployed", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = pyf_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  #geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(-159.5,-130.5), breaks=seq(-155,-135,by=5)) +
  scale_y_continuous(limits=c(-33,-3), breaks=seq(-30,-5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="French Polynesia")



frenchpolynesia_map

