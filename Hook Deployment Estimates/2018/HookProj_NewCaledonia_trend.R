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
effort2018 <- read.csv("2018effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_newcaledonia_2018 <- st_as_sf(effort2018,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(newcaledonia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
newcaledonia_effort_2018 <- effort_longline_sf_newcaledonia_2018 %>% 
  sf::st_join(newcaledonia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'NCL') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
newcaledonia2018 <- newcaledonia_effort_2018

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
newcaledonia2018$fleet[is.na(newcaledonia2018$fleet)] <- "Unknown"

newcaledonia2018$fishing_hours_1000s=newcaledonia2018$fishing_hours/1000
sum(newcaledonia2018$fishing_hours)#98500
rate_newcaledonia = sum(newcaledonia2018$fishing_hours)/1245000 #0.079 hrs/km2

#### Hook projections ####
# data clean
colnames(newcaledonia2018)[2] <- "hours"
colnames(newcaledonia2018)[4] <- "lon"
colnames(newcaledonia2018)[5] <- "lat"
newcaledonia2018 <- subset(newcaledonia2018, hours > 0)
sum(newcaledonia2018$hours)

newcaledonia2018$log_hours <- log(newcaledonia2018$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2018.rds")
newcaledonia_preds <- predict(gamm1, newdata=newcaledonia2018, se.fit=T)

newcaledonia2018$log_hooks <- newcaledonia_preds$fit
newcaledonia2018$log_hooks.se <- newcaledonia_preds$se.fit

newcaledonia2018$hooks <- exp(newcaledonia2018$log_hooks)
sum(newcaledonia2018$hooks, na.rm=T) #7196309 hooks
7196309/1245000 #5.78 hooks/km2

#save
write.csv(newcaledonia2018, "newcaledonia2018.csv", row.names = F) 

#### plotting ####
# Hours

#aggregate across fleets
newcaledonia_2018 <- aggregate(data=newcaledonia_effort_2018, fishing_hours~cell_ll_lon+cell_ll_lat, FUN='sum')
newcaledonia_2018 <- subset(newcaledonia_2018, fishing_hours != 0)
newcaledonia_2018$log_fishing_hours=ifelse(newcaledonia_2018$fishing_hours < 10, 0, log10(newcaledonia_2018$fishing_hours))
range(newcaledonia_2018$fishing_hours)

world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

newcaledonia2018plot <- newcaledonia_2018 %>%
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

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/newcaledonialonglines2018.png", newcaledonia2018plot, width = 12, height = 8)

#### 2017 ####

# read in AIS data
effort2017 <- read.csv("2017effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_newcaledonia_2017 <- st_as_sf(effort2017,
                                                 coords = c("lon", "lat"),
                                                 crs = st_crs(newcaledonia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
newcaledonia_effort_2017 <- effort_longline_sf_newcaledonia_2017 %>% 
  sf::st_join(newcaledonia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'NCL') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
newcaledonia2017 <- newcaledonia_effort_2017

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
newcaledonia2017$fleet[is.na(newcaledonia2017$fleet)] <- "Unknown"

newcaledonia2017$fishing_hours_1000s=newcaledonia2017$fishing_hours/1000
sum(newcaledonia2017$fishing_hours)#89317.39
rate_newcaledonia = sum(newcaledonia2017$fishing_hours)/1245000 #0.072 hrs/km2

#### Hook projections ####
# data clean
colnames(newcaledonia2017)[2] <- "hours"
colnames(newcaledonia2017)[4] <- "lon"
colnames(newcaledonia2017)[5] <- "lat"
newcaledonia2017 <- subset(newcaledonia2017, hours > 0)
sum(newcaledonia2017$hours)

newcaledonia2017$log_hours <- log(newcaledonia2017$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2017.rds")
newcaledonia_preds <- predict(gamm1, newdata=newcaledonia2017, se.fit=T)

newcaledonia2017$log_hooks <- newcaledonia_preds$fit
newcaledonia2017$log_hooks.se <- newcaledonia_preds$se.fit

newcaledonia2017$hooks <- exp(newcaledonia2017$log_hooks)
sum(newcaledonia2017$hooks, na.rm=T) #4167177 hooks
4167177/1245000 #3.347 hooks/km2

#save
write.csv(newcaledonia2017, "newcaledonia2017.csv", row.names = F)

#### 2016 ####


# read in AIS data
effort2016 <- read.csv("2016effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_newcaledonia_2016 <- st_as_sf(effort2016,
                                                 coords = c("lon", "lat"),
                                                 crs = st_crs(newcaledonia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
newcaledonia_effort_2016 <- effort_longline_sf_newcaledonia_2016 %>% 
  sf::st_join(newcaledonia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'NCL') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
newcaledonia2016 <- newcaledonia_effort_2016

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
newcaledonia2016$fleet[is.na(newcaledonia2016$fleet)] <- "Unknown"

newcaledonia2016$fishing_hours_1000s=newcaledonia2016$fishing_hours/1000
sum(newcaledonia2016$fishing_hours)#90494.67
rate_newcaledonia = sum(newcaledonia2016$fishing_hours)/1245000 #0.073 hrs/km2

#### Hook projections ####
# data clean
colnames(newcaledonia2016)[2] <- "hours"
colnames(newcaledonia2016)[4] <- "lon"
colnames(newcaledonia2016)[5] <- "lat"
newcaledonia2016 <- subset(newcaledonia2016, hours > 0)
sum(newcaledonia2016$hours)

newcaledonia2016$log_hours <- log(newcaledonia2016$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2016.rds")
newcaledonia_preds <- predict(gamm1, newdata=newcaledonia2016, se.fit=T)

newcaledonia2016$log_hooks <- newcaledonia_preds$fit
newcaledonia2016$log_hooks.se <- newcaledonia_preds$se.fit

newcaledonia2016$hooks <- exp(newcaledonia2016$log_hooks)
sum(newcaledonia2016$hooks, na.rm=T) #6084749 hooks
6084749/1245000 #4.887 hooks/km2

#save
write.csv(newcaledonia2016, "newcaledonia2016.csv", row.names = F)

