library(tidyverse) 
library(sf) 

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#### Isolate AIS data to sanctuary only ####
# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select marshallislands EEZ
marshallislands <- eezs %>% 
  filter(ISO_Ter1 == 'MHL' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
marshallislands_bbox <- sf::st_bbox(marshallislands)

# read in AIS data
effort2018 <- read.csv("2018effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_marshallislands_2018 <- st_as_sf(effort2018,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(marshallislands))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
marshallislands_effort_2018 <- effort_longline_sf_marshallislands_2018 %>% 
  sf::st_join(marshallislands, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'MHL') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
marshallislands2018 <- marshallislands_effort_2018

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
marshallislands2018$fleet[is.na(marshallislands2018$fleet)] <- "Unknown"

marshallislands2018$fishing_hours_1000s=marshallislands2018$fishing_hours/1000
sum(marshallislands2018$fishing_hours)#247979.7
rate_marshallislands = sum(marshallislands2018$fishing_hours)/1992232 #0.1245 hrs/km2


#### Hook projections ####
# data clean
colnames(marshallislands2018)[2] <- "hours"
colnames(marshallislands2018)[4] <- "lon"
colnames(marshallislands2018)[5] <- "lat"
marshallislands2018 <- subset(marshallislands2018, hours > 0)
sum(marshallislands2018$hours)

marshallislands2018$log_hours <- log(marshallislands2018$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2018.rds")
marshallislands_preds <- predict(gamm1, newdata=marshallislands2018, se.fit=T)

marshallislands2018$log_hooks <- marshallislands_preds$fit
marshallislands2018$log_hooks.se <- marshallislands_preds$se.fit

marshallislands2018$hooks <- exp(marshallislands2018$log_hooks)
sum(marshallislands2018$hooks, na.rm=T) #53770493 hooks
53770493/1992232 #26.9901 hooks/km2

#save
write.csv(marshallislands2018, "marshallislands2018.csv", row.names = F) 

#### 2017 ####

# read in AIS data
effort2017 <- read.csv("2017effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_marshallislands_2017 <- st_as_sf(effort2017,
                                                    coords = c("lon", "lat"),
                                                    crs = st_crs(marshallislands))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
marshallislands_effort_2017 <- effort_longline_sf_marshallislands_2017 %>% 
  sf::st_join(marshallislands, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'MHL') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
marshallislands2017 <- marshallislands_effort_2017

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
marshallislands2017$fleet[is.na(marshallislands2017$fleet)] <- "Unknown"

marshallislands2017$fishing_hours_1000s=marshallislands2017$fishing_hours/1000
sum(marshallislands2017$fishing_hours)#274608.6
rate_marshallislands = sum(marshallislands2017$fishing_hours)/1992232 #0.1378 hrs/km2


#### Hook projections ####
# data clean
colnames(marshallislands2017)[2] <- "hours"
colnames(marshallislands2017)[4] <- "lon"
colnames(marshallislands2017)[5] <- "lat"
marshallislands2017 <- subset(marshallislands2017, hours > 0)
sum(marshallislands2017$hours)

marshallislands2017$log_hours <- log(marshallislands2017$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2017.rds")
marshallislands_preds <- predict(gamm1, newdata=marshallislands2017, se.fit=T)

marshallislands2017$log_hooks <- marshallislands_preds$fit
marshallislands2017$log_hooks.se <- marshallislands_preds$se.fit

marshallislands2017$hooks <- exp(marshallislands2017$log_hooks)
sum(marshallislands2017$hooks, na.rm=T) #46412653 hooks
46412653/1992232 #23.297 hooks/km2

#save
write.csv(marshallislands2017, "marshallislands2017.csv", row.names = F) 

#### 2016 ####

# read in AIS data
effort2016 <- read.csv("2016effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_marshallislands_2016 <- st_as_sf(effort2016,
                                                    coords = c("lon", "lat"),
                                                    crs = st_crs(marshallislands))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
marshallislands_effort_2016 <- effort_longline_sf_marshallislands_2016 %>% 
  sf::st_join(marshallislands, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'MHL') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
marshallislands2016 <- marshallislands_effort_2016

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
marshallislands2016$fleet[is.na(marshallislands2016$fleet)] <- "Unknown"

marshallislands2016$fishing_hours_1000s=marshallislands2016$fishing_hours/1000
sum(marshallislands2016$fishing_hours)#208325.5
rate_marshallislands = sum(marshallislands2016$fishing_hours)/1992232 #0.1046 hrs/km2


#### Hook projections ####
# data clean
colnames(marshallislands2016)[2] <- "hours"
colnames(marshallislands2016)[4] <- "lon"
colnames(marshallislands2016)[5] <- "lat"
marshallislands2016 <- subset(marshallislands2016, hours > 0)
sum(marshallislands2016$hours)

marshallislands2016$log_hours <- log(marshallislands2016$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2016.rds")
marshallislands_preds <- predict(gamm1, newdata=marshallislands2016, se.fit=T)

marshallislands2016$log_hooks <- marshallislands_preds$fit
marshallislands2016$log_hooks.se <- marshallislands_preds$se.fit

marshallislands2016$hooks <- exp(marshallislands2016$log_hooks)
sum(marshallislands2016$hooks, na.rm=T) #43328420 hooks
43328420/1992232 #21.749 hooks/km2

#save
write.csv(marshallislands2016, "marshallislands2016.csv", row.names = F) 