library(tidyverse) 
library(sf) 

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#### Isolate AIS data to sanctuary only ####
# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select cookislands EEZ
cookislands <- eezs %>% 
  filter(ISO_Ter1 == 'COK' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
cookislands_bbox <- sf::st_bbox(cookislands)

# read in AIS data
effort2018 <- read.csv("2018effort_*25x*25.csv")

colnames(effort2018)[4] <- "fishing_hours"

#set as spatial frame
effort_longline_sf_cookislands_2018 <- st_as_sf(effort2018,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(cookislands))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
cookislands_effort_2018 <- effort_longline_sf_cookislands_2018 %>% 
  sf::st_join(cookislands, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'COK') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
cookislands2018 <- cookislands_effort_2018

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
cookislands2018$fleet[is.na(cookislands2018$fleet)] <- "Unknown"

cookislands2018$fishing_hours_1000s=cookislands2018$fishing_hours/1000
sum(cookislands2018$fishing_hours)#95634.27
rate_cookislands = sum(cookislands2018$fishing_hours)/1992232 #0.048 hrs/km2


#### Hook projections ####
# data clean
colnames(cookislands2018)[2] <- "hours"
colnames(cookislands2018)[4] <- "lon"
colnames(cookislands2018)[5] <- "lat"
cookislands2018 <- subset(cookislands2018, hours > 0)
sum(cookislands2018$hours)

cookislands2018$log_hours <- log(cookislands2018$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2018.rds")
cookislands_preds <- predict(gamm1, newdata=cookislands2018, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(cookislands2018$fleet), lat = 10, lon = 168)
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

#COK is average
cookislands2018$fleet <- ifelse(cookislands2018$fleet == "BEL", "COK", cookislands2018$fleet)

cookislands_preds <- predict(gamm1, newdata=cookislands2018, se.fit=T)
cookislands2018$log_hooks <- cookislands_preds$fit
cookislands2018$log_hooks.se <- cookislands_preds$se.fit

cookislands2018$hooks <- exp(cookislands2018$log_hooks)
sum(cookislands2018$hooks, na.rm=T) #18020888
18020888/1960135 #9.194 hooks/km2

#save
write.csv(cookislands2018, "cookislands2018.csv", row.names = F) 

#### 2017 ####

# read in AIS data
effort2017 <- read.csv("2017effort_*25x*25.csv")

colnames(effort2017)[4] <- "fishing_hours"

#set as spatial frame
effort_longline_sf_cookislands_2017 <- st_as_sf(effort2017,
                                                coords = c("lon", "lat"),
                                                crs = st_crs(cookislands))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
cookislands_effort_2017 <- effort_longline_sf_cookislands_2017 %>% 
  sf::st_join(cookislands, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'COK') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
cookislands2017 <- cookislands_effort_2017

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
cookislands2017$fleet[is.na(cookislands2017$fleet)] <- "Unknown"

cookislands2017$fishing_hours_1000s=cookislands2017$fishing_hours/1000
sum(cookislands2017$fishing_hours)#84501.16
rate_cookislands = sum(cookislands2017$fishing_hours)/1992232 #0.042 hrs/km2


#### Hook projections ####
# data clean
colnames(cookislands2017)[2] <- "hours"
colnames(cookislands2017)[4] <- "lon"
colnames(cookislands2017)[5] <- "lat"
cookislands2017 <- subset(cookislands2017, hours > 0)
sum(cookislands2017$hours)

cookislands2017$log_hours <- log(cookislands2017$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2017.rds")
cookislands_preds <- predict(gamm1, newdata=cookislands2017, se.fit=T)

cookislands2017$log_hooks <- cookislands_preds$fit
cookislands2017$log_hooks.se <- cookislands_preds$se.fit

cookislands2017$hooks <- exp(cookislands2017$log_hooks)
sum(cookislands2017$hooks, na.rm=T) #24222935
24222935/1960135 #12.358 hooks/km2

#save
write.csv(cookislands2017, "cookislands2017.csv", row.names = F) 

#### 2016 ####

# read in AIS data
effort2016 <- read.csv("2016effort_*25x*25.csv")

colnames(effort2016)[4] <- "fishing_hours"

#set as spatial frame
effort_longline_sf_cookislands_2016 <- st_as_sf(effort2016,
                                                coords = c("lon", "lat"),
                                                crs = st_crs(cookislands))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
cookislands_effort_2016 <- effort_longline_sf_cookislands_2016 %>% 
  sf::st_join(cookislands, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'COK') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
cookislands2016 <- cookislands_effort_2016

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
cookislands2016$fleet[is.na(cookislands2016$fleet)] <- "Unknown"

cookislands2016$fishing_hours_1000s=cookislands2016$fishing_hours/1000
sum(cookislands2016$fishing_hours)#98449.61
rate_cookislands = sum(cookislands2016$fishing_hours)/1992232 #0.049 hrs/km2


#### Hook projections ####
# data clean
colnames(cookislands2016)[2] <- "hours"
colnames(cookislands2016)[4] <- "lon"
colnames(cookislands2016)[5] <- "lat"
cookislands2016 <- subset(cookislands2016, hours > 0)
sum(cookislands2016$hours)

cookislands2016$log_hours <- log(cookislands2016$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2016.rds")
cookislands_preds <- predict(gamm1, newdata=cookislands2016, se.fit=T)

cookislands2016$log_hooks <- cookislands_preds$fit
cookislands2016$log_hooks.se <- cookislands_preds$se.fit

cookislands2016$hooks <- exp(cookislands2016$log_hooks)
sum(cookislands2016$hooks, na.rm=T) #23835100
23835100/1960135 #12.160 hooks/km2

#save
write.csv(cookislands2016, "cookislands2016.csv", row.names = F) 

