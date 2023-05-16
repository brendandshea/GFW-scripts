library(tidyverse) 
library(sf) 

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#### Isolate AIS data to sanctuary only ####
# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select micronesia EEZ
micronesia <- eezs %>% 
  filter(ISO_Ter1 == 'FSM' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
micronesia_bbox <- sf::st_bbox(micronesia)

# read in AIS data
effort2018 <- read.csv("2018effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_micronesia_2018 <- st_as_sf(effort2018,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(micronesia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
micronesia_effort_2018 <- effort_longline_sf_micronesia_2018 %>% 
  sf::st_join(micronesia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'FSM') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
micronesia2018 <- micronesia_effort_2018

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
micronesia2018$fleet[is.na(micronesia2018$fleet)] <- "Unknown"

micronesia2018$fishing_hours_1000s=micronesia2018$fishing_hours/1000
sum(micronesia2018$fishing_hours)#309183.1
rate_micronesia = sum(micronesia2018$fishing_hours)/2992597 #0.1033 hrs/km2


#### Hook projections ####
# data clean
colnames(micronesia2018)[2] <- "hours"
colnames(micronesia2018)[4] <- "lon"
colnames(micronesia2018)[5] <- "lat"
micronesia2018 <- subset(micronesia2018, hours > 0)
sum(micronesia2018$hours)

micronesia2018$log_hours <- log(micronesia2018$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2018.rds")
micronesia_preds <- predict(gamm1, newdata=micronesia2018, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(micronesia2018$fleet), lat = 7, lon = 151)
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

#FJI is average
micronesia2018$fleet <- ifelse(micronesia2018$fleet == "UNK", "FJI", micronesia2018$fleet)

micronesia_preds <- predict(gamm1, newdata=micronesia2018, se.fit=T)
micronesia2018$log_hooks <- micronesia_preds$fit
micronesia2018$log_hooks.se <- micronesia_preds$se.fit

micronesia2018$hooks <- exp(micronesia2018$log_hooks)
sum(micronesia2018$hooks, na.rm=T) #43916977 hooks
43916977/2992597 #14.675 hooks/km2

#save
write.csv(micronesia2018, "micronesia2018.csv", row.names = F) 

#### 2017 ####

# read in AIS data
effort2017 <- read.csv("2017effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_micronesia_2017 <- st_as_sf(effort2017,
                                               coords = c("lon", "lat"),
                                               crs = st_crs(micronesia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
micronesia_effort_2017 <- effort_longline_sf_micronesia_2017 %>% 
  sf::st_join(micronesia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'FSM') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
micronesia2017 <- micronesia_effort_2017

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
micronesia2017$fleet[is.na(micronesia2017$fleet)] <- "Unknown"

micronesia2017$fishing_hours_1000s=micronesia2017$fishing_hours/1000
sum(micronesia2017$fishing_hours)#215270.8
rate_micronesia = sum(micronesia2017$fishing_hours)/2992597 #0.0719


#### Hook projections ####
# data clean
colnames(micronesia2017)[2] <- "hours"
colnames(micronesia2017)[4] <- "lon"
colnames(micronesia2017)[5] <- "lat"
micronesia2017 <- subset(micronesia2017, hours > 0)
sum(micronesia2017$hours)

micronesia2017$log_hours <- log(micronesia2017$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2017.rds")
micronesia_preds <- predict(gamm1, newdata=micronesia2017, se.fit=T)

micronesia2017$log_hooks <- micronesia_preds$fit
micronesia2017$log_hooks.se <- micronesia_preds$se.fit

micronesia2017$hooks <- exp(micronesia2017$log_hooks)
sum(micronesia2017$hooks, na.rm=T) #30787526 hooks
30787526/2992597 #10.288 hooks/km2

#save
write.csv(micronesia2017, "micronesia2017.csv", row.names = F) 

#### 2016 ####

# read in AIS data
effort2016 <- read.csv("2016effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_micronesia_2016 <- st_as_sf(effort2016,
                                               coords = c("lon", "lat"),
                                               crs = st_crs(micronesia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
micronesia_effort_2016 <- effort_longline_sf_micronesia_2016 %>% 
  sf::st_join(micronesia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'FSM') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
micronesia2016 <- micronesia_effort_2016

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
micronesia2016$fleet[is.na(micronesia2016$fleet)] <- "Unknown"

micronesia2016$fishing_hours_1000s=micronesia2016$fishing_hours/1000
sum(micronesia2016$fishing_hours)#147106.9
rate_micronesia = sum(micronesia2016$fishing_hours)/2992597 #0.0492


#### Hook projections ####
# data clean
colnames(micronesia2016)[2] <- "hours"
colnames(micronesia2016)[4] <- "lon"
colnames(micronesia2016)[5] <- "lat"
micronesia2016 <- subset(micronesia2016, hours > 0)
sum(micronesia2016$hours)

micronesia2016$log_hours <- log(micronesia2016$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2016.rds")
micronesia_preds <- predict(gamm1, newdata=micronesia2016, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(micronesia2016$fleet), lat = 7, lon = 151)
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

#FSM is average
micronesia2016$fleet <- ifelse(micronesia2016$fleet == "IND", "FSM", micronesia2016$fleet)

micronesia_preds <- predict(gamm1, newdata=micronesia2016, se.fit=T)

micronesia2016$log_hooks <- micronesia_preds$fit
micronesia2016$log_hooks.se <- micronesia_preds$se.fit

micronesia2016$hooks <- exp(micronesia2016$log_hooks)
sum(micronesia2016$hooks, na.rm=T) #28051024 hooks
28051024/2992597 #9.373 hooks/km2

#save
write.csv(micronesia2016, "micronesia2016.csv", row.names = F) 
