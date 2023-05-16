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
effort2018 <- read.csv("2018effort_*25x*25.csv")
colnames(effort2018)[4]<-"fishing_hours"


#set as spatial frame
effort_longline_sf_frenchpolynesia_2018 <- st_as_sf(effort2018,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(frenchpolynesia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
frenchpolynesia_effort_2018 <- effort_longline_sf_frenchpolynesia_2018 %>% 
  sf::st_join(frenchpolynesia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'PYF') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
frenchpolynesia2018 <- frenchpolynesia_effort_2018

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
frenchpolynesia2018$fleet[is.na(frenchpolynesia2018$fleet)] <- "Unknown"

frenchpolynesia2018$fishing_hours_1000s=frenchpolynesia2018$fishing_hours/1000
sum(frenchpolynesia2018$fishing_hours)#62895.47
rate_frenchpolynesia = sum(frenchpolynesia2018$fishing_hours)/4767242 #0.0132


#### Hook projections ####
# data clean
colnames(frenchpolynesia2018)[2] <- "hours"
colnames(frenchpolynesia2018)[4] <- "lon"
colnames(frenchpolynesia2018)[5] <- "lat"
frenchpolynesia2018 <- subset(frenchpolynesia2018, hours > 0)
sum(frenchpolynesia2018$hours)

frenchpolynesia2018$log_hours <- log(frenchpolynesia2018$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2018.rds")
frenchpolynesia_preds <- predict(gamm1, newdata=frenchpolynesia2018, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(frenchpolynesia2018$fleet), lat = -17, lon = -143)
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

#CHN is average
frenchpolynesia2018$fleet <- ifelse(frenchpolynesia2018$fleet == "BEL", "CHN", frenchpolynesia2018$fleet)

frenchpolynesia_preds <- predict(gamm1, newdata=frenchpolynesia2018, se.fit=T)

frenchpolynesia2018$log_hooks <- frenchpolynesia_preds$fit
frenchpolynesia2018$log_hooks.se <- frenchpolynesia_preds$se.fit

frenchpolynesia2018$hooks <- exp(frenchpolynesia2018$log_hooks)
sum(frenchpolynesia2018$hooks, na.rm=T) #21800978 hooks
21800978/4767242 #4.57 hooks/km2

#save
write.csv(frenchpolynesia2018, "frenchpolynesia2018.csv", row.names = F) 

#### 2017 ####
# read in AIS data
effort2017 <- read.csv("2017effort_*25x*25.csv")
colnames(effort2017)[4]<-"fishing_hours"


#set as spatial frame
effort_longline_sf_frenchpolynesia_2017 <- st_as_sf(effort2017,
                                                    coords = c("lon", "lat"),
                                                    crs = st_crs(frenchpolynesia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
frenchpolynesia_effort_2017 <- effort_longline_sf_frenchpolynesia_2017 %>% 
  sf::st_join(frenchpolynesia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'PYF') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
frenchpolynesia2017 <- frenchpolynesia_effort_2017

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
frenchpolynesia2017$fleet[is.na(frenchpolynesia2017$fleet)] <- "Unknown"

frenchpolynesia2017$fishing_hours_1000s=frenchpolynesia2017$fishing_hours/1000
sum(frenchpolynesia2017$fishing_hours)#63328.82
rate_frenchpolynesia = sum(frenchpolynesia2017$fishing_hours)/4767242 #0.0133


#### Hook projections ####
# data clean
colnames(frenchpolynesia2017)[2] <- "hours"
colnames(frenchpolynesia2017)[4] <- "lon"
colnames(frenchpolynesia2017)[5] <- "lat"
frenchpolynesia2017 <- subset(frenchpolynesia2017, hours > 0)
sum(frenchpolynesia2017$hours)

frenchpolynesia2017$log_hours <- log(frenchpolynesia2017$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2017.rds")
frenchpolynesia_preds <- predict(gamm1, newdata=frenchpolynesia2017, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(frenchpolynesia2017$fleet), lat = -17, lon = -143)
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

#CHN is average
frenchpolynesia2017$fleet <- ifelse(frenchpolynesia2017$fleet == "BEL", "FJI", frenchpolynesia2017$fleet)

frenchpolynesia_preds <- predict(gamm1, newdata=frenchpolynesia2017, se.fit=T)

frenchpolynesia2017$log_hooks <- frenchpolynesia_preds$fit
frenchpolynesia2017$log_hooks.se <- frenchpolynesia_preds$se.fit

frenchpolynesia2017$hooks <- exp(frenchpolynesia2017$log_hooks)
sum(frenchpolynesia2017$hooks, na.rm=T) #25041044 hooks
25041044/4767242 #5.25 hooks/km2

#save
write.csv(frenchpolynesia2017, "frenchpolynesia2017.csv", row.names = F) 

#### 2016 ####
effort2016 <- read.csv("2016effort_*25x*25.csv")
colnames(effort2016)[4]<-"fishing_hours"


#set as spatial frame
effort_longline_sf_frenchpolynesia_2016 <- st_as_sf(effort2016,
                                                    coords = c("lon", "lat"),
                                                    crs = st_crs(frenchpolynesia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
frenchpolynesia_effort_2016 <- effort_longline_sf_frenchpolynesia_2016 %>% 
  sf::st_join(frenchpolynesia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'PYF') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
frenchpolynesia2016 <- frenchpolynesia_effort_2016

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
frenchpolynesia2016$fleet[is.na(frenchpolynesia2016$fleet)] <- "Unknown"

frenchpolynesia2016$fishing_hours_1000s=frenchpolynesia2016$fishing_hours/1000
sum(frenchpolynesia2016$fishing_hours)#60068.39
rate_frenchpolynesia = sum(frenchpolynesia2016$fishing_hours)/4767242 #0.0133


#### Hook projections ####
# data clean
colnames(frenchpolynesia2016)[2] <- "hours"
colnames(frenchpolynesia2016)[4] <- "lon"
colnames(frenchpolynesia2016)[5] <- "lat"
frenchpolynesia2016 <- subset(frenchpolynesia2016, hours > 0)
sum(frenchpolynesia2016$hours)

frenchpolynesia2016$log_hours <- log(frenchpolynesia2016$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2016.rds")
frenchpolynesia_preds <- predict(gamm1, newdata=frenchpolynesia2016, se.fit=T)

frenchpolynesia2016$log_hooks <- frenchpolynesia_preds$fit
frenchpolynesia2016$log_hooks.se <- frenchpolynesia_preds$se.fit

frenchpolynesia2016$hooks <- exp(frenchpolynesia2016$log_hooks)
sum(frenchpolynesia2016$hooks, na.rm=T) #26481179 hooks
26481179/4767242 #5.25 hooks/km2

#save
write.csv(frenchpolynesia2016, "frenchpolynesia2016.csv", row.names = F) 
