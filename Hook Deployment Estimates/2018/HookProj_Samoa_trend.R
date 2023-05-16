library(tidyverse) 
library(sf) 

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#### Isolate AIS data to sanctuary only ####
# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select samoa EEZ
samoa <- eezs %>% 
  filter(ISO_Ter1 == 'WSM' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
samoa_bbox <- sf::st_bbox(samoa)

# read in AIS data
effort2018 <- read.csv("2018effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_samoa_2018 <- st_as_sf(effort2018,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(samoa))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
samoa_effort_2018 <- effort_longline_sf_samoa_2018 %>% 
  sf::st_join(samoa, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'WSM') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

  sf::sf_use_s2(TRUE)

#Dataclean
samoa2018 <- samoa_effort_2018

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
samoa2018$fleet[is.na(samoa2018$fleet)] <- "Unknown"

samoa2018$fishing_hours_1000s=samoa2018$fishing_hours/1000
sum(samoa2018$fishing_hours)#47745.16
rate_samoa = sum(samoa2018$fishing_hours)/128000 #0.373 hrs/km2


#### Hook projections ####
# data clean
colnames(samoa2018)[2] <- "hours"
colnames(samoa2018)[4] <- "lon"
colnames(samoa2018)[5] <- "lat"
samoa2018 <- subset(samoa2018, hours > 0)
sum(samoa2018$hours)

samoa2018$log_hours <- log(samoa2018$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2018.rds")
samoa_preds <- predict(gamm1, newdata=samoa2018, se.fit=T)

samoa2018$log_hooks <- samoa_preds$fit
samoa2018$log_hooks.se <- samoa_preds$se.fit

samoa2018$hooks <- exp(samoa2018$log_hooks)
sum(samoa2018$hooks, na.rm=T) #6651364 hooks
6651364 /128000 #51.968 hooks/km2

#save
write.csv(samoa2018, "samoa2018.csv", row.names = F) 

#### 2017 ####

# read in AIS data
effort2017 <- read.csv("2017effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_samoa_2017 <- st_as_sf(effort2017,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(samoa))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
samoa_effort_2017 <- effort_longline_sf_samoa_2017 %>% 
  sf::st_join(samoa, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'WSM') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
samoa2017 <- samoa_effort_2017

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
samoa2017$fleet[is.na(samoa2017$fleet)] <- "Unknown"

samoa2017$fishing_hours_1000s=samoa2017$fishing_hours/1000
sum(samoa2017$fishing_hours)#39705.18
rate_samoa = sum(samoa2017$fishing_hours)/128000 #0.310 hrs/km2


#### Hook projections ####
# data clean
colnames(samoa2017)[2] <- "hours"
colnames(samoa2017)[4] <- "lon"
colnames(samoa2017)[5] <- "lat"
samoa2017 <- subset(samoa2017, hours > 0)
sum(samoa2017$hours)

samoa2017$log_hours <- log(samoa2017$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2017.rds")
samoa_preds <- predict(gamm1, newdata=samoa2017, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(samoa2017$fleet), lat = -13, lon = -173)
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
samoa2017$fleet <- ifelse(samoa2017$fleet == "KIR", "CHN", samoa2017$fleet)

samoa_preds <- predict(gamm1, newdata=samoa2017, se.fit=T)
samoa2017$log_hooks <- samoa_preds$fit
samoa2017$log_hooks.se <- samoa_preds$se.fit

samoa2017$hooks <- exp(samoa2017$log_hooks)
sum(samoa2017$hooks, na.rm=T) #8131678hooks
8131678/128000 #63.52873 hooks/km2

#save
write.csv(samoa2017, "samoa2017.csv", row.names = F) 

#### 2016 ####
# read in AIS data
effort2016 <- read.csv("2016effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_samoa_2016 <- st_as_sf(effort2016,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(samoa))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
samoa_effort_2016 <- effort_longline_sf_samoa_2016 %>% 
  sf::st_join(samoa, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'WSM') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
samoa2016 <- samoa_effort_2016

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
samoa2016$fleet[is.na(samoa2016$fleet)] <- "Unknown"

samoa2016$fishing_hours_1000s=samoa2016$fishing_hours/1000
sum(samoa2016$fishing_hours)#41417.46
rate_samoa = sum(samoa2016$fishing_hours)/128000 #0.324 hrs/km2


#### Hook projections ####
# data clean
colnames(samoa2016)[2] <- "hours"
colnames(samoa2016)[4] <- "lon"
colnames(samoa2016)[5] <- "lat"
samoa2016 <- subset(samoa2016, hours > 0)
sum(samoa2016$hours)

samoa2016$log_hours <- log(samoa2016$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2016.rds")
samoa_preds <- predict(gamm1, newdata=samoa2016, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(samoa2016$fleet), lat = -13, lon = -173)
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
samoa2016$fleet <- ifelse(samoa2016$fleet == "KIR", "FJI", samoa2016$fleet)

samoa_preds <- predict(gamm1, newdata=samoa2016, se.fit=T)
samoa2016$log_hooks <- samoa_preds$fit
samoa2016$log_hooks.se <- samoa_preds$se.fit

samoa2016$hooks <- exp(samoa2016$log_hooks)
sum(samoa2016$hooks, na.rm=T) #9609098 hooks
9609098/128000 #75.07 hooks/km2

#save
write.csv(samoa2016, "samoa2016.csv", row.names = F) 
