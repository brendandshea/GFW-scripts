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
effort2018 <- read.csv("2018effort_*25x*25.csv")
colnames(effort2018)[4]<-"fishing_hours"


#set as spatial frame
effort_longline_sf_palau_2018 <- st_as_sf(effort2018,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(palau))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
palau_effort_2018 <- effort_longline_sf_palau_2018 %>% 
  sf::st_join(palau, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'PLW') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
palau2018 <- palau_effort_2018

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
palau2018$fleet[is.na(palau2018$fleet)] <- "Unknown"

palau2018$fishing_hours_1000s=palau2018$fishing_hours/1000
sum(palau2018$fishing_hours)#162293.1
rate_palau = sum(palau2018$fishing_hours)/604289  #0.269


#### Hook projections ####
# data clean
colnames(palau2018)[2] <- "hours"
colnames(palau2018)[4] <- "lon"
colnames(palau2018)[5] <- "lat"
palau2018 <- subset(palau2018, hours > 0)
sum(palau2018$hours)

palau2018$log_hours <- log(palau2018$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2018.rds")
palau_preds <- predict(gamm1, newdata=palau2018, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(palau2018$fleet), lat = 6, lon = 133)
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
palau2018$fleet <- ifelse(palau2018$fleet == "ALB" | palau2018$fleet == "UNK", "CHN", palau2018$fleet)

palau_preds <- predict(gamm1, newdata=palau2018, se.fit=T)

palau2018$log_hooks <- palau_preds$fit
palau2018$log_hooks.se <- palau_preds$se.fit

palau2018$hooks <- exp(palau2018$log_hooks)
sum(palau2018$hooks, na.rm=T) #13698303 hooks
13698303/604289 #22.67 hooks/km2

#save
write.csv(palau2018, "palau2018.csv", row.names = F) 

#### 2017 ####
# read in AIS data
effort2017 <- read.csv("2017effort_*25x*25.csv")
colnames(effort2017)[4]<-"fishing_hours"


#set as spatial frame
effort_longline_sf_palau_2017 <- st_as_sf(effort2017,
                                                    coords = c("lon", "lat"),
                                                    crs = st_crs(palau))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
palau_effort_2017 <- effort_longline_sf_palau_2017 %>% 
  sf::st_join(palau, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'PLW') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
palau2017 <- palau_effort_2017

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
palau2017$fleet[is.na(palau2017$fleet)] <- "Unknown"

palau2017$fishing_hours_1000s=palau2017$fishing_hours/1000
sum(palau2017$fishing_hours)#101089.5
rate_palau = sum(palau2017$fishing_hours)/604289  #0.167


#### Hook projections ####
# data clean
colnames(palau2017)[2] <- "hours"
colnames(palau2017)[4] <- "lon"
colnames(palau2017)[5] <- "lat"
palau2017 <- subset(palau2017, hours > 0)
sum(palau2017$hours)

palau2017$log_hours <- log(palau2017$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2017.rds")
palau_preds <- predict(gamm1, newdata=palau2017, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(palau2017$fleet), lat = 6, lon = 133)
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
palau2017$fleet <- ifelse(palau2017$fleet == "ALB" | palau2017$fleet == "UNK", "FSM", palau2017$fleet)

palau_preds <- predict(gamm1, newdata=palau2017, se.fit=T)

palau2017$log_hooks <- palau_preds$fit
palau2017$log_hooks.se <- palau_preds$se.fit

palau2017$hooks <- exp(palau2017$log_hooks)
sum(palau2017$hooks, na.rm=T) #12347320 hooks
12347320/604289 #20.43 hooks/km2

#save
write.csv(palau2017, "palau2017.csv", row.names = F) 

#### 2016 ####
effort2016 <- read.csv("2016effort_*25x*25.csv")
colnames(effort2016)[4]<-"fishing_hours"


#set as spatial frame
effort_longline_sf_palau_2016 <- st_as_sf(effort2016,
                                                    coords = c("lon", "lat"),
                                                    crs = st_crs(palau))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
palau_effort_2016 <- effort_longline_sf_palau_2016 %>% 
  sf::st_join(palau, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'PLW') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
palau2016 <- palau_effort_2016

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
palau2016$fleet[is.na(palau2016$fleet)] <- "Unknown"

palau2016$fishing_hours_1000s=palau2016$fishing_hours/1000
sum(palau2016$fishing_hours)#51063.39
rate_palau = sum(palau2016$fishing_hours)/604289  #0.0845


#### Hook projections ####
# data clean
colnames(palau2016)[2] <- "hours"
colnames(palau2016)[4] <- "lon"
colnames(palau2016)[5] <- "lat"
palau2016 <- subset(palau2016, hours > 0)
sum(palau2016$hours)

palau2016$log_hours <- log(palau2016$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2016.rds")
palau_preds <- predict(gamm1, newdata=palau2016, se.fit=T)

palau2016$log_hooks <- palau_preds$fit
palau2016$log_hooks.se <- palau_preds$se.fit

palau2016$hooks <- exp(palau2016$log_hooks)
sum(palau2016$hooks, na.rm=T) #9723313 hooks
9723313/604289 #16.09 hooks/km2

#save
write.csv(palau2016, "palau2016.csv", row.names = F) 
