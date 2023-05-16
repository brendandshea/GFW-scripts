library(tidyverse) 
library(sf) 

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#### Isolate AIS data to sanctuary only ####
# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

# Select kiribati EEZ
kiribati <- eezs %>% 
  filter(ISO_Ter1 == 'KIR' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
kiribati_bbox <- sf::st_bbox(kiribati)

# read in AIS data
effort2018 <- read.csv("2018effort_*25x*25.csv")
colnames(effort2018)[4]<-"fishing_hours"

#set as spatial frame
effort_longline_sf_kiribati_2018 <- st_as_sf(effort2018,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(kiribati))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
kiribati_effort_2018 <- effort_longline_sf_kiribati_2018 %>% 
  sf::st_join(kiribati, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'KIR') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
kiribati2018 <- kiribati_effort_2018

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
kiribati2018$fleet[is.na(kiribati2018$fleet)] <- "Unknown"

kiribati2018$fishing_hours_1000s=kiribati2018$fishing_hours/1000
sum(kiribati2018$fishing_hours)#53840.74
rate_kiribati = sum(kiribati2018$fishing_hours)/3437132 #0.0157


#### Hook projections ####
# data clean
colnames(kiribati2018)[2] <- "hours"
colnames(kiribati2018)[4] <- "lon"
colnames(kiribati2018)[5] <- "lat"
kiribati2018 <- subset(kiribati2018, hours > 0)
sum(kiribati2018$hours) #53840.74

kiribati2018$log_hours <- log(kiribati2018$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2018.rds")
kiribati_preds <- predict(gamm1, newdata=kiribati2018, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(kiribati2018$fleet), lat = 0, lon = 186)
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
kiribati2018$fleet <- ifelse(kiribati2018$fleet == "SLV", "FJI", kiribati2018$fleet)

kiribati_preds <- predict(gamm1, newdata=kiribati2018, se.fit=T)
kiribati2018$log_hooks <- kiribati_preds$fit
kiribati2018$log_hooks.se <- kiribati_preds$se.fit

kiribati2018$hooks <- exp(kiribati2018$log_hooks)
sum(kiribati2018$hooks, na.rm=T) ##7356460 hooks
7356460 /3437132 #2.14 hooks/km2

#save
write.csv(kiribati2018, "kiribati2018.csv", row.names = F) 

#### plotting ####
# Hours

#aggregate across fleets
kiribati_2018 <- aggregate(data=kiribati_effort_2018, fishing_hours~cell_ll_lon+cell_ll_lat, FUN='sum')
kiribati_2018 <- subset(kiribati_2018, fishing_hours != 0)
kiribati_2018$log_fishing_hours=ifelse(kiribati_2018$fishing_hours < 10, 0, log10(kiribati_2018$fishing_hours))
range(kiribati_2018$fishing_hours)

world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

kiribati2018plot <- kiribati_2018 %>%
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
  labs(fill  = 'Fishing hours (log scale)',       title = 'kiribati'
  ) +
  guides(fill = guide_colourbar(barwidth = 10)) +
  coord_sf(xlim=c(128,138), ylim=c(0,13))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/kiribatilonglines2018.png", kiribati2018plot, width = 12, height = 8)

#### 2017 ####
# read in AIS data
effort2017 <- read.csv("2017effort_*25x*25.csv")
colnames(effort2017)[4]<-"fishing_hours"

#set as spatial frame
effort_longline_sf_kiribati_2017 <- st_as_sf(effort2017,
                                             coords = c("lon", "lat"),
                                             crs = st_crs(kiribati))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
kiribati_effort_2017 <- effort_longline_sf_kiribati_2017 %>% 
  sf::st_join(kiribati, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'KIR') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
kiribati2017 <- kiribati_effort_2017

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
kiribati2017$fleet[is.na(kiribati2017$fleet)] <- "Unknown"

kiribati2017$fishing_hours_1000s=kiribati2017$fishing_hours/1000
sum(kiribati2017$fishing_hours)#108303.5
rate_kiribati = sum(kiribati2017$fishing_hours)/3437132 #0.0315


#### Hook projections ####
# data clean
colnames(kiribati2017)[2] <- "hours"
colnames(kiribati2017)[4] <- "lon"
colnames(kiribati2017)[5] <- "lat"
kiribati2017 <- subset(kiribati2017, hours > 0)
sum(kiribati2017$hours) #108303.5

kiribati2017$log_hours <- log(kiribati2017$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2017.rds")
kiribati_preds <- predict(gamm1, newdata=kiribati2017, se.fit=T)

kiribati2017$log_hooks <- kiribati_preds$fit
kiribati2017$log_hooks.se <- kiribati_preds$se.fit

kiribati2017$hooks <- exp(kiribati2017$log_hooks)
sum(kiribati2017$hooks, na.rm=T) ##16758707 hooks
16758707 /3437132 #4.876 hooks/km2

#save
write.csv(kiribati2017, "kiribati2017.csv", row.names = F)

#### 2016 ####

effort2016 <- read.csv("2016effort_*25x*25.csv")
colnames(effort2016)[4]<-"fishing_hours"

#set as spatial frame
effort_longline_sf_kiribati_2016 <- st_as_sf(effort2016,
                                             coords = c("lon", "lat"),
                                             crs = st_crs(kiribati))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
kiribati_effort_2016 <- effort_longline_sf_kiribati_2016 %>% 
  sf::st_join(kiribati, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'KIR') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
kiribati2016 <- kiribati_effort_2016

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
kiribati2016$fleet[is.na(kiribati2016$fleet)] <- "Unknown"

kiribati2016$fishing_hours_1000s=kiribati2016$fishing_hours/1000
sum(kiribati2016$fishing_hours)#490972.2
rate_kiribati = sum(kiribati2016$fishing_hours)/3437132 #0.143


#### Hook projections ####
# data clean
colnames(kiribati2016)[2] <- "hours"
colnames(kiribati2016)[4] <- "lon"
colnames(kiribati2016)[5] <- "lat"
kiribati2016 <- subset(kiribati2016, hours > 0)
sum(kiribati2016$hours) #490972.2

kiribati2016$log_hours <- log(kiribati2016$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm_2016.rds")
kiribati_preds <- predict(gamm1, newdata=kiribati2016, se.fit=T)

kiribati2016$log_hooks <- kiribati_preds$fit
kiribati2016$log_hooks.se <- kiribati_preds$se.fit

kiribati2016$hooks <- exp(kiribati2016$log_hooks)
sum(kiribati2016$hooks, na.rm=T) ##73992361 hooks
73992361 /3437132 #21.53736 hooks/km2

#save
write.csv(kiribati2016, "kiribati2016.csv", row.names = F)
