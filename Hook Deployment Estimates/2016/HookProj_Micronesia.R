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
effort2019 <- read.csv("2019effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_micronesia_2019 <- st_as_sf(effort2019,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(micronesia))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
micronesia_effort_2019 <- effort_longline_sf_micronesia_2019 %>% 
  sf::st_join(micronesia, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'FSM') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
micronesia2019 <- micronesia_effort_2019

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
micronesia2019$fleet[is.na(micronesia2019$fleet)] <- "Unknown"

micronesia2019$fishing_hours_1000s=micronesia2019$fishing_hours/1000
sum(micronesia2019$fishing_hours)#159947.7
rate_micronesia = sum(micronesia2019$fishing_hours)/2992597 #0.0534478 hrs/km2


#### Hook projections ####
# data clean
colnames(micronesia2019)[3] <- "hours"
colnames(micronesia2019)[5] <- "lon"
colnames(micronesia2019)[6] <- "lat"
micronesia2019 <- subset(micronesia2019, hours > 0)
sum(micronesia2019$hours)

micronesia2019$log_hours <- log(micronesia2019$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm.rds")
micronesia_preds <- predict(gamm1, newdata=micronesia2019, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(micronesia2019$fleet), lat = 7, lon = 151)
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
micronesia2019$fleet <- ifelse(micronesia2019$fleet == "Unknown", "FSM", micronesia2019$fleet)

micronesia_preds <- predict(gamm1, newdata=micronesia2019, se.fit=T)
micronesia2019$log_hooks <- micronesia_preds$fit
micronesia2019$log_hooks.se <- micronesia_preds$se.fit

micronesia2019$hooks <- exp(micronesia2019$log_hooks)
sum(micronesia2019$hooks, na.rm=T) #74269399 hooks
74269399/2992597 #24.81771 hooks/km2

#save
write.csv(micronesia2019, "micronesia2019.csv", row.names = F) 

#### plotting ####
# Hours

#aggregate across fleets
micronesia_2019 <- aggregate(data=micronesia_effort_2019, fishing_hours~cell_ll_lon+cell_ll_lat, FUN='sum')
micronesia_2019 <- subset(micronesia_2019, fishing_hours != 0)
micronesia_2019$log_fishing_hours=ifelse(micronesia_2019$fishing_hours < 10, 0, log10(micronesia_2019$fishing_hours))
range(micronesia_2019$fishing_hours)

world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

micronesia2019plot <- micronesia_2019 %>%
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
  labs(fill  = 'Fishing hours (log scale)',       title = 'micronesia'
  ) +
  guides(fill = guide_colourbar(barwidth = 10)) +
  coord_sf(xlim=c(132,168), ylim=c(-3,14.5))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/micronesialonglines2019.png", micronesia2019plot, width = 12, height = 8)

#### Hooks plot ####

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
micronesia_ais <- read.csv("micronesia2019.csv")
micronesia_proj <- micronesia_ais
micronesia_proj$thooks = micronesia_proj$hooks/1000
dev.off()
world <- map_data("world")
micronesia_map <- ggplot(data=micronesia_proj, aes(x=lon, y=lat)) +
  geom_tile(aes(fill=thooks)) +
  scale_fill_gradient(name="Thousands of Longline \nHooks Deployed", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = fsm_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  #geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(134,167), breaks=seq(135,165,by=5)) +
  scale_y_continuous(limits=c(-3,14.5), breaks=seq(0,10,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Federated States of Micronesia")


micronesia_map
