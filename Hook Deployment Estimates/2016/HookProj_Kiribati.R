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
effort2019 <- read.csv("2019effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_kiribati_2019 <- st_as_sf(effort2019,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(kiribati))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
kiribati_effort_2019 <- effort_longline_sf_kiribati_2019 %>% 
  sf::st_join(kiribati, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'KIR') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
kiribati2019 <- kiribati_effort_2019

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
kiribati2019$fleet[is.na(kiribati2019$fleet)] <- "Unknown"

kiribati2019$fishing_hours_1000s=kiribati2019$fishing_hours/1000
sum(kiribati2019$fishing_hours)#57880.54
rate_kiribati = sum(kiribati2019$fishing_hours)/3437132 #0.0168398 hrs/km2


#### Hook projections ####
# data clean
colnames(kiribati2019)[3] <- "hours"
colnames(kiribati2019)[5] <- "lon"
colnames(kiribati2019)[6] <- "lat"
kiribati2019 <- subset(kiribati2019, hours > 0)
sum(kiribati2019$hours)

kiribati2019$log_hours <- log(kiribati2019$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm.rds")
kiribati_preds <- predict(gamm1, newdata=kiribati2019, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(kiribati2019$fleet), lat = 0, lon = 186)
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
kiribati2019$fleet <- ifelse(kiribati2019$fleet == "Unknown", "CHN", kiribati2019$fleet)

kiribati_preds <- predict(gamm1, newdata=kiribati2019, se.fit=T)
kiribati2019$log_hooks <- kiribati_preds$fit
kiribati2019$log_hooks.se <- kiribati_preds$se.fit

kiribati2019$hooks <- exp(kiribati2019$log_hooks)
sum(kiribati2019$hooks, na.rm=T) #21239355 hooks
21239355 /3437132 #6.179383 hooks/km2

#save
write.csv(kiribati2019, "kiribati2019.csv", row.names = F) 

#### plotting ####
# Hours

#aggregate across fleets
kiribati_2019 <- aggregate(data=kiribati_effort_2019, fishing_hours~cell_ll_lon+cell_ll_lat, FUN='sum')
kiribati_2019 <- subset(kiribati_2019, fishing_hours != 0)
kiribati_2019$log_fishing_hours=ifelse(kiribati_2019$fishing_hours < 10, 0, log10(kiribati_2019$fishing_hours))
range(kiribati_2019$fishing_hours)

world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

kiribati2019plot <- kiribati_2019 %>%
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

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/kiribatilonglines2019.png", kiribati2019plot, width = 12, height = 8)

#plotting
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
kiribati_ais <- read.csv("kiribati2019.csv")
kiribati_proj <- kiribati_ais
kiribati_proj$thooks = kiribati_proj$hooks/1000
dev.off()
world <- map_data("world")
kiribati_proj$maplon <- ifelse(kiribati_proj$lon < 0, kiribati_proj$lon + 360, kiribati_proj$lon)


kiribati_map <- ggplot(data=kiribati_proj, aes(x=maplon, y=lat)) +
  geom_tile(aes(fill=thooks)) +
  scale_fill_gradient(name="Thousands of Longline \nHooks Deployed", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  scale_x_continuous(limits=c(167,215), breaks=seq(170,210,by=5), labels=c(170,175,180,-175,-170,-165,-160,-155,-150)) +
  scale_y_continuous(limits=c(-14.5,9), breaks=seq(-10,5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Kiribati")

kir_shp$long <- ifelse(kir_shp$long<0, kir_shp$long+360,kir_shp$long)
map.layer <-   geom_polygon(data = kir_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) 

kiribati_map <- kiribati_map + map.layer

kiribati_map