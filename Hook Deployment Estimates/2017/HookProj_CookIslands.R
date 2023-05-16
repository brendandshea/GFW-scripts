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
effort2019 <- read.csv("2019effort_*25x*25.csv")

#set as spatial frame
effort_longline_sf_cookislands_2019 <- st_as_sf(effort2019,
                                          coords = c("lon", "lat"),
                                          crs = st_crs(cookislands))
sf::sf_use_s2(FALSE)

#spatial join to isolate fishing effort in EEZ
cookislands_effort_2019 <- effort_longline_sf_cookislands_2019 %>% 
  sf::st_join(cookislands, join = st_intersects) %>% # use a spatial join
  filter(ISO_Ter1 == 'COK') %>% # filter only datapoints within EEZ
  bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
  rename(cell_ll_lat = Y,
         cell_ll_lon = X) %>%
  st_set_geometry(NULL)

sf::sf_use_s2(TRUE)

#Dataclean
cookislands2019 <- cookislands_effort_2019

#Change NAs to Unknown - this allows the rows to be used in model predictions, it just zeroes the random effect
cookislands2019$fleet[is.na(cookislands2019$fleet)] <- "Unknown"

cookislands2019$fishing_hours_1000s=cookislands2019$fishing_hours/1000
sum(cookislands2019$fishing_hours)#87370.6
rate_cookislands = sum(cookislands2019$fishing_hours)/1992232 #0.0438556 hrs/km2


#### Hook projections ####
# data clean
colnames(cookislands2019)[3] <- "hours"
colnames(cookislands2019)[5] <- "lon"
colnames(cookislands2019)[6] <- "lat"
cookislands2019 <- subset(cookislands2019, hours > 0)
sum(cookislands2019$hours)

cookislands2019$log_hours <- log(cookislands2019$hours)

#hook projections
gamm1 <- readRDS("hooks_gamm.rds")
cookislands_preds <- predict(gamm1, newdata=cookislands2019, se.fit=T)

#determine "average" fleet to asssign to missing fleets
newdata = data.frame(hours = 100, fleet = unique(cookislands2019$fleet), lat = 10, lon = 168)
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

#TWN is average
cookislands2019$fleet <- ifelse(cookislands2019$fleet == "Unknown", "TWN", cookislands2019$fleet)

cookislands_preds <- predict(gamm1, newdata=cookislands2019, se.fit=T)
cookislands2019$log_hooks <- cookislands_preds$fit
cookislands2019$log_hooks.se <- cookislands_preds$se.fit

cookislands2019$hooks <- exp(cookislands2019$log_hooks)
sum(cookislands2019$hooks, na.rm=T) #26209420hooks
26209420/1960135 #12.37123 hooks/km2

#save
write.csv(cookislands2019, "cookislands2019.csv", row.names = F) 

#### plotting ####
# Hours

#aggregate across fleets
cookislands_2019 <- aggregate(data=cookislands_effort_2019, fishing_hours~cell_ll_lon+cell_ll_lat, FUN='sum')
cookislands_2019 <- subset(cookislands_2019, fishing_hours != 0)
cookislands_2019$log_fishing_hours=ifelse(cookislands_2019$fishing_hours < 10, 0, log10(cookislands_2019$fishing_hours))
range(cookislands_2019$fishing_hours)


world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

cookislands2019plot <- cookislands_2019 %>%
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
  labs(fill  = 'Fishing hours (log scale)',       title = 'Cook Islands'
  ) +
  guides(fill = guide_colourbar(barwidth = 10)) +
  coord_sf(xlim=c(-169,-153), ylim=c(-27,-5))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/cookislandslonglines2019.png", cookislands2019plot, width = 12, height = 8)


#### Hooks plot ####

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
cookislands_ais <- read.csv("cookislands2019.csv")
cookislands_proj <- cookislands_ais
cookislands_proj$thooks = cookislands_proj$hooks/1000
dev.off()
world <- map_data("world")
cookislands2019_map <- ggplot(data=cookislands_proj, aes(x=lon, y=lat)) +
  geom_tile(aes(fill=thooks)) +
  scale_fill_gradient(name="Thousands of Longline \nHooks Deployed", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = cok_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, linewidth = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(-169,-153), breaks=seq(-165,-155,by=5)) +
  scale_y_continuous(limits=c(-27,-5), breaks=seq(-25,-10,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Cook Islands")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/cookislandslonglines_hooks2019.png", cookislands2019_map, width = 12, height = 8)


