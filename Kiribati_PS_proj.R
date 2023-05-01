library(tidyverse) 
library(viridis)
library(gfwr)

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJWaXJnaW5pYSBUZWNoIEZlcnJldHRpIExhYiIsInVzZXJJZCI6MTk4NzgsImFwcGxpY2F0aW9uTmFtZSI6IlZpcmdpbmlhIFRlY2ggRmVycmV0dGkgTGFiIiwiaWQiOjExNywidHlwZSI6InVzZXItYXBwbGljYXRpb24ifSwiaWF0IjoxNjYwNzU0MjY2LCJleHAiOjE5NzYxMTQyNjYsImF1ZCI6ImdmdyIsImlzcyI6ImdmdyJ9.e4nWtHsEV57JimuKMtY9aS1zF2GrntrlTBg7fpjeiHl7STMQUPA1JNQHRekP4LsletdmLkT5So4hfaRLkE0JwXDUN1A6XpymggTv5ekNU4qJF6LlgtzovyZsP74SZt9ZtB0AH2_xLcMW130b6SxPFAwGwGVS_pY4EHebt3jO-YV-Qzb0iPJ3uqifTPzOnRGu8BV2j-LvFCD9WJrenBYv2OtBA3rj2Irgv2SC64RikPA2eKEt1ACgfy-XVk4VSXlZgqN8GLHjG7GQ0zVcNwuZ2QoGIGS3hHUlARl_5mTF7eN0M4jQHjhFs6QnVvYXbPrUZxaNLZGTn8TDwGRiN2qLPtyx80TDzNgBJaHE3uZ_d9VEkNHFmDHuaS9f2j9neCR-KizHFud67D7XD7Ro8nvtmK-cnGDvl0jJ6owFkKYeqRuRgbXiNaJlN-cJtv4LlheY_ZUC7nGLtKjAJnDIDyiqVHzG5FhTYFoBEzrB1iQSDelnzodqPip1MvhBHb6qpcr4"

#Gilbert
(code_eez <- get_region_id(region_name="gilbert", region_source = 'eez', key=key))
#8488

gilbertPS.2019 <- get_raster(spatial_resolution = 'high', #0.1 degree res
                  temporal_resolution = 'yearly',
                  group_by = 'flagAndGearType',
                  date_range = '2019-01-01,2020-01-01',
                  region = 8488, #codes for Marshall Islands
                  region_source = 'eez',
                  key = key)

gilbertPS.2019 <- subset(gilbertPS.2019 , Geartype == "tuna_purse_seines" | Geartype == "other_purse_seines")

#Phoenix
(code_eez <- get_region_id(region_name="phoenix", region_source = 'eez', key=key))
#8450
phoenixPS.2019 <- get_raster(spatial_resolution = 'high', #0.1 degree res
                             temporal_resolution = 'yearly',
                             group_by = 'flagAndGearType',
                             date_range = '2019-01-01,2020-01-01',
                             region = 8450, #codes for Marshall Islands
                             region_source = 'eez',
                             key = key)

phoenixPS.2019 <- subset(phoenixPS.2019 , Geartype == "tuna_purse_seines" | Geartype == "other_purse_seines")

#Line
(code_eez <- get_region_id(region_name="line", region_source = 'eez', key=key))
#8441

linePS.2019 <- get_raster(spatial_resolution = 'high', #0.1 degree res
                             temporal_resolution = 'yearly',
                             group_by = 'flagAndGearType',
                             date_range = '2019-01-01,2020-01-01',
                             region = 8441, #codes for Marshall Islands
                             region_source = 'eez',
                             key = key)

linePS.2019 <- subset(linePS.2019 , Geartype == "tuna_purse_seines" | Geartype == "other_purse_seines")

kiribatiPS.2019 <- rbind(gilbertPS.2019,linePS.2019,phoenixPS.2019)

colnames(kiribatiPS.2019)[4] <- "flag"
colnames(kiribatiPS.2019)[6] <- "hours"

kiribatiPS.2019$Lon <- plyr::round_any(kiribatiPS.2019$Lon, 0.25, f = floor)
kiribatiPS.2019$Lat <- plyr::round_any(kiribatiPS.2019$Lat, 0.25, f = floor)
kiribatiPS.2019 <- aggregate(kiribatiPS.2019, hours ~ Lon + Lat + flag, FUN=sum)
kiribatiPS.2019$flag <- ifelse(is.na(kiribatiPS.2019$flag), "Unknown", kiribatiPS.2019$flag)

sum(kiribatiPS.2019$hours) #72711 hours

#### Sets projections ####
# data clean
kiribati2019 <- subset(kiribatiPS.2019, hours > 0)
kiribati2019$log_hours <- log(kiribati2019$hours)
kiribati2019$Lon2 <- ifelse(kiribati2019$Lon < 0, kiribati2019$Lon + 360, kiribati2019$Lon)

#set projections
gamm1 <- readRDS("PS_gamm_2019.rds")
kiribati_preds <- predict(gamm1, newdata=kiribati2019, se.fit=T)
kiribati2019$log_sets <- kiribati_preds$fit
kiribati2019$log_sets.se <- kiribati_preds$se.fit

kiribati2019$sets <- exp(kiribati2019$log_sets)
sum(kiribati2019$sets, na.rm=T) 

# 47740.85 sets

#save
write.csv(kiribati2019, "kiribati2019.csv", row.names = F) 

#### plotting ####
# Hours
world <- map_data("world")

#aggregate across fleets
kiribati_2019 <- aggregate(data=kiribatiPS.2019, hours~Lon+Lat, FUN='sum')
kiribati_2019 <- subset(kiribati_2019, hours != 0)
kiribati_2019$log_hours=log(kiribati_2019$hours)
range(kiribati_2019$hours)
kiribati_2019 <- subset(kiribati_2019, hours >= 1)

world2 <- map_data("world2")

kiribati2019plot <- kiribati_2019 %>%
  ggplot() +
  geom_tile(aes(x = ifelse(Lon<0, Lon+360.125, Lon+0.125), y = Lat+0.125, fill = log_hours),
            height=0.25, width=0.25) +
  geom_polygon(data = kir_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world2, map = world2, aes(long, lat, map_id = region)) +
  scale_fill_viridis(name="Purse Seine \nFishing Hours", option='C', begin=0.2,
                     breaks=c(log(1),log(10),log(100),log(500)),
                     labels=c(1,10,100,500),
                     limits=c(log(0.9),log(502))) +
  scale_x_continuous(limits=c(167,215), breaks=seq(170,210,by=5), labels=c(170,175,180,-175,-170,-165,-160,-155,-150)) +
  scale_y_continuous(limits=c(-14.5,9), breaks=seq(-10,5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Kiribati")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/kiribatiPS2019.png", kiribati2019plot, width = 12, height = 8)

#### Sets Plot ####
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
kiribati_ais <- read.csv("kiribati2019.csv")
kiribati_proj <- kiribati_ais
kiribati_map <- ggplot(data=kiribati_proj, aes(x=ifelse(Lon<0, Lon+360.125, Lon+0.125), y=Lat+0.125)) +
  geom_tile(aes(fill=sets), height=0.25, width=0.25) +
  scale_fill_gradient(name="Purse Seine Sets", low="khaki1", high="red",
                      breaks=seq(0,30,10),
                      labels=seq(0,30,10),
                      limits=c(0,30)) +
  geom_polygon(data = kir_shp, aes(x = maplong, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world2, map = world2, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(167,215), breaks=seq(170,210,by=5), labels=c(170,175,180,-175,-170,-165,-160,-155,-150)) +
  scale_y_continuous(limits=c(-14.5,9), breaks=seq(-10,5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Kiribati")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/kiribatiPS2019_2.png", kiribati_map, width = 12, height = 8)

