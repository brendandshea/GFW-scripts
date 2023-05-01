library(tidyverse) 
library(sf) 
library(viridis)
library(gfwr)

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJWaXJnaW5pYSBUZWNoIEZlcnJldHRpIExhYiIsInVzZXJJZCI6MTk4NzgsImFwcGxpY2F0aW9uTmFtZSI6IlZpcmdpbmlhIFRlY2ggRmVycmV0dGkgTGFiIiwiaWQiOjExNywidHlwZSI6InVzZXItYXBwbGljYXRpb24ifSwiaWF0IjoxNjYwNzU0MjY2LCJleHAiOjE5NzYxMTQyNjYsImF1ZCI6ImdmdyIsImlzcyI6ImdmdyJ9.e4nWtHsEV57JimuKMtY9aS1zF2GrntrlTBg7fpjeiHl7STMQUPA1JNQHRekP4LsletdmLkT5So4hfaRLkE0JwXDUN1A6XpymggTv5ekNU4qJF6LlgtzovyZsP74SZt9ZtB0AH2_xLcMW130b6SxPFAwGwGVS_pY4EHebt3jO-YV-Qzb0iPJ3uqifTPzOnRGu8BV2j-LvFCD9WJrenBYv2OtBA3rj2Irgv2SC64RikPA2eKEt1ACgfy-XVk4VSXlZgqN8GLHjG7GQ0zVcNwuZ2QoGIGS3hHUlARl_5mTF7eN0M4jQHjhFs6QnVvYXbPrUZxaNLZGTn8TDwGRiN2qLPtyx80TDzNgBJaHE3uZ_d9VEkNHFmDHuaS9f2j9neCR-KizHFud67D7XD7Ro8nvtmK-cnGDvl0jJ6owFkKYeqRuRgbXiNaJlN-cJtv4LlheY_ZUC7nGLtKjAJnDIDyiqVHzG5FhTYFoBEzrB1iQSDelnzodqPip1MvhBHb6qpcr4"

(code_eez <- get_region_id(region_name="palau", region_source = 'eez', key=key))
#Palau is 8315

palauPS.2019 <- get_raster(spatial_resolution = 'high', #0.1 degree res
                  temporal_resolution = 'yearly',
                  group_by = 'flagAndGearType',
                  date_range = '2019-01-01,2020-01-01',
                  region = 8315, #codes for Marshall Islands
                  region_source = 'eez',
                  key = key)

palauPS.2019<- subset(palauPS.2019, Geartype == "tuna_purse_seines" | Geartype == "other_purse_seines")


colnames(palauPS.2019)[4] <- "flag"
colnames(palauPS.2019)[6] <- "hours"

palauPS.2019$Lon <- plyr::round_any(palauPS.2019$Lon, 0.25, f = floor)
palauPS.2019$Lat <- plyr::round_any(palauPS.2019$Lat, 0.25, f = floor)
palauPS.2019 <- aggregate(palauPS.2019, hours ~ Lon + Lat + flag, FUN=sum)
palauPS.2019$flag <- ifelse(is.na(palauPS.2019$flag), "Unknown", palauPS.2019$flag)

sum(palauPS.2019$hours) #525 hours

#### Sets projections ####
# data clean
palau2019 <- subset(palauPS.2019, hours > 0)
palau2019$log_hours <- log(palau2019$hours)
palau2019$Lon2 <- ifelse(palau2019$Lon < 0, palau2019$Lon + 360, palau2019$Lon)

#set projections
gamm1 <- readRDS("PS_gamm_2019.rds")
palau_preds <- predict(gamm1, newdata=palau2019, se.fit=T)
palau2019$log_sets <- palau_preds$fit
palau2019$log_sets.se <- palau_preds$se.fit

palau2019$sets <- exp(palau2019$log_sets)
sum(palau2019$sets, na.rm=T) 

# 360.43

#save
write.csv(palau2019, "palau2019.csv", row.names = F) 

#### plotting ####
# Hours
world <- map_data("world")

#aggregate across fleets
palau_2019 <- aggregate(data=palauPS.2019, hours~Lon+Lat, FUN='sum')
palau_2019 <- subset(palau_2019, hours != 0)
palau_2019$log_hours=log(palau_2019$hours)
range(palau_2019$hours)
palau_2019 <- subset(palau_2019, hours >= 1)

palau2019plot <- palau_2019 %>%
  ggplot() +
  geom_tile(aes(x = Lon+0.125, y = Lat+0.125, fill = log_hours),
            height=0.25, width=0.25) +
  geom_polygon(data = plw_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_fill_viridis(name="Purse Seine \nFishing Hours", option='C', begin=0.2,
                     breaks=c(log(1),log(10),log(100),log(500)),
                     labels=c(1,10,100,500),
                     limits=c(log(0.9),log(502))) +
  scale_x_continuous(limits=c(129,138), breaks=seq(130,135,by=5)) +
  scale_y_continuous(limits=c(1,13), breaks=seq(5,10,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Palau")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/palauPS2019.png", palau2019plot, width = 12, height = 8)

#### Sets Plot ####
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
palau_ais <- read.csv("palau2019.csv")
palau_proj <- palau_ais
palau_map <- ggplot(data=palau_proj, aes(x=Lon+0.125, y=Lat+0.125)) +
  geom_tile(aes(fill=sets), height=0.25, width=0.25) +
  scale_fill_gradient(name="Purse Seine Sets", low="khaki1", high="red",
                      breaks=seq(0,30,10),
                      labels=seq(0,30,10),
                      limits=c(0,30)) +
  geom_polygon(data = plw_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(129,138), breaks=seq(130,135,by=5)) +
  scale_y_continuous(limits=c(1,13), breaks=seq(5,10,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Palau")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/palauPS2019_2.png", palau_map, width = 12, height = 8)

