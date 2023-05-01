library(tidyverse) 
library(sf) 
library(viridis)
library(gfwr)

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJWaXJnaW5pYSBUZWNoIEZlcnJldHRpIExhYiIsInVzZXJJZCI6MTk4NzgsImFwcGxpY2F0aW9uTmFtZSI6IlZpcmdpbmlhIFRlY2ggRmVycmV0dGkgTGFiIiwiaWQiOjExNywidHlwZSI6InVzZXItYXBwbGljYXRpb24ifSwiaWF0IjoxNjYwNzU0MjY2LCJleHAiOjE5NzYxMTQyNjYsImF1ZCI6ImdmdyIsImlzcyI6ImdmdyJ9.e4nWtHsEV57JimuKMtY9aS1zF2GrntrlTBg7fpjeiHl7STMQUPA1JNQHRekP4LsletdmLkT5So4hfaRLkE0JwXDUN1A6XpymggTv5ekNU4qJF6LlgtzovyZsP74SZt9ZtB0AH2_xLcMW130b6SxPFAwGwGVS_pY4EHebt3jO-YV-Qzb0iPJ3uqifTPzOnRGu8BV2j-LvFCD9WJrenBYv2OtBA3rj2Irgv2SC64RikPA2eKEt1ACgfy-XVk4VSXlZgqN8GLHjG7GQ0zVcNwuZ2QoGIGS3hHUlARl_5mTF7eN0M4jQHjhFs6QnVvYXbPrUZxaNLZGTn8TDwGRiN2qLPtyx80TDzNgBJaHE3uZ_d9VEkNHFmDHuaS9f2j9neCR-KizHFud67D7XD7Ro8nvtmK-cnGDvl0jJ6owFkKYeqRuRgbXiNaJlN-cJtv4LlheY_ZUC7nGLtKjAJnDIDyiqVHzG5FhTYFoBEzrB1iQSDelnzodqPip1MvhBHb6qpcr4"

(code_eez <- get_region_id(region_name="marshall", region_source = 'eez', key=key))
#MI is 8318

marshallislandsPS.2019 <- get_raster(spatial_resolution = 'high', #0.1 degree res
                  temporal_resolution = 'yearly',
                  group_by = 'flagAndGearType',
                  date_range = '2019-01-01,2020-01-01',
                  region = 8318, #codes for Marshall Islands
                  region_source = 'eez',
                  key = key)

marshallislandsPS.2019<- subset(marshallislandsPS.2019, Geartype == "tuna_purse_seines" | Geartype == "other_purse_seines")


colnames(marshallislandsPS.2019)[4] <- "flag"
colnames(marshallislandsPS.2019)[6] <- "hours"

marshallislandsPS.2019$Lon <- plyr::round_any(marshallislandsPS.2019$Lon, 0.25, f = floor)
marshallislandsPS.2019$Lat <- plyr::round_any(marshallislandsPS.2019$Lat, 0.25, f = floor)
marshallislandsPS.2019 <- aggregate(marshallislandsPS.2019, hours ~ Lon + Lat + flag, FUN=sum)
marshallislandsPS.2019$flag <- ifelse(is.na(marshallislandsPS.2019$flag), "Unknown", marshallislandsPS.2019$flag)

sum(marshallislandsPS.2019$hours) #2921 hours

#### Sets projections ####
# data clean
marshallislands2019 <- subset(marshallislandsPS.2019, hours > 0)
marshallislands2019$log_hours <- log(marshallislands2019$hours)
marshallislands2019$Lon2 <- ifelse(marshallislands2019$Lon < 0, marshallislands2019$Lon + 360, marshallislands2019$Lon)

#set projections
gamm1 <- readRDS("PS_gamm_2019.rds")
marshallislands_preds <- predict(gamm1, newdata=marshallislands2019, se.fit=T)
marshallislands2019$log_sets <- marshallislands_preds$fit
marshallislands2019$log_sets.se <- marshallislands_preds$se.fit

marshallislands2019$sets <- exp(marshallislands2019$log_sets)
sum(marshallislands2019$sets, na.rm=T) 

# 2696.635 sets

#save
write.csv(marshallislands2019, "marshallislands2019.csv", row.names = F) 

#### plotting ####
# Hours
world <- map_data("world")

#aggregate across fleets
marshallislands_2019 <- aggregate(data=marshallislandsPS.2019, hours~Lon+Lat, FUN='sum')
marshallislands_2019 <- subset(marshallislands_2019, hours != 0)
marshallislands_2019$log_hours=log(marshallislands_2019$hours)
range(marshallislands_2019$hours)
marshallislands_2019 <- subset(marshallislands_2019, hours >= 1)

marshallislands2019plot <- marshallislands_2019 %>%
  ggplot() +
  geom_tile(aes(x = Lon+0.125, y = Lat+0.125, fill = log_hours),
            height=0.25, width=0.25) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_fill_viridis(name="Purse Seine \nFishing Hours", option='C', begin=0.2,
                     breaks=c(log(1),log(10),log(100),log(500)),
                     labels=c(1,10,100,500),
                     limits=c(log(0.9),log(502))) +
  scale_x_continuous(limits=c(156,177), breaks=seq(160,175,by=5)) +
  scale_y_continuous(limits=c(0.5,19), breaks=seq(5,15,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Marshall Islands")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/marshallislandsPS2019.png", marshallislands2019plot, width = 12, height = 8)

#### Sets Plot ####
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
marshallislands_ais <- read.csv("marshallislands2019.csv")
marshallislands_proj <- marshallislands_ais
marshallislands_map <- ggplot(data=marshallislands_proj, aes(x=Lon+0.125, y=Lat+0.125)) +
  geom_tile(aes(fill=sets), height=0.25, width=0.25) +
  scale_fill_gradient(name="Purse Seine Sets", low="khaki1", high="red",
                      breaks=seq(0,30,10),
                      labels=seq(0,30,10),
                      limits=c(0,30)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(156,177), breaks=seq(160,175,by=5)) +
  scale_y_continuous(limits=c(0.5,19), breaks=seq(5,15,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Marshall Islands")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/marshallislandsPS2019_2.png", marshallislands_map, width = 12, height = 8)

