library(tidyverse) 
library(sf) 
library(viridis)
library(gfwr)

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJWaXJnaW5pYSBUZWNoIEZlcnJldHRpIExhYiIsInVzZXJJZCI6MTk4NzgsImFwcGxpY2F0aW9uTmFtZSI6IlZpcmdpbmlhIFRlY2ggRmVycmV0dGkgTGFiIiwiaWQiOjExNywidHlwZSI6InVzZXItYXBwbGljYXRpb24ifSwiaWF0IjoxNjYwNzU0MjY2LCJleHAiOjE5NzYxMTQyNjYsImF1ZCI6ImdmdyIsImlzcyI6ImdmdyJ9.e4nWtHsEV57JimuKMtY9aS1zF2GrntrlTBg7fpjeiHl7STMQUPA1JNQHRekP4LsletdmLkT5So4hfaRLkE0JwXDUN1A6XpymggTv5ekNU4qJF6LlgtzovyZsP74SZt9ZtB0AH2_xLcMW130b6SxPFAwGwGVS_pY4EHebt3jO-YV-Qzb0iPJ3uqifTPzOnRGu8BV2j-LvFCD9WJrenBYv2OtBA3rj2Irgv2SC64RikPA2eKEt1ACgfy-XVk4VSXlZgqN8GLHjG7GQ0zVcNwuZ2QoGIGS3hHUlARl_5mTF7eN0M4jQHjhFs6QnVvYXbPrUZxaNLZGTn8TDwGRiN2qLPtyx80TDzNgBJaHE3uZ_d9VEkNHFmDHuaS9f2j9neCR-KizHFud67D7XD7Ro8nvtmK-cnGDvl0jJ6owFkKYeqRuRgbXiNaJlN-cJtv4LlheY_ZUC7nGLtKjAJnDIDyiqVHzG5FhTYFoBEzrB1iQSDelnzodqPip1MvhBHb6qpcr4"

(code_eez <- get_region_id(region_name="samoa", region_source = 'eez', key=key))
#WSM is 8445

samoaPS.2019 <- get_raster(spatial_resolution = 'high', #0.1 degree res
                  temporal_resolution = 'yearly',
                  group_by = 'flagAndGearType',
                  date_range = '2019-01-01,2020-01-01',
                  region = 8445, #codes for Marshall Islands
                  region_source = 'eez',
                  key = key)

samoaPS.2019<- subset(samoaPS.2019, Geartype == "tuna_purse_seines" | Geartype == "other_purse_seines")


colnames(samoaPS.2019)[4] <- "flag"
colnames(samoaPS.2019)[6] <- "hours"

samoaPS.2019$Lon <- plyr::round_any(samoaPS.2019$Lon, 0.25, f = floor)
samoaPS.2019$Lat <- plyr::round_any(samoaPS.2019$Lat, 0.25, f = floor)
samoaPS.2019 <- aggregate(samoaPS.2019, hours ~ Lon + Lat + flag, FUN=sum)
samoaPS.2019$flag <- ifelse(is.na(samoaPS.2019$flag), "Unknown", samoaPS.2019$flag)

sum(samoaPS.2019$hours) #49 hours

#### Sets projections ####
# data clean
samoa2019 <- subset(samoaPS.2019, hours > 0)
samoa2019$log_hours <- log(samoa2019$hours)
samoa2019$Lon2 <- ifelse(samoa2019$Lon < 0, samoa2019$Lon + 360, samoa2019$Lon)

#set projections
gamm1 <- readRDS("PS_gamm_2019.rds")
samoa_preds <- predict(gamm1, newdata=samoa2019, se.fit=T)
samoa2019$log_sets <- samoa_preds$fit
samoa2019$log_sets.se <- samoa_preds$se.fit

samoa2019$sets <- exp(samoa2019$log_sets)
sum(samoa2019$sets, na.rm=T) 

# 44 sets

#save
write.csv(samoa2019, "samoa2019.csv", row.names = F) 

#### plotting ####
# Hours
world <- map_data("world")

#aggregate across fleets
samoa_2019 <- aggregate(data=samoaPS.2019, hours~Lon+Lat, FUN='sum')
samoa_2019 <- subset(samoa_2019, hours != 0)
samoa_2019$log_hours=log(samoa_2019$hours)
range(samoa_2019$hours)
samoa_2019 <- subset(samoa_2019, hours >= 1)

samoa2019plot <- samoa_2019 %>%
  ggplot() +
  geom_tile(aes(x = Lon+0.125, y = Lat+0.125, fill = log_hours),
            height=0.25, width=0.25) +
  geom_polygon(data = wsm_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_fill_viridis(name="Purse Seine \nFishing Hours", option='C', begin=0.2,
                     breaks=c(log(1),log(10),log(100),log(500)),
                     labels=c(1,10,100,500),
                     limits=c(log(0.9),log(502))) +
  scale_x_continuous(limits=c(-175.5,-169.5), breaks=seq(-175,-170,by=5)) +
  scale_y_continuous(limits=c(-16,-10.5), breaks=seq(-15,-5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Samoa")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/samoaPS2019.png", samoa2019plot, width = 12, height = 8)

#### Sets Plot ####
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
samoa_ais <- read.csv("samoa2019.csv")
samoa_proj <- samoa_ais
samoa_map <- ggplot(data=samoa_proj, aes(x=Lon+0.125, y=Lat+0.125)) +
  geom_tile(aes(fill=sets), height=0.25, width=0.25) +
  scale_fill_gradient(name="Purse Seine Sets", low="khaki1", high="red",
                      breaks=seq(0,30,10),
                      labels=seq(0,30,10),
                      limits=c(0,30)) +
  geom_polygon(data = wsm_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(-175.5,-169.5), breaks=seq(-175,-170,by=5)) +
  scale_y_continuous(limits=c(-16,-10.5), breaks=seq(-15,-5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Samoa")

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/samoaPS2019_2.png", samoa_map, width = 12, height = 8)

