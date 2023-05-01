library(tidyverse) 
library(sf) 
library(viridis)
library(gfwr)

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJWaXJnaW5pYSBUZWNoIEZlcnJldHRpIExhYiIsInVzZXJJZCI6MTk4NzgsImFwcGxpY2F0aW9uTmFtZSI6IlZpcmdpbmlhIFRlY2ggRmVycmV0dGkgTGFiIiwiaWQiOjExNywidHlwZSI6InVzZXItYXBwbGljYXRpb24ifSwiaWF0IjoxNjYwNzU0MjY2LCJleHAiOjE5NzYxMTQyNjYsImF1ZCI6ImdmdyIsImlzcyI6ImdmdyJ9.e4nWtHsEV57JimuKMtY9aS1zF2GrntrlTBg7fpjeiHl7STMQUPA1JNQHRekP4LsletdmLkT5So4hfaRLkE0JwXDUN1A6XpymggTv5ekNU4qJF6LlgtzovyZsP74SZt9ZtB0AH2_xLcMW130b6SxPFAwGwGVS_pY4EHebt3jO-YV-Qzb0iPJ3uqifTPzOnRGu8BV2j-LvFCD9WJrenBYv2OtBA3rj2Irgv2SC64RikPA2eKEt1ACgfy-XVk4VSXlZgqN8GLHjG7GQ0zVcNwuZ2QoGIGS3hHUlARl_5mTF7eN0M4jQHjhFs6QnVvYXbPrUZxaNLZGTn8TDwGRiN2qLPtyx80TDzNgBJaHE3uZ_d9VEkNHFmDHuaS9f2j9neCR-KizHFud67D7XD7Ro8nvtmK-cnGDvl0jJ6owFkKYeqRuRgbXiNaJlN-cJtv4LlheY_ZUC7nGLtKjAJnDIDyiqVHzG5FhTYFoBEzrB1iQSDelnzodqPip1MvhBHb6qpcr4"

(code_eez <- get_region_id(region_name="french polynesia", region_source = 'eez', key=key))
#FP is 8440

frenchpolynesiaPS.2019 <- get_raster(spatial_resolution = 'high', #0.1 degree res
                  temporal_resolution = 'yearly',
                  group_by = 'flagAndGearType',
                  date_range = '2019-01-01,2020-01-01',
                  region = 8440, #codes for Marshall Islands
                  region_source = 'eez',
                  key = key)

frenchpolynesiaPS.2019<- subset(frenchpolynesiaPS.2019, Geartype == "tuna_purse_seines" | Geartype == "other_purse_seines")


colnames(frenchpolynesiaPS.2019)[4] <- "flag"
colnames(frenchpolynesiaPS.2019)[6] <- "hours"

frenchpolynesiaPS.2019$Lon <- plyr::round_any(frenchpolynesiaPS.2019$Lon, 0.25, f = floor)
frenchpolynesiaPS.2019$Lat <- plyr::round_any(frenchpolynesiaPS.2019$Lat, 0.25, f = floor)
frenchpolynesiaPS.2019 <- aggregate(frenchpolynesiaPS.2019, hours ~ Lon + Lat + flag, FUN=sum)
frenchpolynesiaPS.2019$flag <- ifelse(is.na(frenchpolynesiaPS.2019$flag), "Unknown", frenchpolynesiaPS.2019$flag)

sum(frenchpolynesiaPS.2019$hours) #no effort
