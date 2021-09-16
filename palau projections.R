library(tidyverse)
library(fields)
library(stringr)

hours=read.csv("effort_longline_2017.csv") #AIS Data
hooks=read.csv("LONGLINE.csv") #hooks data
hooks=subset(hooks, yy=="2017") #subset for 2017
hooks$lat <- str_extract(hooks$lat5, "[[:digit:]]+") #extract numerical values for lat
hooks$lat <- as.numeric(hooks$lat) #convert new column to numerical 
hooks$lat <- ifelse(str_extract(hooks$lat5, "[[:upper:]]")=="S", -(hooks$lat), hooks$lat) #make southern latitudes negative
hooks$lon <- str_extract(hooks$lon5, "[[:digit:]]+") #extract numerical values for lon
hooks$lon <- as.numeric(hooks$lon) #convert new column to numerical 
hooks$lon <- ifelse(str_extract(hooks$lon5, "[[:upper:]]")=="W", -(hooks$lon), hooks$lon) #make western longitudes negative
hooks$hooks = hooks$hhooks*100 #convert hundreds of hooks to hooks

#aggregate hooks data so that there is one hooks value per cell
hooks2=aggregate(data=hooks, hooks~lat*lon, FUN="sum")

sum(hours$fishing_hours)
#Aggregate AIS data at 5*5 resolution
hours$roundedlat=round(hours$lat_bin/5)*5
hours$roundedlon=round(hours$lon_bin/5)*5

hours2=aggregate(data=hours, fishing_hours~roundedlat*roundedlon, FUN="sum") #removed flag
#hours2=aggregate(data=hours, fishing_hours~roundedlat*roundedlon*flag, FUN="sum")
sum(hours2$fishing_hours)

#make variable names constant
names(hours2)[1]="lat"
names(hours2)[2]="lon"
#names(hours2)[3]="fleet"
names(hours2)[3]="hours"
#names(hours2)[4]="hours"


palau_regional_hooks=subset(hooks2, lat >= 0 & lat < 15 & lon >=125 & lon < 140)
palau_regional_hours=subset(hours2, lat >= 0 & lat < 15 & lon >=125 & lon < 140)

#merge dataframes and remove rows missing values for hooks or hours
palau_regional_master=merge(palau_regional_hooks,palau_regional_hours, by = c("lat","lon"))

palau_ais <- read.csv("palau_all.csv")
palau_ais <- subset(palau_ais, year==2017)

sum(palau_ais$fishing_hours)
sum(palau_regional_master$hours, na.rm=T)

prop_hours <- sum(palau_ais$fishing_hours, na.rm=TRUE)/sum(palau_regional_master$hours, na.rm=TRUE)

palau <- data.frame("Hours" = sum(palau_ais$fishing_hours), "Hooks" = prop_hours*sum(palau_regional_master$hooks))

palau_catches <- read.csv("palau cpues.csv")
palau_catches <- subset(palau_catches, select = c("Common", "Species", "CPUE", "Hook_Survival", "Fr"))

palau_catches$projectedcatch = palau_catches$CPUE*sum(palau$Hooks)/1000 #multiple CPUEs by hooks (divide by 1000 to match units)
palau_catches$hook_mortality <- (100 - palau_catches$Hook_Survival)/100 #get hook mortality and put in terms of proportions

palau_catches$DOA <- palau_catches$projectedcatch*palau_catches$hook_mortality # Number dead on landing
palau_catches$released <- palau_catches$projectedcatch - palau_catches$DOA # Number released alive (assuming compliance)
palau_catches$postreleasemortality <- palau_catches$released * palau_catches$Fr # number of deaths post release
palau_catches$totalmortality <- ifelse(is.na(palau_catches$postreleasemortality), palau_catches$DOA, palau_catches$DOA + palau_catches$postreleasemortality) #total mortality
palau_2017 <- sum(palau_catches$totalmortality)
palau_2017_per100km2 <- palau_2017/604253*100



write_csv(palau_catches,"palau_projections.csv")
