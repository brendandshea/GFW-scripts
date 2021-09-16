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


MI_regional_hooks=subset(hooks2, lat >= 0 & lat < 20 & lon >=155 & lon < 180)
MI_regional_hours=subset(hours2, lat >= 0 & lat < 20 & lon >=155 & lon < 180)

#merge dataframes and remove rows missing values for hooks or hours
MI_regional_master=merge(MI_regional_hooks,MI_regional_hours, by = c("lat","lon"))

MI_ais <- read.csv("marshallislands_all.csv")
MI_ais <- subset(MI_ais, year==2017)

sum(MI_ais$fishing_hours)
sum(MI_regional_master$hours, na.rm=T)

prop_hours_MI <- sum(MI_ais$fishing_hours, na.rm=TRUE)/sum(MI_regional_master$hours, na.rm=TRUE)

marshallislands <- data.frame("Hours" = sum(MI_ais$fishing_hours), "Hooks" = prop_hours_MI*sum(MI_regional_master$hooks))

MI_catches <- read.csv("marshallislands_cpues.csv")

MI_catches <- subset(MI_catches, select = c("Species", "CPUE", "hook_mortality", "Fr"))

MI_catches$projectedcatch = MI_catches$CPUE*sum(marshallislands$Hooks)/1000 #multiple CPUEs by hooks (divide by 1000 to match units)
MI_catches$hook_mortality <- MI_catches$hook_mortality/100
MI_catches$DOA <- MI_catches$projectedcatch*MI_catches$hook_mortality # Number dead on landing
MI_catches$released <- MI_catches$projectedcatch - MI_catches$DOA # Number released alive (assuming compliance)
MI_catches$postreleasemortality <- MI_catches$released * MI_catches$Fr # number of deaths post release
MI_catches$totalmortality <- ifelse(is.na(MI_catches$postreleasemortality), MI_catches$DOA, MI_catches$DOA + MI_catches$postreleasemortality) #total mortality
Marshall_2017 <- sum(MI_catches$totalmortality)
Marhsall_2017_per100km2 <- Marshall_2017/1992022*100

write_csv(MI_catches,"MI_projections.csv")
