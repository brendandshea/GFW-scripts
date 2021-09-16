surround_MI <- read.csv("marshallislands_cpues.csv")
surround_MI <- subset(surround_MI, select = c("Species", "CPUE", "hook_mortality", "Fr"))
colnames(surround_MI) <- c("Species", "CPUE1", "HM1", "Fr1")

surround_Palau <- read.csv("palau cpues.csv")
surround_Palau <- subset(palau_catches, select = c("Species", "CPUE", "Hook_Survival", "Fr"))
surround_Palau$Hook_Survival <- 100 - surround_Palau$Hook_Survival
colnames(surround_Palau) <- c("Species", "CPUE2", "HM2", "Fr2")

micronesia_catches <- merge(surround_MI,surround_Palau,by="Species", all = T)

micronesia_regional_hooks=subset(hooks2, lat >= -5 & lat < 15 & lon >=135 & lon < 170)
micronesia_regional_hours=subset(hours2, lat >= -5 & lat < 15 & lon >=135 & lon < 170)

#merge dataframes and remove rows missing values for hooks or hours
micronesia_regional_master=merge(micronesia_regional_hooks,micronesia_regional_hours, by = c("lat","lon"))

micronesia_ais <- read.csv("micronesia_all.csv")
micronesia_ais <- subset(micronesia_ais, year==2017)

sum(micronesia_ais$fishing_hours)
sum(micronesia_regional_master$hours, na.rm=T)

prop_hours_micronesia <- sum(micronesia_ais$fishing_hours, na.rm=TRUE)/sum(micronesia_regional_master$hours, na.rm=TRUE)

micronesia <- data.frame("Hours" = sum(micronesia_ais$fishing_hours), "Hooks" = prop_hours_micronesia*sum(micronesia_regional_master$hooks))

str(micronesia_catches)
micronesia_catches$CPUE <- rowMeans(micronesia_catches[,c(2,5)], na.rm=T)
micronesia_catches$hook_mortality <- rowMeans(micronesia_catches[,c(3,6)], na.rm=T)
micronesia_catches$Fr <- micronesia_catches$Fr1

micronesia_catches$projectedcatch = micronesia_catches$CPUE*sum(micronesia$Hooks)/1000 #multiple CPUEs by hooks (divide by 1000 to match units)
micronesia_catches$hook_mortality <- micronesia_catches$hook_mortality/100
micronesia_catches$DOA <- micronesia_catches$projectedcatch*micronesia_catches$hook_mortality # Number dead on landing
micronesia_catches$released <- micronesia_catches$projectedcatch - micronesia_catches$DOA # Number released alive (assumicronesiang compliance)
micronesia_catches$postreleasemortality <- micronesia_catches$released * micronesia_catches$Fr # number of deaths post release
micronesia_catches$totalmortality <- ifelse(is.na(micronesia_catches$postreleasemortality), micronesia_catches$DOA, micronesia_catches$DOA + micronesia_catches$postreleasemortality) #total mortality
micronesia_2017 <- sum(micronesia_catches$totalmortality)

micronesia_2017_per100km2 <- micronesia_2017/2992415 * 100

sum(palau_2017,micronesia_2017,Marshall_2017)

write_csv(micronesia_catches,"micronesia_projections.csv")
