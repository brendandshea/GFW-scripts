library(tidyverse)
library(fields)
library(stringr)
library(viridis)

rm(list=ls())

data <- read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/WCPFC_bycatch.csv")

data <- subset(data, Species.Category == "SHK")
colnames(data)[1] = "Year"
colnames(data)[3] = "Lat"
colnames(data)[4] = "Long"
colnames(data)[6] = "Species"
colnames(data)[8] = "Captures"
colnames(data)[9] = "Nominal_CPUE"
colnames(data)[10] = "Mortalities"
colnames(data)[11] = "Morality Rate"
data$hooks=data$Captures/(data$Nominal_CPUE/1000)
data$log_hooks <- log(data$hooks)


species.list <- unique(data$Species)
species.list <- as.data.frame(species.list)

WCPFC_thresher_data <- subset(data, Species=="BIGEYE THRESHER SHARK" | Species=="THRESHER SHARKS NEI" | 
                                Species == "PELAGIC THRESHER SHARK" | Species == "THRESHER SHARK (VULPINUS)")
WCPFC_thresher_captures <- aggregate(data=WCPFC_thresher_data, Captures~Year+Lat+Long+hooks+log_hooks, FUN="sum")
WCPFC_thresher_nominal <- aggregate(data=WCPFC_thresher_data, Nominal~Year+Lat+Long+hooks+log_hooks, FUN="sum")

WCPFC_blueshark_data <- subset(data, Species=="BLUE SHARK")
WCPFC_hammerhead_data <- subset(data, Species=="GREAT HAMMERHEAD" | Species=="HAMMERHEAD SHARKS NEI" |
                                  Species == "SCALLOPED HAMMERHEAD" | Species == "SMOOTH HAMMERHEAD")
WCPFC_shortfinmako_data <- subset(data, Species=="SHORTFIN MAKO")
WCPFC_longfinmako_data <- subset(data, Species=="LONGFIN MAKO")
WCPFC_unidmako_data <- subset(data,Species =="MAKO SHARKS")
WCPFC_oceanicwhitetip_data <- subset(data, Species=="OCEANIC WHITETIP SHARK")
WCPFC_silky_data <- subset(data, Species=="SILKY SHARK")
WCPFC_othersharks_data <- subset(data, Species == "PORBEAGLE SHARK" | Species == "OTHERS" | Species == "WHALE SHARK")

library(MASS)
library(car)
#### Threshers ####
WCPFC_thresher_model <- glm.nb(Captures ~ Lat + Long + Year + offset(log_hooks), 
                               data=WCPFC_thresher_captures)
summary(WCPFC_thresher_model)
Anova(WCPFC_thresher_model)
plot(WCPFC_thresher_model)

(pseudo_rsq_WCPFC_thresher <- 1 - WCPFC_thresher_model$deviance / WCPFC_thresher_model$null.deviance)

dummy.allthresher <- data.frame("Year"=WCPFC_thresher_captures$Year,"Lat"=WCPFC_thresher_captures$Lat,
                               "Long"=WCPFC_thresher_captures$Long,"log_hooks"=(log(1000)))
pred.thresher <- data.frame(dummy.allthresher, predicted=predict(WCPFC_thresher_model, newdata=dummy.allthresher, type = "response", se.fit=TRUE))

#### Blue Sharks #### CURRENT ISSUE WITH DISTRIBUTION
WCPFC_blueshark_model <- glm(Captures ~ Lat + Long + Year + offset(log_hooks), 
                               data=WCPFC_blueshark_data, family="quasipoisson")
summary(WCPFC_blueshark_model)
Anova(WCPFC_blueshark_model)
plot(WCPFC_blueshark_model)

(pseudo_rsq_WCPFC_blueshark <- 1 - WCPFC_blueshark_model$deviance / WCPFC_blueshark_model$null.deviance)

dummy.blueshark <- data.frame("Year"=WCPFC_blueshark_data$Year,"Lat"=WCPFC_blueshark_data$Lat,
                          "Long"=WCPFC_blueshark_data$Long,"log_hooks"=(log(1000)))
pred.blueshark <- data.frame(dummy.blueshark, predicted=predict(WCPFC_blueshark_model, newdata=dummy.blueshark, type = "response", se.fit=TRUE))

#### Hammerheads ####
WCPFC_hammer_model <- glm.nb(Captures ~ Lat + Long + Year + offset(log_hooks), 
                               data=WCPFC_hammerhead_data)
summary(WCPFC_hammer_model)
Anova(WCPFC_hammer_model)
plot(WCPFC_hammer_model)

(pseudo_rsq_WCPFC_hammer <- 1 - WCPFC_hammer_model$deviance / WCPFC_hammer_model$null.deviance)

WCPFC_hammer_model2 <- glm(Captures ~ Lat + Long + Year + offset(log_hooks), 
                             data=WCPFC_hammerhead_data, family="quasipoisson")
summary(WCPFC_hammer_model2)
Anova(WCPFC_hammer_model2)
plot(WCPFC_hammer_model2)

(pseudo_rsq_WCPFC_hammer2 <- 1 - WCPFC_hammer_model2$deviance / WCPFC_hammer_model2$null.deviance)


dummy.allhammers <- data.frame("Year"=WCPFC_hammerhead_data$Year,"Lat"=WCPFC_hammerhead_data$Lat,
                                "Long"=WCPFC_hammerhead_data$Long,"log_hooks"=(log(1000)))
pred.allhammers <- data.frame(dummy.allhammers, predicted=predict(WCPFC_hammer_model2, newdata=dummy.allhammers, type = "response", se.fit=TRUE))

#### Silky Shark ####
WCPFC_silky_model <- glm.nb(Captures ~ Lat + Long + Year + offset(log_hooks), 
                             data=WCPFC_silky_data)
summary(WCPFC_silky_model)
Anova(WCPFC_silky_model)
plot(WCPFC_silky_model)

(pseudo_rsq_WCPFC_silky <- 1 - WCPFC_silky_model$deviance / WCPFC_silky_model$null.deviance)

dummy.silky <- data.frame("Year"=WCPFC_silky_data$Year,"Lat"=WCPFC_silky_data$Lat,
                               "Long"=WCPFC_silky_data$Long,"log_hooks"=(log(1000)))
pred.silky <- data.frame(dummy.silky, predicted=predict(WCPFC_silky_model, newdata=dummy.silky, type = "response", se.fit=TRUE))

#### Shortfin Mako #### 
WCPFC_shortfinmako_model <- glm.nb(Captures ~ Lat + Long + Year + offset(log_hooks), 
                            data=WCPFC_shortfinmako_data)
summary(WCPFC_shortfinmako_model)
Anova(WCPFC_shortfinmako_model)
plot(WCPFC_shortfinmako_model)

(pseudo_rsq_WCPFC_shortfinmako <- 1 - WCPFC_shortfinmako_model$deviance / WCPFC_shortfinmako_model$null.deviance)

WCPFC_shortfinmako_model2 <- glm(Captures ~ Lat + Long + Year + offset(log_hooks), 
                                   data=WCPFC_shortfinmako_data, family="quasipoisson")
summary(WCPFC_shortfinmako_model2)
Anova(WCPFC_shortfinmako_model2)
plot(WCPFC_shortfinmako_model2)

(pseudo_rsq_WCPFC_shortfinmako2 <- 1 - WCPFC_shortfinmako_model2$deviance / WCPFC_shortfinmako_model2$null.deviance)

dummy.shortfinmako <- data.frame("Year"=WCPFC_shortfinmako_data$Year,"Lat"=WCPFC_shortfinmako_data$Lat,
                                 "Long"=WCPFC_shortfinmako_data$Long,"log_hooks"=(log(1000)))
pred.shortfinmako <- data.frame(dummy.shortfinmako, predicted=predict(WCPFC_shortfinmako_model2, newdata=dummy.shortfinmako, type = "response", se.fit=TRUE))

#### Longfin Mako ####
WCPFC_longfinmako_model <- glm.nb(Captures ~ Lat + Long + Year + offset(log_hooks), 
                                   data=WCPFC_longfinmako_data)
summary(WCPFC_longfinmako_model)
Anova(WCPFC_longfinmako_model)
plot(WCPFC_longfinmako_model)

(pseudo_rsq_WCPFC_longfinmako <- 1 - WCPFC_longfinmako_model$deviance / WCPFC_longfinmako_model$null.deviance)

dummy.longfinmako <- data.frame("Year"=WCPFC_longfinmako_data$Year,"Lat"=WCPFC_longfinmako_data$Lat,
                                 "Long"=WCPFC_longfinmako_data$Long,"log_hooks"=(log(1000)))
pred.longfinmako <- data.frame(dummy.longfinmako, predicted=predict(WCPFC_longfinmako_model, newdata=dummy.longfinmako, type = "response", se.fit=TRUE))

#### Unid Mako ####
WCPFC_unidmako_model <- glm.nb(Captures ~ Lat + Long + Year + offset(log_hooks), 
                                  data=WCPFC_unidmako_data)
summary(WCPFC_unidmako_model)
Anova(WCPFC_unidmako_model)
plot(WCPFC_unidmako_model)

(pseudo_rsq_WCPFC_unidmako <- 1 - WCPFC_unidmako_model$deviance / WCPFC_unidmako_model$null.deviance)

dummy.unidmako <- data.frame("Year"=WCPFC_unidmako_data$Year,"Lat"=WCPFC_unidmako_data$Lat,
                                "Long"=WCPFC_unidmako_data$Long,"log_hooks"=(log(1000)))
pred.unidmako <- data.frame(dummy.unidmako, predicted=predict(WCPFC_unidmako_model, newdata=dummy.unidmako, type = "response", se.fit=TRUE))

#### Oceanic whitetip ####

WCPFC_oceanicwhitetip_model <- glm.nb(Captures ~ Lat + Long + Year + offset(log_hooks), 
                                  data=WCPFC_oceanicwhitetip_data)
summary(WCPFC_oceanicwhitetip_model)
Anova(WCPFC_oceanicwhitetip_model)
plot(WCPFC_oceanicwhitetip_model)

(pseudo_rsq_WCPFC_oceanicwhitetip <- 1 - WCPFC_oceanicwhitetip_model$deviance / WCPFC_oceanicwhitetip_model$null.deviance)

dummy.oceanicwhitetip <- data.frame("Year"=WCPFC_oceanicwhitetip_data$Year,"Lat"=WCPFC_oceanicwhitetip_data$Lat,
                                "Long"=WCPFC_oceanicwhitetip_data$Long,"log_hooks"=(log(1000)))
pred.oceanicwhitetip <- data.frame(dummy.oceanicwhitetip, predicted=predict(WCPFC_oceanicwhitetip_model, newdata=dummy.oceanicwhitetip, type = "response", se.fit=TRUE))

#### Other Sharks ####

WCPFC_othersharks_model <- glm.nb(Captures ~ Lat + Long + Year + offset(log_hooks), 
                                  data=WCPFC_othersharks_data)
summary(WCPFC_othersharks_model)
Anova(WCPFC_othersharks_model)
plot(WCPFC_othersharks_model)

(pseudo_rsq_WCPFC_othersharks <- 1 - WCPFC_othersharks_model$deviance / WCPFC_othersharks_model$null.deviance)

dummy.othersharks <- data.frame("Year"=WCPFC_othersharks_data$Year,"Lat"=WCPFC_othersharks_data$Lat,
                                "Long"=WCPFC_othersharks_data$Long,"log_hooks"=(log(1000)))
pred.othersharks <- data.frame(dummy.othersharks, predicted=predict(WCPFC_othersharks_model, newdata=dummy.othersharks, type = "response", se.fit=TRUE))

WCPFC_CPUEs <- merge(pred.blueshark, pred.silky, by=c("Year", "Lat", "Long", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[5]<- "CPUE.Blueshark"
colnames(WCPFC_CPUEs)[6]<- "SE.Blueshark"
colnames(WCPFC_CPUEs)[7]<- "ResidScale.Blueshark"
colnames(WCPFC_CPUEs)[8]<- "CPUE.Silky"
colnames(WCPFC_CPUEs)[9]<- "SE.Silky"
colnames(WCPFC_CPUEs)[10]<- "ResidScale.Silky"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.shortfinmako, by=c("Year", "Lat", "Long", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[11]<- "CPUE.Shortfinmako"
colnames(WCPFC_CPUEs)[12]<- "SE.Shortfinmako"
colnames(WCPFC_CPUEs)[13]<- "ResidScale.Shortfinmako"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.longfinmako, by=c("Year", "Lat", "Long", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[14]<- "CPUE.longfinmako"
colnames(WCPFC_CPUEs)[15]<- "SE.longfinmako"
colnames(WCPFC_CPUEs)[16]<- "ResidScale.longfinmako"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.unidmako, by=c("Year", "Lat", "Long", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[17]<- "CPUE.unidmako"
colnames(WCPFC_CPUEs)[18]<- "SE.unidmako"
colnames(WCPFC_CPUEs)[19]<- "ResidScale.unidmako"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.thresher, by=c("Year", "Lat", "Long", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[20]<- "CPUE.allthresher"
colnames(WCPFC_CPUEs)[21]<- "SE.allthresher"
colnames(WCPFC_CPUEs)[22]<- "ResidScale.allthresher"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.allhammers, by=c("Year", "Lat", "Long", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[23]<- "CPUE.allhammers"
colnames(WCPFC_CPUEs)[24]<- "SE.allhammers"
colnames(WCPFC_CPUEs)[25]<- "ResidScale.allhammers"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.oceanicwhitetip, by=c("Year", "Lat", "Long", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[26]<- "CPUE.oceanicwhitetip"
colnames(WCPFC_CPUEs)[27]<- "SE.oceanicwhitetip"
colnames(WCPFC_CPUEs)[28]<- "ResidScale.oceanicwhitetip"
WCPFC_CPUEs <- merge(WCPFC_CPUEs, pred.othersharks, by=c("Year", "Lat", "Long", "log_hooks"), all=T)
colnames(WCPFC_CPUEs)[29]<- "CPUE.othersharks"
colnames(WCPFC_CPUEs)[30]<- "SE.othersharks"
colnames(WCPFC_CPUEs)[31]<- "ResidScale.othersharks"
WCPFC_CPUEs <- unique(WCPFC_CPUEs)

write_csv(WCPFC_CPUEs, "~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/WCPFC CPUEs.csv")

data <- subset(data, Calendar.Year==2017)
colnames(data)[6] <- "Species"
colnames(data)[3] <- "Lat"
colnames(data)[4] <- "Long"
data2 <- aggregate(data=data, Species~Lat+Long, FUN=length)



bbox1 = c(-35,35,120,180)
ylim1 <- c(bbox1[1], bbox1[2]) 
xlim1 <- c(bbox1[3], bbox1[4])
species_cpue_Grid1 = with(data2,mapplots::make.grid(x = Long, y = Lat, z = Species, xlim=xlim1,ylim =ylim1, byx=5,byy=5))
zlim1 = range(species_cpue_Grid1, na.rm = TRUE)
zlim1 = c(0,11)

lati1 = as.numeric(dimnames(species_cpue_Grid1)[[2]])
long1 = as.numeric(dimnames(species_cpue_Grid1)[[1]])

image.plot(long1, lati1, species_cpue_Grid1, las = 1, ylab = "Latitude", xlab = "Longitude", main = "WCPFC CPUE Coverage", legend.lab="# of Species", zlim = zlim1, col=viridis(6))
maps::map("world", xlim = xlim1, ylim = ylim1, add = TRUE, fill = TRUE, col="black")
plot(NCL_shp, add=TRUE, col="gray")
plot(PLW_shp, add=TRUE, col="gray")
plot(FSM_shp, add=TRUE, col="gray")
plot(MHL_shp, add=TRUE, col="gray")
plot(KIR_shp, add=TRUE, col="gray")
plot(WSM_shp, add=TRUE, col="gray")
image.plot(long1, lati1, species_cpue_Grid1, las = 1, zlim = zlim1, col=viridis(6), add=TRUE)
plot(NCL_shp, add=TRUE)
plot(PLW_shp, add=TRUE)
plot(FSM_shp, add=TRUE)
plot(MHL_shp, add=TRUE)
plot(KIR_shp, add=TRUE)
plot(WSM_shp, add=TRUE)

bbox2 = c(-35,35,-180,-130)
ylim2 <- c(bbox2[1], bbox2[2]) 
xlim2 <- c(bbox2[3], bbox2[4])
species_cpue_Grid2 = with(data2,mapplots::make.grid(x = Long, y = Lat, z = Species, xlim=xlim2,ylim =ylim2, byx=5,byy=5))
zlim2 = range(species_cpue_Grid2, na.rm = TRUE)
zlim2 = c(0,11)

lati2 = as.numeric(dimnames(species_cpue_Grid2)[[2]])
long2 = as.numeric(dimnames(species_cpue_Grid2)[[1]])

image.plot(long2, lati2, species_cpue_Grid2, las = 1, ylab = "Latitude", xlab = "Longitude", main = "WCPFC CPUE Coverage", legend.lab="# of Species", zlim = zlim2, col=viridis(6))
maps::map("world", xlim = xlim2, ylim = ylim2, add = TRUE, fill = TRUE, col="black")
plot(PYF_shp, add=TRUE, col="gray")
plot(KIR_shp, add=TRUE, col="gray")
plot(WSM_shp, add=TRUE, col="gray")
plot(COK_shp, add=TRUE, col="gray")
image.plot(long2, lati2, species_cpue_Grid2, las = 1, zlim = zlim2, col=viridis(6), add=TRUE)
plot(PYF_shp, add=TRUE)
plot(KIR_shp, add=TRUE)
plot(WSM_shp, add=TRUE)
plot(COK_shp, add=TRUE)




