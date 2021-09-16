library(tidyverse)
library(sf)

rm(list=ls())
wcpfc <- read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/WCPFC CPUEs.csv")
haw_ams <- read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/HawAmS_CPUEs.csv")

colnames(haw_ams)[1] <- "Lat"
colnames(haw_ams)[2] <- "Long"
colnames(haw_ams)[3] <- "Year"
colnames(haw_ams)[5] <- "CPUE.Blueshark"
colnames(haw_ams)[6] <- "SE.Blueshark"
colnames(haw_ams)[7] <- "CPUE.Silky"
colnames(haw_ams)[9] <- "SE.Silky"
colnames(haw_ams)[11] <- "CPUE.unidmako"
colnames(haw_ams)[12] <- "SE.unidmako"
colnames(haw_ams)[14] <- "CPUE.Shortfinmako"
colnames(haw_ams)[15] <- "SE.Shortfinmako"
colnames(haw_ams)[17] <- "CPUE.longfinmako"
colnames(haw_ams)[18] <- "SE.longfinmako"
colnames(haw_ams)[20] <- "CPUE.allthresher"
colnames(haw_ams)[21] <- "SE.allthresher"
colnames(haw_ams)[23] <- "CPUE.allhammers"
colnames(haw_ams)[24] <- "CPUE.oceanicwhitetip"
colnames(haw_ams)[25] <- "SE.oceanicwhitetip"
colnames(haw_ams)[27] <- "CPUE.othersharks"
colnames(haw_ams)[28] <- "SE.othersharks"


# Load EEZ polygons
eezs <- read_sf('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/World_EEZ_v10_20180221/', layer = 'eez_v10') %>% 
  filter(Pol_type == '200NM') # select the 200 nautical mile polygon layer

#### Cook Islands ####
cookislands <- eezs %>% 
  filter(ISO_Ter1 == 'COK' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
cook_bbox <- sf::st_bbox(cookislands)

cook_wcpfc <- wcpfc %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= cook_bbox[2] & Lat <= cook_bbox[4] & Long >= cook_bbox[1] & Long <= cook_bbox[3], "yes", "no")) 
cook_wcpfc <- subset(cook_wcpfc, passes_through_box == "yes")

cook_wcpfc_cpues <- data.frame("Country" = "Cook Islands",
                               "Source" = "wcpfc",
                               "Blueshark" = mean(cook_wcpfc$CPUE.Blueshark, na.rm = T),
                               "Silky" = mean(cook_wcpfc$CPUE.Silky, na.rm=T),
                               "unidmako" = mean(cook_wcpfc$CPUE.unidmako, na.rm=T),
                               "Shortfinmako" = mean(cook_wcpfc$CPUE.Shortfinmako, na.rm=T),
                               "longfinmako" = mean(cook_wcpfc$CPUE.longfinmako, na.rm=T),
                               "allthresher"= mean(cook_wcpfc$CPUE.allthresher, na.rm=T),
                               "allhammers" = mean(cook_wcpfc$CPUE.allhammers, na.rm=T),
                               "oceanicwhitetip" = mean(cook_wcpfc$CPUE.oceanicwhitetip, na.rm=T),
                               "othersharks" = mean(cook_wcpfc$CPUE.othersharks, na.rm=T))

cook_haw_ams <- haw_ams %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= cook_bbox[2] & Lat <= cook_bbox[4] & Long >= cook_bbox[1] & Long <= cook_bbox[3], "yes", "no")) 
cook_haw_ams <- subset(cook_haw_ams, passes_through_box == "yes")

cook_haw_ams_cpues <- data.frame("Country" = "Cook Islands",
                                 "Source" = "haw_ams",
                                 "Blueshark" = mean(cook_haw_ams$CPUE.Blueshark, na.rm = T),
                                 "Silky" = mean(cook_haw_ams$CPUE.Silky, na.rm=T),
                                 "unidmako" = mean(cook_haw_ams$CPUE.unidmako, na.rm=T),
                                 "Shortfinmako" = mean(cook_haw_ams$CPUE.Shortfinmako, na.rm=T),
                                 "longfinmako" = mean(cook_haw_ams$CPUE.longfinmako, na.rm=T),
                                 "allthresher"= mean(cook_haw_ams$CPUE.allthresher, na.rm=T),
                                 "allhammers" = mean(cook_haw_ams$CPUE.allhammers, na.rm=T),
                                 "oceanicwhitetip" = mean(cook_haw_ams$CPUE.oceanicwhitetip, na.rm=T),
                                 "othersharks" = mean(cook_haw_ams$CPUE.othersharks, na.rm=T))

cook_cpues <- rbind(cook_wcpfc_cpues,cook_haw_ams_cpues)

#### French Polynesia ####
frenchpolynesia <- eezs %>% 
  filter(ISO_Ter1 == 'PYF' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
FP_bbox <- sf::st_bbox(frenchpolynesia)

FP_wcpfc <- wcpfc %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= FP_bbox[2] & Lat <= FP_bbox[4] & Long >= FP_bbox[1] & Long <= FP_bbox[3], "yes", "no")) 
FP_wcpfc <- subset(FP_wcpfc, passes_through_box == "yes")

FP_wcpfc_cpues <- data.frame("Country" = "French Polynesia",
                             "Source" = "wcpfc",
                             "Blueshark" = mean(FP_wcpfc$CPUE.Blueshark, na.rm = T),
                             "Silky" = mean(FP_wcpfc$CPUE.Silky, na.rm=T),
                             "unidmako" = mean(FP_wcpfc$CPUE.unidmako, na.rm=T),
                             "Shortfinmako" = mean(FP_wcpfc$CPUE.Shortfinmako, na.rm=T),
                             "longfinmako" = mean(FP_wcpfc$CPUE.longfinmako, na.rm=T),
                             "allthresher"= mean(FP_wcpfc$CPUE.allthresher, na.rm=T),
                             "allhammers" = mean(FP_wcpfc$CPUE.allhammers, na.rm=T),
                             "oceanicwhitetip" = mean(FP_wcpfc$CPUE.oceanicwhitetip, na.rm=T),
                             "othersharks" = mean(FP_wcpfc$CPUE.othersharks, na.rm=T))

FP_haw_ams <- haw_ams %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat+7 >= FP_bbox[2] & Lat-7 <= FP_bbox[4] & Long+7 >= FP_bbox[1] & Long-7 <= FP_bbox[3], "yes", "no")) 
FP_haw_ams <- subset(FP_haw_ams, passes_through_box == "yes")

FP_haw_ams_cpues <- data.frame("Country" = "French Polynesia",
                               "Source" = "haw_ams",
                               "Blueshark" = mean(FP_haw_ams$CPUE.Blueshark, na.rm = T),
                               "Silky" = mean(FP_haw_ams$CPUE.Silky, na.rm=T),
                               "unidmako" = mean(FP_haw_ams$CPUE.unidmako, na.rm=T),
                               "Shortfinmako" = mean(FP_haw_ams$CPUE.Shortfinmako, na.rm=T),
                               "longfinmako" = mean(FP_haw_ams$CPUE.longfinmako, na.rm=T),
                               "allthresher"= mean(FP_haw_ams$CPUE.allthresher, na.rm=T),
                               "allhammers" = mean(FP_haw_ams$CPUE.allhammers, na.rm=T),
                               "oceanicwhitetip" = mean(FP_haw_ams$CPUE.oceanicwhitetip, na.rm=T),
                               "othersharks" = mean(FP_haw_ams$CPUE.othersharks, na.rm=T))

FP_cpues <- rbind(FP_wcpfc_cpues,FP_haw_ams_cpues)

#### Samoa #### 
#3 degrees of extension used for WCPFC; 
samoa <- eezs %>% 
  filter(ISO_Ter1 == 'WSM' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
samoa_bbox <- sf::st_bbox(samoa)

samoa_wcpfc <- wcpfc %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat+3 >= samoa_bbox[2] & Lat-3 <= samoa_bbox[4] & Long+3 >= samoa_bbox[1] & Long-3 <= samoa_bbox[3], "yes", "no")) 
samoa_wcpfc <- subset(samoa_wcpfc, passes_through_box == "yes")

samoa_wcpfc_cpues <- data.frame("Country" = "Samoa",
                                "Source" = "wcpfc",
                                "Blueshark" = mean(samoa_wcpfc$CPUE.Blueshark, na.rm = T),
                                "Silky" = mean(samoa_wcpfc$CPUE.Silky, na.rm=T),
                                "unidmako" = mean(samoa_wcpfc$CPUE.unidmako, na.rm=T),
                                "Shortfinmako" = mean(samoa_wcpfc$CPUE.Shortfinmako, na.rm=T),
                                "longfinmako" = mean(samoa_wcpfc$CPUE.longfinmako, na.rm=T),
                                "allthresher"= mean(samoa_wcpfc$CPUE.allthresher, na.rm=T),
                                "allhammers" = mean(samoa_wcpfc$CPUE.allhammers, na.rm=T),
                                "oceanicwhitetip" = mean(samoa_wcpfc$CPUE.oceanicwhitetip, na.rm=T),
                                "othersharks" = mean(samoa_wcpfc$CPUE.othersharks, na.rm=T))

samoa_haw_ams <- haw_ams %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= samoa_bbox[2] & Lat <= samoa_bbox[4] & Long >= samoa_bbox[1] & Long <= samoa_bbox[3], "yes", "no")) 
samoa_haw_ams <- subset(samoa_haw_ams, passes_through_box == "yes")

samoa_haw_ams_cpues <- data.frame("Country" = "Samoa",
                                  "Source" = "haw_ams",
                                  "Blueshark" = mean(samoa_haw_ams$CPUE.Blueshark, na.rm = T),
                                  "Silky" = mean(samoa_haw_ams$CPUE.Silky, na.rm=T),
                                  "unidmako" = mean(samoa_haw_ams$CPUE.unidmako, na.rm=T),
                                  "Shortfinmako" = mean(samoa_haw_ams$CPUE.Shortfinmako, na.rm=T),
                                  "longfinmako" = mean(samoa_haw_ams$CPUE.longfinmako, na.rm=T),
                                  "allthresher"= mean(samoa_haw_ams$CPUE.allthresher, na.rm=T),
                                  "allhammers" = mean(samoa_haw_ams$CPUE.allhammers, na.rm=T),
                                  "oceanicwhitetip" = mean(samoa_haw_ams$CPUE.oceanicwhitetip, na.rm=T),
                                  "othersharks" = mean(samoa_haw_ams$CPUE.othersharks, na.rm=T))
samoa_wcpfc_cpues[1,5] = NA 
samoa_cpues <- rbind(samoa_wcpfc_cpues,samoa_haw_ams_cpues)

#### Palau ####
palau <- eezs %>% 
  filter(ISO_Ter1 == 'PLW' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

# Get bounding box for EEZ
palau_bbox <- sf::st_bbox(palau)

palau_wcpfc <- wcpfc %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= palau_bbox[2] & Lat <= palau_bbox[4] & Long >= palau_bbox[1] & Long <= palau_bbox[3], "yes", "no")) 
palau_wcpfc <- subset(palau_wcpfc, passes_through_box == "yes")

palau_wcpfc_cpues <- data.frame("Country" = "Palau",
                                "Source" = "wcpfc",
                                "Blueshark" = mean(palau_wcpfc$CPUE.Blueshark, na.rm = T),
                                "Silky" = mean(palau_wcpfc$CPUE.Silky, na.rm=T),
                                "unidmako" = mean(palau_wcpfc$CPUE.unidmako, na.rm=T),
                                "Shortfinmako" = mean(palau_wcpfc$CPUE.Shortfinmako, na.rm=T),
                                "longfinmako" = mean(palau_wcpfc$CPUE.longfinmako, na.rm=T),
                                "allthresher"= mean(palau_wcpfc$CPUE.allthresher, na.rm=T),
                                "allhammers" = mean(palau_wcpfc$CPUE.allhammers, na.rm=T),
                                "oceanicwhitetip" = mean(palau_wcpfc$CPUE.oceanicwhitetip, na.rm=T),
                                "othersharks" = mean(palau_wcpfc$CPUE.othersharks, na.rm=T))

palau_gilman <- read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/palau cpues.csv")
palau_gilman <- as.matrix(palau_gilman)
palau_gilman <- t(palau_gilman)
palau_gilman <- as.data.frame(palau_gilman, row.names = NULL, col.names=palau_gilman[1,])
names(palau_gilman) <- palau_gilman[1,]
palau_gilman <- palau_gilman[3,]
row.names(palau_gilman) = NULL
colnames(palau_gilman)[1] = "Bigeyethresher"
colnames(palau_gilman)[2] = "Bignose"
colnames(palau_gilman)[3] = "Blacktipreef"
colnames(palau_gilman)[4] = "Blueshark"
colnames(palau_gilman)[5] = "Copper"
colnames(palau_gilman)[6] = "Common thresher"
colnames(palau_gilman)[7] = "Crocodile"
colnames(palau_gilman)[8] = "Galapagos"
colnames(palau_gilman)[9] = "Great hammer"

palau_gilman$allthresher.CPUE <- palau_gilman$`Bigeye thresher` + palau_gilman$`Common thresher` +
  palau_gilman$`Pelagic thresher` + palau_gilman$`Thresher spp.`

master_cpues <-rbind(FP_cpues,cook_cpues,samoa_cpues)

#### Kiribati - need to split shapefiles for different territories ####
Kiribati <- eezs %>% 
  filter(ISO_Ter1 == 'KIR' & Pol_type == '200NM') %>% 
  dplyr::select(ISO_Ter1, geometry)

#### Line Islands ####
KIR_line <- Kiribati[2,]

# Get bounding box for EEZ
kir_line_bbox <- sf::st_bbox(KIR_line)

kir_line_wcpfc <- wcpfc %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= kir_line_bbox[2] & Lat <= kir_line_bbox[4] & Long >= kir_line_bbox[1] & Long <= kir_line_bbox[3], "yes", "no")) 
kir_line_wcpfc <- subset(kir_line_wcpfc, passes_through_box == "yes")

KIR_line_wcpfc_cpues <- data.frame("Country" = "Kiribati - Line Islands",
                                   "Source" = "wcpfc",
                                   "Blueshark" = mean(kir_line_wcpfc$CPUE.Blueshark, na.rm = T),
                                   "Silky" = mean(kir_line_wcpfc$CPUE.Silky, na.rm=T),
                                   "unidmako" = mean(kir_line_wcpfc$CPUE.unidmako, na.rm=T),
                                   "Shortfinmako" = mean(kir_line_wcpfc$CPUE.Shortfinmako, na.rm=T),
                                   "longfinmako" = mean(kir_line_wcpfc$CPUE.longfinmako, na.rm=T),
                                   "allthresher"= mean(kir_line_wcpfc$CPUE.allthresher, na.rm=T),
                                   "allhammers" = mean(kir_line_wcpfc$CPUE.allhammers, na.rm=T),
                                   "oceanicwhitetip" = mean(kir_line_wcpfc$CPUE.oceanicwhitetip, na.rm=T),
                                   "othersharks" = mean(kir_line_wcpfc$CPUE.othersharks, na.rm=T))

kir_line_haw_ams <- haw_ams %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= kir_line_bbox[2] & Lat <= kir_line_bbox[4] & Long >= kir_line_bbox[1] & Long <= kir_line_bbox[3], "yes", "no")) 
kir_line_haw_ams <- subset(kir_line_haw_ams, passes_through_box == "yes")

KIR_line_haw_ams_cpues <- data.frame("Country" = "Kiribati - Line Islands",
                                     "Source" = "haw_ams",
                                     "Blueshark" = mean(kir_line_haw_ams$CPUE.Blueshark, na.rm = T),
                                     "Silky" = mean(kir_line_haw_ams$CPUE.Silky, na.rm=T),
                                     "unidmako" = mean(kir_line_haw_ams$CPUE.unidmako, na.rm=T),
                                     "Shortfinmako" = mean(kir_line_haw_ams$CPUE.Shortfinmako, na.rm=T),
                                     "longfinmako" = mean(kir_line_haw_ams$CPUE.longfinmako, na.rm=T),
                                     "allthresher"= mean(kir_line_haw_ams$CPUE.allthresher, na.rm=T),
                                     "allhammers" = mean(kir_line_haw_ams$CPUE.allhammers, na.rm=T),
                                     "oceanicwhitetip" = mean(kir_line_haw_ams$CPUE.oceanicwhitetip, na.rm=T),
                                     "othersharks" = mean(kir_line_haw_ams$CPUE.othersharks, na.rm=T))

KIR_line_cpues <- rbind(KIR_line_wcpfc_cpues, KIR_line_haw_ams_cpues)

#### Phoenix Islands ####
KIR_phoenix <- Kiribati[1,]

# Get bounding box for EEZ
kir_phoenix_bbox <- sf::st_bbox(KIR_phoenix)

kir_phoenix_wcpfc <- wcpfc %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= kir_phoenix_bbox[2] & Lat <= kir_phoenix_bbox[4] & Long >= kir_phoenix_bbox[1] & Long <= kir_phoenix_bbox[3], "yes", "no")) 
kir_phoenix_wcpfc <- subset(kir_phoenix_wcpfc, passes_through_box == "yes")

KIR_phoenix_wcpfc_cpues <- data.frame("Country" = "Kiribati - Phoenix Islands",
                                      "Source" = "wcpfc",
                                      "Blueshark" = mean(kir_phoenix_wcpfc$CPUE.Blueshark, na.rm = T),
                                      "Silky" = mean(kir_phoenix_wcpfc$CPUE.Silky, na.rm=T),
                                      "unidmako" = mean(kir_phoenix_wcpfc$CPUE.unidmako, na.rm=T),
                                      "Shortfinmako" = mean(kir_phoenix_wcpfc$CPUE.Shortfinmako, na.rm=T),
                                      "longfinmako" = mean(kir_phoenix_wcpfc$CPUE.longfinmako, na.rm=T),
                                      "allthresher"= mean(kir_phoenix_wcpfc$CPUE.allthresher, na.rm=T),
                                      "allhammers" = mean(kir_phoenix_wcpfc$CPUE.allhammers, na.rm=T),
                                      "oceanicwhitetip" = mean(kir_phoenix_wcpfc$CPUE.oceanicwhitetip, na.rm=T),
                                      "othersharks" = mean(kir_phoenix_wcpfc$CPUE.othersharks, na.rm=T))

kir_phoenix_haw_ams <- haw_ams %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= kir_phoenix_bbox[2] & Lat <= kir_phoenix_bbox[4] & Long >= kir_phoenix_bbox[1] & Long <= kir_phoenix_bbox[3], "yes", "no")) 
kir_phoenix_haw_ams <- subset(kir_phoenix_haw_ams, passes_through_box == "yes")

KIR_phoenix_haw_ams_cpues <- data.frame("Country" = "Kiribati - Phoenix Islands",
                                        "Source" = "haw_ams",
                                        "Blueshark" = mean(kir_phoenix_haw_ams$CPUE.Blueshark, na.rm = T),
                                        "Silky" = mean(kir_phoenix_haw_ams$CPUE.Silky, na.rm=T),
                                        "unidmako" = mean(kir_phoenix_haw_ams$CPUE.unidmako, na.rm=T),
                                        "Shortfinmako" = mean(kir_phoenix_haw_ams$CPUE.Shortfinmako, na.rm=T),
                                        "longfinmako" = mean(kir_phoenix_haw_ams$CPUE.longfinmako, na.rm=T),
                                        "allthresher"= mean(kir_phoenix_haw_ams$CPUE.allthresher, na.rm=T),
                                        "allhammers" = mean(kir_phoenix_haw_ams$CPUE.allhammers, na.rm=T),
                                        "oceanicwhitetip" = mean(kir_phoenix_haw_ams$CPUE.oceanicwhitetip, na.rm=T),
                                        "othersharks" = mean(kir_phoenix_haw_ams$CPUE.othersharks, na.rm=T))

KIR_phoenix_cpues <- rbind(KIR_phoenix_wcpfc_cpues, KIR_phoenix_haw_ams_cpues)

#### Gilbert Islands ####
KIR_gilbert <- Kiribati[3,]


# Get bounding box for EEZ
kir_gilbert_bbox <- sf::st_bbox(KIR_gilbert)

kir_gilbert_bbox_w <- c(167.8684,kir_gilbert_bbox[2], kir_gilbert_bbox[3], kir_gilbert_bbox[4])


kir_gilbert_w_wcpfc <- wcpfc %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= kir_gilbert_bbox_w[2] & Lat <= kir_gilbert_bbox_w[4] & Long >= kir_gilbert_bbox_w[1] & Long <= kir_gilbert_bbox_w[3], "yes", "no")) 
kir_gilbert_w_wcpfc <- subset(kir_gilbert_w_wcpfc, passes_through_box == "yes")

KIR_gilbert_w_wcpfc_cpues <- data.frame("Country" = "Kiribati - Gilbert Islands",
                                        "Source" = "wcpfc",
                                        "Blueshark" = mean(kir_gilbert_wcpfc$CPUE.Blueshark, na.rm = T),
                                        "Silky" = mean(kir_gilbert_wcpfc$CPUE.Silky, na.rm=T),
                                        "unidmako" = mean(kir_gilbert_wcpfc$CPUE.unidmako, na.rm=T),
                                        "Shortfinmako" = mean(kir_gilbert_wcpfc$CPUE.Shortfinmako, na.rm=T),
                                        "longfinmako" = mean(kir_gilbert_wcpfc$CPUE.longfinmako, na.rm=T),
                                        "allthresher"= mean(kir_gilbert_wcpfc$CPUE.allthresher, na.rm=T),
                                        "allhammers" = mean(kir_gilbert_wcpfc$CPUE.allhammers, na.rm=T),
                                        "oceanicwhitetip" = mean(kir_gilbert_wcpfc$CPUE.oceanicwhitetip, na.rm=T),
                                        "othersharks" = mean(kir_gilbert_wcpfc$CPUE.othersharks, na.rm=T))

kir_gilbert_bbox_e <- c(kir_gilbert_bbox[1],-3.7750, -179.8239, -1.5740)


kir_gilbert_e_wcpfc <- wcpfc %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat+5 >= kir_gilbert_bbox_e[2] & Lat-5 <= kir_gilbert_bbox_e[4] & Long+5 >= kir_gilbert_bbox_e[1] & Long-5 <= kir_gilbert_bbox_e[3], "yes", "no")) 
kir_gilbert_e_wcpfc <- subset(kir_gilbert_e_wcpfc, passes_through_box == "yes")

KIR_gilbert_e_wcpfc_cpues <- data.frame("Country" = "Kiribati - Gilbert Islands",
                                        "Source" = "wcpfc",
                                        "Blueshark" = mean(kir_gilbert_wcpfc$CPUE.Blueshark, na.rm = T),
                                        "Silky" = mean(kir_gilbert_wcpfc$CPUE.Silky, na.rm=T),
                                        "unidmako" = mean(kir_gilbert_wcpfc$CPUE.unidmako, na.rm=T),
                                        "Shortfinmako" = mean(kir_gilbert_wcpfc$CPUE.Shortfinmako, na.rm=T),
                                        "longfinmako" = mean(kir_gilbert_wcpfc$CPUE.longfinmako, na.rm=T),
                                        "allthresher"= mean(kir_gilbert_wcpfc$CPUE.allthresher, na.rm=T),
                                        "allhammers" = mean(kir_gilbert_wcpfc$CPUE.allhammers, na.rm=T),
                                        "oceanicwhitetip" = mean(kir_gilbert_wcpfc$CPUE.oceanicwhitetip, na.rm=T),
                                        "othersharks" = mean(kir_gilbert_wcpfc$CPUE.othersharks, na.rm=T))


kir_gilbert_haw_ams <- haw_ams %>% 
  #check if measurement is withing boundaries (yes, or not (no))
  mutate( passes_through_box = ifelse( Lat >= kir_gilbert_bbox[2] & Lat <= kir_gilbert_bbox[4] & Long >= kir_gilbert_bbox[1] & Long <= kir_gilbert_bbox[3], "yes", "no")) 
kir_gilbert_haw_ams <- subset(kir_gilbert_haw_ams, passes_through_box == "yes")

KIR_gilbert_haw_ams_cpues <- data.frame("Country" = "Kiribati - Gilbert Islands",
                                        "Source" = "haw_ams",
                                        "Blueshark" = mean(kir_gilbert_haw_ams$CPUE.Blueshark, na.rm = T),
                                        "Silky" = mean(kir_gilbert_haw_ams$CPUE.Silky, na.rm=T),
                                        "unidmako" = mean(kir_gilbert_haw_ams$CPUE.unidmako, na.rm=T),
                                        "Shortfinmako" = mean(kir_gilbert_haw_ams$CPUE.Shortfinmako, na.rm=T),
                                        "longfinmako" = mean(kir_gilbert_haw_ams$CPUE.longfinmako, na.rm=T),
                                        "allthresher"= mean(kir_gilbert_haw_ams$CPUE.allthresher, na.rm=T),
                                        "allhammers" = mean(kir_gilbert_haw_ams$CPUE.allhammers, na.rm=T),
                                        "oceanicwhitetip" = mean(kir_gilbert_haw_ams$CPUE.oceanicwhitetip, na.rm=T),
                                        "othersharks" = mean(kir_gilbert_haw_ams$CPUE.othersharks, na.rm=T))

KIR_gilbert_cpues <- rbind(KIR_gilbert_wcpfc_cpues, KIR_gilbert_haw_ams_cpues)




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