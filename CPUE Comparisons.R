
#within grid cells containing any part of Palau
data_palau <- subset(data, Lat >= 0 & Lat < 15 & Long >=125 & Long < 140)
colnames(data_palau)[9] <- "CPUE"

data_palau_species <- aggregate(data=data_palau, CPUE~Species, FUN=mean)

colnames(data_palau_species)[1] = "Common"
colnames(data_palau_species)[2] = "CPUE"
data_palau_species$Group = "WCPFC"

data_palau_species[1,1] <- "BIGEYE THRESHER"
data_palau_species[4,1] <- "PELAGIC THRESHER"

palau_primary <- read.csv("palau cpues.csv")
palau_primary$Common = palau_primary$Common %>% toupper()
palau_primary <- subset(palau_primary, Common == "BLUE SHARK" |
                          Common == "SILKY SHARK" |
                          Common == "PELAGIC THRESHER" |
                          Common == "BIGEYE THRESHER" |
                          Common == "SHORTFIN MAKO")
palau_primary <- data.frame("Common" = palau_primary$Common, "CPUE" = palau_primary$CPUE, "Group" = "Primary Lit")

data_palau_species <- subset(data_palau_species, Common == "BLUE SHARK" |
                          Common == "SILKY SHARK" |
                          Common == "PELAGIC THRESHER" |
                          Common == "BIGEYE THRESHER" |
                          Common == "SHORTFIN MAKO")

palau_combined <- rbind(palau_primary, data_palau_species)
palau_primary$CPUE1=palau_primary$CPUE
data_palau_species$CPUE2=data_palau_species$CPUE
palau_combined2<-merge(palau_primary,data_palau_species,by="Common")



ggplot(data=palau_combined, aes(x=Common, y=CPUE, fill=Group)) +
  geom_bar(stat = "identity", position="dodge") +
  ggtitle("CPUE Comparisons - Palau") +
  xlab("Species") +
  labs(fill="Data Source")

#### Expand out additional 5 degrees in each direction ####
data_palau <- subset(data, Lat >= -5 & Lat < 20 & Long >=120 & Long < 145)
colnames(data_palau)[9] <- "CPUE"

data_palau_species <- aggregate(data=data_palau, CPUE~Species, FUN=mean)

colnames(data_palau_species)[1] = "Common"
colnames(data_palau_species)[2] = "CPUE"
data_palau_species$Group = "WCPFC"

data_palau_species[1,1] <- "BIGEYE THRESHER"
data_palau_species[4,1] <- "PELAGIC THRESHER"

data_palau_species <- subset(data_palau_species, Common == "BLUE SHARK" |
                               Common == "SILKY SHARK" |
                               Common == "PELAGIC THRESHER" |
                               Common == "BIGEYE THRESHER" |
                               Common == "SHORTFIN MAKO")

palau_combined <- rbind(palau_primary, data_palau_species)
str(palau_combined)

ggplot(data=palau_combined, aes(x=Common, y=CPUE, fill=Group)) +
  geom_bar(stat = "identity", position="dodge") +
  ggtitle("CPUE Comparisons - Palau") +
  xlab("Species") +
  labs(fill="Data Source")

#### Marshall Islands ####

MI_primary <- read.csv("marshallislands_cpues.csv")
MI_primary$Species[1] = "PELAGIC THRESHER"
MI_primary$Species[2] = "BIGEYE THRESHER"
MI_primary$Species[3] = "COMMON THRESHER"
MI_primary$Species[17] = "BLUE SHARK"
MI_primary$Species[13] = "SHORTFIN MAKO"
MI_primary$Species[14] = "LONGFIN MAKO"
MI_primary$Species[10] = "OCEANIC WHITETIP SHARK"
MI_primary$Species[18] = "SCALLOPED HAMMERHEAD"
MI_primary$Species[7] = "SILKY SHARK"

colnames(MI_primary)[1]<-"Common"
  
MI_primary <- subset(MI_primary, Common == "BLUE SHARK" |
                          Common == "SILKY SHARK" |
                          Common == "PELAGIC THRESHER" |
                          Common == "BIGEYE THRESHER" |
                       Common == "OCEANIC WHITETIP SHARK" |
                       Common == "SCALLOPED HAMMERHEAD" |
                       Common == "COMMON THRESHER" |
                       Common == "LONGFIN MAKO" |
                       Common == "SHORTFIN MAKO")
MI_primary <- data.frame("Common" = MI_primary$Common, "CPUE" = MI_primary$CPUE, "Group" = "Primary Lit")

data_MI<- subset(data, Lat >= 0 & Lat < 20 & Long >=155 & Long < 180)
colnames(data_MI)[9] <- "CPUE"

data_MI_species <- aggregate(data=data_MI, CPUE~Species, FUN=mean)
data_MI_species[1,1] <- "BIGEYE THRESHER"
data_MI_species[6,1] <- "PELAGIC THRESHER"
data_MI_species[11,1] <- "COMMON THRESHER"
colnames(data_MI_species)[1] <- "Common"
data_MI_species <- subset(data_MI_species, Common == "BLUE SHARK" |
                            Common == "SILKY SHARK" |
                            Common == "PELAGIC THRESHER" |
                            Common == "BIGEYE THRESHER" |
                            Common == "OCEANIC WHITETIP SHARK" |
                            Common == "SCALLOPED HAMMERHEAD" |
                            Common == "COMMON THRESHER" |
                            Common == "LONGFIN MAKO" |
                            Common == "SHORTFIN MAKO")
data_MI_species$Group = "WCPFC"

MI_combined <- rbind(MI_primary, data_MI_species)
MI_primary$CPUE1=MI_primary$CPUE
data_MI_species$CPUE2=data_MI_species$CPUE
MI_combined2<-merge(MI_primary,data_MI_species,by="Common")

ggplot(data=MI_combined, aes(x=Common, y=CPUE, fill=Group)) +
  geom_bar(stat = "identity", position="dodge") +
  ggtitle("CPUE Comparisons - Marshall Islands") +
  xlab("Species") +
  labs(fill="Data Source") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

MI_combined2$region="MI"
palau_combined2$region="Palau"
MI_palau_combined=rbind(MI_combined2,palau_combined2)

ggplot(MI_palau_combined) + 
  geom_point(aes(x=CPUE1,y=CPUE2, color=Common)) +
  geom_abline(intercept = 0, slope=1, aes(colour="red")) +
  xlim(c(0,1.25)) +
  ylim(c(0,1.25)) +
  xlab("Primary Lit") +
  ylab("Nominal CPUEs") +
  labs(color="Species")+
  theme_bw()
  
