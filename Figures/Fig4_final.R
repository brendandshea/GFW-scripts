setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

alopias_title <- expression(paste(italic("Alopias")," spp."))
isurus_title <- expression(paste(italic("Isurus")," spp."))
sphyrnas_title <- expression(paste(italic("Sphyrnas")," spp."))

my_pal2 <- rcartocolor::carto_pal(n=8, name = "Antique")

#summaries and combine
palau_summary <- read.csv("palau_summary.csv"); palau_summary$Sanct <- "Palau"
micronesia_summary <- read.csv("micronesia_summary.csv"); micronesia_summary$Sanct <- "Federated States of Micronesia"
frenchpolynesia_summary <- read.csv("frenchpolynesia_summary.csv"); frenchpolynesia_summary$Sanct <- "French Polynesia"
newcaledonia_summary <- read.csv("newcaledonia_summary.csv"); newcaledonia_summary$Sanct <- "New Caledonia"
samoa_summary <- read.csv("samoa_summary.csv"); samoa_summary$Sanct <- "Samoa"
cookislands_summary <- read.csv("cookislands_summary.csv"); cookislands_summary$Sanct <- "Cook Islands"
kiribati_summary <- read.csv("kiribati_summary.csv"); kiribati_summary$Sanct <- "Kiribati"
marshallislands_summary <- read.csv("marshallislands_summary.csv"); marshallislands_summary$Sanct <- "Marshall Islands"

allsanctuary_summary <- rbind(palau_summary,
                              micronesia_summary,
                              newcaledonia_summary,
                              samoa_summary,
                              kiribati_summary,
                              marshallislands_summary,
                              cookislands_summary,
                              frenchpolynesia_summary)

allsanctuary_summary$Species <- as.factor(allsanctuary_summary$Species)
allsanctuary_summary$Species <- factor(allsanctuary_summary$Species, 
                                       levels=c("Blue Shark", 
                                                "Silky Shark", 
                                                "Thresher", 
                                                "Mako", 
                                                "Oceanic Whitetip", 
                                                "Hammerhead", 
                                                "Other Sharks"
                                       )
)

allsanctuary_summary$Sanct <- ifelse(allsanctuary_summary$Sanct == "Federated States of Micronesia", "FSM", allsanctuary_summary$Sanct)
allsanctuary_summary$Sanct <- as.factor(allsanctuary_summary$Sanct)
allsanctuary_summary$Sanct <- factor(allsanctuary_summary$Sanct, levels=c("FSM",
                                                                          "Marshall Islands",
                                                                          "Palau",
                                                                          "Kiribati",
                                                                          "French Polynesia",
                                                                          "New Caledonia",
                                                                          "Cook Islands",
                                                                          "Samoa"
)
)

####species plots - silky ####
allsilky <- subset(allsanctuary_summary, Species == "Silky Shark")
allsilky$Sanctarea <- c(604289, 2992597, 1245000,128000, 3437132, 1992232, 1960135, 4767242)
allsilky$total.upr <- allsilky$Total + 1.96*allsilky$Total.sd
allsilky$totalbyarea <- allsilky$Total/allsilky$Sanctarea*1000
allsilky$uprbyarea <- allsilky$total.upr/allsilky$Sanctarea*1000

fal_msy=12162*1000 #convert metric tons to kg
fal_wt<-35.4 #animal wt in kg
fal_stockarea <- 59689229 #km2

silky <- ggplot(allsilky, aes(x=Sanct, y=totalbyarea, fill=Sanct)) +
  geom_bar(stat="identity", color='black') +
  geom_errorbar(aes(ymin=totalbyarea, ymax=uprbyarea), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_hline(aes(yintercept=(fal_msy/fal_wt/fal_stockarea*1000),linetype=''), color='red') + #to get to per 1000 km2
  scale_y_continuous(name=expression(italic("M"[Sanctuary]))) +
  scale_x_discrete(name="Sanctuary") +
  scale_fill_manual(guide='none', values=my_pal2) +
  #scale_color_manual(guide='none', values=my_pal2) +
  scale_linetype_manual(name = expression(italic("H"[MSY])), values = 1, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme(panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major.y = element_line(colour="grey90"),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title.x=element_blank(),
        legend.position = 'bottom')

silky


#mako
allmako <- subset(allsanctuary_summary, Species == "Mako")
allmako$Sanctarea <- c(604289, 2992597, 1245000,128000, 3437132, 1992232, 1960135, 4767242)
allmako <- subset(allmako, Sanct == "FSM" | Sanct == "Palau" | Sanct == "Marshall Islands" | Sanct == "Kiribati")

#Detailed preds for splitting north
total_preds<-read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/total_preds.csv")  
mako <- subset(total_preds, Species == "Mako")
mako$total.var <- mako$total.sd^2
north <- subset(mako, Lat >= 0)
south <- subset(mako, Lat < 0)

mako_north <- aggregate(data=north, total~country, FUN='sum')
mako_north.var <- aggregate(data=north, total.var~country, FUN='sum')
mako_north$total.sd <- sqrt(mako_north.var$total.var)

#FSM
FSM_mako = 941.13161; FSM_mako.sd = 8.144492; FSM_north= 2949341.2953
allmako$Total <- ifelse(allmako$Sanct == "FSM", FSM_mako, allmako$Total)
allmako$Total.sd <- ifelse(allmako$Sanct=="FSM", FSM_mako.sd, allmako$Total.sd)
allmako$Sanctarea <- ifelse(allmako$Sanct=="FSM", FSM_north, allmako$Sanctarea)

#KIR
KIR_mako = 91.56491; KIR_mako.sd = 1.929176; KIR_north= 1002722

allmako$Total <- ifelse(allmako$Sanct == "KIR", KIR_mako, allmako$Total)
allmako$Total.sd <- ifelse(allmako$Sanct=="KIR", KIR_mako.sd, allmako$Total.sd)
allmako$Sanctarea <- ifelse(allmako$Sanct=="KIR", KIR_north, allmako$Sanctarea)

allmako$total.upr <- allmako$Total + 1.96*allmako$Total.sd
allmako$totalbyarea <- allmako$Total/allmako$Sanctarea*1000
allmako$uprbyarea <- allmako$total.upr/allmako$Sanctarea*1000


mak_msy=3127*1000 #convert metric tons to kg
mak_wt<-31.3 #animal wt in kg
mak_stockarea <- 38027072 #km2

mako <- ggplot(allmako, aes(x=Sanct, y=totalbyarea, fill=Sanct)) +
  geom_bar(stat="identity", color='black') +
  geom_errorbar(aes(ymin=totalbyarea, ymax=uprbyarea), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_hline(aes(yintercept=(mak_msy/mak_wt/mak_stockarea*1000),linetype=""),color='red') + #to get to per 1000 km2
  scale_y_continuous(name=expression(italic("M"[Sanctuary]))) +
  scale_x_discrete(name="Sanctuary") +
  scale_fill_manual(guide='none', values=my_pal2) +
  scale_color_manual(guide='none', values=my_pal2) +
  scale_linetype_manual(name = expression(italic("H"[MSY])), values = 1, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme(panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major.y = element_line(colour="grey90"),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title.x=element_blank(),
        legend.position = 'bottom')

mako
#### oceanicwhitetip ####

alloceanicwhitetip <- subset(allsanctuary_summary, Species == "Oceanic Whitetip")
alloceanicwhitetip$Sanctarea <- c(604289, 2992597, 1245000,128000, 3437132, 1992232, 1960135, 4767242)
alloceanicwhitetip$total.upr <- alloceanicwhitetip$Total + 1.96*alloceanicwhitetip$Total.sd
alloceanicwhitetip$totalbyarea <- alloceanicwhitetip$Total/alloceanicwhitetip$Sanctarea*1000
alloceanicwhitetip$uprbyarea <- alloceanicwhitetip$total.upr/alloceanicwhitetip$Sanctarea*1000

ocs_msy=7055*1000 #convert metric tons to kg
ocs_wt<-51.3 #animal wt in kg
ocs_stockarea <- 57585206 #km2

oceanicwhitetip <- ggplot(alloceanicwhitetip, aes(x=Sanct, y=totalbyarea, fill=Sanct)) +
  geom_bar(stat="identity", color='black') +
  geom_errorbar(aes(ymin=totalbyarea, ymax=uprbyarea), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_hline(aes(yintercept=(ocs_msy/ocs_wt/ocs_stockarea*1000),linetype=''), color='red') + #to get to per 1000 km2
  scale_y_continuous(expression(italic("M"[Sanctuary]))) +
  scale_x_discrete(name="Sanctuary") +
  scale_fill_manual(guide='none', values=my_pal2) +
  scale_color_manual(guide='none', values=my_pal2) +
  scale_linetype_manual(name = expression(italic("H"[MSY])), values = 1, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme(panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major.y = element_line(colour="grey90"),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle=45,hjust=1),
        legend.position = 'bottom')
oceanicwhitetip
#### blue ####

allblue <- subset(allsanctuary_summary, Species == "Blue Shark")
allblue$Sanctarea <- c(604289, 2992597, 1245000,128000, 3437132, 1992232, 1960135, 4767242)
allblue_n <- subset(allblue, Sanct== "FSM" | Sanct == "Kiribati" | Sanct == "Palau" | Sanct == "Marshall Islands")
allblue_s <- subset(allblue, Sanct== "Samoa" | Sanct == "FSM" | Sanct == "Kiribati" | Sanct == "New Caledonia" | Sanct == "Cook Islands" | Sanct == "French Polynesia")   

#north
#find north totals
blueshark <- subset(total_preds, Species == "Blue Shark")
blueshark$total.var <- blueshark$total.sd^2
north <- subset(blueshark, Lat >= 0)
south <- subset(blueshark, Lat <0 )

bsh_north <- aggregate(data=north, total~country, FUN='sum')
bsh_north.var <- aggregate(data=north, total.var~country, FUN='sum')
bsh_north$total.sd <- sqrt(bsh_north.var$total.var)
#FSM
FSM_blue_n = 10702.0660; FSM_blue.sd_n = 86.767187; FSM_north= 2949341.2953

allblue_n$Total <- ifelse(allblue_n$Sanct == "FSM", FSM_blue_n, allblue_n$Total)
allblue_n$Total.sd <- ifelse(allblue_n$Sanct=="FSM", FSM_blue.sd_n, allblue_n$Total.sd)
allblue_n$Sanctarea <- ifelse(allblue_n$Sanct=="FSM", FSM_north, allblue_n$Sanctarea)

#KIR
KIR_blue_n = 425.7126; KIR_blue.sd_n = 8.200335; KIR_north= 1002722

allblue_n$Total <- ifelse(allblue_n$Sanct == "KIR", KIR_blue_n, allblue_n$Total)
allblue_n$Total.sd <- ifelse(allblue_n$Sanct=="KIR", KIR_blue.sd_n, allblue_n$Total.sd)
allblue_n$Sanctarea <- ifelse(allblue_n$Sanct=="KIR", KIR_north, allblue_n$Sanctarea)

allblue_n$total.upr <- allblue_n$Total + 1.96*allblue_n$Total.sd
allblue_n$totalbyarea <- allblue_n$Total/allblue_n$Sanctarea*1000
allblue_n$uprbyarea <- allblue_n$total.upr/allblue_n$Sanctarea*1000

bsh_msy_n=99927*1000 #convert metric tons to kg
bsh_wt<-24.9 #animal wt in kg
bsh_stockarea_n <- 38585660  #km2

bluesharks_n <- ggplot(allblue_n, aes(x=Sanct, y=totalbyarea, fill=Sanct)) +
  geom_bar(stat="identity", color='black') +
  geom_errorbar(aes(ymin=totalbyarea, ymax=uprbyarea), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_hline(aes(yintercept=(bsh_msy_n/bsh_wt/bsh_stockarea_n*1000),linetype=''), color='red') + #to get to per 1000 km2
  scale_y_continuous(name=expression(italic("M"[Sanctuary]))) +
  scale_x_discrete(name="Sanctuary") +
  scale_fill_manual(guide='none', values=my_pal2) +
  scale_color_manual(guide='none', values=my_pal2) +
  scale_linetype_manual(name = expression(italic("H"[MSY])), values = 1, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme(panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major.y = element_line(colour="grey90"),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title.x=element_blank(),
        legend.position = 'bottom')
bluesharks_n

#south
#find southtotals
bsh_south <- aggregate(data=south, total~country, FUN='sum')
bsh_south.var <- aggregate(data=south, total.var~country, FUN='sum')
bsh_south$total.sd <- sqrt(bsh_south.var$total.var)
#FSM
FSM_blue_s = 1.270592; FSM_blue.sd_s = 0.1258953; FSM_south= 61302.7656

allblue_s$Total <- ifelse(allblue_s$Sanct == "FSM", FSM_blue_s, allblue_s$Total)
allblue_s$Total.sd <- ifelse(allblue_s$Sanct=="FSM", FSM_blue.sd_s, allblue_s$Total.sd)
allblue_s$Sanctarea <- ifelse(allblue_s$Sanct=="FSM", FSM_south, allblue_s$Sanctarea)

#KIR
KIR_blue_s = 1986.578117; KIR_blue.sd_s = 22.4830764; KIR_south= 2437497.8708

allblue_s$Total <- ifelse(allblue_s$Sanct == "KIR", KIR_blue_s, allblue_s$Total)
allblue_s$Total.sd <- ifelse(allblue_s$Sanct=="KIR", KIR_blue.sd_s, allblue_s$Total.sd)
allblue_s$Sanctarea <- ifelse(allblue_s$Sanct=="KIR", KIR_south, allblue_s$Sanctarea)

allblue_s$total.upr <- allblue_s$Total + 1.96*allblue_s$Total.sd
allblue_s$totalbyarea <- allblue_s$Total/allblue_s$Sanctarea*1000
allblue_s$uprbyarea <- allblue_s$total.upr/allblue_s$Sanctarea*1000

bsh_msy_s=13234*1000 #convert metric tons to kg
bsh_wt<-24.9 #animal wt in kg
bsh_stockarea_s <- 36025997  #km2

bluesharks_s <- ggplot(allblue_s, aes(x=Sanct, y=totalbyarea, fill=Sanct)) +
  geom_bar(stat="identity", color='black') +
  geom_errorbar(aes(ymin=totalbyarea, ymax=uprbyarea), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_hline(aes(yintercept=(bsh_msy_s/bsh_wt/bsh_stockarea_s*1000),linetype=''), color='red') + #to get to per 1000 km2
  scale_y_continuous(name=expression(italic("M"[Sanctuary]))) +
  scale_x_discrete(name="Sanctuary") +
  scale_fill_manual(guide='none', values=my_pal2[c(1,4:8)]) +
  scale_color_manual(guide='none', values=my_pal2[c(1,4:8)]) +
  scale_linetype_manual(name = expression(italic("H"[MSY])), values = 1, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme(panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major.y = element_line(colour="grey90"),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title.x=element_blank(),
        legend.position = 'bottom')
bluesharks_s



#### Figure paneling ####
library(ggpubr)
library(grid)

#combine plots
allstocks <- ggarrange(bsh_stock_n,bsh_stock_s,mak_stock,ocs_stock,fal_stock,
                       labels="AUTO", vjust=7,
                       nrow=1)

allstocks2 <- ggarrange(bluesharks_n, bluesharks_s,mako,oceanicwhitetip,silky,
                        nrow=1,common.legend=T, legend = 'top',align='hv')

allstocks3 <- ggarrange(allstocks,
                        allstocks2,
                        nrow=2,align='hv')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/Fig4stocks.png",allstocks3,
       height=10,width=16,dpi=300, bg='white')
