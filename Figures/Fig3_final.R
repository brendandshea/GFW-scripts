library(tidyverse)

setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#titles for genera
alopias_title <- expression(paste(italic("Alopias")," spp."))
isurus_title <- expression(paste(italic("Isurus")," spp."))
sphyrnas_title <- expression(paste(italic("Sphyrnas")," spp."))

#color palette
my_pal <- rcartocolor::carto_pal(n = 12, name = "Bold")[c(3,4,8,1,7,12,5)] 

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

#plotting ####

#break into high catch and low catch 
highcatch <- subset(allsanctuary_summary, Sanct == "FSM" | Sanct == "Palau" | Sanct == "Marshall Islands")
lowcatch <- subset(allsanctuary_summary, Sanct != "FSM" & Sanct != "Palau" & Sanct != "Marshall Islands")

highcatches <- ggplot(data=highcatch, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal, guide='none') +
  scale_fill_manual(values=my_pal
  ) +
  theme(legend.text.align=0,
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major.y = element_line(colour="grey90"),
        panel.grid.major.x=element_blank(),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position = 'none',
        axis.title = element_blank()
  ) +
  scale_x_discrete(guide = 'none') +
  facet_wrap(~Sanct) +
  theme(strip.background = element_blank())

highcatches

lowcatches <- ggplot(data=lowcatch, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal, guide='none') +
  scale_fill_manual(values=my_pal, guide='none') +
  theme(legend.text.align=0,
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major.y = element_line(colour="grey90"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position = 'none',
        axis.title = element_blank()
  ) +
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  facet_wrap(~Sanct) +
  theme(strip.background = element_blank())

lowcatches
library(ggpubr)
Fig3 <- ggarrange(highcatches,lowcatches, ncol=1, heights = c(1,2), common.legend = T, 
                  labels="AUTO", legend='none')
Fig3 <- Fig3 %>%
  annotate_figure(Fig3, left = grid::textGrob("Total Catch (Bar Height) and Mortality (Shaded) in # of Individuals", 
                                              rot = 90, vjust = 1), fig.lab.size = 6)
#inset maps ####



Fig3insets <- Fig3 +
  patchwork::inset_element(micronesia_map, 0.185, 0.84, .385, .95)+ #row 1 col 1
  patchwork::inset_element(marshallislands_map, 0.5, 0.815,.68, .95)+
  patchwork::inset_element(palau_map, 0.83, 0.81, .98, .95)+ #row 2 col 1
  patchwork::inset_element(kiribati_map, 0.135, 0.52, .34, .63)+
  patchwork::inset_element(frenchpolynesia_map, 0.5, 0.46, .685, .63)+ #row 3 col 1
  patchwork::inset_element(newcaledonia_map, 0.8, 0.5, 1, .63) +
  patchwork::inset_element(cookislands_map, .17, 0.19, 0.35, .35)+ #row 4 col 1
  patchwork::inset_element(samoa_map, 0.48, 0.19, .66, .35) + #row4 col2
  patchwork::plot_layout(guides='collect') &
  theme(legend.position = 'bottom',
        legend.justification = 'right')

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/Fig3final.png",
       Fig3insets, width = 8, height = 8, dpi=300, bg="white")

