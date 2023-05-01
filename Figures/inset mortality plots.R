setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
world <- map_data("world")

#### palau ####
palau_summary <- read.csv("palau_summary.csv")
palau_summary$Species <- as.factor(palau_summary$Species)
palau_summary$Species <- factor(palau_summary$Species, levels=c("Blue Shark", "Silky Shark", "Thresher", "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"))

palau_mortality <- read.csv("palau_total_preds.csv")
palau_proj <- aggregate(data=palau_mortality, total ~ Lat + Long, FUN='sum')

palau_map <- ggplot(data=palau_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Mortality \n(Indiv.)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = plw_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(129,138), breaks=seq(130,135,by=5)) +
  scale_y_continuous(limits=c(1,13), breaks=seq(5,10,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

palau_total_plot <- ggplot(palau_summary, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total-.05, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values=my_pal) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        axis.title = element_blank(),
        axis.ticks = element_blank()
      ) +
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  ggtitle("Palau")

palau_total_plot2 <- palau_total_plot +
  patchwork::inset_element(palau_map, 0.5, 0.5, 1, 1) +
  patchwork::plot_layout(guides='keep')

palau_total_plot2

ggsave("~/Desktop/palau_plot.png", palau_total_plot2,
       height=6, width=8, bg='white')

#### micronesia ####
micronesia_summary <- read.csv("micronesia_summary.csv")
micronesia_summary$Species <- as.factor(micronesia_summary$Species)
micronesia_summary$Species <- factor(micronesia_summary$Species, levels=c("Blue Shark", "Silky Shark", "Thresher", "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"))

micronesia_mortality <- read.csv("micronesia_total_preds.csv")
micronesia_proj <- aggregate(data=micronesia_mortality, total ~ Lat + Long, FUN='sum')

micronesia_map <- ggplot(data=micronesia_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Mortality \n(Indiv.)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = fsm_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(134,167), breaks=seq(135,165,by=30)) +
  scale_y_continuous(limits=c(-3,14.5), breaks=seq(0,10,by=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

micronesia_total_plot <- ggplot(micronesia_summary, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total-.05, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values=my_pal) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        axis.title = element_blank()
  ) + 
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  ggtitle("FSM")

micronesia_total_plot2 <- micronesia_total_plot +
  patchwork::inset_element(micronesia_map, 0.4, 0.6, 1, 1) +
  patchwork::plot_layout(guides='keep')

micronesia_total_plot2

ggsave("~/Desktop/micronesia_plot.png", micronesia_total_plot2,
       height=6, width=8, bg='white')

#### kiribati ####
kiribati_summary <- read.csv("kiribati_summary.csv")
kiribati_summary$Species <- as.factor(kiribati_summary$Species)
kiribati_summary$Species <- factor(kiribati_summary$Species, levels=c("Blue Shark", "Silky Shark", "Thresher", "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"))

kiribati_mortality <- read.csv("kiribati_total_preds.csv")
kiribati_proj <- aggregate(data=kiribati_mortality, total ~ Lat + Long, FUN='sum')

kiribati_proj$maplon <- ifelse(kiribati_proj$Long<0,kiribati_proj$Long+360, kiribati_proj$Long)

kiribati_map <- ggplot(data=kiribati_proj, aes(x=maplon, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Mortality \n(Indiv.)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  scale_x_continuous(limits=c(167,215), breaks=seq(170,210,by=40), labels=c(170,-150)) +
  scale_y_continuous(limits=c(-15.5,9), breaks=seq(-15,5,by=20)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

kir_shp$long <- ifelse(kir_shp$long<0, kir_shp$long+360,kir_shp$long)
map.layer <-   geom_polygon(data = kir_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) 

kiribati_map <- kiribati_map + map.layer

kiribati_total_plot <- ggplot(kiribati_summary, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total-.05, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values=my_pal) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        axis.title = element_blank()
  ) + 
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  ggtitle("Kiribati")

kiribati_total_plot2 <- kiribati_total_plot +
  patchwork::inset_element(kiribati_map, 0.2, 0.6, .8, 1) +
  patchwork::plot_layout(guides='keep')

kiribati_total_plot2

ggsave("~/Desktop/kiribati_plot.png", kiribati_total_plot2,
       height=6, width=8, bg='white')

#### cookislands ####
cookislands_summary <- read.csv("cookislands_summary.csv")
cookislands_summary$Species <- as.factor(cookislands_summary$Species)
cookislands_summary$Species <- factor(cookislands_summary$Species, levels=c("Blue Shark", "Silky Shark", "Thresher", "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"))

cookislands_mortality <- read.csv("cookislands_total_preds.csv")
cookislands_proj <- aggregate(data=cookislands_mortality, total ~ Lat + Long, FUN='sum')

cookislands_map <- ggplot(data=cookislands_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Mortality \n(Indiv.)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = cok_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(-169,-153), breaks=seq(-165,-155,by=10)) +
  scale_y_continuous(limits=c(-27,-5), breaks=seq(-25,-10,by=15)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour="grey97"),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

cookislands_total_plot <- ggplot(cookislands_summary, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total-.05, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values=my_pal) +
  theme(panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        axis.title = element_blank(),
        legend.position = 'none'
  ) +
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  ggtitle("Cook Islands")

cookislands_total_plot2 <- cookislands_total_plot +
  patchwork::inset_element(cookislands_map, 0.4, 0.5, .9, 1) +
  patchwork::plot_layout(guides='keep')

cookislands_total_plot2

ggsave("~/Desktop/cookislands_plot.png", cookislands_total_plot2,
       height=6, width=8, bg='white')

#### frenchpolynesia ####
frenchpolynesia_summary <- read.csv("frenchpolynesia_summary.csv")
frenchpolynesia_summary$Species <- as.factor(frenchpolynesia_summary$Species)
frenchpolynesia_summary$Species <- factor(frenchpolynesia_summary$Species, levels=c("Blue Shark", "Silky Shark", "Thresher", "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"))

frenchpolynesia_mortality <- read.csv("frenchpolynesia_total_preds.csv")
frenchpolynesia_proj <- aggregate(data=frenchpolynesia_mortality, total ~ Lat + Long, FUN='sum')

frenchpolynesia_map <- ggplot(data=frenchpolynesia_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Mortality \n(Indiv.)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = pyf_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(-159.5,-130.5), breaks=seq(-155,-135,by=20)) +
  scale_y_continuous(limits=c(-33,-3), breaks=seq(-30,-5,by=25)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

frenchpolynesia_total_plot <- ggplot(frenchpolynesia_summary, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total-.05, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values=my_pal) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        axis.title = element_blank()
  ) +
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  ggtitle("French Polynesia")

frenchpolynesia_total_plot2 <- frenchpolynesia_total_plot +
  patchwork::inset_element(frenchpolynesia_map, 0.5, 0.5, 1, 1) +
  patchwork::plot_layout(guides='keep')

frenchpolynesia_total_plot2

ggsave("~/Desktop/frenchpolynesia_plot.png", frenchpolynesia_total_plot2,
       height=6, width=8, bg='white')

#### marshallislands ####
marshallislands_summary <- read.csv("marshallislands_summary.csv")
marshallislands_summary$Species <- as.factor(marshallislands_summary$Species)
marshallislands_summary$Species <- factor(marshallislands_summary$Species, levels=c("Blue Shark", "Silky Shark", "Thresher", "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"))

marshallislands_mortality <- read.csv("marshallislands_total_preds.csv")
marshallislands_proj <- aggregate(data=marshallislands_mortality, total ~ Lat + Long, FUN='sum')

marshallislands_map <- ggplot(data=marshallislands_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Mortality \n(Indiv.)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(156,177), breaks=seq(160,175,by=15)) +
  scale_y_continuous(limits=c(0.5,19), breaks=seq(5,15,by=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

marshallislands_total_plot <- ggplot(marshallislands_summary, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total-.05, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values=my_pal) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        axis.title = element_blank()
  ) +
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  ggtitle("Marshall Islands")

marshallislands_total_plot2 <- marshallislands_total_plot +
  patchwork::inset_element(marshallislands_map, 0.5, 0.5, 1, 1) +
  patchwork::plot_layout(guides='keep')

marshallislands_total_plot2

ggsave("~/Desktop/marshallislands_plot.png", marshallislands_total_plot2,
       height=6, width=8, bg='white')

#### samoa ####
samoa_summary <- read.csv("samoa_summary.csv")
samoa_summary$Species <- as.factor(samoa_summary$Species)
samoa_summary$Species <- factor(samoa_summary$Species, levels=c("Blue Shark", "Silky Shark", "Thresher", "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"))

samoa_mortality <- read.csv("samoa_total_preds.csv")
samoa_proj <- aggregate(data=samoa_mortality, total ~ Lat + Long, FUN='sum')

samoa_map <- ggplot(data=samoa_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Mortality \n(Indiv.)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = wsm_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(-175.5,-169.5), breaks=seq(-175,-170,by=5)) +
  scale_y_continuous(limits=c(-16,-10.5), breaks=seq(-15,-5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

samoa_total_plot <- ggplot(samoa_summary, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total-.05, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values=my_pal) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        axis.title = element_blank(),
        axis.ticks = element_blank()
  ) +
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  ggtitle("Samoa")

samoa_total_plot2 <- samoa_total_plot +
  patchwork::inset_element(samoa_map, 0.5, 0.6, 1, 1) +
  patchwork::plot_layout(guides='keep')

samoa_total_plot2

ggsave("~/Desktop/samoa_plot.png", samoa_total_plot2,
       height=6, width=8, bg='white')

#### newcaledonia ####
newcaledonia_summary <- read.csv("newcaledonia_summary.csv")
newcaledonia_summary$Species <- as.factor(newcaledonia_summary$Species)
newcaledonia_summary$Species <- factor(newcaledonia_summary$Species, levels=c("Blue Shark", "Silky Shark", "Thresher", "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"))

newcaledonia_mortality <- read.csv("newcaledonia_total_preds.csv")
newcaledonia_proj <- aggregate(data=newcaledonia_mortality, total ~ Lat + Long, FUN='sum')

newcaledonia_map <- ggplot(data=newcaledonia_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Mortality \n(Indiv.)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = ncl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(155,175), breaks=seq(155,175,by=20)) +
  scale_y_continuous(limits=c(-27,-14), breaks=seq(-25,15,by=10)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

newcaledonia_total_plot <- ggplot(newcaledonia_summary, aes(x=Species, color=Species, y=Catch, fill=Species)) +
  geom_errorbar(aes(ymin=Catch, ymax=Catch+1.96*Catch.SD), width=0.4, position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar(aes(ymin=Total-.05, ymax=Total+1.96*Total.sd), width=0.4, position=position_dodge(width=0.9), stat="identity", color="black") +
  geom_col(alpha=0.3) +
  geom_col(aes(y=Total)) +
  scale_color_manual(values = my_pal) +
  scale_fill_manual(values=my_pal) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        axis.title = element_blank(),
  ) +
  scale_x_discrete(labels=c("Blue Shark",
                            "Silky Shark",
                            alopias_title,
                            isurus_title,
                            "Oceanic Whitetip",
                            sphyrnas_title,
                            "Other Sharks")) +
  ggtitle("New Caledonia")

newcaledonia_total_plot2 <- newcaledonia_total_plot +
  patchwork::inset_element(newcaledonia_map, 0.4, 0.55, 1, 1) +
  patchwork::plot_layout(guides='keep')

newcaledonia_total_plot2

ggsave("~/Desktop/newcaledonia_plot.png", newcaledonia_total_plot2,
       height=6, width=8, bg='white')

####arrange

Fig4insets <- ggarrange(micronesia_total_plot2,
          marshallislands_total_plot2,
          palau_total_plot2,
          kiribati_total_plot2,
          frenchpolynesia_total_plot2,
          newcaledonia_total_plot2,
          cookislands_total_plot2,
          samoa_total_plot2,
          ncol=3,nrow=3, align='v') 
  
library(grid)
Fig4insetFinal <- Fig4insets %>%
  annotate_figure(Fig4insets, left = textGrob("Catch (Bar Height) and Mortality (Shaded) - # of Individuals", rot = 90, vjust = 1),
                  bottom = textGrob("Species"))

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/Fig4insets.png", Fig4insetFinal,
       height=8, width=12, dpi=300)

Fig4 <- Fig4insets <- ggarrange(micronesia_total_plot,
                                marshallislands_total_plot,
                                palau_total_plot,
                                kiribati_total_plot,
                                frenchpolynesia_total_plot,
                                newcaledonia_total_plot,
                                cookislands_total_plot,
                                samoa_total_plot,
                                ncol=3,nrow=3, align='v') 


