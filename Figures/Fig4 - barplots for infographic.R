library(tidyverse)

#silky
df <- data_frame("x" = 1, "y"=40.51)

my_pal <- rcartocolor::carto_pal(n = 12, name = "Bold")[c(3,4,8,1,7,12,5)]

g <- ggplot(df, aes(x=x,y=y)) +
  geom_bar(stat="identity", color=my_pal[2], fill=my_pal[2]) +
  scale_y_continuous(name = '% of Area-Standardized Harvest Rate at MSY', limits=c(0,100), expand=c(0,0)) +
  coord_flip() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = rep(unit(0,"null"),4)
  ) 

ggsave("~/Desktop/silkypct.png", g, height=194.99791667, width=613.56875, units='mm')

#ocs####
df <- data_frame("x" = 1, "y"=15.38)

g <- ggplot(df, aes(x=x,y=y)) +
  geom_bar(stat="identity", color=my_pal[5], fill=my_pal[5]) +
  scale_y_continuous(name = '% of Area-Standardized Harvest Rate at MSY', limits=c(0,100), expand=c(0,0)) +
  coord_flip() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = rep(unit(0,"null"),4)
  ) 

ggsave("~/Desktop/ocspct.png", g, height=194.99791667, width=613.56875, units='mm')

#blue north####
df <- data_frame("x" = 1, "y"=4.01)

g <- ggplot(df, aes(x=x,y=y)) +
  geom_bar(stat="identity", color=my_pal[1], fill=my_pal[1]) +
  scale_y_continuous(name = '% of Area-Standardized Harvest Rate at MSY', limits=c(0,100), expand=c(0,0)) +
  coord_flip() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = rep(unit(0,"null"),4)
  ) 

ggsave("~/Desktop/bluepct_n.png", g, height=194.99791667, width=613.56875, units='mm')

#blue south####
df <- data_frame("x" = 1, "y"=5.40)

g <- ggplot(df, aes(x=x,y=y)) +
  geom_bar(stat="identity", color=my_pal[1], fill=my_pal[1]) +
  scale_y_continuous(name = '% of Area-Standardized Harvest Rate at MSY', limits=c(0,100), expand=c(0,0)) +
  coord_flip() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = rep(unit(0,"null"),4)
  ) 

ggsave("~/Desktop/bluepct_s.png", g, height=194.99791667, width=613.56875, units='mm')

#mako####
df <- data_frame("x" = 1, "y"=14.38)

g <- ggplot(df, aes(x=x,y=y)) +
  geom_bar(stat="identity", color=my_pal[4], fill=my_pal[4]) +
  scale_y_continuous(name = '% of Area-Standardized Harvest Rate at MSY', limits=c(0,100), expand=c(0,0)) +
  coord_flip() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill="white", colour = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = rep(unit(0,"null"),4)
  ) 

ggsave("~/Desktop/makopct.png", g, height=194.99791667, width=613.56875, units='mm')



