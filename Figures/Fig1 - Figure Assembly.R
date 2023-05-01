library(ggpubr)

top<-ggarrange(palau_map,micronesia_map,cookislands_map,
               ncol=3,nrow=1, legend="none", widths = c(1,1.9,1))

middle<-ggarrange(marshallislands_map,samoa_map,newcaledonia_map,
               ncol=3,nrow=1, legend="none", widths = c(1.4,1,1.4))

bottom <-ggarrange(kiribati_map,frenchpolynesia_map,
               ncol=2,nrow=1, common.legend=T, legend="right", widths = c(1.75,1))

hooks_proj_fig <- ggarrange(top,middle,bottom,ncol=1,nrow=3)

library(grid)
hooks_proj_fig <- hooks_proj_fig %>%
  annotate_figure(hooks_proj_fig, left = textGrob("Latitude", rot = 90, vjust = 1),
                  bottom = textGrob("Longitude"))
  #annotate_figure(fig.lab = "b)")

print(hooks_proj_fig)

ggsave("Fig1B.png", hooks_proj_fig,width=6.5,height=4.5,units="in")

#need sanctuary map figure build from "map script" ####
fig1full <- ggarrange(sanctuarymap,hooks_proj_fig, ncol=1, nrow=2, 
                      labels="AUTO", heights = c(1,1.3), widths=1)

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Fig1Full.png", 
       fig1full, width = 9, height = 13.5, dpi=320, bg="white")
