library(tidyverse)

setwd("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir")
micronesia2016 <- read.csv("micronesia2016.csv")
micronesia2016$year <- 2016
micronesia2017 <- read.csv("micronesia2017.csv")
micronesia2017$year <- 2017
micronesia2018 <- read.csv("micronesia2018.csv")
micronesia2018$year <- 2018
micronesia2019 <- read.csv("micronesia2019.csv")
micronesia <- rbind(micronesia2016,micronesia2017,micronesia2018,micronesia2019)

frenchpolynesia2016 <- read.csv("frenchpolynesia2016.csv")
frenchpolynesia2016$year <- 2016
frenchpolynesia2017 <- read.csv("frenchpolynesia2017.csv")
frenchpolynesia2017$year <- 2017
frenchpolynesia2018 <- read.csv("frenchpolynesia2018.csv")
frenchpolynesia2018$year <- 2018
frenchpolynesia2019 <- read.csv("frenchpolynesia2019.csv")
frenchpolynesia <- rbind(frenchpolynesia2016,frenchpolynesia2017,frenchpolynesia2018,frenchpolynesia2019)

samoa2016 <- read.csv("samoa2016.csv")
samoa2016$year <- 2016
samoa2017 <- read.csv("samoa2017.csv")
samoa2017$year <- 2017
samoa2018 <- read.csv("samoa2018.csv")
samoa2018$year <- 2018
samoa2019 <- read.csv("samoa2019.csv")
samoa <- rbind(samoa2016,samoa2017,samoa2018,samoa2019)

kiribati2016 <- read.csv("kiribati2016.csv")
kiribati2016$year <- 2016
kiribati2017 <- read.csv("kiribati2017.csv")
kiribati2017$year <- 2017
kiribati2018 <- read.csv("kiribati2018.csv")
kiribati2018$year <- 2018
kiribati2019 <- read.csv("kiribati2019.csv")
kiribati <- rbind(kiribati2016,kiribati2017,kiribati2018,kiribati2019)

marshallislands2016 <- read.csv("marshallislands2016.csv")
marshallislands2016$year <- 2016
marshallislands2017 <- read.csv("marshallislands2017.csv")
marshallislands2017$year <- 2017
marshallislands2018 <- read.csv("marshallislands2018.csv")
marshallislands2018$year <- 2018
marshallislands2019 <- read.csv("marshallislands2019.csv")
marshallislands <- rbind(marshallislands2016,marshallislands2017,marshallislands2018,marshallislands2019)

palau2016 <- read.csv("palau2016.csv")
palau2016$year <- 2016
palau2017 <- read.csv("palau2017.csv")
palau2017$year <- 2017
palau2018 <- read.csv("palau2018.csv")
palau2018$year <- 2018
palau2019 <- read.csv("palau2019.csv")
palau2019 <- palau2019[,-9]
palau <- rbind(palau2016,palau2017,palau2018,palau2019)

cookislands2016 <- read.csv("cookislands2016.csv")
cookislands2016$year <- 2016
cookislands2017 <- read.csv("cookislands2017.csv")
cookislands2017$year <- 2017
cookislands2018 <- read.csv("cookislands2018.csv")
cookislands2018$year <- 2018
cookislands2019 <- read.csv("cookislands2019.csv")
cookislands <- rbind(cookislands2016,cookislands2017,cookislands2018,cookislands2019)

newcaledonia2016 <- read.csv("newcaledonia2016.csv")
newcaledonia2016$year <- 2016
newcaledonia2017 <- read.csv("newcaledonia2017.csv")
newcaledonia2017$year <- 2017
newcaledonia2018 <- read.csv("newcaledonia2018.csv")
newcaledonia2018$year <- 2018
newcaledonia2019 <- read.csv("newcaledonia2019.csv")
newcaledonia <- rbind(newcaledonia2016,newcaledonia2017,newcaledonia2018,newcaledonia2019)

all <- rbind(micronesia,newcaledonia,frenchpolynesia,samoa,palau,cookislands,marshallislands,kiribati)

all$country<-ifelse(all$ISO_Ter1 == "FSM", "FSM",
                    ifelse(all$ISO_Ter1 == "PLW", "Palau",
                           ifelse(all$ISO_Ter1 == "COK", "Cook Islands",
                                  ifelse(all$ISO_Ter1 == "KIR", "Kiribati",
                                         ifelse(all$ISO_Ter1 == "WSM", "Samoa",
                                                ifelse(all$ISO_Ter1 == "MHL", "Marshall Islands",
                                                       ifelse(all$ISO_Ter1 == "NCL", "New Caledonia", "French Polynesia")))))))

all_summary <- aggregate(data=all, hooks~country+year, FUN='sum')
all_summary$country <- as.factor(all_summary$country)
all_summary$country <- factor(all_summary$country, levels=c(""))

my_pal <- rcartocolor::carto_pal(n = 12, name = "Bold")[c(3,4,8,1,7,12,5)]
my_pal2 <- rcartocolor::carto_pal(n=8, name = "Antique")[c(7,5,1,4,2,6,3,8)]

trends<-ggplot(all_summary, aes(x=year, y=hooks/1000, fill=country))+
  geom_bar(position = 'stack', stat='identity')+
  scale_fill_manual(values=my_pal2, name="Sanctuary")+
  scale_y_continuous(name="1000s of Hooks", limits=c(0,250000), expand=c(0,0))+
  scale_x_continuous(name="Year")+
  theme_bw()

ggsave("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/Figures/hooktrends.png", trends,height=4,width=6,dpi=300, bg='white')
