library(tidyverse)
library(fields)
library(stringr)
library(sf)
library(logitnorm)

rm(list=ls())
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')

#CPUE data
master_cpue <- read.csv("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir/WCPFC CPUEs.csv")
master_cpue <- master_cpue[,-4] #get rid of Log_hooks column so we can use model estiamted log_hooks
master_cpue$Long <- ifelse(master_cpue$Long2 >= 180, master_cpue$Long2 - 360, master_cpue$Long2) #correct back for lat/lon

#### mortality ####
mortalityrates <-read.csv("mortalityrates2.csv")

#mins and maxes for uniform distributions for sample and replace
mortalityrates$HookMin <- with(mortalityrates, pmin(Hook_mortality_gilman, Hook_mortality_hutch, na.rm=T))
mortalityrates$HookMax <- with(mortalityrates, pmax(Hook_mortality_gilman, Hook_mortality_hutch, na.rm=T))

mortalityrates$logit.prm<-log(mortalityrates$prm.mean/(1-mortalityrates$prm.mean))
mortalityrates$logit.prm.upr <- log(mortalityrates$prm.upr/(1-mortalityrates$prm.upr))
mortalityrates$logit.prm.lwr <- log(mortalityrates$prm.lwr/(1-mortalityrates$prm.lwr))
mortalityrates$diff1 <- mortalityrates$logit.prm.upr-mortalityrates$logit.prm
mortalityrates$diff2 <- mortalityrates$logit.prm-mortalityrates$logit.prm.lwr
mortalityrates$logit.prm.se <- rowMeans(mortalityrates[c(13:14)])

mortality <- mortalityrates[,c(1,2,8:10,15)]

#### Hook data ####
newcaledonia_proj <- read.csv("newcaledonia2019.csv")

newcaledonia_proj2 <- newcaledonia_proj[,c(5,6,9,10)]
#rename
colnames(newcaledonia_proj2)[1] <- "Long"
colnames(newcaledonia_proj2)[2] <- "Lat"

#merge
newcaledonia_comb <- merge(newcaledonia_proj2, master_cpue, by=c("Lat", "Long"), all=T)
newcaledonia_comb <- newcaledonia_comb %>% drop_na(log_hooks)

n <- nrow(newcaledonia_comb)

newcaledonia_hook_preds <- matrix(nrow=n, ncol=1000)
for (j in 1:n) {
  for (i in 1:1000) {
    newcaledonia_hook_preds[j,i] <- rlnorm(1,newcaledonia_comb$log_hooks[j],newcaledonia_comb$log_hooks.se[j])
  }
}

{start.time <- Sys.time()
#### blue sharks ####
#row 2 in mortality; need to change this for each species when drawing rates)
r_bsh = 2

newcaledonia_bsh <- newcaledonia_comb[,c(1:4,9,10)]

newcaledonia_bsh_catches <- matrix(nrow=n, ncol=1000)
newcaledonia_bsh_DOA <- matrix(nrow=n, ncol=1000)
newcaledonia_bsh_released <- matrix(nrow=n, ncol=1000)
newcaledonia_bsh_PRM<- matrix(nrow=n, ncol=1000)
newcaledonia_bsh_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    newcaledonia_bsh_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$blueshark.logcpue[j], newcaledonia_comb$blueshark.logcpue.se[j])
    newcaledonia_bsh_DOA[j,i] <- newcaledonia_bsh_catches[j,i] * runif(1,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh])
    newcaledonia_bsh_released[j,i] <- newcaledonia_bsh_catches[j,i] - newcaledonia_bsh_DOA[j,i]
    newcaledonia_bsh_PRM[j,i] <- newcaledonia_bsh_released[j,i] * rlogitnorm(1, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh]) 
    newcaledonia_bsh_total[j,i] <- newcaledonia_bsh_DOA[j,i] + newcaledonia_bsh_PRM[j,i]
  }
}

#catches
newcaledonia_bsh$catch <- rowMeans(newcaledonia_bsh_catches[,1:1000])
newcaledonia_bsh$catch.sd = apply(newcaledonia_bsh_catches[,1:1000], 1, sd)

 #DOA
newcaledonia_bsh$DOA <- rowMeans(newcaledonia_bsh_DOA[,1:1000])
newcaledonia_bsh$DOA.sd = apply(newcaledonia_bsh_DOA[,1:1000], 1, sd)

#prm
newcaledonia_bsh$PRM = rowMeans(newcaledonia_bsh_PRM[,1:1000])
newcaledonia_bsh$PRM.sd = apply(newcaledonia_bsh_PRM[,1:1000], 1, sd)

#total
newcaledonia_bsh$total = rowMeans(newcaledonia_bsh_total[,1:1000])
newcaledonia_bsh$total.sd = apply(newcaledonia_bsh_total[,1:1000], 1, sd)

#### silky sharks ####
r_silky=5 #row 5 in mortality; need to change this for each species when drawing rates)

newcaledonia_silky <- newcaledonia_comb[,c(1:4,13,14)] #create dataframe

newcaledonia_silky_catches <- matrix(nrow=n, ncol=1000)
newcaledonia_silky_DOA <- matrix(nrow=n, ncol=1000)
newcaledonia_silky_released <- matrix(nrow=n, ncol=1000)
newcaledonia_silky_PRM<- matrix(nrow=n, ncol=1000)
newcaledonia_silky_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    newcaledonia_silky_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$silkyshark.logcpue[j], newcaledonia_comb$silkyshark.logcpue.se[j])
    newcaledonia_silky_DOA[j,i] <- newcaledonia_silky_catches[j,i] * runif(1,mortality$HookMin[r_silky],mortality$HookMax[r_silky])  
    newcaledonia_silky_released[j,i] <- newcaledonia_silky_catches[j,i] - newcaledonia_silky_DOA[j,i]
    newcaledonia_silky_PRM[j,i] <- newcaledonia_silky_released[j,i] * rlogitnorm(1, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky]) 
    newcaledonia_silky_total[j,i] <- newcaledonia_silky_DOA[j,i] + newcaledonia_silky_PRM[j,i]
  }
}

#catches
newcaledonia_silky$catch <- rowMeans(newcaledonia_silky_catches[,1:1000])
newcaledonia_silky$catch.sd = apply(newcaledonia_silky_catches[,1:1000], 1, sd)

#DOA
newcaledonia_silky$DOA <- rowMeans(newcaledonia_silky_DOA[,1:1000])
newcaledonia_silky$DOA.sd = apply(newcaledonia_silky_DOA[,1:1000], 1, sd)

#prm
newcaledonia_silky$PRM = rowMeans(newcaledonia_silky_PRM[,1:1000])
newcaledonia_silky$PRM.sd = apply(newcaledonia_silky_PRM[,1:1000], 1, sd)

#total
newcaledonia_silky$total = rowMeans(newcaledonia_silky_total[,1:1000])
newcaledonia_silky$total.sd = apply(newcaledonia_silky_total[,1:1000], 1, sd)

#### thresher sharks ####
#row 1 in mortality; need to change this for each species when drawing rates)
r_thresher = 1

newcaledonia_thresher <- newcaledonia_comb[,c(1:4,21,22)] #create dataframe

newcaledonia_thresher_catches <- matrix(nrow=n, ncol=1000)
newcaledonia_thresher_DOA <- matrix(nrow=n, ncol=1000)
newcaledonia_thresher_released <- matrix(nrow=n, ncol=1000)
newcaledonia_thresher_PRM<- matrix(nrow=n, ncol=1000)
newcaledonia_thresher_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    newcaledonia_thresher_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$thresher.logcpue[j], newcaledonia_comb$thresher.logcpue.se[j])
    newcaledonia_thresher_DOA[j,i] <- newcaledonia_thresher_catches[j,i] * runif(1,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher]) 
    newcaledonia_thresher_released[j,i] <- newcaledonia_thresher_catches[j,i] - newcaledonia_thresher_DOA[j,i]
    newcaledonia_thresher_PRM[j,i] <- newcaledonia_thresher_released[j,i] * rlogitnorm(1, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher]) 
    newcaledonia_thresher_total[j,i] <- newcaledonia_thresher_DOA[j,i] + newcaledonia_thresher_PRM[j,i]
  }
}

#catches
newcaledonia_thresher$catch <- rowMeans(newcaledonia_thresher_catches[,1:1000])
newcaledonia_thresher$catch.sd = apply(newcaledonia_thresher_catches[,1:1000], 1, sd)

#DOA
newcaledonia_thresher$DOA <- rowMeans(newcaledonia_thresher_DOA[,1:1000])
newcaledonia_thresher$DOA.sd = apply(newcaledonia_thresher_DOA[,1:1000], 1, sd)

#prm
newcaledonia_thresher$PRM = rowMeans(newcaledonia_thresher_PRM[,1:1000])
newcaledonia_thresher$PRM.sd = apply(newcaledonia_thresher_PRM[,1:1000], 1, sd)

#total
newcaledonia_thresher$total = rowMeans(newcaledonia_thresher_total[,1:1000])
newcaledonia_thresher$total.sd = apply(newcaledonia_thresher_total[,1:1000], 1, sd)

#### shortfinmako sharks ####
#row 4 in mortality; need to change this for each species when drawing rates)
r_mako = 4

newcaledonia_mako <- newcaledonia_comb[,c(1:4,17,18)] #create dataframe

newcaledonia_mako_catches <- matrix(nrow=n, ncol=1000)
newcaledonia_mako_DOA <- matrix(nrow=n, ncol=1000)
newcaledonia_mako_released <- matrix(nrow=n, ncol=1000)
newcaledonia_mako_PRM<- matrix(nrow=n, ncol=1000)
newcaledonia_mako_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    newcaledonia_mako_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$mako.logcpue[j], newcaledonia_comb$mako.logcpue.se[j])
    newcaledonia_mako_DOA[j,i] <- newcaledonia_mako_catches[j,i] * runif(1,mortality$HookMin[r_mako],mortality$HookMax[r_mako])  
    newcaledonia_mako_released[j,i] <- newcaledonia_mako_catches[j,i] - newcaledonia_mako_DOA[j,i]
    newcaledonia_mako_PRM[j,i] <- newcaledonia_mako_released[j,i] * rlogitnorm(1, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako]) 
    newcaledonia_mako_total[j,i] <- newcaledonia_mako_DOA[j,i] + newcaledonia_mako_PRM[j,i]
  }
}

#catches
newcaledonia_mako$catch <- rowMeans(newcaledonia_mako_catches[,1:1000])
newcaledonia_mako$catch.sd = apply(newcaledonia_mako_catches[,1:1000], 1, sd)

#DOA
newcaledonia_mako$DOA <- rowMeans(newcaledonia_mako_DOA[,1:1000])
newcaledonia_mako$DOA.sd = apply(newcaledonia_mako_DOA[,1:1000], 1, sd)

#prm
newcaledonia_mako$PRM = rowMeans(newcaledonia_mako_PRM[,1:1000])
newcaledonia_mako$PRM.sd = apply(newcaledonia_mako_PRM[,1:1000], 1, sd)

#total
newcaledonia_mako$total = rowMeans(newcaledonia_mako_total[,1:1000])
newcaledonia_mako$total.sd = apply(newcaledonia_mako_total[,1:1000], 1, sd)

#### oceanic whitetip sharks ####
#row 8 in mortality; need to change this for each species when drawing rates)
r_oceanicwhitetip = 8

newcaledonia_oceanicwhitetip <- newcaledonia_comb[,c(1:4,25,26)] #create dataframe

newcaledonia_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
newcaledonia_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
newcaledonia_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
newcaledonia_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
newcaledonia_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    newcaledonia_oceanicwhitetip_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$oceanicwhitetip.logcpue[j], newcaledonia_comb$oceanicwhitetip.logcpue.se[j])
    newcaledonia_oceanicwhitetip_DOA[j,i] <- newcaledonia_oceanicwhitetip_catches[j,i] * runif(1,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip]) 
    newcaledonia_oceanicwhitetip_released[j,i] <- newcaledonia_oceanicwhitetip_catches[j,i] - newcaledonia_oceanicwhitetip_DOA[j,i]
    newcaledonia_oceanicwhitetip_PRM[j,i] <- newcaledonia_oceanicwhitetip_released[j,i] * rlogitnorm(1, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip]) 
    newcaledonia_oceanicwhitetip_total[j,i] <- newcaledonia_oceanicwhitetip_DOA[j,i] + newcaledonia_oceanicwhitetip_PRM[j,i]
  }
}

#catches
newcaledonia_oceanicwhitetip$catch <- rowMeans(newcaledonia_oceanicwhitetip_catches[,1:1000])
newcaledonia_oceanicwhitetip$catch.sd = apply(newcaledonia_oceanicwhitetip_catches[,1:1000], 1, sd)

#DOA
newcaledonia_oceanicwhitetip$DOA <- rowMeans(newcaledonia_oceanicwhitetip_DOA[,1:1000])
newcaledonia_oceanicwhitetip$DOA.sd = apply(newcaledonia_oceanicwhitetip_DOA[,1:1000], 1, sd)

#prm
newcaledonia_oceanicwhitetip$PRM = rowMeans(newcaledonia_oceanicwhitetip_PRM[,1:1000])
newcaledonia_oceanicwhitetip$PRM.sd = apply(newcaledonia_oceanicwhitetip_PRM[,1:1000], 1, sd)

#total
newcaledonia_oceanicwhitetip$total = rowMeans(newcaledonia_oceanicwhitetip_total[,1:1000])
newcaledonia_oceanicwhitetip$total.sd = apply(newcaledonia_oceanicwhitetip_total[,1:1000], 1, sd)

#### hammerhead sharks ####
#row 9 in mortality; need to change this for each species when drawing rates)
r_hammerhead = 9

newcaledonia_hammerhead <- newcaledonia_comb[,c(1:4,29,30)] #create dataframe

newcaledonia_hammerhead_catches <- matrix(nrow=n, ncol=1000)
newcaledonia_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
newcaledonia_hammerhead_released <- matrix(nrow=n, ncol=1000)
newcaledonia_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
newcaledonia_hammerhead_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    newcaledonia_hammerhead_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$hammerhead.logcpue[j], newcaledonia_comb$hammerhead.logcpue.se[j])
    newcaledonia_hammerhead_DOA[j,i] <- newcaledonia_hammerhead_catches[j,i] * runif(1,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead])  
    newcaledonia_hammerhead_released[j,i] <- newcaledonia_hammerhead_catches[j,i] - newcaledonia_hammerhead_DOA[j,i]
    newcaledonia_hammerhead_PRM[j,i] <- newcaledonia_hammerhead_released[j,i] * rlogitnorm(1,mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead]) 
    newcaledonia_hammerhead_total[j,i] <- newcaledonia_hammerhead_DOA[j,i] + newcaledonia_hammerhead_PRM[j,i]
  }
}

#catches
newcaledonia_hammerhead$catch <- rowMeans(newcaledonia_hammerhead_catches[,1:1000])
newcaledonia_hammerhead$catch.sd = apply(newcaledonia_hammerhead_catches[,1:1000], 1, sd)

#DOA
newcaledonia_hammerhead$DOA <- rowMeans(newcaledonia_hammerhead_DOA[,1:1000])
newcaledonia_hammerhead$DOA.sd = apply(newcaledonia_hammerhead_DOA[,1:1000], 1, sd)

#prm
newcaledonia_hammerhead$PRM = rowMeans(newcaledonia_hammerhead_PRM[,1:1000])
newcaledonia_hammerhead$PRM.sd = apply(newcaledonia_hammerhead_PRM[,1:1000], 1, sd)

#total
newcaledonia_hammerhead$total = rowMeans(newcaledonia_hammerhead_total[,1:1000])
newcaledonia_hammerhead$total.sd = apply(newcaledonia_hammerhead_total[,1:1000], 1, sd)

#### othersharks sharks ####
#row 10 in mortality; need to change this for each species when drawing rates)
r_othersharks = 10

newcaledonia_othersharks <- newcaledonia_comb[,c(1:4,33,34)] #create dataframe

newcaledonia_othersharks_catches <- matrix(nrow=n, ncol=1000)
newcaledonia_othersharks_DOA <- matrix(nrow=n, ncol=1000)
newcaledonia_othersharks_released <- matrix(nrow=n, ncol=1000)
newcaledonia_othersharks_PRM<- matrix(nrow=n, ncol=1000)
newcaledonia_othersharks_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    newcaledonia_othersharks_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$othersharks.logcpue[j], newcaledonia_comb$othersharks.logcpue.se[j])
    newcaledonia_othersharks_DOA[j,i] <- newcaledonia_othersharks_catches[j,i] * runif(1,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks])  
    newcaledonia_othersharks_released[j,i] <- newcaledonia_othersharks_catches[j,i] - newcaledonia_othersharks_DOA[j,i]
    newcaledonia_othersharks_PRM[j,i] <- newcaledonia_othersharks_released[j,i] * rlogitnorm(1, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks]) 
    newcaledonia_othersharks_total[j,i] <- newcaledonia_othersharks_DOA[j,i] + newcaledonia_othersharks_PRM[j,i]
  }
}

#catches
newcaledonia_othersharks$catch <- rowMeans(newcaledonia_othersharks_catches[,1:1000])
newcaledonia_othersharks$catch.sd = apply(newcaledonia_othersharks_catches[,1:1000], 1, sd)

#DOA
newcaledonia_othersharks$DOA <- rowMeans(newcaledonia_othersharks_DOA[,1:1000])
newcaledonia_othersharks$DOA.sd = apply(newcaledonia_othersharks_DOA[,1:1000], 1, sd)

#prm
newcaledonia_othersharks$PRM = rowMeans(newcaledonia_othersharks_PRM[,1:1000])
newcaledonia_othersharks$PRM.sd = apply(newcaledonia_othersharks_PRM[,1:1000], 1, sd)

#total
newcaledonia_othersharks$total = rowMeans(newcaledonia_othersharks_total[,1:1000])
newcaledonia_othersharks$total.sd = apply(newcaledonia_othersharks_total[,1:1000], 1, sd)

#### newcaledonia Totals ####

newcaledonia_bsh$Species = "Blue Shark"
newcaledonia_silky$Species = "Silky Shark"
newcaledonia_thresher$Species="Thresher"
newcaledonia_mako$Species="Mako"
newcaledonia_oceanicwhitetip$Species="Oceanic Whitetip"
newcaledonia_hammerhead$Species="Hammerhead"
newcaledonia_othersharks$Species="Other Sharks"

colnames(newcaledonia_bsh)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(newcaledonia_silky)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(newcaledonia_thresher)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(newcaledonia_mako)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(newcaledonia_oceanicwhitetip)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(newcaledonia_hammerhead)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(newcaledonia_othersharks)[c(5,6)] <- c("logcpue","logcpue.se")

newcaledonia_total_preds <- rbind(newcaledonia_bsh,
                          newcaledonia_silky,
                          newcaledonia_thresher,
                          newcaledonia_mako,
                          newcaledonia_oceanicwhitetip,
                          newcaledonia_hammerhead,
                          newcaledonia_othersharks)

end.time <- Sys.time()
end.time - start.time}

write.csv(newcaledonia_total_preds, "newcaledonia_total_preds.csv", row.names = F)

#### summary ####
dat <- read.csv("newcaledonia_total_preds.csv")
newcaledonia_bsh <- subset(dat, Species == "Blue Shark")
newcaledonia_silky <- subset(dat, Species == "Silky Shark")
newcaledonia_thresher <- subset(dat, Species == "Thresher")
newcaledonia_mako <- subset(dat, Species == "Mako")
newcaledonia_oceanicwhitetip <- subset(dat, Species == "Oceanic Whitetip")
newcaledonia_hammerhead <- subset(dat, Species == "Hammerhead")
newcaledonia_othersharks <- subset(dat, Species == "Other Sharks")




newcaledonia_summary <- data.frame("Species" = c("Blue Shark", "Silky Shark", "Thresher", 
                                          "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"),
                            "Catch" = c(sum(newcaledonia_bsh$catch),
                                        sum(newcaledonia_silky$catch), 
                                        sum(newcaledonia_thresher$catch),
                                        sum(newcaledonia_mako$catch),
                                        sum(newcaledonia_oceanicwhitetip$catch),
                                        sum(newcaledonia_hammerhead$catch),
                                        sum(newcaledonia_othersharks$catch)),
                            "Catch.SD" = c(sqrt(sum(newcaledonia_bsh$catch.sd^2, na.rm=T)),
                                           sqrt(sum(newcaledonia_silky$catch.sd^2, na.rm=T)),
                                           sqrt(sum(newcaledonia_thresher$catch.sd^2, na.rm=T)),
                                           sqrt(sum(newcaledonia_mako$catch.sd^2, na.rm=T)),
                                           sqrt(sum(newcaledonia_oceanicwhitetip$catch.sd^2, na.rm=T)),
                                           sqrt(sum(newcaledonia_hammerhead$catch.sd^2, na.rm=T)),
                                           sqrt(sum(newcaledonia_othersharks$catch.sd^2, na.rm=T))),
                            "CM" = c(sum(newcaledonia_bsh$DOA),
                                     sum(newcaledonia_silky$DOA),
                                     sum(newcaledonia_thresher$DOA),
                                     sum(newcaledonia_mako$DOA),
                                     sum(newcaledonia_oceanicwhitetip$DOA),
                                     sum(newcaledonia_hammerhead$DOA),
                                     sum(newcaledonia_othersharks$DOA)),
                            "CM.sd"=c(sqrt(sum(newcaledonia_bsh$DOA.sd^2)),
                                      sqrt(sum(newcaledonia_silky$DOA.sd^2)),
                                      sqrt(sum(newcaledonia_thresher$DOA.sd^2)),
                                      sqrt(sum(newcaledonia_mako$DOA.sd^2)),
                                      sqrt(sum(newcaledonia_oceanicwhitetip$DOA.sd^2)),
                                      sqrt(sum(newcaledonia_hammerhead$DOA.sd^2)),
                                      sqrt(sum(newcaledonia_othersharks$DOA.sd^2))),
                            "PRM" = c(sum(newcaledonia_bsh$PRM),
                                      sum(newcaledonia_silky$PRM),
                                      sum(newcaledonia_thresher$PRM),
                                      sum(newcaledonia_mako$PRM),
                                      sum(newcaledonia_oceanicwhitetip$PRM), 
                                      sum(newcaledonia_hammerhead$PRM), 
                                      sum(newcaledonia_othersharks$PRM)),
                            "PRM.sd"=c(sqrt(sum(newcaledonia_bsh$PRM.sd^2)),
                                       sqrt(sum(newcaledonia_silky$PRM.sd^2)),
                                       sqrt(sum(newcaledonia_thresher$PRM.sd^2)),
                                       sqrt(sum(newcaledonia_mako$PRM.sd^2)),
                                       sqrt(sum(newcaledonia_oceanicwhitetip$PRM.sd^2)),
                                       sqrt(sum(newcaledonia_hammerhead$PRM.sd^2)),
                                       sqrt(sum(newcaledonia_othersharks$PRM.sd^2))),
                            "Total"=c(sum(newcaledonia_bsh$total),
                                      sum(newcaledonia_silky$total),
                                      sum(newcaledonia_thresher$total),
                                      sum(newcaledonia_mako$total),
                                      sum(newcaledonia_oceanicwhitetip$total),
                                      sum(newcaledonia_hammerhead$total),
                                      sum(newcaledonia_othersharks$total)),
                            "Total.sd"=c(sqrt(sum(newcaledonia_bsh$total.sd^2)),
                                         sqrt(sum(newcaledonia_silky$total.sd^2)),
                                         sqrt(sum(newcaledonia_thresher$total.sd^2)), 
                                         sqrt(sum(newcaledonia_mako$total.sd^2)),
                                         sqrt(sum(newcaledonia_oceanicwhitetip$total.sd^2)),
                                         sqrt(sum(newcaledonia_hammerhead$total.sd^2)), 
                                         sqrt(sum(newcaledonia_othersharks$total.sd^2))))


write.csv(newcaledonia_summary, "newcaledonia_summary.csv", row.names = F)


#plotting for Fig1
newcaledonia_mortality <- read.csv("newcaledonia_total_preds.csv")
newcaledonia_proj <- aggregate(data=newcaledonia_mortality, total ~ Lat + Long, FUN='sum')
dev.off()
world <- map_data("world")
newcaledonia_map <- ggplot(data=newcaledonia_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Bycatch Mortality \n(# of individuals)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = ncl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(155,175), breaks=seq(155,175,by=5)) +
  scale_y_continuous(limits=c(-27,-14), breaks=seq(-25,15,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="New Caledonia")

