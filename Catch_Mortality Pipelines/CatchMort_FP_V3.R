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
frenchpolynesia_proj <- read.csv("frenchpolynesia2019.csv")

frenchpolynesia_proj2 <- frenchpolynesia_proj[,c(5,6,9,10)]

#rename
colnames(frenchpolynesia_proj2)[1] <- "Long"
colnames(frenchpolynesia_proj2)[2] <- "Lat"

#merge
frenchpolynesia_comb <- merge(frenchpolynesia_proj2, master_cpue, by=c("Lat", "Long"), all=T)
frenchpolynesia_comb <- frenchpolynesia_comb %>% drop_na(log_hooks)

#5 of 1,567 cells outside domain - forced to equal CPUEs from nearest cell
frenchpolynesia_comb[1,5:34] <- frenchpolynesia_comb[8,5:34]
frenchpolynesia_comb[2:4,5:34] <- frenchpolynesia_comb[6,5:34]
frenchpolynesia_comb[5,5:34] <- frenchpolynesia_comb[7,5:34]
frenchpolynesia_comb[425:427,5:34] <- frenchpolynesia_comb[424,5:34]
frenchpolynesia_comb[470:475,5:34] <- frenchpolynesia_comb[469,5:34]
frenchpolynesia_comb[514:517,5:34] <- frenchpolynesia_comb[513,5:34]
frenchpolynesia_comb[554:559,5:34] <- frenchpolynesia_comb[553,5:34]
frenchpolynesia_comb[597:600,5:34] <- frenchpolynesia_comb[596,5:34]
frenchpolynesia_comb[642,5:34] <- frenchpolynesia_comb[641,5:34]
frenchpolynesia_comb[791:793,5:34] <- frenchpolynesia_comb[794,5:34]
frenchpolynesia_comb[833:836,5:34] <- frenchpolynesia_comb[832,5:34]

n <- nrow(frenchpolynesia_comb)

frenchpolynesia_hook_preds <- matrix(nrow=n, ncol=1000)
for (j in 1:n) {
  for (i in 1:1000) {
    frenchpolynesia_hook_preds[j,i] <- rlnorm(1,frenchpolynesia_comb$log_hooks[j],frenchpolynesia_comb$log_hooks.se[j])
  }
}

{start.time <- Sys.time()
#### blue sharks ####
#row 2 in mortality; need to change this for each species when drawing rates)
r_bsh = 2

frenchpolynesia_bsh <- frenchpolynesia_comb[,c(1:4,9,10)]

bsh_cm <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh]) 
bsh_prm <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh]) 

frenchpolynesia_bsh_catches <- matrix(nrow=n, ncol=1000)
frenchpolynesia_bsh_DOA <- matrix(nrow=n, ncol=1000)
frenchpolynesia_bsh_released <- matrix(nrow=n, ncol=1000)
frenchpolynesia_bsh_PRM<- matrix(nrow=n, ncol=1000)
frenchpolynesia_bsh_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    frenchpolynesia_bsh_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$blueshark.logcpue[j], frenchpolynesia_comb$blueshark.logcpue.se[j])
    frenchpolynesia_bsh_DOA[j,i] <- frenchpolynesia_bsh_catches[j,i] * bsh_cm[i] 
    frenchpolynesia_bsh_released[j,i] <- frenchpolynesia_bsh_catches[j,i] - frenchpolynesia_bsh_DOA[j,i]
    frenchpolynesia_bsh_PRM[j,i] <- frenchpolynesia_bsh_released[j,i] * bsh_prm[i] 
    frenchpolynesia_bsh_total[j,i] <- frenchpolynesia_bsh_DOA[j,i] + frenchpolynesia_bsh_PRM[j,i]
  }
}

#catches
frenchpolynesia_bsh$catch <- rowMeans(frenchpolynesia_bsh_catches[,1:1000])
frenchpolynesia_bsh$catch.sd = apply(frenchpolynesia_bsh_catches[,1:1000], 1, sd)

 #DOA
frenchpolynesia_bsh$DOA <- rowMeans(frenchpolynesia_bsh_DOA[,1:1000])
frenchpolynesia_bsh$DOA.sd = apply(frenchpolynesia_bsh_DOA[,1:1000], 1, sd)

#prm
frenchpolynesia_bsh$PRM = rowMeans(frenchpolynesia_bsh_PRM[,1:1000])
frenchpolynesia_bsh$PRM.sd = apply(frenchpolynesia_bsh_PRM[,1:1000], 1, sd)

#total
frenchpolynesia_bsh$total = rowMeans(frenchpolynesia_bsh_total[,1:1000])
frenchpolynesia_bsh$total.sd = apply(frenchpolynesia_bsh_total[,1:1000], 1, sd)

#### silky sharks ####
r_silky=5 #row 5 in mortality; need to change this for each species when drawing rates)

frenchpolynesia_silky <- frenchpolynesia_comb[,c(1:4,13,14)] #create dataframe

silky_cm <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky]) 
silky_prm <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky]) 

frenchpolynesia_silky_catches <- matrix(nrow=n, ncol=1000)
frenchpolynesia_silky_DOA <- matrix(nrow=n, ncol=1000)
frenchpolynesia_silky_released <- matrix(nrow=n, ncol=1000)
frenchpolynesia_silky_PRM<- matrix(nrow=n, ncol=1000)
frenchpolynesia_silky_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    frenchpolynesia_silky_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$silkyshark.logcpue[j], frenchpolynesia_comb$silkyshark.logcpue.se[j])
    frenchpolynesia_silky_DOA[j,i] <- frenchpolynesia_silky_catches[j,i] * silky_cm[i] 
    frenchpolynesia_silky_released[j,i] <- frenchpolynesia_silky_catches[j,i] - frenchpolynesia_silky_DOA[j,i]
    frenchpolynesia_silky_PRM[j,i] <- frenchpolynesia_silky_released[j,i] * silky_prm[i] 
    frenchpolynesia_silky_total[j,i] <- frenchpolynesia_silky_DOA[j,i] + frenchpolynesia_silky_PRM[j,i]
  }
}

#catches
frenchpolynesia_silky$catch <- rowMeans(frenchpolynesia_silky_catches[,1:1000])
frenchpolynesia_silky$catch.sd = apply(frenchpolynesia_silky_catches[,1:1000], 1, sd)

#DOA
frenchpolynesia_silky$DOA <- rowMeans(frenchpolynesia_silky_DOA[,1:1000])
frenchpolynesia_silky$DOA.sd = apply(frenchpolynesia_silky_DOA[,1:1000], 1, sd)

#prm
frenchpolynesia_silky$PRM = rowMeans(frenchpolynesia_silky_PRM[,1:1000])
frenchpolynesia_silky$PRM.sd = apply(frenchpolynesia_silky_PRM[,1:1000], 1, sd)

#total
frenchpolynesia_silky$total = rowMeans(frenchpolynesia_silky_total[,1:1000])
frenchpolynesia_silky$total.sd = apply(frenchpolynesia_silky_total[,1:1000], 1, sd)

#### thresher sharks ####
#row 1 in mortality; need to change this for each species when drawing rates)
r_thresher = 1

frenchpolynesia_thresher <- frenchpolynesia_comb[,c(1:4,21,22)] #create dataframe

thresher_cm <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher]) 
thresher_prm <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher]) 

frenchpolynesia_thresher_catches <- matrix(nrow=n, ncol=1000)
frenchpolynesia_thresher_DOA <- matrix(nrow=n, ncol=1000)
frenchpolynesia_thresher_released <- matrix(nrow=n, ncol=1000)
frenchpolynesia_thresher_PRM<- matrix(nrow=n, ncol=1000)
frenchpolynesia_thresher_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    frenchpolynesia_thresher_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$thresher.logcpue[j], frenchpolynesia_comb$thresher.logcpue.se[j])
    frenchpolynesia_thresher_DOA[j,i] <- frenchpolynesia_thresher_catches[j,i] * thresher_cm[i] 
    frenchpolynesia_thresher_released[j,i] <- frenchpolynesia_thresher_catches[j,i] - frenchpolynesia_thresher_DOA[j,i]
    frenchpolynesia_thresher_PRM[j,i] <- frenchpolynesia_thresher_released[j,i] * thresher_prm[i] 
    frenchpolynesia_thresher_total[j,i] <- frenchpolynesia_thresher_DOA[j,i] + frenchpolynesia_thresher_PRM[j,i]
  }
}

#catches
frenchpolynesia_thresher$catch <- rowMeans(frenchpolynesia_thresher_catches[,1:1000])
frenchpolynesia_thresher$catch.sd = apply(frenchpolynesia_thresher_catches[,1:1000], 1, sd)

#DOA
frenchpolynesia_thresher$DOA <- rowMeans(frenchpolynesia_thresher_DOA[,1:1000])
frenchpolynesia_thresher$DOA.sd = apply(frenchpolynesia_thresher_DOA[,1:1000], 1, sd)

#prm
frenchpolynesia_thresher$PRM = rowMeans(frenchpolynesia_thresher_PRM[,1:1000])
frenchpolynesia_thresher$PRM.sd = apply(frenchpolynesia_thresher_PRM[,1:1000], 1, sd)

#total
frenchpolynesia_thresher$total = rowMeans(frenchpolynesia_thresher_total[,1:1000])
frenchpolynesia_thresher$total.sd = apply(frenchpolynesia_thresher_total[,1:1000], 1, sd)

#### shortfinmako sharks ####
#row 4 in mortality; need to change this for each species when drawing rates)
r_mako = 4

frenchpolynesia_mako <- frenchpolynesia_comb[,c(1:4,17,18)] #create dataframe

mako_cm <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako]) 
mako_prm <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako]) 

frenchpolynesia_mako_catches <- matrix(nrow=n, ncol=1000)
frenchpolynesia_mako_DOA <- matrix(nrow=n, ncol=1000)
frenchpolynesia_mako_released <- matrix(nrow=n, ncol=1000)
frenchpolynesia_mako_PRM<- matrix(nrow=n, ncol=1000)
frenchpolynesia_mako_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    frenchpolynesia_mako_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$mako.logcpue[j], frenchpolynesia_comb$mako.logcpue.se[j])
    frenchpolynesia_mako_DOA[j,i] <- frenchpolynesia_mako_catches[j,i] * mako_cm[i] 
    frenchpolynesia_mako_released[j,i] <- frenchpolynesia_mako_catches[j,i] - frenchpolynesia_mako_DOA[j,i]
    frenchpolynesia_mako_PRM[j,i] <- frenchpolynesia_mako_released[j,i] * mako_prm[i] 
    frenchpolynesia_mako_total[j,i] <- frenchpolynesia_mako_DOA[j,i] + frenchpolynesia_mako_PRM[j,i]
  }
}

#catches
frenchpolynesia_mako$catch <- rowMeans(frenchpolynesia_mako_catches[,1:1000])
frenchpolynesia_mako$catch.sd = apply(frenchpolynesia_mako_catches[,1:1000], 1, sd)

#DOA
frenchpolynesia_mako$DOA <- rowMeans(frenchpolynesia_mako_DOA[,1:1000])
frenchpolynesia_mako$DOA.sd = apply(frenchpolynesia_mako_DOA[,1:1000], 1, sd)

#prm
frenchpolynesia_mako$PRM = rowMeans(frenchpolynesia_mako_PRM[,1:1000])
frenchpolynesia_mako$PRM.sd = apply(frenchpolynesia_mako_PRM[,1:1000], 1, sd)

#total
frenchpolynesia_mako$total = rowMeans(frenchpolynesia_mako_total[,1:1000])
frenchpolynesia_mako$total.sd = apply(frenchpolynesia_mako_total[,1:1000], 1, sd)

#### oceanic whitetip sharks ####
#row 8 in mortality; need to change this for each species when drawing rates)
r_oceanicwhitetip = 8

frenchpolynesia_oceanicwhitetip <- frenchpolynesia_comb[,c(1:4,25,26)] #create dataframe
oceanicwhitetip_cm <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip]) 
oceanicwhitetip_prm <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip]) 

frenchpolynesia_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
frenchpolynesia_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
frenchpolynesia_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
frenchpolynesia_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
frenchpolynesia_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    frenchpolynesia_oceanicwhitetip_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$oceanicwhitetip.logcpue[j], frenchpolynesia_comb$oceanicwhitetip.logcpue.se[j])
    frenchpolynesia_oceanicwhitetip_DOA[j,i] <- frenchpolynesia_oceanicwhitetip_catches[j,i] * oceanicwhitetip_cm[i] 
    frenchpolynesia_oceanicwhitetip_released[j,i] <- frenchpolynesia_oceanicwhitetip_catches[j,i] - frenchpolynesia_oceanicwhitetip_DOA[j,i]
    frenchpolynesia_oceanicwhitetip_PRM[j,i] <- frenchpolynesia_oceanicwhitetip_released[j,i] * oceanicwhitetip_prm[i] 
    frenchpolynesia_oceanicwhitetip_total[j,i] <- frenchpolynesia_oceanicwhitetip_DOA[j,i] + frenchpolynesia_oceanicwhitetip_PRM[j,i]
  }
}

#catches
frenchpolynesia_oceanicwhitetip$catch <- rowMeans(frenchpolynesia_oceanicwhitetip_catches[,1:1000])
frenchpolynesia_oceanicwhitetip$catch.sd = apply(frenchpolynesia_oceanicwhitetip_catches[,1:1000], 1, sd)

#DOA
frenchpolynesia_oceanicwhitetip$DOA <- rowMeans(frenchpolynesia_oceanicwhitetip_DOA[,1:1000])
frenchpolynesia_oceanicwhitetip$DOA.sd = apply(frenchpolynesia_oceanicwhitetip_DOA[,1:1000], 1, sd)

#prm
frenchpolynesia_oceanicwhitetip$PRM = rowMeans(frenchpolynesia_oceanicwhitetip_PRM[,1:1000])
frenchpolynesia_oceanicwhitetip$PRM.sd = apply(frenchpolynesia_oceanicwhitetip_PRM[,1:1000], 1, sd)

#total
frenchpolynesia_oceanicwhitetip$total = rowMeans(frenchpolynesia_oceanicwhitetip_total[,1:1000])
frenchpolynesia_oceanicwhitetip$total.sd = apply(frenchpolynesia_oceanicwhitetip_total[,1:1000], 1, sd)

#### hammerhead sharks ####
#row 9 in mortality; need to change this for each species when drawing rates)
r_hammerhead = 9

frenchpolynesia_hammerhead <- frenchpolynesia_comb[,c(1:4,29,30)] #create dataframe

hammerhead_cm <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead]) 
hammerhead_prm <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead]) 

frenchpolynesia_hammerhead_catches <- matrix(nrow=n, ncol=1000)
frenchpolynesia_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
frenchpolynesia_hammerhead_released <- matrix(nrow=n, ncol=1000)
frenchpolynesia_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
frenchpolynesia_hammerhead_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    frenchpolynesia_hammerhead_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$hammerhead.logcpue[j], frenchpolynesia_comb$hammerhead.logcpue.se[j])
    frenchpolynesia_hammerhead_DOA[j,i] <- frenchpolynesia_hammerhead_catches[j,i] * hammerhead_cm[i] 
    frenchpolynesia_hammerhead_released[j,i] <- frenchpolynesia_hammerhead_catches[j,i] - frenchpolynesia_hammerhead_DOA[j,i]
    frenchpolynesia_hammerhead_PRM[j,i] <- frenchpolynesia_hammerhead_released[j,i] * hammerhead_prm[i] 
    frenchpolynesia_hammerhead_total[j,i] <- frenchpolynesia_hammerhead_DOA[j,i] + frenchpolynesia_hammerhead_PRM[j,i]
  }
}

#catches
frenchpolynesia_hammerhead$catch <- rowMeans(frenchpolynesia_hammerhead_catches[,1:1000])
frenchpolynesia_hammerhead$catch.sd = apply(frenchpolynesia_hammerhead_catches[,1:1000], 1, sd)

#DOA
frenchpolynesia_hammerhead$DOA <- rowMeans(frenchpolynesia_hammerhead_DOA[,1:1000])
frenchpolynesia_hammerhead$DOA.sd = apply(frenchpolynesia_hammerhead_DOA[,1:1000], 1, sd)

#prm
frenchpolynesia_hammerhead$PRM = rowMeans(frenchpolynesia_hammerhead_PRM[,1:1000])
frenchpolynesia_hammerhead$PRM.sd = apply(frenchpolynesia_hammerhead_PRM[,1:1000], 1, sd)

#total
frenchpolynesia_hammerhead$total = rowMeans(frenchpolynesia_hammerhead_total[,1:1000])
frenchpolynesia_hammerhead$total.sd = apply(frenchpolynesia_hammerhead_total[,1:1000], 1, sd)

#### othersharks sharks ####
#row 10 in mortality; need to change this for each species when drawing rates)
r_othersharks = 10

frenchpolynesia_othersharks <- frenchpolynesia_comb[,c(1:4,33,34)] #create dataframe

othersharks_cm <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks]) 
othersharks_prm <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks]) 

frenchpolynesia_othersharks_catches <- matrix(nrow=n, ncol=1000)
frenchpolynesia_othersharks_DOA <- matrix(nrow=n, ncol=1000)
frenchpolynesia_othersharks_released <- matrix(nrow=n, ncol=1000)
frenchpolynesia_othersharks_PRM<- matrix(nrow=n, ncol=1000)
frenchpolynesia_othersharks_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    frenchpolynesia_othersharks_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$othersharks.logcpue[j], frenchpolynesia_comb$othersharks.logcpue.se[j])
    frenchpolynesia_othersharks_DOA[j,i] <- frenchpolynesia_othersharks_catches[j,i] * othersharks_cm[i] 
    frenchpolynesia_othersharks_released[j,i] <- frenchpolynesia_othersharks_catches[j,i] - frenchpolynesia_othersharks_DOA[j,i]
    frenchpolynesia_othersharks_PRM[j,i] <- frenchpolynesia_othersharks_released[j,i] * othersharks_prm[i] 
    frenchpolynesia_othersharks_total[j,i] <- frenchpolynesia_othersharks_DOA[j,i] + frenchpolynesia_othersharks_PRM[j,i]
  }
}

#catches
frenchpolynesia_othersharks$catch <- rowMeans(frenchpolynesia_othersharks_catches[,1:1000])
frenchpolynesia_othersharks$catch.sd = apply(frenchpolynesia_othersharks_catches[,1:1000], 1, sd)

#DOA
frenchpolynesia_othersharks$DOA <- rowMeans(frenchpolynesia_othersharks_DOA[,1:1000])
frenchpolynesia_othersharks$DOA.sd = apply(frenchpolynesia_othersharks_DOA[,1:1000], 1, sd)

#prm
frenchpolynesia_othersharks$PRM = rowMeans(frenchpolynesia_othersharks_PRM[,1:1000])
frenchpolynesia_othersharks$PRM.sd = apply(frenchpolynesia_othersharks_PRM[,1:1000], 1, sd)

#total
frenchpolynesia_othersharks$total = rowMeans(frenchpolynesia_othersharks_total[,1:1000])
frenchpolynesia_othersharks$total.sd = apply(frenchpolynesia_othersharks_total[,1:1000], 1, sd)

#### frenchpolynesia Totals ####

frenchpolynesia_bsh$Species = "Blue Shark"
frenchpolynesia_silky$Species = "Silky Shark"
frenchpolynesia_thresher$Species="Thresher"
frenchpolynesia_mako$Species="Mako"
frenchpolynesia_oceanicwhitetip$Species="Oceanic Whitetip"
frenchpolynesia_hammerhead$Species="Hammerhead"
frenchpolynesia_othersharks$Species="Other Sharks"

colnames(frenchpolynesia_bsh)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(frenchpolynesia_silky)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(frenchpolynesia_thresher)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(frenchpolynesia_mako)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(frenchpolynesia_oceanicwhitetip)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(frenchpolynesia_hammerhead)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(frenchpolynesia_othersharks)[c(5,6)] <- c("logcpue","logcpue.se")

frenchpolynesia_total_preds <- rbind(frenchpolynesia_bsh,
                          frenchpolynesia_silky,
                          frenchpolynesia_thresher,
                          frenchpolynesia_mako,
                          frenchpolynesia_oceanicwhitetip,
                          frenchpolynesia_hammerhead,
                          frenchpolynesia_othersharks)

end.time <- Sys.time()
end.time - start.time}

write.csv(frenchpolynesia_total_preds, "frenchpolynesia_total_preds.csv", row.names = F)

#### summary ####
dat <- read.csv("frenchpolynesia_total_preds.csv")
frenchpolynesia_bsh <- subset(dat, Species == "Blue Shark")
frenchpolynesia_silky <- subset(dat, Species == "Silky Shark")
frenchpolynesia_thresher <- subset(dat, Species == "Thresher")
frenchpolynesia_mako <- subset(dat, Species == "Mako")
frenchpolynesia_oceanicwhitetip <- subset(dat, Species == "Oceanic Whitetip")
frenchpolynesia_hammerhead <- subset(dat, Species == "Hammerhead")
frenchpolynesia_othersharks <- subset(dat, Species == "Other Sharks")




frenchpolynesia_summary <- data.frame("Species" = c("Blue Shark", "Silky Shark", "Thresher", 
                                          "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"),
                            "Catch" = c(sum(frenchpolynesia_bsh$catch),
                                        sum(frenchpolynesia_silky$catch), 
                                        sum(frenchpolynesia_thresher$catch),
                                        sum(frenchpolynesia_mako$catch),
                                        sum(frenchpolynesia_oceanicwhitetip$catch),
                                        sum(frenchpolynesia_hammerhead$catch),
                                        sum(frenchpolynesia_othersharks$catch)),
                            "Catch.SD" = c(sqrt(sum(frenchpolynesia_bsh$catch.sd^2, na.rm=T)),
                                           sqrt(sum(frenchpolynesia_silky$catch.sd^2, na.rm=T)),
                                           sqrt(sum(frenchpolynesia_thresher$catch.sd^2, na.rm=T)),
                                           sqrt(sum(frenchpolynesia_mako$catch.sd^2, na.rm=T)),
                                           sqrt(sum(frenchpolynesia_oceanicwhitetip$catch.sd^2, na.rm=T)),
                                           sqrt(sum(frenchpolynesia_hammerhead$catch.sd^2, na.rm=T)),
                                           sqrt(sum(frenchpolynesia_othersharks$catch.sd^2, na.rm=T))),
                            "CM" = c(sum(frenchpolynesia_bsh$DOA),
                                     sum(frenchpolynesia_silky$DOA),
                                     sum(frenchpolynesia_thresher$DOA),
                                     sum(frenchpolynesia_mako$DOA),
                                     sum(frenchpolynesia_oceanicwhitetip$DOA),
                                     sum(frenchpolynesia_hammerhead$DOA),
                                     sum(frenchpolynesia_othersharks$DOA)),
                            "CM.sd"=c(sqrt(sum(frenchpolynesia_bsh$DOA.sd^2)),
                                      sqrt(sum(frenchpolynesia_silky$DOA.sd^2)),
                                      sqrt(sum(frenchpolynesia_thresher$DOA.sd^2)),
                                      sqrt(sum(frenchpolynesia_mako$DOA.sd^2)),
                                      sqrt(sum(frenchpolynesia_oceanicwhitetip$DOA.sd^2)),
                                      sqrt(sum(frenchpolynesia_hammerhead$DOA.sd^2)),
                                      sqrt(sum(frenchpolynesia_othersharks$DOA.sd^2))),
                            "PRM" = c(sum(frenchpolynesia_bsh$PRM),
                                      sum(frenchpolynesia_silky$PRM),
                                      sum(frenchpolynesia_thresher$PRM),
                                      sum(frenchpolynesia_mako$PRM),
                                      sum(frenchpolynesia_oceanicwhitetip$PRM), 
                                      sum(frenchpolynesia_hammerhead$PRM), 
                                      sum(frenchpolynesia_othersharks$PRM)),
                            "PRM.sd"=c(sqrt(sum(frenchpolynesia_bsh$PRM.sd^2)),
                                       sqrt(sum(frenchpolynesia_silky$PRM.sd^2)),
                                       sqrt(sum(frenchpolynesia_thresher$PRM.sd^2)),
                                       sqrt(sum(frenchpolynesia_mako$PRM.sd^2)),
                                       sqrt(sum(frenchpolynesia_oceanicwhitetip$PRM.sd^2)),
                                       sqrt(sum(frenchpolynesia_hammerhead$PRM.sd^2)),
                                       sqrt(sum(frenchpolynesia_othersharks$PRM.sd^2))),
                            "Total"=c(sum(frenchpolynesia_bsh$total),
                                      sum(frenchpolynesia_silky$total),
                                      sum(frenchpolynesia_thresher$total),
                                      sum(frenchpolynesia_mako$total),
                                      sum(frenchpolynesia_oceanicwhitetip$total),
                                      sum(frenchpolynesia_hammerhead$total),
                                      sum(frenchpolynesia_othersharks$total)),
                            "Total.sd"=c(sqrt(sum(frenchpolynesia_bsh$total.sd^2)),
                                         sqrt(sum(frenchpolynesia_silky$total.sd^2)),
                                         sqrt(sum(frenchpolynesia_thresher$total.sd^2)), 
                                         sqrt(sum(frenchpolynesia_mako$total.sd^2)),
                                         sqrt(sum(frenchpolynesia_oceanicwhitetip$total.sd^2)),
                                         sqrt(sum(frenchpolynesia_hammerhead$total.sd^2)), 
                                         sqrt(sum(frenchpolynesia_othersharks$total.sd^2))))


write.csv(frenchpolynesia_summary, "frenchpolynesia_summary.csv", row.names = F)

#mortality plotting for Fig1
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
frenchpolynesia_mortality <- read.csv("frenchpolynesia_total_preds.csv")
frenchpolynesia_proj <- aggregate(data=frenchpolynesia_mortality, total ~ Lat + Long, FUN='sum')
dev.off()
world <- map_data("world")
frenchpolynesia_map <- ggplot(data=frenchpolynesia_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Bycatch Mortality \n(# of individuals)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = pyf_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(-159.5,-130.5), breaks=seq(-155,-135,by=5)) +
  scale_y_continuous(limits=c(-33,-3), breaks=seq(-30,-5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="French Polynesia")

