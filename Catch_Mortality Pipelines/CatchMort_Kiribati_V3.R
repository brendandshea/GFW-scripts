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
kiribati_proj <- read.csv("kiribati2019.csv")

kiribati_proj2 <- kiribati_proj[,c(5,6,9,10)]
#rename
colnames(kiribati_proj2)[1] <- "Long"
colnames(kiribati_proj2)[2] <- "Lat"

#merge
kiribati_comb <- merge(kiribati_proj2, master_cpue, by=c("Lat", "Long"), all=T)
kiribati_comb <- kiribati_comb %>% drop_na(log_hooks)

n <- nrow(kiribati_comb)

kiribati_hook_preds <- matrix(nrow=n, ncol=1000)
for (j in 1:n) {
  for (i in 1:1000) {
    kiribati_hook_preds[j,i] <- rlnorm(1,kiribati_comb$log_hooks[j],kiribati_comb$log_hooks.se[j])
  }
}

{start.time <- Sys.time()
#### blue sharks ####
#row 2 in mortality; need to change this for each species when drawing rates)
r_bsh = 2

kiribati_bsh <- kiribati_comb[,c(1:4,9,10)]

bsh_cm <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh]) 
bsh_prm <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh]) 

kiribati_bsh_catches <- matrix(nrow=n, ncol=1000)
kiribati_bsh_DOA <- matrix(nrow=n, ncol=1000)
kiribati_bsh_released <- matrix(nrow=n, ncol=1000)
kiribati_bsh_PRM<- matrix(nrow=n, ncol=1000)
kiribati_bsh_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    kiribati_bsh_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$blueshark.logcpue[j], kiribati_comb$blueshark.logcpue.se[j])
    kiribati_bsh_DOA[j,i] <- kiribati_bsh_catches[j,i] * bsh_cm[i] 
    kiribati_bsh_released[j,i] <- kiribati_bsh_catches[j,i] - kiribati_bsh_DOA[j,i]
    kiribati_bsh_PRM[j,i] <- kiribati_bsh_released[j,i] * bsh_prm[i] 
    kiribati_bsh_total[j,i] <- kiribati_bsh_DOA[j,i] + kiribati_bsh_PRM[j,i]
  }
}

#catches
kiribati_bsh$catch <- rowMeans(kiribati_bsh_catches[,1:1000])
kiribati_bsh$catch.sd = apply(kiribati_bsh_catches[,1:1000], 1, sd)

 #DOA
kiribati_bsh$DOA <- rowMeans(kiribati_bsh_DOA[,1:1000])
kiribati_bsh$DOA.sd = apply(kiribati_bsh_DOA[,1:1000], 1, sd)

#prm
kiribati_bsh$PRM = rowMeans(kiribati_bsh_PRM[,1:1000])
kiribati_bsh$PRM.sd = apply(kiribati_bsh_PRM[,1:1000], 1, sd)

#total
kiribati_bsh$total = rowMeans(kiribati_bsh_total[,1:1000])
kiribati_bsh$total.sd = apply(kiribati_bsh_total[,1:1000], 1, sd)

#### silky sharks ####
r_silky=5 #row 5 in mortality; need to change this for each species when drawing rates)

kiribati_silky <- kiribati_comb[,c(1:4,13,14)] #create dataframe

silky_cm <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky]) 
silky_prm <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky]) 

kiribati_silky_catches <- matrix(nrow=n, ncol=1000)
kiribati_silky_DOA <- matrix(nrow=n, ncol=1000)
kiribati_silky_released <- matrix(nrow=n, ncol=1000)
kiribati_silky_PRM<- matrix(nrow=n, ncol=1000)
kiribati_silky_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    kiribati_silky_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$silkyshark.logcpue[j], kiribati_comb$silkyshark.logcpue.se[j])
    kiribati_silky_DOA[j,i] <- kiribati_silky_catches[j,i] * silky_cm[i] 
    kiribati_silky_released[j,i] <- kiribati_silky_catches[j,i] - kiribati_silky_DOA[j,i]
    kiribati_silky_PRM[j,i] <- kiribati_silky_released[j,i] * silky_prm[i] 
    kiribati_silky_total[j,i] <- kiribati_silky_DOA[j,i] + kiribati_silky_PRM[j,i]
  }
}

#catches
kiribati_silky$catch <- rowMeans(kiribati_silky_catches[,1:1000])
kiribati_silky$catch.sd = apply(kiribati_silky_catches[,1:1000], 1, sd)

#DOA
kiribati_silky$DOA <- rowMeans(kiribati_silky_DOA[,1:1000])
kiribati_silky$DOA.sd = apply(kiribati_silky_DOA[,1:1000], 1, sd)

#prm
kiribati_silky$PRM = rowMeans(kiribati_silky_PRM[,1:1000])
kiribati_silky$PRM.sd = apply(kiribati_silky_PRM[,1:1000], 1, sd)

#total
kiribati_silky$total = rowMeans(kiribati_silky_total[,1:1000])
kiribati_silky$total.sd = apply(kiribati_silky_total[,1:1000], 1, sd)

#### thresher sharks ####
#row 1 in mortality; need to change this for each species when drawing rates)
r_thresher = 1

kiribati_thresher <- kiribati_comb[,c(1:4,21,22)] #create dataframe

thresher_cm <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher]) 
thresher_prm <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher]) 

kiribati_thresher_catches <- matrix(nrow=n, ncol=1000)
kiribati_thresher_DOA <- matrix(nrow=n, ncol=1000)
kiribati_thresher_released <- matrix(nrow=n, ncol=1000)
kiribati_thresher_PRM<- matrix(nrow=n, ncol=1000)
kiribati_thresher_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    kiribati_thresher_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$thresher.logcpue[j], kiribati_comb$thresher.logcpue.se[j])
    kiribati_thresher_DOA[j,i] <- kiribati_thresher_catches[j,i] * thresher_cm[i] 
    kiribati_thresher_released[j,i] <- kiribati_thresher_catches[j,i] - kiribati_thresher_DOA[j,i]
    kiribati_thresher_PRM[j,i] <- kiribati_thresher_released[j,i] * thresher_prm[i] 
    kiribati_thresher_total[j,i] <- kiribati_thresher_DOA[j,i] + kiribati_thresher_PRM[j,i]
  }
}

#catches
kiribati_thresher$catch <- rowMeans(kiribati_thresher_catches[,1:1000])
kiribati_thresher$catch.sd = apply(kiribati_thresher_catches[,1:1000], 1, sd)

#DOA
kiribati_thresher$DOA <- rowMeans(kiribati_thresher_DOA[,1:1000])
kiribati_thresher$DOA.sd = apply(kiribati_thresher_DOA[,1:1000], 1, sd)

#prm
kiribati_thresher$PRM = rowMeans(kiribati_thresher_PRM[,1:1000])
kiribati_thresher$PRM.sd = apply(kiribati_thresher_PRM[,1:1000], 1, sd)

#total
kiribati_thresher$total = rowMeans(kiribati_thresher_total[,1:1000])
kiribati_thresher$total.sd = apply(kiribati_thresher_total[,1:1000], 1, sd)

#### shortfinmako sharks ####
#row 4 in mortality; need to change this for each species when drawing rates)
r_mako = 4

kiribati_mako <- kiribati_comb[,c(1:4,17,18)] #create dataframe

mako_cm <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako]) 
mako_prm <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako]) 

kiribati_mako_catches <- matrix(nrow=n, ncol=1000)
kiribati_mako_DOA <- matrix(nrow=n, ncol=1000)
kiribati_mako_released <- matrix(nrow=n, ncol=1000)
kiribati_mako_PRM<- matrix(nrow=n, ncol=1000)
kiribati_mako_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    kiribati_mako_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$mako.logcpue[j], kiribati_comb$mako.logcpue.se[j])
    kiribati_mako_DOA[j,i] <- kiribati_mako_catches[j,i] * mako_cm[i] 
    kiribati_mako_released[j,i] <- kiribati_mako_catches[j,i] - kiribati_mako_DOA[j,i]
    kiribati_mako_PRM[j,i] <- kiribati_mako_released[j,i] * mako_prm[i] 
    kiribati_mako_total[j,i] <- kiribati_mako_DOA[j,i] + kiribati_mako_PRM[j,i]
  }
}

#catches
kiribati_mako$catch <- rowMeans(kiribati_mako_catches[,1:1000])
kiribati_mako$catch.sd = apply(kiribati_mako_catches[,1:1000], 1, sd)

#DOA
kiribati_mako$DOA <- rowMeans(kiribati_mako_DOA[,1:1000])
kiribati_mako$DOA.sd = apply(kiribati_mako_DOA[,1:1000], 1, sd)

#prm
kiribati_mako$PRM = rowMeans(kiribati_mako_PRM[,1:1000])
kiribati_mako$PRM.sd = apply(kiribati_mako_PRM[,1:1000], 1, sd)

#total
kiribati_mako$total = rowMeans(kiribati_mako_total[,1:1000])
kiribati_mako$total.sd = apply(kiribati_mako_total[,1:1000], 1, sd)

#### oceanic whitetip sharks ####
#row 8 in mortality; need to change this for each species when drawing rates)
r_oceanicwhitetip = 8

kiribati_oceanicwhitetip <- kiribati_comb[,c(1:4,25,26)] #create dataframe
oceanicwhitetip_cm <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip]) 
oceanicwhitetip_prm <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip]) 

kiribati_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
kiribati_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
kiribati_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
kiribati_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
kiribati_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    kiribati_oceanicwhitetip_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$oceanicwhitetip.logcpue[j], kiribati_comb$oceanicwhitetip.logcpue.se[j])
    kiribati_oceanicwhitetip_DOA[j,i] <- kiribati_oceanicwhitetip_catches[j,i] * oceanicwhitetip_cm[i] 
    kiribati_oceanicwhitetip_released[j,i] <- kiribati_oceanicwhitetip_catches[j,i] - kiribati_oceanicwhitetip_DOA[j,i]
    kiribati_oceanicwhitetip_PRM[j,i] <- kiribati_oceanicwhitetip_released[j,i] * oceanicwhitetip_prm[i] 
    kiribati_oceanicwhitetip_total[j,i] <- kiribati_oceanicwhitetip_DOA[j,i] + kiribati_oceanicwhitetip_PRM[j,i]
  }
}

#catches
kiribati_oceanicwhitetip$catch <- rowMeans(kiribati_oceanicwhitetip_catches[,1:1000])
kiribati_oceanicwhitetip$catch.sd = apply(kiribati_oceanicwhitetip_catches[,1:1000], 1, sd)

#DOA
kiribati_oceanicwhitetip$DOA <- rowMeans(kiribati_oceanicwhitetip_DOA[,1:1000])
kiribati_oceanicwhitetip$DOA.sd = apply(kiribati_oceanicwhitetip_DOA[,1:1000], 1, sd)

#prm
kiribati_oceanicwhitetip$PRM = rowMeans(kiribati_oceanicwhitetip_PRM[,1:1000])
kiribati_oceanicwhitetip$PRM.sd = apply(kiribati_oceanicwhitetip_PRM[,1:1000], 1, sd)

#total
kiribati_oceanicwhitetip$total = rowMeans(kiribati_oceanicwhitetip_total[,1:1000])
kiribati_oceanicwhitetip$total.sd = apply(kiribati_oceanicwhitetip_total[,1:1000], 1, sd)

#### hammerhead sharks ####
#row 9 in mortality; need to change this for each species when drawing rates)
r_hammerhead = 9

kiribati_hammerhead <- kiribati_comb[,c(1:4,29,30)] #create dataframe

hammerhead_cm <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead]) 
hammerhead_prm <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead]) 

kiribati_hammerhead_catches <- matrix(nrow=n, ncol=1000)
kiribati_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
kiribati_hammerhead_released <- matrix(nrow=n, ncol=1000)
kiribati_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
kiribati_hammerhead_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    kiribati_hammerhead_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$hammerhead.logcpue[j], kiribati_comb$hammerhead.logcpue.se[j])
    kiribati_hammerhead_DOA[j,i] <- kiribati_hammerhead_catches[j,i] * hammerhead_cm[i] 
    kiribati_hammerhead_released[j,i] <- kiribati_hammerhead_catches[j,i] - kiribati_hammerhead_DOA[j,i]
    kiribati_hammerhead_PRM[j,i] <- kiribati_hammerhead_released[j,i] * hammerhead_prm[i] 
    kiribati_hammerhead_total[j,i] <- kiribati_hammerhead_DOA[j,i] + kiribati_hammerhead_PRM[j,i]
  }
}

#catches
kiribati_hammerhead$catch <- rowMeans(kiribati_hammerhead_catches[,1:1000])
kiribati_hammerhead$catch.sd = apply(kiribati_hammerhead_catches[,1:1000], 1, sd)

#DOA
kiribati_hammerhead$DOA <- rowMeans(kiribati_hammerhead_DOA[,1:1000])
kiribati_hammerhead$DOA.sd = apply(kiribati_hammerhead_DOA[,1:1000], 1, sd)

#prm
kiribati_hammerhead$PRM = rowMeans(kiribati_hammerhead_PRM[,1:1000])
kiribati_hammerhead$PRM.sd = apply(kiribati_hammerhead_PRM[,1:1000], 1, sd)

#total
kiribati_hammerhead$total = rowMeans(kiribati_hammerhead_total[,1:1000])
kiribati_hammerhead$total.sd = apply(kiribati_hammerhead_total[,1:1000], 1, sd)

#### othersharks sharks ####
#row 10 in mortality; need to change this for each species when drawing rates)
r_othersharks = 10

kiribati_othersharks <- kiribati_comb[,c(1:4,33,34)] #create dataframe

othersharks_cm <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks]) 
othersharks_prm <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks]) 

kiribati_othersharks_catches <- matrix(nrow=n, ncol=1000)
kiribati_othersharks_DOA <- matrix(nrow=n, ncol=1000)
kiribati_othersharks_released <- matrix(nrow=n, ncol=1000)
kiribati_othersharks_PRM<- matrix(nrow=n, ncol=1000)
kiribati_othersharks_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    kiribati_othersharks_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$othersharks.logcpue[j], kiribati_comb$othersharks.logcpue.se[j])
    kiribati_othersharks_DOA[j,i] <- kiribati_othersharks_catches[j,i] * othersharks_cm[i] 
    kiribati_othersharks_released[j,i] <- kiribati_othersharks_catches[j,i] - kiribati_othersharks_DOA[j,i]
    kiribati_othersharks_PRM[j,i] <- kiribati_othersharks_released[j,i] * othersharks_prm[i] 
    kiribati_othersharks_total[j,i] <- kiribati_othersharks_DOA[j,i] + kiribati_othersharks_PRM[j,i]
  }
}

#catches
kiribati_othersharks$catch <- rowMeans(kiribati_othersharks_catches[,1:1000])
kiribati_othersharks$catch.sd = apply(kiribati_othersharks_catches[,1:1000], 1, sd)

#DOA
kiribati_othersharks$DOA <- rowMeans(kiribati_othersharks_DOA[,1:1000])
kiribati_othersharks$DOA.sd = apply(kiribati_othersharks_DOA[,1:1000], 1, sd)

#prm
kiribati_othersharks$PRM = rowMeans(kiribati_othersharks_PRM[,1:1000])
kiribati_othersharks$PRM.sd = apply(kiribati_othersharks_PRM[,1:1000], 1, sd)

#total
kiribati_othersharks$total = rowMeans(kiribati_othersharks_total[,1:1000])
kiribati_othersharks$total.sd = apply(kiribati_othersharks_total[,1:1000], 1, sd)

#### kiribati Totals ####

kiribati_bsh$Species = "Blue Shark"
kiribati_silky$Species = "Silky Shark"
kiribati_thresher$Species="Thresher"
kiribati_mako$Species="Mako"
kiribati_oceanicwhitetip$Species="Oceanic Whitetip"
kiribati_hammerhead$Species="Hammerhead"
kiribati_othersharks$Species="Other Sharks"

colnames(kiribati_bsh)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(kiribati_silky)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(kiribati_thresher)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(kiribati_mako)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(kiribati_oceanicwhitetip)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(kiribati_hammerhead)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(kiribati_othersharks)[c(5,6)] <- c("logcpue","logcpue.se")

kiribati_total_preds <- rbind(kiribati_bsh,
                          kiribati_silky,
                          kiribati_thresher,
                          kiribati_mako,
                          kiribati_oceanicwhitetip,
                          kiribati_hammerhead,
                          kiribati_othersharks)

end.time <- Sys.time()
end.time - start.time}

write.csv(kiribati_total_preds, "kiribati_total_preds.csv", row.names = F)

#### summary ####
dat <- read.csv("kiribati_total_preds.csv")
kiribati_bsh <- subset(dat, Species == "Blue Shark")
kiribati_silky <- subset(dat, Species == "Silky Shark")
kiribati_thresher <- subset(dat, Species == "Thresher")
kiribati_mako <- subset(dat, Species == "Mako")
kiribati_oceanicwhitetip <- subset(dat, Species == "Oceanic Whitetip")
kiribati_hammerhead <- subset(dat, Species == "Hammerhead")
kiribati_othersharks <- subset(dat, Species == "Other Sharks")




kiribati_summary <- data.frame("Species" = c("Blue Shark", "Silky Shark", "Thresher", 
                                          "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"),
                            "Catch" = c(sum(kiribati_bsh$catch),
                                        sum(kiribati_silky$catch), 
                                        sum(kiribati_thresher$catch),
                                        sum(kiribati_mako$catch),
                                        sum(kiribati_oceanicwhitetip$catch),
                                        sum(kiribati_hammerhead$catch),
                                        sum(kiribati_othersharks$catch)),
                            "Catch.SD" = c(sqrt(sum(kiribati_bsh$catch.sd^2, na.rm=T)),
                                           sqrt(sum(kiribati_silky$catch.sd^2, na.rm=T)),
                                           sqrt(sum(kiribati_thresher$catch.sd^2, na.rm=T)),
                                           sqrt(sum(kiribati_mako$catch.sd^2, na.rm=T)),
                                           sqrt(sum(kiribati_oceanicwhitetip$catch.sd^2, na.rm=T)),
                                           sqrt(sum(kiribati_hammerhead$catch.sd^2, na.rm=T)),
                                           sqrt(sum(kiribati_othersharks$catch.sd^2, na.rm=T))),
                            "CM" = c(sum(kiribati_bsh$DOA),
                                     sum(kiribati_silky$DOA),
                                     sum(kiribati_thresher$DOA),
                                     sum(kiribati_mako$DOA),
                                     sum(kiribati_oceanicwhitetip$DOA),
                                     sum(kiribati_hammerhead$DOA),
                                     sum(kiribati_othersharks$DOA)),
                            "CM.sd"=c(sqrt(sum(kiribati_bsh$DOA.sd^2)),
                                      sqrt(sum(kiribati_silky$DOA.sd^2)),
                                      sqrt(sum(kiribati_thresher$DOA.sd^2)),
                                      sqrt(sum(kiribati_mako$DOA.sd^2)),
                                      sqrt(sum(kiribati_oceanicwhitetip$DOA.sd^2)),
                                      sqrt(sum(kiribati_hammerhead$DOA.sd^2)),
                                      sqrt(sum(kiribati_othersharks$DOA.sd^2))),
                            "PRM" = c(sum(kiribati_bsh$PRM),
                                      sum(kiribati_silky$PRM),
                                      sum(kiribati_thresher$PRM),
                                      sum(kiribati_mako$PRM),
                                      sum(kiribati_oceanicwhitetip$PRM), 
                                      sum(kiribati_hammerhead$PRM), 
                                      sum(kiribati_othersharks$PRM)),
                            "PRM.sd"=c(sqrt(sum(kiribati_bsh$PRM.sd^2)),
                                       sqrt(sum(kiribati_silky$PRM.sd^2)),
                                       sqrt(sum(kiribati_thresher$PRM.sd^2)),
                                       sqrt(sum(kiribati_mako$PRM.sd^2)),
                                       sqrt(sum(kiribati_oceanicwhitetip$PRM.sd^2)),
                                       sqrt(sum(kiribati_hammerhead$PRM.sd^2)),
                                       sqrt(sum(kiribati_othersharks$PRM.sd^2))),
                            "Total"=c(sum(kiribati_bsh$total),
                                      sum(kiribati_silky$total),
                                      sum(kiribati_thresher$total),
                                      sum(kiribati_mako$total),
                                      sum(kiribati_oceanicwhitetip$total),
                                      sum(kiribati_hammerhead$total),
                                      sum(kiribati_othersharks$total)),
                            "Total.sd"=c(sqrt(sum(kiribati_bsh$total.sd^2)),
                                         sqrt(sum(kiribati_silky$total.sd^2)),
                                         sqrt(sum(kiribati_thresher$total.sd^2)), 
                                         sqrt(sum(kiribati_mako$total.sd^2)),
                                         sqrt(sum(kiribati_oceanicwhitetip$total.sd^2)),
                                         sqrt(sum(kiribati_hammerhead$total.sd^2)), 
                                         sqrt(sum(kiribati_othersharks$total.sd^2))))


write.csv(kiribati_summary, "kiribati_summary.csv", row.names = F)


#plotting for Fig1
kiribati_mortality <- read.csv("kiribati_total_preds.csv")
kiribati_proj <- aggregate(data=kiribati_mortality, total ~ Lat + Long, FUN='sum')
dev.off()
world <- map_data("world")
kiribati_proj$maplon <- ifelse(kiribati_proj$Long < 0, kiribati_proj$Long + 360, kiribati_proj$Long)


kiribati_map <- ggplot(data=kiribati_proj, aes(x=maplon, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Bycatch Mortality \n(# of individuals)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  scale_x_continuous(limits=c(167,215), breaks=seq(170,210,by=5), labels=c(170,175,180,-175,-170,-165,-160,-155,-150)) +
  scale_y_continuous(limits=c(-14.5,9), breaks=seq(-10,5,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Kiribati")

kir_shp$long <- ifelse(kir_shp$long<0, kir_shp$long+360,kir_shp$long)
map.layer <-   geom_polygon(data = kir_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) 

kiribati_map <- kiribati_map + map.layer

kiribati_map
