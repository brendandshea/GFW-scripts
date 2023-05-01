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
cookislands_proj <- read.csv("cookislands2019.csv")

cookislands_proj2 <- cookislands_proj[,c(5,6,9,10)]
#rename
colnames(cookislands_proj2)[1] <- "Long"
colnames(cookislands_proj2)[2] <- "Lat"

#merge
cookislands_comb <- merge(cookislands_proj2, master_cpue, by=c("Lat", "Long"), all=T)
cookislands_comb <- cookislands_comb %>% drop_na(log_hooks)

n <- nrow(cookislands_comb)

cookislands_hook_preds <- matrix(nrow=n, ncol=1000)
for (j in 1:n) {
  for (i in 1:1000) {
    cookislands_hook_preds[j,i] <- rlnorm(1,cookislands_comb$log_hooks[j],cookislands_comb$log_hooks.se[j])
  }
}

{start.time <- Sys.time()
#### blue sharks ####
#row 2 in mortality; need to change this for each species when drawing rates)
r_bsh = 2

cookislands_bsh <- cookislands_comb[,c(1:4,9,10)]

bsh_cm <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh]) 
bsh_prm <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh]) 

cookislands_bsh_catches <- matrix(nrow=n, ncol=1000)
cookislands_bsh_DOA <- matrix(nrow=n, ncol=1000)
cookislands_bsh_released <- matrix(nrow=n, ncol=1000)
cookislands_bsh_PRM<- matrix(nrow=n, ncol=1000)
cookislands_bsh_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    cookislands_bsh_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$blueshark.logcpue[j], cookislands_comb$blueshark.logcpue.se[j])
    cookislands_bsh_DOA[j,i] <- cookislands_bsh_catches[j,i] * bsh_cm[i] 
    cookislands_bsh_released[j,i] <- cookislands_bsh_catches[j,i] - cookislands_bsh_DOA[j,i]
    cookislands_bsh_PRM[j,i] <- cookislands_bsh_released[j,i] * bsh_prm[i] 
    cookislands_bsh_total[j,i] <- cookislands_bsh_DOA[j,i] + cookislands_bsh_PRM[j,i]
  }
}

#catches
cookislands_bsh$catch <- rowMeans(cookislands_bsh_catches[,1:1000])
cookislands_bsh$catch.sd = apply(cookislands_bsh_catches[,1:1000], 1, sd)

 #DOA
cookislands_bsh$DOA <- rowMeans(cookislands_bsh_DOA[,1:1000])
cookislands_bsh$DOA.sd = apply(cookislands_bsh_DOA[,1:1000], 1, sd)

#prm
cookislands_bsh$PRM = rowMeans(cookislands_bsh_PRM[,1:1000])
cookislands_bsh$PRM.sd = apply(cookislands_bsh_PRM[,1:1000], 1, sd)

#total
cookislands_bsh$total = rowMeans(cookislands_bsh_total[,1:1000])
cookislands_bsh$total.sd = apply(cookislands_bsh_total[,1:1000], 1, sd)

#### silky sharks ####
r_silky=5 #row 5 in mortality; need to change this for each species when drawing rates)

cookislands_silky <- cookislands_comb[,c(1:4,13,14)] #create dataframe

silky_cm <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky]) 
silky_prm <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky]) 

cookislands_silky_catches <- matrix(nrow=n, ncol=1000)
cookislands_silky_DOA <- matrix(nrow=n, ncol=1000)
cookislands_silky_released <- matrix(nrow=n, ncol=1000)
cookislands_silky_PRM<- matrix(nrow=n, ncol=1000)
cookislands_silky_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    cookislands_silky_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$silkyshark.logcpue[j], cookislands_comb$silkyshark.logcpue.se[j])
    cookislands_silky_DOA[j,i] <- cookislands_silky_catches[j,i] * silky_cm[i] 
    cookislands_silky_released[j,i] <- cookislands_silky_catches[j,i] - cookislands_silky_DOA[j,i]
    cookislands_silky_PRM[j,i] <- cookislands_silky_released[j,i] * silky_prm[i] 
    cookislands_silky_total[j,i] <- cookislands_silky_DOA[j,i] + cookislands_silky_PRM[j,i]
  }
}

#catches
cookislands_silky$catch <- rowMeans(cookislands_silky_catches[,1:1000])
cookislands_silky$catch.sd = apply(cookislands_silky_catches[,1:1000], 1, sd)

#DOA
cookislands_silky$DOA <- rowMeans(cookislands_silky_DOA[,1:1000])
cookislands_silky$DOA.sd = apply(cookislands_silky_DOA[,1:1000], 1, sd)

#prm
cookislands_silky$PRM = rowMeans(cookislands_silky_PRM[,1:1000])
cookislands_silky$PRM.sd = apply(cookislands_silky_PRM[,1:1000], 1, sd)

#total
cookislands_silky$total = rowMeans(cookislands_silky_total[,1:1000])
cookislands_silky$total.sd = apply(cookislands_silky_total[,1:1000], 1, sd)

#### thresher sharks ####
#row 1 in mortality; need to change this for each species when drawing rates)
r_thresher = 1

cookislands_thresher <- cookislands_comb[,c(1:4,21,22)] #create dataframe

thresher_cm <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher]) 
thresher_prm <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher]) 

cookislands_thresher_catches <- matrix(nrow=n, ncol=1000)
cookislands_thresher_DOA <- matrix(nrow=n, ncol=1000)
cookislands_thresher_released <- matrix(nrow=n, ncol=1000)
cookislands_thresher_PRM<- matrix(nrow=n, ncol=1000)
cookislands_thresher_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    cookislands_thresher_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$thresher.logcpue[j], cookislands_comb$thresher.logcpue.se[j])
    cookislands_thresher_DOA[j,i] <- cookislands_thresher_catches[j,i] * thresher_cm[i] 
    cookislands_thresher_released[j,i] <- cookislands_thresher_catches[j,i] - cookislands_thresher_DOA[j,i]
    cookislands_thresher_PRM[j,i] <- cookislands_thresher_released[j,i] * thresher_prm[i] 
    cookislands_thresher_total[j,i] <- cookislands_thresher_DOA[j,i] + cookislands_thresher_PRM[j,i]
  }
}

#catches
cookislands_thresher$catch <- rowMeans(cookislands_thresher_catches[,1:1000])
cookislands_thresher$catch.sd = apply(cookislands_thresher_catches[,1:1000], 1, sd)

#DOA
cookislands_thresher$DOA <- rowMeans(cookislands_thresher_DOA[,1:1000])
cookislands_thresher$DOA.sd = apply(cookislands_thresher_DOA[,1:1000], 1, sd)

#prm
cookislands_thresher$PRM = rowMeans(cookislands_thresher_PRM[,1:1000])
cookislands_thresher$PRM.sd = apply(cookislands_thresher_PRM[,1:1000], 1, sd)

#total
cookislands_thresher$total = rowMeans(cookislands_thresher_total[,1:1000])
cookislands_thresher$total.sd = apply(cookislands_thresher_total[,1:1000], 1, sd)

#### shortfinmako sharks ####
#row 4 in mortality; need to change this for each species when drawing rates)
r_mako = 4

cookislands_mako <- cookislands_comb[,c(1:4,17,18)] #create dataframe

mako_cm <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako]) 
mako_prm <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako]) 

cookislands_mako_catches <- matrix(nrow=n, ncol=1000)
cookislands_mako_DOA <- matrix(nrow=n, ncol=1000)
cookislands_mako_released <- matrix(nrow=n, ncol=1000)
cookislands_mako_PRM<- matrix(nrow=n, ncol=1000)
cookislands_mako_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    cookislands_mako_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$mako.logcpue[j], cookislands_comb$mako.logcpue.se[j])
    cookislands_mako_DOA[j,i] <- cookislands_mako_catches[j,i] * mako_cm[i] 
    cookislands_mako_released[j,i] <- cookislands_mako_catches[j,i] - cookislands_mako_DOA[j,i]
    cookislands_mako_PRM[j,i] <- cookislands_mako_released[j,i] * mako_prm[i] 
    cookislands_mako_total[j,i] <- cookislands_mako_DOA[j,i] + cookislands_mako_PRM[j,i]
  }
}

#catches
cookislands_mako$catch <- rowMeans(cookislands_mako_catches[,1:1000])
cookislands_mako$catch.sd = apply(cookislands_mako_catches[,1:1000], 1, sd)

#DOA
cookislands_mako$DOA <- rowMeans(cookislands_mako_DOA[,1:1000])
cookislands_mako$DOA.sd = apply(cookislands_mako_DOA[,1:1000], 1, sd)

#prm
cookislands_mako$PRM = rowMeans(cookislands_mako_PRM[,1:1000])
cookislands_mako$PRM.sd = apply(cookislands_mako_PRM[,1:1000], 1, sd)

#total
cookislands_mako$total = rowMeans(cookislands_mako_total[,1:1000])
cookislands_mako$total.sd = apply(cookislands_mako_total[,1:1000], 1, sd)

#### oceanic whitetip sharks ####
#row 8 in mortality; need to change this for each species when drawing rates)
r_oceanicwhitetip = 8

cookislands_oceanicwhitetip <- cookislands_comb[,c(1:4,25,26)] #create dataframe
oceanicwhitetip_cm <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip]) 
oceanicwhitetip_prm <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip]) 

cookislands_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
cookislands_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
cookislands_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
cookislands_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
cookislands_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    cookislands_oceanicwhitetip_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$oceanicwhitetip.logcpue[j], cookislands_comb$oceanicwhitetip.logcpue.se[j])
    cookislands_oceanicwhitetip_DOA[j,i] <- cookislands_oceanicwhitetip_catches[j,i] * oceanicwhitetip_cm[i] 
    cookislands_oceanicwhitetip_released[j,i] <- cookislands_oceanicwhitetip_catches[j,i] - cookislands_oceanicwhitetip_DOA[j,i]
    cookislands_oceanicwhitetip_PRM[j,i] <- cookislands_oceanicwhitetip_released[j,i] * oceanicwhitetip_prm[i] 
    cookislands_oceanicwhitetip_total[j,i] <- cookislands_oceanicwhitetip_DOA[j,i] + cookislands_oceanicwhitetip_PRM[j,i]
  }
}

#catches
cookislands_oceanicwhitetip$catch <- rowMeans(cookislands_oceanicwhitetip_catches[,1:1000])
cookislands_oceanicwhitetip$catch.sd = apply(cookislands_oceanicwhitetip_catches[,1:1000], 1, sd)

#DOA
cookislands_oceanicwhitetip$DOA <- rowMeans(cookislands_oceanicwhitetip_DOA[,1:1000])
cookislands_oceanicwhitetip$DOA.sd = apply(cookislands_oceanicwhitetip_DOA[,1:1000], 1, sd)

#prm
cookislands_oceanicwhitetip$PRM = rowMeans(cookislands_oceanicwhitetip_PRM[,1:1000])
cookislands_oceanicwhitetip$PRM.sd = apply(cookislands_oceanicwhitetip_PRM[,1:1000], 1, sd)

#total
cookislands_oceanicwhitetip$total = rowMeans(cookislands_oceanicwhitetip_total[,1:1000])
cookislands_oceanicwhitetip$total.sd = apply(cookislands_oceanicwhitetip_total[,1:1000], 1, sd)

#### hammerhead sharks ####
#row 9 in mortality; need to change this for each species when drawing rates)
r_hammerhead = 9

cookislands_hammerhead <- cookislands_comb[,c(1:4,29,30)] #create dataframe

hammerhead_cm <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead]) 
hammerhead_prm <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead]) 

cookislands_hammerhead_catches <- matrix(nrow=n, ncol=1000)
cookislands_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
cookislands_hammerhead_released <- matrix(nrow=n, ncol=1000)
cookislands_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
cookislands_hammerhead_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    cookislands_hammerhead_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$hammerhead.logcpue[j], cookislands_comb$hammerhead.logcpue.se[j])
    cookislands_hammerhead_DOA[j,i] <- cookislands_hammerhead_catches[j,i] * hammerhead_cm[i] 
    cookislands_hammerhead_released[j,i] <- cookislands_hammerhead_catches[j,i] - cookislands_hammerhead_DOA[j,i]
    cookislands_hammerhead_PRM[j,i] <- cookislands_hammerhead_released[j,i] * hammerhead_prm[i] 
    cookislands_hammerhead_total[j,i] <- cookislands_hammerhead_DOA[j,i] + cookislands_hammerhead_PRM[j,i]
  }
}

#catches
cookislands_hammerhead$catch <- rowMeans(cookislands_hammerhead_catches[,1:1000])
cookislands_hammerhead$catch.sd = apply(cookislands_hammerhead_catches[,1:1000], 1, sd)

#DOA
cookislands_hammerhead$DOA <- rowMeans(cookislands_hammerhead_DOA[,1:1000])
cookislands_hammerhead$DOA.sd = apply(cookislands_hammerhead_DOA[,1:1000], 1, sd)

#prm
cookislands_hammerhead$PRM = rowMeans(cookislands_hammerhead_PRM[,1:1000])
cookislands_hammerhead$PRM.sd = apply(cookislands_hammerhead_PRM[,1:1000], 1, sd)

#total
cookislands_hammerhead$total = rowMeans(cookislands_hammerhead_total[,1:1000])
cookislands_hammerhead$total.sd = apply(cookislands_hammerhead_total[,1:1000], 1, sd)

#### othersharks sharks ####
#row 10 in mortality; need to change this for each species when drawing rates)
r_othersharks = 10

cookislands_othersharks <- cookislands_comb[,c(1:4,33,34)] #create dataframe

othersharks_cm <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks]) 
othersharks_prm <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks]) 

cookislands_othersharks_catches <- matrix(nrow=n, ncol=1000)
cookislands_othersharks_DOA <- matrix(nrow=n, ncol=1000)
cookislands_othersharks_released <- matrix(nrow=n, ncol=1000)
cookislands_othersharks_PRM<- matrix(nrow=n, ncol=1000)
cookislands_othersharks_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    cookislands_othersharks_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$othersharks.logcpue[j], cookislands_comb$othersharks.logcpue.se[j])
    cookislands_othersharks_DOA[j,i] <- cookislands_othersharks_catches[j,i] * othersharks_cm[i] 
    cookislands_othersharks_released[j,i] <- cookislands_othersharks_catches[j,i] - cookislands_othersharks_DOA[j,i]
    cookislands_othersharks_PRM[j,i] <- cookislands_othersharks_released[j,i] * othersharks_prm[i] 
    cookislands_othersharks_total[j,i] <- cookislands_othersharks_DOA[j,i] + cookislands_othersharks_PRM[j,i]
  }
}

#catches
cookislands_othersharks$catch <- rowMeans(cookislands_othersharks_catches[,1:1000])
cookislands_othersharks$catch.sd = apply(cookislands_othersharks_catches[,1:1000], 1, sd)

#DOA
cookislands_othersharks$DOA <- rowMeans(cookislands_othersharks_DOA[,1:1000])
cookislands_othersharks$DOA.sd = apply(cookislands_othersharks_DOA[,1:1000], 1, sd)

#prm
cookislands_othersharks$PRM = rowMeans(cookislands_othersharks_PRM[,1:1000])
cookislands_othersharks$PRM.sd = apply(cookislands_othersharks_PRM[,1:1000], 1, sd)

#total
cookislands_othersharks$total = rowMeans(cookislands_othersharks_total[,1:1000])
cookislands_othersharks$total.sd = apply(cookislands_othersharks_total[,1:1000], 1, sd)

#### cookislands Totals ####

cookislands_bsh$Species = "Blue Shark"
cookislands_silky$Species = "Silky Shark"
cookislands_thresher$Species="Thresher"
cookislands_mako$Species="Mako"
cookislands_oceanicwhitetip$Species="Oceanic Whitetip"
cookislands_hammerhead$Species="Hammerhead"
cookislands_othersharks$Species="Other Sharks"

colnames(cookislands_bsh)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(cookislands_silky)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(cookislands_thresher)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(cookislands_mako)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(cookislands_oceanicwhitetip)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(cookislands_hammerhead)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(cookislands_othersharks)[c(5,6)] <- c("logcpue","logcpue.se")

cookislands_total_preds <- rbind(cookislands_bsh,
                          cookislands_silky,
                          cookislands_thresher,
                          cookislands_mako,
                          cookislands_oceanicwhitetip,
                          cookislands_hammerhead,
                          cookislands_othersharks)

end.time <- Sys.time()
end.time - start.time}

write.csv(cookislands_total_preds, "cookislands_total_preds.csv", row.names = F)

#### summary ####
dat <- read.csv("cookislands_total_preds.csv")
cookislands_bsh <- subset(dat, Species == "Blue Shark")
cookislands_silky <- subset(dat, Species == "Silky Shark")
cookislands_thresher <- subset(dat, Species == "Thresher")
cookislands_mako <- subset(dat, Species == "Mako")
cookislands_oceanicwhitetip <- subset(dat, Species == "Oceanic Whitetip")
cookislands_hammerhead <- subset(dat, Species == "Hammerhead")
cookislands_othersharks <- subset(dat, Species == "Other Sharks")




cookislands_summary <- data.frame("Species" = c("Blue Shark", "Silky Shark", "Thresher", 
                                          "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"),
                            "Catch" = c(sum(cookislands_bsh$catch),
                                        sum(cookislands_silky$catch), 
                                        sum(cookislands_thresher$catch),
                                        sum(cookislands_mako$catch),
                                        sum(cookislands_oceanicwhitetip$catch),
                                        sum(cookislands_hammerhead$catch),
                                        sum(cookislands_othersharks$catch)),
                            "Catch.SD" = c(sqrt(sum(cookislands_bsh$catch.sd^2, na.rm=T)),
                                           sqrt(sum(cookislands_silky$catch.sd^2, na.rm=T)),
                                           sqrt(sum(cookislands_thresher$catch.sd^2, na.rm=T)),
                                           sqrt(sum(cookislands_mako$catch.sd^2, na.rm=T)),
                                           sqrt(sum(cookislands_oceanicwhitetip$catch.sd^2, na.rm=T)),
                                           sqrt(sum(cookislands_hammerhead$catch.sd^2, na.rm=T)),
                                           sqrt(sum(cookislands_othersharks$catch.sd^2, na.rm=T))),
                            "CM" = c(sum(cookislands_bsh$DOA),
                                     sum(cookislands_silky$DOA),
                                     sum(cookislands_thresher$DOA),
                                     sum(cookislands_mako$DOA),
                                     sum(cookislands_oceanicwhitetip$DOA),
                                     sum(cookislands_hammerhead$DOA),
                                     sum(cookislands_othersharks$DOA)),
                            "CM.sd"=c(sqrt(sum(cookislands_bsh$DOA.sd^2)),
                                      sqrt(sum(cookislands_silky$DOA.sd^2)),
                                      sqrt(sum(cookislands_thresher$DOA.sd^2)),
                                      sqrt(sum(cookislands_mako$DOA.sd^2)),
                                      sqrt(sum(cookislands_oceanicwhitetip$DOA.sd^2)),
                                      sqrt(sum(cookislands_hammerhead$DOA.sd^2)),
                                      sqrt(sum(cookislands_othersharks$DOA.sd^2))),
                            "PRM" = c(sum(cookislands_bsh$PRM),
                                      sum(cookislands_silky$PRM),
                                      sum(cookislands_thresher$PRM),
                                      sum(cookislands_mako$PRM),
                                      sum(cookislands_oceanicwhitetip$PRM), 
                                      sum(cookislands_hammerhead$PRM), 
                                      sum(cookislands_othersharks$PRM)),
                            "PRM.sd"=c(sqrt(sum(cookislands_bsh$PRM.sd^2)),
                                       sqrt(sum(cookislands_silky$PRM.sd^2)),
                                       sqrt(sum(cookislands_thresher$PRM.sd^2)),
                                       sqrt(sum(cookislands_mako$PRM.sd^2)),
                                       sqrt(sum(cookislands_oceanicwhitetip$PRM.sd^2)),
                                       sqrt(sum(cookislands_hammerhead$PRM.sd^2)),
                                       sqrt(sum(cookislands_othersharks$PRM.sd^2))),
                            "Total"=c(sum(cookislands_bsh$total),
                                      sum(cookislands_silky$total),
                                      sum(cookislands_thresher$total),
                                      sum(cookislands_mako$total),
                                      sum(cookislands_oceanicwhitetip$total),
                                      sum(cookislands_hammerhead$total),
                                      sum(cookislands_othersharks$total)),
                            "Total.sd"=c(sqrt(sum(cookislands_bsh$total.sd^2)),
                                         sqrt(sum(cookislands_silky$total.sd^2)),
                                         sqrt(sum(cookislands_thresher$total.sd^2)), 
                                         sqrt(sum(cookislands_mako$total.sd^2)),
                                         sqrt(sum(cookislands_oceanicwhitetip$total.sd^2)),
                                         sqrt(sum(cookislands_hammerhead$total.sd^2)), 
                                         sqrt(sum(cookislands_othersharks$total.sd^2))))


write.csv(cookislands_summary, "cookislands_summary.csv", row.names = F)

#plotting for Fig1
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
cookislands_mortality <- read.csv("cookislands_total_preds.csv")
cookislands_proj <- aggregate(data=cookislands_mortality, total ~ Lat + Long, FUN='sum')
dev.off()
world <- map_data("world")
cookislands_map <- ggplot(data=cookislands_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Bycatch Mortality \n(# of individuals)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = cok_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(-169,-153), breaks=seq(-165,-155,by=5)) +
  scale_y_continuous(limits=c(-27,-5), breaks=seq(-25,-10,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Cook Islands")
