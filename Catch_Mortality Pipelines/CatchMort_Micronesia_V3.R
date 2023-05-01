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
micronesia_proj <- read.csv("micronesia2019.csv")

micronesia_proj2 <- micronesia_proj[,c(5,6,9,10)]
#rename
colnames(micronesia_proj2)[1] <- "Long"
colnames(micronesia_proj2)[2] <- "Lat"

#merge
micronesia_comb <- merge(micronesia_proj2, master_cpue, by=c("Lat", "Long"), all=T)
micronesia_comb <- micronesia_comb %>% drop_na(log_hooks)

n <- nrow(micronesia_comb)

micronesia_hook_preds <- matrix(nrow=n, ncol=1000)
for (j in 1:n) {
  for (i in 1:1000) {
    micronesia_hook_preds[j,i] <- rlnorm(1,micronesia_comb$log_hooks[j],micronesia_comb$log_hooks.se[j])
  }
}

{start.time <- Sys.time()
#### blue sharks ####
#row 2 in mortality; need to change this for each species when drawing rates)
r_bsh = 2

micronesia_bsh <- micronesia_comb[,c(1:4,9,10)]

bsh_cm <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh]) 
bsh_prm <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh]) 

micronesia_bsh_catches <- matrix(nrow=n, ncol=1000)
micronesia_bsh_DOA <- matrix(nrow=n, ncol=1000)
micronesia_bsh_released <- matrix(nrow=n, ncol=1000)
micronesia_bsh_PRM<- matrix(nrow=n, ncol=1000)
micronesia_bsh_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    micronesia_bsh_catches[j,i] <- micronesia_hook_preds[j,i]/1000 * rlnorm(1, micronesia_comb$blueshark.logcpue[j], micronesia_comb$blueshark.logcpue.se[j])
    micronesia_bsh_DOA[j,i] <- micronesia_bsh_catches[j,i] * bsh_cm[i] 
    micronesia_bsh_released[j,i] <- micronesia_bsh_catches[j,i] - micronesia_bsh_DOA[j,i]
    micronesia_bsh_PRM[j,i] <- micronesia_bsh_released[j,i] * bsh_prm[i] 
    micronesia_bsh_total[j,i] <- micronesia_bsh_DOA[j,i] + micronesia_bsh_PRM[j,i]
  }
}

#catches
micronesia_bsh$catch <- rowMeans(micronesia_bsh_catches[,1:1000])
micronesia_bsh$catch.sd = apply(micronesia_bsh_catches[,1:1000], 1, sd)

 #DOA
micronesia_bsh$DOA <- rowMeans(micronesia_bsh_DOA[,1:1000])
micronesia_bsh$DOA.sd = apply(micronesia_bsh_DOA[,1:1000], 1, sd)

#prm
micronesia_bsh$PRM = rowMeans(micronesia_bsh_PRM[,1:1000])
micronesia_bsh$PRM.sd = apply(micronesia_bsh_PRM[,1:1000], 1, sd)

#total
micronesia_bsh$total = rowMeans(micronesia_bsh_total[,1:1000])
micronesia_bsh$total.sd = apply(micronesia_bsh_total[,1:1000], 1, sd)

#### silky sharks ####
r_silky=5 #row 5 in mortality; need to change this for each species when drawing rates)

micronesia_silky <- micronesia_comb[,c(1:4,13,14)] #create dataframe

silky_cm <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky]) 
silky_prm <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky]) 

micronesia_silky_catches <- matrix(nrow=n, ncol=1000)
micronesia_silky_DOA <- matrix(nrow=n, ncol=1000)
micronesia_silky_released <- matrix(nrow=n, ncol=1000)
micronesia_silky_PRM<- matrix(nrow=n, ncol=1000)
micronesia_silky_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    micronesia_silky_catches[j,i] <- micronesia_hook_preds[j,i]/1000 * rlnorm(1, micronesia_comb$silkyshark.logcpue[j], micronesia_comb$silkyshark.logcpue.se[j])
    micronesia_silky_DOA[j,i] <- micronesia_silky_catches[j,i] * silky_cm[i] 
    micronesia_silky_released[j,i] <- micronesia_silky_catches[j,i] - micronesia_silky_DOA[j,i]
    micronesia_silky_PRM[j,i] <- micronesia_silky_released[j,i] * silky_prm[i] 
    micronesia_silky_total[j,i] <- micronesia_silky_DOA[j,i] + micronesia_silky_PRM[j,i]
  }
}

#catches
micronesia_silky$catch <- rowMeans(micronesia_silky_catches[,1:1000])
micronesia_silky$catch.sd = apply(micronesia_silky_catches[,1:1000], 1, sd)

#DOA
micronesia_silky$DOA <- rowMeans(micronesia_silky_DOA[,1:1000])
micronesia_silky$DOA.sd = apply(micronesia_silky_DOA[,1:1000], 1, sd)

#prm
micronesia_silky$PRM = rowMeans(micronesia_silky_PRM[,1:1000])
micronesia_silky$PRM.sd = apply(micronesia_silky_PRM[,1:1000], 1, sd)

#total
micronesia_silky$total = rowMeans(micronesia_silky_total[,1:1000])
micronesia_silky$total.sd = apply(micronesia_silky_total[,1:1000], 1, sd)

#### thresher sharks ####
#row 1 in mortality; need to change this for each species when drawing rates)
r_thresher = 1

micronesia_thresher <- micronesia_comb[,c(1:4,21,22)] #create dataframe

thresher_cm <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher]) 
thresher_prm <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher]) 

micronesia_thresher_catches <- matrix(nrow=n, ncol=1000)
micronesia_thresher_DOA <- matrix(nrow=n, ncol=1000)
micronesia_thresher_released <- matrix(nrow=n, ncol=1000)
micronesia_thresher_PRM<- matrix(nrow=n, ncol=1000)
micronesia_thresher_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    micronesia_thresher_catches[j,i] <- micronesia_hook_preds[j,i]/1000 * rlnorm(1, micronesia_comb$thresher.logcpue[j], micronesia_comb$thresher.logcpue.se[j])
    micronesia_thresher_DOA[j,i] <- micronesia_thresher_catches[j,i] * thresher_cm[i] 
    micronesia_thresher_released[j,i] <- micronesia_thresher_catches[j,i] - micronesia_thresher_DOA[j,i]
    micronesia_thresher_PRM[j,i] <- micronesia_thresher_released[j,i] * thresher_prm[i] 
    micronesia_thresher_total[j,i] <- micronesia_thresher_DOA[j,i] + micronesia_thresher_PRM[j,i]
  }
}

#catches
micronesia_thresher$catch <- rowMeans(micronesia_thresher_catches[,1:1000])
micronesia_thresher$catch.sd = apply(micronesia_thresher_catches[,1:1000], 1, sd)

#DOA
micronesia_thresher$DOA <- rowMeans(micronesia_thresher_DOA[,1:1000])
micronesia_thresher$DOA.sd = apply(micronesia_thresher_DOA[,1:1000], 1, sd)

#prm
micronesia_thresher$PRM = rowMeans(micronesia_thresher_PRM[,1:1000])
micronesia_thresher$PRM.sd = apply(micronesia_thresher_PRM[,1:1000], 1, sd)

#total
micronesia_thresher$total = rowMeans(micronesia_thresher_total[,1:1000])
micronesia_thresher$total.sd = apply(micronesia_thresher_total[,1:1000], 1, sd)

#### shortfinmako sharks ####
#row 4 in mortality; need to change this for each species when drawing rates)
r_mako = 4

micronesia_mako <- micronesia_comb[,c(1:4,17,18)] #create dataframe

mako_cm <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako]) 
mako_prm <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako]) 

micronesia_mako_catches <- matrix(nrow=n, ncol=1000)
micronesia_mako_DOA <- matrix(nrow=n, ncol=1000)
micronesia_mako_released <- matrix(nrow=n, ncol=1000)
micronesia_mako_PRM<- matrix(nrow=n, ncol=1000)
micronesia_mako_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    micronesia_mako_catches[j,i] <- micronesia_hook_preds[j,i]/1000 * rlnorm(1, micronesia_comb$mako.logcpue[j], micronesia_comb$mako.logcpue.se[j])
    micronesia_mako_DOA[j,i] <- micronesia_mako_catches[j,i] * mako_cm[i] 
    micronesia_mako_released[j,i] <- micronesia_mako_catches[j,i] - micronesia_mako_DOA[j,i]
    micronesia_mako_PRM[j,i] <- micronesia_mako_released[j,i] * mako_prm[i] 
    micronesia_mako_total[j,i] <- micronesia_mako_DOA[j,i] + micronesia_mako_PRM[j,i]
  }
}

#catches
micronesia_mako$catch <- rowMeans(micronesia_mako_catches[,1:1000])
micronesia_mako$catch.sd = apply(micronesia_mako_catches[,1:1000], 1, sd)

#DOA
micronesia_mako$DOA <- rowMeans(micronesia_mako_DOA[,1:1000])
micronesia_mako$DOA.sd = apply(micronesia_mako_DOA[,1:1000], 1, sd)

#prm
micronesia_mako$PRM = rowMeans(micronesia_mako_PRM[,1:1000])
micronesia_mako$PRM.sd = apply(micronesia_mako_PRM[,1:1000], 1, sd)

#total
micronesia_mako$total = rowMeans(micronesia_mako_total[,1:1000])
micronesia_mako$total.sd = apply(micronesia_mako_total[,1:1000], 1, sd)

#### oceanic whitetip sharks ####
#row 8 in mortality; need to change this for each species when drawing rates)
r_oceanicwhitetip = 8

micronesia_oceanicwhitetip <- micronesia_comb[,c(1:4,25,26)] #create dataframe
oceanicwhitetip_cm <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip]) 
oceanicwhitetip_prm <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip]) 

micronesia_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
micronesia_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
micronesia_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
micronesia_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
micronesia_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    micronesia_oceanicwhitetip_catches[j,i] <- micronesia_hook_preds[j,i]/1000 * rlnorm(1, micronesia_comb$oceanicwhitetip.logcpue[j], micronesia_comb$oceanicwhitetip.logcpue.se[j])
    micronesia_oceanicwhitetip_DOA[j,i] <- micronesia_oceanicwhitetip_catches[j,i] * oceanicwhitetip_cm[i] 
    micronesia_oceanicwhitetip_released[j,i] <- micronesia_oceanicwhitetip_catches[j,i] - micronesia_oceanicwhitetip_DOA[j,i]
    micronesia_oceanicwhitetip_PRM[j,i] <- micronesia_oceanicwhitetip_released[j,i] * oceanicwhitetip_prm[i] 
    micronesia_oceanicwhitetip_total[j,i] <- micronesia_oceanicwhitetip_DOA[j,i] + micronesia_oceanicwhitetip_PRM[j,i]
  }
}

#catches
micronesia_oceanicwhitetip$catch <- rowMeans(micronesia_oceanicwhitetip_catches[,1:1000])
micronesia_oceanicwhitetip$catch.sd = apply(micronesia_oceanicwhitetip_catches[,1:1000], 1, sd)

#DOA
micronesia_oceanicwhitetip$DOA <- rowMeans(micronesia_oceanicwhitetip_DOA[,1:1000])
micronesia_oceanicwhitetip$DOA.sd = apply(micronesia_oceanicwhitetip_DOA[,1:1000], 1, sd)

#prm
micronesia_oceanicwhitetip$PRM = rowMeans(micronesia_oceanicwhitetip_PRM[,1:1000])
micronesia_oceanicwhitetip$PRM.sd = apply(micronesia_oceanicwhitetip_PRM[,1:1000], 1, sd)

#total
micronesia_oceanicwhitetip$total = rowMeans(micronesia_oceanicwhitetip_total[,1:1000])
micronesia_oceanicwhitetip$total.sd = apply(micronesia_oceanicwhitetip_total[,1:1000], 1, sd)

#### hammerhead sharks ####
#row 9 in mortality; need to change this for each species when drawing rates)
r_hammerhead = 9

micronesia_hammerhead <- micronesia_comb[,c(1:4,29,30)] #create dataframe

hammerhead_cm <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead]) 
hammerhead_prm <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead]) 

micronesia_hammerhead_catches <- matrix(nrow=n, ncol=1000)
micronesia_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
micronesia_hammerhead_released <- matrix(nrow=n, ncol=1000)
micronesia_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
micronesia_hammerhead_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    micronesia_hammerhead_catches[j,i] <- micronesia_hook_preds[j,i]/1000 * rlnorm(1, micronesia_comb$hammerhead.logcpue[j], micronesia_comb$hammerhead.logcpue.se[j])
    micronesia_hammerhead_DOA[j,i] <- micronesia_hammerhead_catches[j,i] * hammerhead_cm[i] 
    micronesia_hammerhead_released[j,i] <- micronesia_hammerhead_catches[j,i] - micronesia_hammerhead_DOA[j,i]
    micronesia_hammerhead_PRM[j,i] <- micronesia_hammerhead_released[j,i] * hammerhead_prm[i] 
    micronesia_hammerhead_total[j,i] <- micronesia_hammerhead_DOA[j,i] + micronesia_hammerhead_PRM[j,i]
  }
}

#catches
micronesia_hammerhead$catch <- rowMeans(micronesia_hammerhead_catches[,1:1000])
micronesia_hammerhead$catch.sd = apply(micronesia_hammerhead_catches[,1:1000], 1, sd)

#DOA
micronesia_hammerhead$DOA <- rowMeans(micronesia_hammerhead_DOA[,1:1000])
micronesia_hammerhead$DOA.sd = apply(micronesia_hammerhead_DOA[,1:1000], 1, sd)

#prm
micronesia_hammerhead$PRM = rowMeans(micronesia_hammerhead_PRM[,1:1000])
micronesia_hammerhead$PRM.sd = apply(micronesia_hammerhead_PRM[,1:1000], 1, sd)

#total
micronesia_hammerhead$total = rowMeans(micronesia_hammerhead_total[,1:1000])
micronesia_hammerhead$total.sd = apply(micronesia_hammerhead_total[,1:1000], 1, sd)

#### othersharks sharks ####
#row 10 in mortality; need to change this for each species when drawing rates)
r_othersharks = 10

micronesia_othersharks <- micronesia_comb[,c(1:4,33,34)] #create dataframe

othersharks_cm <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks]) 
othersharks_prm <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks]) 

micronesia_othersharks_catches <- matrix(nrow=n, ncol=1000)
micronesia_othersharks_DOA <- matrix(nrow=n, ncol=1000)
micronesia_othersharks_released <- matrix(nrow=n, ncol=1000)
micronesia_othersharks_PRM<- matrix(nrow=n, ncol=1000)
micronesia_othersharks_total <- matrix(nrow=n, ncol=1000)

for (j in 1:n) {
  for (i in 1:1000) {
    micronesia_othersharks_catches[j,i] <- micronesia_hook_preds[j,i]/1000 * rlnorm(1, micronesia_comb$othersharks.logcpue[j], micronesia_comb$othersharks.logcpue.se[j])
    micronesia_othersharks_DOA[j,i] <- micronesia_othersharks_catches[j,i] * othersharks_cm[i] 
    micronesia_othersharks_released[j,i] <- micronesia_othersharks_catches[j,i] - micronesia_othersharks_DOA[j,i]
    micronesia_othersharks_PRM[j,i] <- micronesia_othersharks_released[j,i] * othersharks_prm[i] 
    micronesia_othersharks_total[j,i] <- micronesia_othersharks_DOA[j,i] + micronesia_othersharks_PRM[j,i]
  }
}

#catches
micronesia_othersharks$catch <- rowMeans(micronesia_othersharks_catches[,1:1000])
micronesia_othersharks$catch.sd = apply(micronesia_othersharks_catches[,1:1000], 1, sd)

#DOA
micronesia_othersharks$DOA <- rowMeans(micronesia_othersharks_DOA[,1:1000])
micronesia_othersharks$DOA.sd = apply(micronesia_othersharks_DOA[,1:1000], 1, sd)

#prm
micronesia_othersharks$PRM = rowMeans(micronesia_othersharks_PRM[,1:1000])
micronesia_othersharks$PRM.sd = apply(micronesia_othersharks_PRM[,1:1000], 1, sd)

#total
micronesia_othersharks$total = rowMeans(micronesia_othersharks_total[,1:1000])
micronesia_othersharks$total.sd = apply(micronesia_othersharks_total[,1:1000], 1, sd)

#### micronesia Totals ####

micronesia_bsh$Species = "Blue Shark"
micronesia_silky$Species = "Silky Shark"
micronesia_thresher$Species="Thresher"
micronesia_mako$Species="Mako"
micronesia_oceanicwhitetip$Species="Oceanic Whitetip"
micronesia_hammerhead$Species="Hammerhead"
micronesia_othersharks$Species="Other Sharks"

colnames(micronesia_bsh)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(micronesia_silky)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(micronesia_thresher)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(micronesia_mako)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(micronesia_oceanicwhitetip)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(micronesia_hammerhead)[c(5,6)] <- c("logcpue","logcpue.se")
colnames(micronesia_othersharks)[c(5,6)] <- c("logcpue","logcpue.se")

micronesia_total_preds <- rbind(micronesia_bsh,
                          micronesia_silky,
                          micronesia_thresher,
                          micronesia_mako,
                          micronesia_oceanicwhitetip,
                          micronesia_hammerhead,
                          micronesia_othersharks)

end.time <- Sys.time()
end.time - start.time}

write.csv(micronesia_total_preds, "micronesia_total_preds.csv", row.names = F)

#### summary ####
dat <- read.csv("micronesia_total_preds.csv")
micronesia_bsh <- subset(dat, Species == "Blue Shark")
micronesia_silky <- subset(dat, Species == "Silky Shark")
micronesia_thresher <- subset(dat, Species == "Thresher")
micronesia_mako <- subset(dat, Species == "Mako")
micronesia_oceanicwhitetip <- subset(dat, Species == "Oceanic Whitetip")
micronesia_hammerhead <- subset(dat, Species == "Hammerhead")
micronesia_othersharks <- subset(dat, Species == "Other Sharks")




micronesia_summary <- data.frame("Species" = c("Blue Shark", "Silky Shark", "Thresher", 
                                          "Mako", "Oceanic Whitetip", "Hammerhead", "Other Sharks"),
                            "Catch" = c(sum(micronesia_bsh$catch),
                                        sum(micronesia_silky$catch), 
                                        sum(micronesia_thresher$catch),
                                        sum(micronesia_mako$catch),
                                        sum(micronesia_oceanicwhitetip$catch),
                                        sum(micronesia_hammerhead$catch),
                                        sum(micronesia_othersharks$catch)),
                            "Catch.SD" = c(sqrt(sum(micronesia_bsh$catch.sd^2, na.rm=T)),
                                           sqrt(sum(micronesia_silky$catch.sd^2, na.rm=T)),
                                           sqrt(sum(micronesia_thresher$catch.sd^2, na.rm=T)),
                                           sqrt(sum(micronesia_mako$catch.sd^2, na.rm=T)),
                                           sqrt(sum(micronesia_oceanicwhitetip$catch.sd^2, na.rm=T)),
                                           sqrt(sum(micronesia_hammerhead$catch.sd^2, na.rm=T)),
                                           sqrt(sum(micronesia_othersharks$catch.sd^2, na.rm=T))),
                            "CM" = c(sum(micronesia_bsh$DOA),
                                     sum(micronesia_silky$DOA),
                                     sum(micronesia_thresher$DOA),
                                     sum(micronesia_mako$DOA),
                                     sum(micronesia_oceanicwhitetip$DOA),
                                     sum(micronesia_hammerhead$DOA),
                                     sum(micronesia_othersharks$DOA)),
                            "CM.sd"=c(sqrt(sum(micronesia_bsh$DOA.sd^2)),
                                      sqrt(sum(micronesia_silky$DOA.sd^2)),
                                      sqrt(sum(micronesia_thresher$DOA.sd^2)),
                                      sqrt(sum(micronesia_mako$DOA.sd^2)),
                                      sqrt(sum(micronesia_oceanicwhitetip$DOA.sd^2)),
                                      sqrt(sum(micronesia_hammerhead$DOA.sd^2)),
                                      sqrt(sum(micronesia_othersharks$DOA.sd^2))),
                            "PRM" = c(sum(micronesia_bsh$PRM),
                                      sum(micronesia_silky$PRM),
                                      sum(micronesia_thresher$PRM),
                                      sum(micronesia_mako$PRM),
                                      sum(micronesia_oceanicwhitetip$PRM), 
                                      sum(micronesia_hammerhead$PRM), 
                                      sum(micronesia_othersharks$PRM)),
                            "PRM.sd"=c(sqrt(sum(micronesia_bsh$PRM.sd^2)),
                                       sqrt(sum(micronesia_silky$PRM.sd^2)),
                                       sqrt(sum(micronesia_thresher$PRM.sd^2)),
                                       sqrt(sum(micronesia_mako$PRM.sd^2)),
                                       sqrt(sum(micronesia_oceanicwhitetip$PRM.sd^2)),
                                       sqrt(sum(micronesia_hammerhead$PRM.sd^2)),
                                       sqrt(sum(micronesia_othersharks$PRM.sd^2))),
                            "Total"=c(sum(micronesia_bsh$total),
                                      sum(micronesia_silky$total),
                                      sum(micronesia_thresher$total),
                                      sum(micronesia_mako$total),
                                      sum(micronesia_oceanicwhitetip$total),
                                      sum(micronesia_hammerhead$total),
                                      sum(micronesia_othersharks$total)),
                            "Total.sd"=c(sqrt(sum(micronesia_bsh$total.sd^2)),
                                         sqrt(sum(micronesia_silky$total.sd^2)),
                                         sqrt(sum(micronesia_thresher$total.sd^2)), 
                                         sqrt(sum(micronesia_mako$total.sd^2)),
                                         sqrt(sum(micronesia_oceanicwhitetip$total.sd^2)),
                                         sqrt(sum(micronesia_hammerhead$total.sd^2)), 
                                         sqrt(sum(micronesia_othersharks$total.sd^2))))


write.csv(micronesia_summary, "micronesia_summary.csv", row.names = F)

#Plotting gor fig1
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
micronesia_mortality <- read.csv("micronesia_total_preds.csv")
micronesia_proj <- aggregate(data=micronesia_mortality, total ~ Lat + Long, FUN='sum')
dev.off()
world <- map_data("world")
micronesia_map <- ggplot(data=micronesia_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Bycatch Mortality \n(# of individuals)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = fsm_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(134,167), breaks=seq(135,165,by=5)) +
  scale_y_continuous(limits=c(-3,14.5), breaks=seq(0,10,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Federated States of Micronesia")

