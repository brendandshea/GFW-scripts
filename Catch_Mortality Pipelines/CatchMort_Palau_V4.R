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
palau_proj <- read.csv("palau2019.csv")

palau_proj2 <- palau_proj[,c(5,6,10,11)]
#rename
colnames(palau_proj2)[1] <- "Long"
colnames(palau_proj2)[2] <- "Lat"

#correct for out of domain
palau_proj2$LatMatch <- ifelse(palau_proj2$Lat<5,5,palau_proj2$Lat)
master_cpue$LatMatch <- master_cpue$Lat
palau_proj2$LongMatch <- ifelse(palau_proj2$Long<130,130,palau_proj2$Long)
master_cpue$LongMatch <- master_cpue$Long

#merge
palau_comb <- left_join(palau_proj2,master_cpue[,-c(2,32)],by=c("LatMatch",'LongMatch'))
palau_comb <- palau_comb %>% drop_na(log_hooks)

palau_comb <- palau_comb[,-c(5,6)]

n <- nrow(palau_comb)

palau_hook_preds <- matrix(nrow=n, ncol=1000)
for (j in 1:n) {
  for (i in 1:1000) {
    palau_hook_preds[j,i] <- rlnorm(1,palau_comb$log_hooks[j],palau_comb$log_hooks.se[j])
  }
}

{start.time <- Sys.time()
  #### blue sharks ####
  #row 2 in mortality; need to change this for each species when drawing rates)
  r_bsh = 2
  
  palau_bsh <- palau_comb[,c(1:4,9,10)]
  
  palau_bsh_catches <- matrix(nrow=n, ncol=1000)
  palau_bsh_DOA <- matrix(nrow=n, ncol=1000)
  palau_bsh_released <- matrix(nrow=n, ncol=1000)
  palau_bsh_PRM<- matrix(nrow=n, ncol=1000)
  palau_bsh_total <- matrix(nrow=n, ncol=1000)
  
  palau_bsh_CM_rate <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh])
  palau_bsh_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      palau_bsh_catches[j,i] <- palau_hook_preds[j,i]/1000 * rlnorm(1, palau_comb$blueshark.logcpue[j], palau_comb$blueshark.logcpue.se[j])
      palau_bsh_DOA[j,i] <- palau_bsh_catches[j,i] *  palau_bsh_CM_rate[i]
      palau_bsh_released[j,i] <- palau_bsh_catches[j,i] - palau_bsh_DOA[j,i]
      palau_bsh_PRM[j,i] <- palau_bsh_released[j,i] * palau_bsh_PRM_rate[i] 
      palau_bsh_total[j,i] <- palau_bsh_DOA[j,i] + palau_bsh_PRM[j,i]
    }
  }
  
  #run averages
  palau_bsh_proj <- data.frame("Species" = "Blue Shark",
                                     "catch" = NA,
                                     "catch.sd" = NA,
                                     "DOA" = NA,
                                     "DOA.sd" = NA,
                                     "PRM" = NA,
                                     "PRM.sd" = NA,
                                     "total" = NA,
                                     "total.sd" = NA)
  palau_bsh_total_catches <- colSums(palau_bsh_catches)
  palau_bsh_proj$catch <- mean(palau_bsh_total_catches)
  palau_bsh_proj$catch.sd <- sd(palau_bsh_total_catches)
  
  palau_bsh_total_DOA <- colSums(palau_bsh_DOA)
  palau_bsh_proj$DOA <- mean(palau_bsh_total_DOA)
  palau_bsh_proj$DOA.sd <- sd(palau_bsh_total_DOA)
  
  palau_bsh_total_PRM <- colSums(palau_bsh_PRM)
  palau_bsh_proj$PRM <- mean(palau_bsh_total_PRM)
  palau_bsh_proj$PRM.sd <- sd(palau_bsh_total_PRM)
  
  palau_bsh_total_total <- colSums(palau_bsh_total)
  palau_bsh_proj$total <- mean(palau_bsh_total_total)
  palau_bsh_proj$total.sd <- sd(palau_bsh_total_total)
  
  #cell averages
  #catches
  palau_bsh$catch <- rowMeans(palau_bsh_catches[,1:1000])
  palau_bsh$catch.sd = apply(palau_bsh_catches[,1:1000], 1, sd)
  
  #DOA
  palau_bsh$DOA <- rowMeans(palau_bsh_DOA[,1:1000])
  palau_bsh$DOA.sd = apply(palau_bsh_DOA[,1:1000], 1, sd)
  
  #prm
  palau_bsh$PRM = rowMeans(palau_bsh_PRM[,1:1000])
  palau_bsh$PRM.sd = apply(palau_bsh_PRM[,1:1000], 1, sd)
  
  #total
  palau_bsh$total = rowMeans(palau_bsh_total[,1:1000])
  palau_bsh$total.sd = apply(palau_bsh_total[,1:1000], 1, sd)
  
  #### silky sharks ####
  r_silky=5 #row 5 in mortality; need to change this for each species when drawing rates)
  
  palau_silky <- palau_comb[,c(1:4,13,14)] #create dataframe
  
  palau_silky_catches <- matrix(nrow=n, ncol=1000)
  palau_silky_DOA <- matrix(nrow=n, ncol=1000)
  palau_silky_released <- matrix(nrow=n, ncol=1000)
  palau_silky_PRM<- matrix(nrow=n, ncol=1000)
  palau_silky_total <- matrix(nrow=n, ncol=1000)
  
  palau_silky_CM_rate <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky])
  palau_silky_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      palau_silky_catches[j,i] <- palau_hook_preds[j,i]/1000 * rlnorm(1, palau_comb$silkyshark.logcpue[j], palau_comb$silkyshark.logcpue.se[j])
      palau_silky_DOA[j,i] <- palau_silky_catches[j,i] * palau_silky_CM_rate[i]
      palau_silky_released[j,i] <- palau_silky_catches[j,i] - palau_silky_DOA[j,i]
      palau_silky_PRM[j,i] <- palau_silky_released[j,i] * palau_silky_PRM_rate[i]
      palau_silky_total[j,i] <- palau_silky_DOA[j,i] + palau_silky_PRM[j,i]
    }
  }
  #run averages
  palau_silky_proj <- data.frame("Species" = "Silky Shark",
                                       "catch" = NA,
                                       "catch.sd" = NA,
                                       "DOA" = NA,
                                       "DOA.sd" = NA,
                                       "PRM" = NA,
                                       "PRM.sd" = NA,
                                       "total" = NA,
                                       "total.sd" = NA)
  
  palau_silky_total_catches <- colSums(palau_silky_catches)
  palau_silky_proj$catch <- mean(palau_silky_total_catches)
  palau_silky_proj$catch.sd <-sd(palau_silky_total_catches)
  
  palau_silky_total_DOA <- colSums(palau_silky_DOA)
  palau_silky_proj$DOA <- mean(palau_silky_total_DOA)
  palau_silky_proj$DOA.sd <- sd(palau_silky_total_DOA)
  
  palau_silky_total_PRM <- colSums(palau_silky_PRM)
  palau_silky_proj$PRM <- mean(palau_silky_total_PRM)
  palau_silky_proj$PRM.sd <- sd(palau_silky_total_PRM)
  
  palau_silky_total_total <- colSums(palau_silky_total)
  palau_silky_proj$total <- mean(palau_silky_total_total)
  palau_silky_proj$total.sd <- sd(palau_silky_total_total)
  
  #cell averages
  #catches
  palau_silky$catch <- rowMeans(palau_silky_catches[,1:1000])
  palau_silky$catch.sd = apply(palau_silky_catches[,1:1000], 1, sd)
  
  #DOA
  palau_silky$DOA <- rowMeans(palau_silky_DOA[,1:1000])
  palau_silky$DOA.sd = apply(palau_silky_DOA[,1:1000], 1, sd)
  
  #prm
  palau_silky$PRM = rowMeans(palau_silky_PRM[,1:1000])
  palau_silky$PRM.sd = apply(palau_silky_PRM[,1:1000], 1, sd)
  
  #total
  palau_silky$total = rowMeans(palau_silky_total[,1:1000])
  palau_silky$total.sd = apply(palau_silky_total[,1:1000], 1, sd)
  
  #### thresher sharks ####
  #row 1 in mortality; need to change this for each species when drawing rates)
  r_thresher = 1
  
  palau_thresher <- palau_comb[,c(1:4,21,22)] #create dataframe
  
  palau_thresher_catches <- matrix(nrow=n, ncol=1000)
  palau_thresher_DOA <- matrix(nrow=n, ncol=1000)
  palau_thresher_released <- matrix(nrow=n, ncol=1000)
  palau_thresher_PRM<- matrix(nrow=n, ncol=1000)
  palau_thresher_total <- matrix(nrow=n, ncol=1000)
  
  palau_thresher_CM_rate <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher])
  palau_thresher_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      palau_thresher_catches[j,i] <- palau_hook_preds[j,i]/1000 * rlnorm(1, palau_comb$thresher.logcpue[j], palau_comb$thresher.logcpue.se[j])
      palau_thresher_DOA[j,i] <- palau_thresher_catches[j,i] * palau_thresher_CM_rate[i]
      palau_thresher_released[j,i] <- palau_thresher_catches[j,i] - palau_thresher_DOA[j,i]
      palau_thresher_PRM[j,i] <- palau_thresher_released[j,i] * palau_thresher_PRM_rate[i]
      palau_thresher_total[j,i] <- palau_thresher_DOA[j,i] + palau_thresher_PRM[j,i]
    }
  }
  
  #run averages
  palau_thresher_proj <- data.frame("Species" = "Thresher",
                                          "catch" = NA,
                                          "catch.sd" = NA,
                                          "DOA" = NA,
                                          "DOA.sd" = NA,
                                          "PRM" = NA,
                                          "PRM.sd" = NA,
                                          "total" = NA,
                                          "total.sd" = NA)
  
  palau_thresher_total_catches <- colSums(palau_thresher_catches)
  palau_thresher_proj$catch <- mean(palau_thresher_total_catches)
  palau_thresher_proj$catch.sd <-sd(palau_thresher_total_catches)
  
  palau_thresher_total_DOA <- colSums(palau_thresher_DOA)
  palau_thresher_proj$DOA <- mean(palau_thresher_total_DOA)
  palau_thresher_proj$DOA.sd <- sd(palau_thresher_total_DOA)
  
  palau_thresher_total_PRM <- colSums(palau_thresher_PRM)
  palau_thresher_proj$PRM <- mean(palau_thresher_total_PRM)
  palau_thresher_proj$PRM.sd <- sd(palau_thresher_total_PRM)
  
  palau_thresher_total_total <- colSums(palau_thresher_total)
  palau_thresher_proj$total <- mean(palau_thresher_total_total)
  palau_thresher_proj$total.sd <- sd(palau_thresher_total_total)
  
  #cell averages
  #catches
  palau_thresher$catch <- rowMeans(palau_thresher_catches[,1:1000])
  palau_thresher$catch.sd = apply(palau_thresher_catches[,1:1000], 1, sd)
  
  #DOA
  palau_thresher$DOA <- rowMeans(palau_thresher_DOA[,1:1000])
  palau_thresher$DOA.sd = apply(palau_thresher_DOA[,1:1000], 1, sd)
  
  #prm
  palau_thresher$PRM = rowMeans(palau_thresher_PRM[,1:1000])
  palau_thresher$PRM.sd = apply(palau_thresher_PRM[,1:1000], 1, sd)
  
  #total
  palau_thresher$total = rowMeans(palau_thresher_total[,1:1000])
  palau_thresher$total.sd = apply(palau_thresher_total[,1:1000], 1, sd)
  
  #### shortfinmako sharks ####
  #row 4 in mortality; need to change this for each species when drawing rates)
  r_mako = 4
  
  palau_mako <- palau_comb[,c(1:4,17,18)] #create dataframe
  
  palau_mako_catches <- matrix(nrow=n, ncol=1000)
  palau_mako_DOA <- matrix(nrow=n, ncol=1000)
  palau_mako_released <- matrix(nrow=n, ncol=1000)
  palau_mako_PRM<- matrix(nrow=n, ncol=1000)
  palau_mako_total <- matrix(nrow=n, ncol=1000)
  
  palau_mako_CM_rate <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako])
  palau_mako_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      palau_mako_catches[j,i] <- palau_hook_preds[j,i]/1000 * rlnorm(1, palau_comb$mako.logcpue[j], palau_comb$mako.logcpue.se[j])
      palau_mako_DOA[j,i] <- palau_mako_catches[j,i] * palau_mako_CM_rate[i]
      palau_mako_released[j,i] <- palau_mako_catches[j,i] - palau_mako_DOA[j,i]
      palau_mako_PRM[j,i] <- palau_mako_released[j,i] * palau_mako_PRM_rate[i]
      palau_mako_total[j,i] <- palau_mako_DOA[j,i] + palau_mako_PRM[j,i]
    }
  }
  
  #run averages
  palau_mako_proj <- data.frame("Species" = "Mako",
                                      "catch" = NA,
                                      "catch.sd" = NA,
                                      "DOA" = NA,
                                      "DOA.sd" = NA,
                                      "PRM" = NA,
                                      "PRM.sd" = NA,
                                      "total" = NA,
                                      "total.sd" = NA)
  
  palau_mako_total_catches <- colSums(palau_mako_catches)
  palau_mako_proj$catch <- mean(palau_mako_total_catches)
  palau_mako_proj$catch.sd <-sd(palau_mako_total_catches)
  
  palau_mako_total_DOA <- colSums(palau_mako_DOA)
  palau_mako_proj$DOA <- mean(palau_mako_total_DOA)
  palau_mako_proj$DOA.sd <- sd(palau_mako_total_DOA)
  
  palau_mako_total_PRM <- colSums(palau_mako_PRM)
  palau_mako_proj$PRM <- mean(palau_mako_total_PRM)
  palau_mako_proj$PRM.sd <- sd(palau_mako_total_PRM)
  
  palau_mako_total_total <- colSums(palau_mako_total)
  palau_mako_proj$total <- mean(palau_mako_total_total)
  palau_mako_proj$total.sd <- sd(palau_mako_total_total)
  
  #cell averages
  #catches
  palau_mako$catch <- rowMeans(palau_mako_catches[,1:1000])
  palau_mako$catch.sd = apply(palau_mako_catches[,1:1000], 1, sd)
  
  #DOA
  palau_mako$DOA <- rowMeans(palau_mako_DOA[,1:1000])
  palau_mako$DOA.sd = apply(palau_mako_DOA[,1:1000], 1, sd)
  
  #prm
  palau_mako$PRM = rowMeans(palau_mako_PRM[,1:1000])
  palau_mako$PRM.sd = apply(palau_mako_PRM[,1:1000], 1, sd)
  
  #total
  palau_mako$total = rowMeans(palau_mako_total[,1:1000])
  palau_mako$total.sd = apply(palau_mako_total[,1:1000], 1, sd)
  
  #### oceanic whitetip sharks ####
  #row 8 in mortality; need to change this for each species when drawing rates)
  r_oceanicwhitetip = 8
  
  palau_oceanicwhitetip <- palau_comb[,c(1:4,25,26)] #create dataframe
  
  palau_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
  palau_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
  palau_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
  palau_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
  palau_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)
  
  palau_oceanicwhitetip_CM_rate <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip])
  palau_oceanicwhitetip_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      palau_oceanicwhitetip_catches[j,i] <- palau_hook_preds[j,i]/1000 * rlnorm(1, palau_comb$oceanicwhitetip.logcpue[j], palau_comb$oceanicwhitetip.logcpue.se[j])
      palau_oceanicwhitetip_DOA[j,i] <- palau_oceanicwhitetip_catches[j,i] * palau_oceanicwhitetip_CM_rate[i]
      palau_oceanicwhitetip_released[j,i] <- palau_oceanicwhitetip_catches[j,i] - palau_oceanicwhitetip_DOA[j,i]
      palau_oceanicwhitetip_PRM[j,i] <- palau_oceanicwhitetip_released[j,i] * palau_oceanicwhitetip_PRM_rate[i]
      palau_oceanicwhitetip_total[j,i] <- palau_oceanicwhitetip_DOA[j,i] + palau_oceanicwhitetip_PRM[j,i]
    }
  }
  
  #run averages
  palau_oceanicwhitetip_proj <- data.frame("Species" = "Oceanic Whitetip",
                                                 "catch" = NA,
                                                 "catch.sd" = NA,
                                                 "DOA" = NA,
                                                 "DOA.sd" = NA,
                                                 "PRM" = NA,
                                                 "PRM.sd" = NA,
                                                 "total" = NA,
                                                 "total.sd" = NA)
  
  palau_oceanicwhitetip_total_catches <- colSums(palau_oceanicwhitetip_catches)
  palau_oceanicwhitetip_proj$catch <- mean(palau_oceanicwhitetip_total_catches)
  palau_oceanicwhitetip_proj$catch.sd <-sd(palau_oceanicwhitetip_total_catches)
  
  palau_oceanicwhitetip_total_DOA <- colSums(palau_oceanicwhitetip_DOA)
  palau_oceanicwhitetip_proj$DOA <- mean(palau_oceanicwhitetip_total_DOA)
  palau_oceanicwhitetip_proj$DOA.sd <- sd(palau_oceanicwhitetip_total_DOA)
  
  palau_oceanicwhitetip_total_PRM <- colSums(palau_oceanicwhitetip_PRM)
  palau_oceanicwhitetip_proj$PRM <- mean(palau_oceanicwhitetip_total_PRM)
  palau_oceanicwhitetip_proj$PRM.sd <- sd(palau_oceanicwhitetip_total_PRM)
  
  palau_oceanicwhitetip_total_total <- colSums(palau_oceanicwhitetip_total)
  palau_oceanicwhitetip_proj$total <- mean(palau_oceanicwhitetip_total_total)
  palau_oceanicwhitetip_proj$total.sd <- sd(palau_oceanicwhitetip_total_total)
  
  #cell averages
  #catches
  palau_oceanicwhitetip$catch <- rowMeans(palau_oceanicwhitetip_catches[,1:1000])
  palau_oceanicwhitetip$catch.sd = apply(palau_oceanicwhitetip_catches[,1:1000], 1, sd)
  
  #DOA
  palau_oceanicwhitetip$DOA <- rowMeans(palau_oceanicwhitetip_DOA[,1:1000])
  palau_oceanicwhitetip$DOA.sd = apply(palau_oceanicwhitetip_DOA[,1:1000], 1, sd)
  
  #prm
  palau_oceanicwhitetip$PRM = rowMeans(palau_oceanicwhitetip_PRM[,1:1000])
  palau_oceanicwhitetip$PRM.sd = apply(palau_oceanicwhitetip_PRM[,1:1000], 1, sd)
  
  #total
  palau_oceanicwhitetip$total = rowMeans(palau_oceanicwhitetip_total[,1:1000])
  palau_oceanicwhitetip$total.sd = apply(palau_oceanicwhitetip_total[,1:1000], 1, sd)
  
  #### hammerhead sharks ####
  #row 9 in mortality; need to change this for each species when drawing rates)
  r_hammerhead = 9
  
  palau_hammerhead <- palau_comb[,c(1:4,29,30)] #create dataframe
  
  palau_hammerhead_catches <- matrix(nrow=n, ncol=1000)
  palau_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
  palau_hammerhead_released <- matrix(nrow=n, ncol=1000)
  palau_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
  palau_hammerhead_total <- matrix(nrow=n, ncol=1000)
  
  palau_hammerhead_CM_rate <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead])
  palau_hammerhead_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      palau_hammerhead_catches[j,i] <- palau_hook_preds[j,i]/1000 * rlnorm(1, palau_comb$hammerhead.logcpue[j], palau_comb$hammerhead.logcpue.se[j])
      palau_hammerhead_DOA[j,i] <- palau_hammerhead_catches[j,i] * palau_hammerhead_CM_rate[i]
      palau_hammerhead_released[j,i] <- palau_hammerhead_catches[j,i] - palau_hammerhead_DOA[j,i]
      palau_hammerhead_PRM[j,i] <- palau_hammerhead_released[j,i] * palau_hammerhead_PRM_rate[i]
      palau_hammerhead_total[j,i] <- palau_hammerhead_DOA[j,i] + palau_hammerhead_PRM[j,i]
    }
  }
  
  #run averages
  palau_hammerhead_proj <- data.frame("Species" = "Hammerhead",
                                            "catch" = NA,
                                            "catch.sd" = NA,
                                            "DOA" = NA,
                                            "DOA.sd" = NA,
                                            "PRM" = NA,
                                            "PRM.sd" = NA,
                                            "total" = NA,
                                            "total.sd" = NA)
  
  palau_hammerhead_total_catches <- colSums(palau_hammerhead_catches)
  palau_hammerhead_proj$catch <- mean(palau_hammerhead_total_catches)
  palau_hammerhead_proj$catch.sd <-sd(palau_hammerhead_total_catches)
  
  palau_hammerhead_total_DOA <- colSums(palau_hammerhead_DOA)
  palau_hammerhead_proj$DOA <- mean(palau_hammerhead_total_DOA)
  palau_hammerhead_proj$DOA.sd <- sd(palau_hammerhead_total_DOA)
  
  palau_hammerhead_total_PRM <- colSums(palau_hammerhead_PRM)
  palau_hammerhead_proj$PRM <- mean(palau_hammerhead_total_PRM)
  palau_hammerhead_proj$PRM.sd <- sd(palau_hammerhead_total_PRM)
  
  palau_hammerhead_total_total <- colSums(palau_hammerhead_total)
  palau_hammerhead_proj$total <- mean(palau_hammerhead_total_total)
  palau_hammerhead_proj$total.sd <- sd(palau_hammerhead_total_total)
  
  #cell averages
  #catches
  palau_hammerhead$catch <- rowMeans(palau_hammerhead_catches[,1:1000])
  palau_hammerhead$catch.sd = apply(palau_hammerhead_catches[,1:1000], 1, sd)
  
  #DOA
  palau_hammerhead$DOA <- rowMeans(palau_hammerhead_DOA[,1:1000])
  palau_hammerhead$DOA.sd = apply(palau_hammerhead_DOA[,1:1000], 1, sd)
  
  #prm
  palau_hammerhead$PRM = rowMeans(palau_hammerhead_PRM[,1:1000])
  palau_hammerhead$PRM.sd = apply(palau_hammerhead_PRM[,1:1000], 1, sd)
  
  #total
  palau_hammerhead$total = rowMeans(palau_hammerhead_total[,1:1000])
  palau_hammerhead$total.sd = apply(palau_hammerhead_total[,1:1000], 1, sd)
  
  #### othersharks sharks ####
  #row 10 in mortality; need to change this for each species when drawing rates)
  r_othersharks = 10
  
  palau_othersharks <- palau_comb[,c(1:4,33,34)] #create dataframe
  
  palau_othersharks_catches <- matrix(nrow=n, ncol=1000)
  palau_othersharks_DOA <- matrix(nrow=n, ncol=1000)
  palau_othersharks_released <- matrix(nrow=n, ncol=1000)
  palau_othersharks_PRM<- matrix(nrow=n, ncol=1000)
  palau_othersharks_total <- matrix(nrow=n, ncol=1000)
  
  palau_othersharks_CM_rate <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks])
  palau_othersharks_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      palau_othersharks_catches[j,i] <- palau_hook_preds[j,i]/1000 * rlnorm(1, palau_comb$othersharks.logcpue[j], palau_comb$othersharks.logcpue.se[j])
      palau_othersharks_DOA[j,i] <- palau_othersharks_catches[j,i] * palau_othersharks_CM_rate[i]
      palau_othersharks_released[j,i] <- palau_othersharks_catches[j,i] - palau_othersharks_DOA[j,i]
      palau_othersharks_PRM[j,i] <- palau_othersharks_released[j,i] * palau_othersharks_PRM_rate[i]
      palau_othersharks_total[j,i] <- palau_othersharks_DOA[j,i] + palau_othersharks_PRM[j,i]
    }
  }
  
  #run averages
  palau_othersharks_proj <- data.frame("Species" = "Other Sharks",
                                             "catch" = NA,
                                             "catch.sd" = NA,
                                             "DOA" = NA,
                                             "DOA.sd" = NA,
                                             "PRM" = NA,
                                             "PRM.sd" = NA,
                                             "total" = NA,
                                             "total.sd" = NA)
  
  palau_othersharks_total_catches <- colSums(palau_othersharks_catches)
  palau_othersharks_proj$catch <- mean(palau_othersharks_total_catches)
  palau_othersharks_proj$catch.sd <-sd(palau_othersharks_total_catches)
  
  palau_othersharks_total_DOA <- colSums(palau_othersharks_DOA)
  palau_othersharks_proj$DOA <- mean(palau_othersharks_total_DOA)
  palau_othersharks_proj$DOA.sd <- sd(palau_othersharks_total_DOA)
  
  palau_othersharks_total_PRM <- colSums(palau_othersharks_PRM)
  palau_othersharks_proj$PRM <- mean(palau_othersharks_total_PRM)
  palau_othersharks_proj$PRM.sd <- sd(palau_othersharks_total_PRM)
  
  palau_othersharks_total_total <- colSums(palau_othersharks_total)
  palau_othersharks_proj$total <- mean(palau_othersharks_total_total)
  palau_othersharks_proj$total.sd <- sd(palau_othersharks_total_total)
  
  #cell averages
  #catches
  palau_othersharks$catch <- rowMeans(palau_othersharks_catches[,1:1000])
  palau_othersharks$catch.sd = apply(palau_othersharks_catches[,1:1000], 1, sd)
  
  #DOA
  palau_othersharks$DOA <- rowMeans(palau_othersharks_DOA[,1:1000])
  palau_othersharks$DOA.sd = apply(palau_othersharks_DOA[,1:1000], 1, sd)
  
  #prm
  palau_othersharks$PRM = rowMeans(palau_othersharks_PRM[,1:1000])
  palau_othersharks$PRM.sd = apply(palau_othersharks_PRM[,1:1000], 1, sd)
  
  #total
  palau_othersharks$total = rowMeans(palau_othersharks_total[,1:1000])
  palau_othersharks$total.sd = apply(palau_othersharks_total[,1:1000], 1, sd)
  
  #### palau Totals ####
  
  palau_bsh$Species = "Blue Shark"
  palau_silky$Species = "Silky Shark"
  palau_thresher$Species="Thresher"
  palau_mako$Species="Mako"
  palau_oceanicwhitetip$Species="Oceanic Whitetip"
  palau_hammerhead$Species="Hammerhead"
  palau_othersharks$Species="Other Sharks"
  
  colnames(palau_bsh)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(palau_silky)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(palau_thresher)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(palau_mako)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(palau_oceanicwhitetip)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(palau_hammerhead)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(palau_othersharks)[c(5,6)] <- c("logcpue","logcpue.se")
  
  palau_total_preds <- rbind(palau_bsh,
                                   palau_silky,
                                   palau_thresher,
                                   palau_mako,
                                   palau_oceanicwhitetip,
                                   palau_hammerhead,
                                   palau_othersharks)
  
  end.time <- Sys.time()
  end.time - start.time}

write.csv(palau_total_preds, "palau_total_preds.csv", row.names = F)

#### summary ####
palau_summary <- rbind(palau_bsh_proj,
                             palau_silky_proj,
                             palau_thresher_proj,
                             palau_mako_proj,
                             palau_oceanicwhitetip_proj,
                             palau_hammerhead_proj,
                             palau_othersharks_proj)

write.csv(palau_summary, "palau_summary.csv", row.names = F)

#Mortality plotting for Fig1
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
palau_mortality <- read.csv("palau_total_preds.csv")
palau_proj <- aggregate(data=palau_mortality, total ~ Lat + Long, FUN='sum')
dev.off()
world <- map_data("world")
palau_map <- ggplot(data=palau_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Bycatch Mortality \n(# of individuals)", low="khaki1", high="red",
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
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Palau")

ggsave("palau_map.png", palau_map,
       height=6, width=8, dpi=300, bg='white')
