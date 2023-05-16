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
samoa_proj <- read.csv("samoa2019.csv")

samoa_proj2 <- samoa_proj[,c(5,6,9,10)]
#rename
colnames(samoa_proj2)[1] <- "Long"
colnames(samoa_proj2)[2] <- "Lat"

#merge
samoa_comb <- merge(samoa_proj2, master_cpue, by=c("Lat", "Long"), all=T)
samoa_comb <- samoa_comb %>% drop_na(log_hooks)

n <- nrow(samoa_comb)

samoa_hook_preds <- matrix(nrow=n, ncol=1000)
for (j in 1:n) {
  for (i in 1:1000) {
    samoa_hook_preds[j,i] <- rlnorm(1,samoa_comb$log_hooks[j],samoa_comb$log_hooks.se[j])
  }
}

{start.time <- Sys.time()
  #### blue sharks ####
  #row 2 in mortality; need to change this for each species when drawing rates)
  r_bsh = 2
  
  samoa_bsh <- samoa_comb[,c(1:4,9,10)]
  
  samoa_bsh_catches <- matrix(nrow=n, ncol=1000)
  samoa_bsh_DOA <- matrix(nrow=n, ncol=1000)
  samoa_bsh_released <- matrix(nrow=n, ncol=1000)
  samoa_bsh_PRM<- matrix(nrow=n, ncol=1000)
  samoa_bsh_total <- matrix(nrow=n, ncol=1000)
  
  samoa_bsh_CM_rate <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh])
  samoa_bsh_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      samoa_bsh_catches[j,i] <- samoa_hook_preds[j,i]/1000 * rlnorm(1, samoa_comb$blueshark.logcpue[j], samoa_comb$blueshark.logcpue.se[j])
      samoa_bsh_DOA[j,i] <- samoa_bsh_catches[j,i] *  samoa_bsh_CM_rate[i]
      samoa_bsh_released[j,i] <- samoa_bsh_catches[j,i] - samoa_bsh_DOA[j,i]
      samoa_bsh_PRM[j,i] <- samoa_bsh_released[j,i] * samoa_bsh_PRM_rate[i] 
      samoa_bsh_total[j,i] <- samoa_bsh_DOA[j,i] + samoa_bsh_PRM[j,i]
    }
  }
  
  #run averages
  samoa_bsh_proj <- data.frame("Species" = "Blue Shark",
                               "catch" = NA,
                               "catch.sd" = NA,
                               "DOA" = NA,
                               "DOA.sd" = NA,
                               "PRM" = NA,
                               "PRM.sd" = NA,
                               "total" = NA,
                               "total.sd" = NA)
  samoa_bsh_total_catches <- colSums(samoa_bsh_catches)
  samoa_bsh_proj$catch <- mean(samoa_bsh_total_catches)
  samoa_bsh_proj$catch.sd <- sd(samoa_bsh_total_catches)
  
  samoa_bsh_total_DOA <- colSums(samoa_bsh_DOA)
  samoa_bsh_proj$DOA <- mean(samoa_bsh_total_DOA)
  samoa_bsh_proj$DOA.sd <- sd(samoa_bsh_total_DOA)
  
  samoa_bsh_total_PRM <- colSums(samoa_bsh_PRM)
  samoa_bsh_proj$PRM <- mean(samoa_bsh_total_PRM)
  samoa_bsh_proj$PRM.sd <- sd(samoa_bsh_total_PRM)
  
  samoa_bsh_total_total <- colSums(samoa_bsh_total)
  samoa_bsh_proj$total <- mean(samoa_bsh_total_total)
  samoa_bsh_proj$total.sd <- sd(samoa_bsh_total_total)
  
  #cell averages
  #catches
  samoa_bsh$catch <- rowMeans(samoa_bsh_catches[,1:1000])
  samoa_bsh$catch.sd = apply(samoa_bsh_catches[,1:1000], 1, sd)
  
  #DOA
  samoa_bsh$DOA <- rowMeans(samoa_bsh_DOA[,1:1000])
  samoa_bsh$DOA.sd = apply(samoa_bsh_DOA[,1:1000], 1, sd)
  
  #prm
  samoa_bsh$PRM = rowMeans(samoa_bsh_PRM[,1:1000])
  samoa_bsh$PRM.sd = apply(samoa_bsh_PRM[,1:1000], 1, sd)
  
  #total
  samoa_bsh$total = rowMeans(samoa_bsh_total[,1:1000])
  samoa_bsh$total.sd = apply(samoa_bsh_total[,1:1000], 1, sd)
  
  #### silky sharks ####
  r_silky=5 #row 5 in mortality; need to change this for each species when drawing rates)
  
  samoa_silky <- samoa_comb[,c(1:4,13,14)] #create dataframe
  
  samoa_silky_catches <- matrix(nrow=n, ncol=1000)
  samoa_silky_DOA <- matrix(nrow=n, ncol=1000)
  samoa_silky_released <- matrix(nrow=n, ncol=1000)
  samoa_silky_PRM<- matrix(nrow=n, ncol=1000)
  samoa_silky_total <- matrix(nrow=n, ncol=1000)
  
  samoa_silky_CM_rate <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky])
  samoa_silky_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      samoa_silky_catches[j,i] <- samoa_hook_preds[j,i]/1000 * rlnorm(1, samoa_comb$silkyshark.logcpue[j], samoa_comb$silkyshark.logcpue.se[j])
      samoa_silky_DOA[j,i] <- samoa_silky_catches[j,i] * samoa_silky_CM_rate[i]
      samoa_silky_released[j,i] <- samoa_silky_catches[j,i] - samoa_silky_DOA[j,i]
      samoa_silky_PRM[j,i] <- samoa_silky_released[j,i] * samoa_silky_PRM_rate[i]
      samoa_silky_total[j,i] <- samoa_silky_DOA[j,i] + samoa_silky_PRM[j,i]
    }
  }
  #run averages
  samoa_silky_proj <- data.frame("Species" = "Silky Shark",
                                 "catch" = NA,
                                 "catch.sd" = NA,
                                 "DOA" = NA,
                                 "DOA.sd" = NA,
                                 "PRM" = NA,
                                 "PRM.sd" = NA,
                                 "total" = NA,
                                 "total.sd" = NA)
  
  samoa_silky_total_catches <- colSums(samoa_silky_catches)
  samoa_silky_proj$catch <- mean(samoa_silky_total_catches)
  samoa_silky_proj$catch.sd <-sd(samoa_silky_total_catches)
  
  samoa_silky_total_DOA <- colSums(samoa_silky_DOA)
  samoa_silky_proj$DOA <- mean(samoa_silky_total_DOA)
  samoa_silky_proj$DOA.sd <- sd(samoa_silky_total_DOA)
  
  samoa_silky_total_PRM <- colSums(samoa_silky_PRM)
  samoa_silky_proj$PRM <- mean(samoa_silky_total_PRM)
  samoa_silky_proj$PRM.sd <- sd(samoa_silky_total_PRM)
  
  samoa_silky_total_total <- colSums(samoa_silky_total)
  samoa_silky_proj$total <- mean(samoa_silky_total_total)
  samoa_silky_proj$total.sd <- sd(samoa_silky_total_total)
  
  #cell averages
  #catches
  samoa_silky$catch <- rowMeans(samoa_silky_catches[,1:1000])
  samoa_silky$catch.sd = apply(samoa_silky_catches[,1:1000], 1, sd)
  
  #DOA
  samoa_silky$DOA <- rowMeans(samoa_silky_DOA[,1:1000])
  samoa_silky$DOA.sd = apply(samoa_silky_DOA[,1:1000], 1, sd)
  
  #prm
  samoa_silky$PRM = rowMeans(samoa_silky_PRM[,1:1000])
  samoa_silky$PRM.sd = apply(samoa_silky_PRM[,1:1000], 1, sd)
  
  #total
  samoa_silky$total = rowMeans(samoa_silky_total[,1:1000])
  samoa_silky$total.sd = apply(samoa_silky_total[,1:1000], 1, sd)
  
  #### thresher sharks ####
  #row 1 in mortality; need to change this for each species when drawing rates)
  r_thresher = 1
  
  samoa_thresher <- samoa_comb[,c(1:4,21,22)] #create dataframe
  
  samoa_thresher_catches <- matrix(nrow=n, ncol=1000)
  samoa_thresher_DOA <- matrix(nrow=n, ncol=1000)
  samoa_thresher_released <- matrix(nrow=n, ncol=1000)
  samoa_thresher_PRM<- matrix(nrow=n, ncol=1000)
  samoa_thresher_total <- matrix(nrow=n, ncol=1000)
  
  samoa_thresher_CM_rate <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher])
  samoa_thresher_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      samoa_thresher_catches[j,i] <- samoa_hook_preds[j,i]/1000 * rlnorm(1, samoa_comb$thresher.logcpue[j], samoa_comb$thresher.logcpue.se[j])
      samoa_thresher_DOA[j,i] <- samoa_thresher_catches[j,i] * samoa_thresher_CM_rate[i]
      samoa_thresher_released[j,i] <- samoa_thresher_catches[j,i] - samoa_thresher_DOA[j,i]
      samoa_thresher_PRM[j,i] <- samoa_thresher_released[j,i] * samoa_thresher_PRM_rate[i]
      samoa_thresher_total[j,i] <- samoa_thresher_DOA[j,i] + samoa_thresher_PRM[j,i]
    }
  }
  
  #run averages
  samoa_thresher_proj <- data.frame("Species" = "Thresher",
                                    "catch" = NA,
                                    "catch.sd" = NA,
                                    "DOA" = NA,
                                    "DOA.sd" = NA,
                                    "PRM" = NA,
                                    "PRM.sd" = NA,
                                    "total" = NA,
                                    "total.sd" = NA)
  
  samoa_thresher_total_catches <- colSums(samoa_thresher_catches)
  samoa_thresher_proj$catch <- mean(samoa_thresher_total_catches)
  samoa_thresher_proj$catch.sd <-sd(samoa_thresher_total_catches)
  
  samoa_thresher_total_DOA <- colSums(samoa_thresher_DOA)
  samoa_thresher_proj$DOA <- mean(samoa_thresher_total_DOA)
  samoa_thresher_proj$DOA.sd <- sd(samoa_thresher_total_DOA)
  
  samoa_thresher_total_PRM <- colSums(samoa_thresher_PRM)
  samoa_thresher_proj$PRM <- mean(samoa_thresher_total_PRM)
  samoa_thresher_proj$PRM.sd <- sd(samoa_thresher_total_PRM)
  
  samoa_thresher_total_total <- colSums(samoa_thresher_total)
  samoa_thresher_proj$total <- mean(samoa_thresher_total_total)
  samoa_thresher_proj$total.sd <- sd(samoa_thresher_total_total)
  
  #cell averages
  #catches
  samoa_thresher$catch <- rowMeans(samoa_thresher_catches[,1:1000])
  samoa_thresher$catch.sd = apply(samoa_thresher_catches[,1:1000], 1, sd)
  
  #DOA
  samoa_thresher$DOA <- rowMeans(samoa_thresher_DOA[,1:1000])
  samoa_thresher$DOA.sd = apply(samoa_thresher_DOA[,1:1000], 1, sd)
  
  #prm
  samoa_thresher$PRM = rowMeans(samoa_thresher_PRM[,1:1000])
  samoa_thresher$PRM.sd = apply(samoa_thresher_PRM[,1:1000], 1, sd)
  
  #total
  samoa_thresher$total = rowMeans(samoa_thresher_total[,1:1000])
  samoa_thresher$total.sd = apply(samoa_thresher_total[,1:1000], 1, sd)
  
  #### shortfinmako sharks ####
  #row 4 in mortality; need to change this for each species when drawing rates)
  r_mako = 4
  
  samoa_mako <- samoa_comb[,c(1:4,17,18)] #create dataframe
  
  samoa_mako_catches <- matrix(nrow=n, ncol=1000)
  samoa_mako_DOA <- matrix(nrow=n, ncol=1000)
  samoa_mako_released <- matrix(nrow=n, ncol=1000)
  samoa_mako_PRM<- matrix(nrow=n, ncol=1000)
  samoa_mako_total <- matrix(nrow=n, ncol=1000)
  
  samoa_mako_CM_rate <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako])
  samoa_mako_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      samoa_mako_catches[j,i] <- samoa_hook_preds[j,i]/1000 * rlnorm(1, samoa_comb$mako.logcpue[j], samoa_comb$mako.logcpue.se[j])
      samoa_mako_DOA[j,i] <- samoa_mako_catches[j,i] * samoa_mako_CM_rate[i]
      samoa_mako_released[j,i] <- samoa_mako_catches[j,i] - samoa_mako_DOA[j,i]
      samoa_mako_PRM[j,i] <- samoa_mako_released[j,i] * samoa_mako_PRM_rate[i]
      samoa_mako_total[j,i] <- samoa_mako_DOA[j,i] + samoa_mako_PRM[j,i]
    }
  }
  
  #run averages
  samoa_mako_proj <- data.frame("Species" = "Mako",
                                "catch" = NA,
                                "catch.sd" = NA,
                                "DOA" = NA,
                                "DOA.sd" = NA,
                                "PRM" = NA,
                                "PRM.sd" = NA,
                                "total" = NA,
                                "total.sd" = NA)
  
  samoa_mako_total_catches <- colSums(samoa_mako_catches)
  samoa_mako_proj$catch <- mean(samoa_mako_total_catches)
  samoa_mako_proj$catch.sd <-sd(samoa_mako_total_catches)
  
  samoa_mako_total_DOA <- colSums(samoa_mako_DOA)
  samoa_mako_proj$DOA <- mean(samoa_mako_total_DOA)
  samoa_mako_proj$DOA.sd <- sd(samoa_mako_total_DOA)
  
  samoa_mako_total_PRM <- colSums(samoa_mako_PRM)
  samoa_mako_proj$PRM <- mean(samoa_mako_total_PRM)
  samoa_mako_proj$PRM.sd <- sd(samoa_mako_total_PRM)
  
  samoa_mako_total_total <- colSums(samoa_mako_total)
  samoa_mako_proj$total <- mean(samoa_mako_total_total)
  samoa_mako_proj$total.sd <- sd(samoa_mako_total_total)
  
  #cell averages
  #catches
  samoa_mako$catch <- rowMeans(samoa_mako_catches[,1:1000])
  samoa_mako$catch.sd = apply(samoa_mako_catches[,1:1000], 1, sd)
  
  #DOA
  samoa_mako$DOA <- rowMeans(samoa_mako_DOA[,1:1000])
  samoa_mako$DOA.sd = apply(samoa_mako_DOA[,1:1000], 1, sd)
  
  #prm
  samoa_mako$PRM = rowMeans(samoa_mako_PRM[,1:1000])
  samoa_mako$PRM.sd = apply(samoa_mako_PRM[,1:1000], 1, sd)
  
  #total
  samoa_mako$total = rowMeans(samoa_mako_total[,1:1000])
  samoa_mako$total.sd = apply(samoa_mako_total[,1:1000], 1, sd)
  
  #### oceanic whitetip sharks ####
  #row 8 in mortality; need to change this for each species when drawing rates)
  r_oceanicwhitetip = 8
  
  samoa_oceanicwhitetip <- samoa_comb[,c(1:4,25,26)] #create dataframe
  
  samoa_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
  samoa_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
  samoa_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
  samoa_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
  samoa_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)
  
  samoa_oceanicwhitetip_CM_rate <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip])
  samoa_oceanicwhitetip_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      samoa_oceanicwhitetip_catches[j,i] <- samoa_hook_preds[j,i]/1000 * rlnorm(1, samoa_comb$oceanicwhitetip.logcpue[j], samoa_comb$oceanicwhitetip.logcpue.se[j])
      samoa_oceanicwhitetip_DOA[j,i] <- samoa_oceanicwhitetip_catches[j,i] * samoa_oceanicwhitetip_CM_rate[i]
      samoa_oceanicwhitetip_released[j,i] <- samoa_oceanicwhitetip_catches[j,i] - samoa_oceanicwhitetip_DOA[j,i]
      samoa_oceanicwhitetip_PRM[j,i] <- samoa_oceanicwhitetip_released[j,i] * samoa_oceanicwhitetip_PRM_rate[i]
      samoa_oceanicwhitetip_total[j,i] <- samoa_oceanicwhitetip_DOA[j,i] + samoa_oceanicwhitetip_PRM[j,i]
    }
  }
  
  #run averages
  samoa_oceanicwhitetip_proj <- data.frame("Species" = "Oceanic Whitetip",
                                           "catch" = NA,
                                           "catch.sd" = NA,
                                           "DOA" = NA,
                                           "DOA.sd" = NA,
                                           "PRM" = NA,
                                           "PRM.sd" = NA,
                                           "total" = NA,
                                           "total.sd" = NA)
  
  samoa_oceanicwhitetip_total_catches <- colSums(samoa_oceanicwhitetip_catches)
  samoa_oceanicwhitetip_proj$catch <- mean(samoa_oceanicwhitetip_total_catches)
  samoa_oceanicwhitetip_proj$catch.sd <-sd(samoa_oceanicwhitetip_total_catches)
  
  samoa_oceanicwhitetip_total_DOA <- colSums(samoa_oceanicwhitetip_DOA)
  samoa_oceanicwhitetip_proj$DOA <- mean(samoa_oceanicwhitetip_total_DOA)
  samoa_oceanicwhitetip_proj$DOA.sd <- sd(samoa_oceanicwhitetip_total_DOA)
  
  samoa_oceanicwhitetip_total_PRM <- colSums(samoa_oceanicwhitetip_PRM)
  samoa_oceanicwhitetip_proj$PRM <- mean(samoa_oceanicwhitetip_total_PRM)
  samoa_oceanicwhitetip_proj$PRM.sd <- sd(samoa_oceanicwhitetip_total_PRM)
  
  samoa_oceanicwhitetip_total_total <- colSums(samoa_oceanicwhitetip_total)
  samoa_oceanicwhitetip_proj$total <- mean(samoa_oceanicwhitetip_total_total)
  samoa_oceanicwhitetip_proj$total.sd <- sd(samoa_oceanicwhitetip_total_total)
  
  #cell averages
  #catches
  samoa_oceanicwhitetip$catch <- rowMeans(samoa_oceanicwhitetip_catches[,1:1000])
  samoa_oceanicwhitetip$catch.sd = apply(samoa_oceanicwhitetip_catches[,1:1000], 1, sd)
  
  #DOA
  samoa_oceanicwhitetip$DOA <- rowMeans(samoa_oceanicwhitetip_DOA[,1:1000])
  samoa_oceanicwhitetip$DOA.sd = apply(samoa_oceanicwhitetip_DOA[,1:1000], 1, sd)
  
  #prm
  samoa_oceanicwhitetip$PRM = rowMeans(samoa_oceanicwhitetip_PRM[,1:1000])
  samoa_oceanicwhitetip$PRM.sd = apply(samoa_oceanicwhitetip_PRM[,1:1000], 1, sd)
  
  #total
  samoa_oceanicwhitetip$total = rowMeans(samoa_oceanicwhitetip_total[,1:1000])
  samoa_oceanicwhitetip$total.sd = apply(samoa_oceanicwhitetip_total[,1:1000], 1, sd)
  
  #### hammerhead sharks ####
  #row 9 in mortality; need to change this for each species when drawing rates)
  r_hammerhead = 9
  
  samoa_hammerhead <- samoa_comb[,c(1:4,29,30)] #create dataframe
  
  samoa_hammerhead_catches <- matrix(nrow=n, ncol=1000)
  samoa_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
  samoa_hammerhead_released <- matrix(nrow=n, ncol=1000)
  samoa_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
  samoa_hammerhead_total <- matrix(nrow=n, ncol=1000)
  
  samoa_hammerhead_CM_rate <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead])
  samoa_hammerhead_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      samoa_hammerhead_catches[j,i] <- samoa_hook_preds[j,i]/1000 * rlnorm(1, samoa_comb$hammerhead.logcpue[j], samoa_comb$hammerhead.logcpue.se[j])
      samoa_hammerhead_DOA[j,i] <- samoa_hammerhead_catches[j,i] * samoa_hammerhead_CM_rate[i]
      samoa_hammerhead_released[j,i] <- samoa_hammerhead_catches[j,i] - samoa_hammerhead_DOA[j,i]
      samoa_hammerhead_PRM[j,i] <- samoa_hammerhead_released[j,i] * samoa_hammerhead_PRM_rate[i]
      samoa_hammerhead_total[j,i] <- samoa_hammerhead_DOA[j,i] + samoa_hammerhead_PRM[j,i]
    }
  }
  
  #run averages
  samoa_hammerhead_proj <- data.frame("Species" = "Hammerhead",
                                      "catch" = NA,
                                      "catch.sd" = NA,
                                      "DOA" = NA,
                                      "DOA.sd" = NA,
                                      "PRM" = NA,
                                      "PRM.sd" = NA,
                                      "total" = NA,
                                      "total.sd" = NA)
  
  samoa_hammerhead_total_catches <- colSums(samoa_hammerhead_catches)
  samoa_hammerhead_proj$catch <- mean(samoa_hammerhead_total_catches)
  samoa_hammerhead_proj$catch.sd <-sd(samoa_hammerhead_total_catches)
  
  samoa_hammerhead_total_DOA <- colSums(samoa_hammerhead_DOA)
  samoa_hammerhead_proj$DOA <- mean(samoa_hammerhead_total_DOA)
  samoa_hammerhead_proj$DOA.sd <- sd(samoa_hammerhead_total_DOA)
  
  samoa_hammerhead_total_PRM <- colSums(samoa_hammerhead_PRM)
  samoa_hammerhead_proj$PRM <- mean(samoa_hammerhead_total_PRM)
  samoa_hammerhead_proj$PRM.sd <- sd(samoa_hammerhead_total_PRM)
  
  samoa_hammerhead_total_total <- colSums(samoa_hammerhead_total)
  samoa_hammerhead_proj$total <- mean(samoa_hammerhead_total_total)
  samoa_hammerhead_proj$total.sd <- sd(samoa_hammerhead_total_total)
  
  #cell averages
  #catches
  samoa_hammerhead$catch <- rowMeans(samoa_hammerhead_catches[,1:1000])
  samoa_hammerhead$catch.sd = apply(samoa_hammerhead_catches[,1:1000], 1, sd)
  
  #DOA
  samoa_hammerhead$DOA <- rowMeans(samoa_hammerhead_DOA[,1:1000])
  samoa_hammerhead$DOA.sd = apply(samoa_hammerhead_DOA[,1:1000], 1, sd)
  
  #prm
  samoa_hammerhead$PRM = rowMeans(samoa_hammerhead_PRM[,1:1000])
  samoa_hammerhead$PRM.sd = apply(samoa_hammerhead_PRM[,1:1000], 1, sd)
  
  #total
  samoa_hammerhead$total = rowMeans(samoa_hammerhead_total[,1:1000])
  samoa_hammerhead$total.sd = apply(samoa_hammerhead_total[,1:1000], 1, sd)
  
  #### othersharks sharks ####
  #row 10 in mortality; need to change this for each species when drawing rates)
  r_othersharks = 10
  
  samoa_othersharks <- samoa_comb[,c(1:4,33,34)] #create dataframe
  
  samoa_othersharks_catches <- matrix(nrow=n, ncol=1000)
  samoa_othersharks_DOA <- matrix(nrow=n, ncol=1000)
  samoa_othersharks_released <- matrix(nrow=n, ncol=1000)
  samoa_othersharks_PRM<- matrix(nrow=n, ncol=1000)
  samoa_othersharks_total <- matrix(nrow=n, ncol=1000)
  
  samoa_othersharks_CM_rate <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks])
  samoa_othersharks_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      samoa_othersharks_catches[j,i] <- samoa_hook_preds[j,i]/1000 * rlnorm(1, samoa_comb$othersharks.logcpue[j], samoa_comb$othersharks.logcpue.se[j])
      samoa_othersharks_DOA[j,i] <- samoa_othersharks_catches[j,i] * samoa_othersharks_CM_rate[i]
      samoa_othersharks_released[j,i] <- samoa_othersharks_catches[j,i] - samoa_othersharks_DOA[j,i]
      samoa_othersharks_PRM[j,i] <- samoa_othersharks_released[j,i] * samoa_othersharks_PRM_rate[i]
      samoa_othersharks_total[j,i] <- samoa_othersharks_DOA[j,i] + samoa_othersharks_PRM[j,i]
    }
  }
  
  #run averages
  samoa_othersharks_proj <- data.frame("Species" = "Other Sharks",
                                       "catch" = NA,
                                       "catch.sd" = NA,
                                       "DOA" = NA,
                                       "DOA.sd" = NA,
                                       "PRM" = NA,
                                       "PRM.sd" = NA,
                                       "total" = NA,
                                       "total.sd" = NA)
  
  samoa_othersharks_total_catches <- colSums(samoa_othersharks_catches)
  samoa_othersharks_proj$catch <- mean(samoa_othersharks_total_catches)
  samoa_othersharks_proj$catch.sd <-sd(samoa_othersharks_total_catches)
  
  samoa_othersharks_total_DOA <- colSums(samoa_othersharks_DOA)
  samoa_othersharks_proj$DOA <- mean(samoa_othersharks_total_DOA)
  samoa_othersharks_proj$DOA.sd <- sd(samoa_othersharks_total_DOA)
  
  samoa_othersharks_total_PRM <- colSums(samoa_othersharks_PRM)
  samoa_othersharks_proj$PRM <- mean(samoa_othersharks_total_PRM)
  samoa_othersharks_proj$PRM.sd <- sd(samoa_othersharks_total_PRM)
  
  samoa_othersharks_total_total <- colSums(samoa_othersharks_total)
  samoa_othersharks_proj$total <- mean(samoa_othersharks_total_total)
  samoa_othersharks_proj$total.sd <- sd(samoa_othersharks_total_total)
  
  #cell averages
  #catches
  samoa_othersharks$catch <- rowMeans(samoa_othersharks_catches[,1:1000])
  samoa_othersharks$catch.sd = apply(samoa_othersharks_catches[,1:1000], 1, sd)
  
  #DOA
  samoa_othersharks$DOA <- rowMeans(samoa_othersharks_DOA[,1:1000])
  samoa_othersharks$DOA.sd = apply(samoa_othersharks_DOA[,1:1000], 1, sd)
  
  #prm
  samoa_othersharks$PRM = rowMeans(samoa_othersharks_PRM[,1:1000])
  samoa_othersharks$PRM.sd = apply(samoa_othersharks_PRM[,1:1000], 1, sd)
  
  #total
  samoa_othersharks$total = rowMeans(samoa_othersharks_total[,1:1000])
  samoa_othersharks$total.sd = apply(samoa_othersharks_total[,1:1000], 1, sd)
  
  #### samoa Totals ####
  
  samoa_bsh$Species = "Blue Shark"
  samoa_silky$Species = "Silky Shark"
  samoa_thresher$Species="Thresher"
  samoa_mako$Species="Mako"
  samoa_oceanicwhitetip$Species="Oceanic Whitetip"
  samoa_hammerhead$Species="Hammerhead"
  samoa_othersharks$Species="Other Sharks"
  
  colnames(samoa_bsh)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(samoa_silky)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(samoa_thresher)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(samoa_mako)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(samoa_oceanicwhitetip)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(samoa_hammerhead)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(samoa_othersharks)[c(5,6)] <- c("logcpue","logcpue.se")
  
  samoa_total_preds <- rbind(samoa_bsh,
                             samoa_silky,
                             samoa_thresher,
                             samoa_mako,
                             samoa_oceanicwhitetip,
                             samoa_hammerhead,
                             samoa_othersharks)
  
  end.time <- Sys.time()
  end.time - start.time}

write.csv(samoa_total_preds, "samoa_total_preds.csv", row.names = F)

#### summary ####
samoa_summary <- rbind(samoa_bsh_proj,
                       samoa_silky_proj,
                       samoa_thresher_proj,
                       samoa_mako_proj,
                       samoa_oceanicwhitetip_proj,
                       samoa_hammerhead_proj,
                       samoa_othersharks_proj)

write.csv(samoa_summary, "samoa_summary.csv", row.names = F)

#plotting for fig1
samoa_mortality <- read.csv("samoa_total_preds.csv")
samoa_proj <- aggregate(data=samoa_mortality, total ~ Lat + Long, FUN='sum')
dev.off()
world <- map_data("world")
samoa_map <- ggplot(data=samoa_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Bycatch Mortality \n(# of individuals", low="khaki1", high="red",
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
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Samoa")




