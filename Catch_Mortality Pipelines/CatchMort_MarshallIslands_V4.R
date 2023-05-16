library(tidyverse)
library(fields)
library(stringr)
library(sf)
library(logitnorm)

set.seed(1000)

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
marshallislands_proj <- read.csv("marshallislands2019.csv")

marshallislands_proj2 <- marshallislands_proj[,c(5,6,9,10)]
#rename
colnames(marshallislands_proj2)[1] <- "Long"
colnames(marshallislands_proj2)[2] <- "Lat"

#merge
marshallislands_comb <- merge(marshallislands_proj2, master_cpue, by=c("Lat", "Long"), all=T)
marshallislands_comb <- marshallislands_comb %>% drop_na(log_hooks)

n <- nrow(marshallislands_comb)

marshallislands_hook_preds <- matrix(nrow=n, ncol=1000)
for (j in 1:n) {
  for (i in 1:1000) {
    marshallislands_hook_preds[j,i] <- rlnorm(1,marshallislands_comb$log_hooks[j],marshallislands_comb$log_hooks.se[j])
  }
}

{start.time <- Sys.time()
  #### blue sharks ####
  #row 2 in mortality; need to change this for each species when drawing rates)
  r_bsh = 2
  
  marshallislands_bsh <- marshallislands_comb[,c(1:4,9,10)]
  
  marshallislands_bsh_catches <- matrix(nrow=n, ncol=1000)
  marshallislands_bsh_DOA <- matrix(nrow=n, ncol=1000)
  marshallislands_bsh_released <- matrix(nrow=n, ncol=1000)
  marshallislands_bsh_PRM<- matrix(nrow=n, ncol=1000)
  marshallislands_bsh_total <- matrix(nrow=n, ncol=1000)
  
  marshallislands_bsh_CM_rate <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh])
  marshallislands_bsh_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      marshallislands_bsh_catches[j,i] <- marshallislands_hook_preds[j,i]/1000 * rlnorm(1, marshallislands_comb$blueshark.logcpue[j], marshallislands_comb$blueshark.logcpue.se[j])
      marshallislands_bsh_DOA[j,i] <- marshallislands_bsh_catches[j,i] *  marshallislands_bsh_CM_rate[i]
      marshallislands_bsh_released[j,i] <- marshallislands_bsh_catches[j,i] - marshallislands_bsh_DOA[j,i]
      marshallislands_bsh_PRM[j,i] <- marshallislands_bsh_released[j,i] * marshallislands_bsh_PRM_rate[i] 
      marshallislands_bsh_total[j,i] <- marshallislands_bsh_DOA[j,i] + marshallislands_bsh_PRM[j,i]
    }
  }
  
  #run averages
  marshallislands_bsh_proj <- data.frame("Species" = "Blue Shark",
                                  "catch" = NA,
                                  "catch.upr" = NA,
                                  "catch.lwr" = NA,
                                  "DOA" = NA,
                                  "DOA.upr" = NA,
                                  "DOA.lwr" = NA,
                                  "PRM" = NA,
                                  "PRM.upr" = NA,
                                  "PRM.lwr" = NA,
                                  "total" = NA,
                                  "total.upr" = NA,
                                  "total.lwr" = NA)
  
  marshallislands_bsh_total_catches <- colSums(marshallislands_bsh_catches)
  marshallislands_bsh_proj$catch <- mean(marshallislands_bsh_total_catches)
  marshallislands_bsh_proj$catch.upr <- quantile(marshallislands_bsh_total_catches, 0.95)
  marshallislands_bsh_proj$catch.lwr <- quantile(marshallislands_bsh_total_catches, 0.05)
  
  marshallislands_bsh_total_DOA <- colSums(marshallislands_bsh_DOA)
  marshallislands_bsh_proj$DOA <- mean(marshallislands_bsh_total_DOA)
  marshallislands_bsh_proj$DOA.upr <- quantile(marshallislands_bsh_total_DOA, 0.95)
  marshallislands_bsh_proj$DOA.lwr <- quantile(marshallislands_bsh_total_DOA, 0.05)
  
  marshallislands_bsh_total_PRM <- colSums(marshallislands_bsh_PRM)
  marshallislands_bsh_proj$PRM <- mean(marshallislands_bsh_total_PRM)
  marshallislands_bsh_proj$PRM.upr <- quantile(marshallislands_bsh_total_PRM, 0.95)
  marshallislands_bsh_proj$PRM.lwr <- quantile(marshallislands_bsh_total_PRM, 0.05)
  
  marshallislands_bsh_total_total <- colSums(marshallislands_bsh_total)
  marshallislands_bsh_proj$total <- mean(marshallislands_bsh_total_total)
  marshallislands_bsh_proj$total.upr <- quantile(marshallislands_bsh_total_total, 0.95)
  marshallislands_bsh_proj$total.lwr <- quantile(marshallislands_bsh_total_total, 0.05)
  
  #cell averages
  #catches
  marshallislands_bsh$catch <- rowMeans(marshallislands_bsh_catches[,1:1000])
  marshallislands_bsh$catch.sd = apply(marshallislands_bsh_catches[,1:1000], 1, sd)
  
  #DOA
  marshallislands_bsh$DOA <- rowMeans(marshallislands_bsh_DOA[,1:1000])
  marshallislands_bsh$DOA.sd = apply(marshallislands_bsh_DOA[,1:1000], 1, sd)
  
  #prm
  marshallislands_bsh$PRM = rowMeans(marshallislands_bsh_PRM[,1:1000])
  marshallislands_bsh$PRM.sd = apply(marshallislands_bsh_PRM[,1:1000], 1, sd)
  
  #total
  marshallislands_bsh$total = rowMeans(marshallislands_bsh_total[,1:1000])
  marshallislands_bsh$total.sd = apply(marshallislands_bsh_total[,1:1000], 1, sd)
  
  #### silky sharks ####
  r_silky=5 #row 5 in mortality; need to change this for each species when drawing rates)
  
  marshallislands_silky <- marshallislands_comb[,c(1:4,13,14)] #create dataframe
  
  marshallislands_silky_catches <- matrix(nrow=n, ncol=1000)
  marshallislands_silky_DOA <- matrix(nrow=n, ncol=1000)
  marshallislands_silky_released <- matrix(nrow=n, ncol=1000)
  marshallislands_silky_PRM<- matrix(nrow=n, ncol=1000)
  marshallislands_silky_total <- matrix(nrow=n, ncol=1000)
  
  marshallislands_silky_CM_rate <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky])
  marshallislands_silky_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      marshallislands_silky_catches[j,i] <- marshallislands_hook_preds[j,i]/1000 * rlnorm(1, marshallislands_comb$silkyshark.logcpue[j], marshallislands_comb$silkyshark.logcpue.se[j])
      marshallislands_silky_DOA[j,i] <- marshallislands_silky_catches[j,i] * marshallislands_silky_CM_rate[i]
      marshallislands_silky_released[j,i] <- marshallislands_silky_catches[j,i] - marshallislands_silky_DOA[j,i]
      marshallislands_silky_PRM[j,i] <- marshallislands_silky_released[j,i] * marshallislands_silky_PRM_rate[i]
      marshallislands_silky_total[j,i] <- marshallislands_silky_DOA[j,i] + marshallislands_silky_PRM[j,i]
    }
  }
  marshallislands_silky_proj <- data.frame("Species" = "Silky Shark",
                                    "catch" = NA,
                                    "catch.upr" = NA,
                                    "catch.lwr" = NA,
                                    "DOA" = NA,
                                    "DOA.upr" = NA,
                                    "DOA.lwr" = NA,
                                    "PRM" = NA,
                                    "PRM.upr" = NA,
                                    "PRM.lwr" = NA,
                                    "total" = NA,
                                    "total.upr" = NA,
                                    "total.lwr" = NA)
  
  marshallislands_silky_total_catches <- colSums(marshallislands_silky_catches)
  marshallislands_silky_proj$catch <- mean(marshallislands_silky_total_catches)
  marshallislands_silky_proj$catch.upr <- quantile(marshallislands_silky_total_catches, 0.95)
  marshallislands_silky_proj$catch.lwr <- quantile(marshallislands_silky_total_catches, 0.05)
  
  marshallislands_silky_total_DOA <- colSums(marshallislands_silky_DOA)
  marshallislands_silky_proj$DOA <- mean(marshallislands_silky_total_DOA)
  marshallislands_silky_proj$DOA.upr <- quantile(marshallislands_silky_total_DOA, 0.95)
  marshallislands_silky_proj$DOA.lwr <- quantile(marshallislands_silky_total_DOA, 0.05)
  
  marshallislands_silky_total_PRM <- colSums(marshallislands_silky_PRM)
  marshallislands_silky_proj$PRM <- mean(marshallislands_silky_total_PRM)
  marshallislands_silky_proj$PRM.upr <- quantile(marshallislands_silky_total_PRM, 0.95)
  marshallislands_silky_proj$PRM.lwr <- quantile(marshallislands_silky_total_PRM, 0.05)
  
  marshallislands_silky_total_total <- colSums(marshallislands_silky_total)
  marshallislands_silky_proj$total <- mean(marshallislands_silky_total_total)
  marshallislands_silky_proj$total.upr <- quantile(marshallislands_silky_total_total, 0.95)
  marshallislands_silky_proj$total.lwr <- quantile(marshallislands_silky_total_total, 0.05)
  
  #cell averages
  #catches
  marshallislands_silky$catch <- rowMeans(marshallislands_silky_catches[,1:1000])
  marshallislands_silky$catch.sd = apply(marshallislands_silky_catches[,1:1000], 1, sd)
  
  #DOA
  marshallislands_silky$DOA <- rowMeans(marshallislands_silky_DOA[,1:1000])
  marshallislands_silky$DOA.sd = apply(marshallislands_silky_DOA[,1:1000], 1, sd)
  
  #prm
  marshallislands_silky$PRM = rowMeans(marshallislands_silky_PRM[,1:1000])
  marshallislands_silky$PRM.sd = apply(marshallislands_silky_PRM[,1:1000], 1, sd)
  
  #total
  marshallislands_silky$total = rowMeans(marshallislands_silky_total[,1:1000])
  marshallislands_silky$total.sd = apply(marshallislands_silky_total[,1:1000], 1, sd)
  
  #### thresher sharks ####
  #row 1 in mortality; need to change this for each species when drawing rates)
  r_thresher = 1
  
  marshallislands_thresher <- marshallislands_comb[,c(1:4,21,22)] #create dataframe
  
  marshallislands_thresher_catches <- matrix(nrow=n, ncol=1000)
  marshallislands_thresher_DOA <- matrix(nrow=n, ncol=1000)
  marshallislands_thresher_released <- matrix(nrow=n, ncol=1000)
  marshallislands_thresher_PRM<- matrix(nrow=n, ncol=1000)
  marshallislands_thresher_total <- matrix(nrow=n, ncol=1000)
  
  marshallislands_thresher_CM_rate <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher])
  marshallislands_thresher_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      marshallislands_thresher_catches[j,i] <- marshallislands_hook_preds[j,i]/1000 * rlnorm(1, marshallislands_comb$thresher.logcpue[j], marshallislands_comb$thresher.logcpue.se[j])
      marshallislands_thresher_DOA[j,i] <- marshallislands_thresher_catches[j,i] * marshallislands_thresher_CM_rate[i]
      marshallislands_thresher_released[j,i] <- marshallislands_thresher_catches[j,i] - marshallislands_thresher_DOA[j,i]
      marshallislands_thresher_PRM[j,i] <- marshallislands_thresher_released[j,i] * marshallislands_thresher_PRM_rate[i]
      marshallislands_thresher_total[j,i] <- marshallislands_thresher_DOA[j,i] + marshallislands_thresher_PRM[j,i]
    }
  }
  
  marshallislands_thresher_proj <- data.frame("Species" = "Thresher",
                                       "catch" = NA,
                                       "catch.upr" = NA,
                                       "catch.lwr" = NA,
                                       "DOA" = NA,
                                       "DOA.upr" = NA,
                                       "DOA.lwr" = NA,
                                       "PRM" = NA,
                                       "PRM.upr" = NA,
                                       "PRM.lwr" = NA,
                                       "total" = NA,
                                       "total.upr" = NA,
                                       "total.lwr" = NA)
  
  marshallislands_thresher_total_catches <- colSums(marshallislands_thresher_catches)
  marshallislands_thresher_proj$catch <- mean(marshallislands_thresher_total_catches)
  marshallislands_thresher_proj$catch.upr <- quantile(marshallislands_thresher_total_catches, 0.95)
  marshallislands_thresher_proj$catch.lwr <- quantile(marshallislands_thresher_total_catches, 0.05)
  
  marshallislands_thresher_total_DOA <- colSums(marshallislands_thresher_DOA)
  marshallislands_thresher_proj$DOA <- mean(marshallislands_thresher_total_DOA)
  marshallislands_thresher_proj$DOA.upr <- quantile(marshallislands_thresher_total_DOA, 0.95)
  marshallislands_thresher_proj$DOA.lwr <- quantile(marshallislands_thresher_total_DOA, 0.05)
  
  marshallislands_thresher_total_PRM <- colSums(marshallislands_thresher_PRM)
  marshallislands_thresher_proj$PRM <- mean(marshallislands_thresher_total_PRM)
  marshallislands_thresher_proj$PRM.upr <- quantile(marshallislands_thresher_total_PRM, 0.95)
  marshallislands_thresher_proj$PRM.lwr <- quantile(marshallislands_thresher_total_PRM, 0.05)
  
  marshallislands_thresher_total_total <- colSums(marshallislands_thresher_total)
  marshallislands_thresher_proj$total <- mean(marshallislands_thresher_total_total)
  marshallislands_thresher_proj$total.upr <- quantile(marshallislands_thresher_total_total, 0.95)
  marshallislands_thresher_proj$total.lwr <- quantile(marshallislands_thresher_total_total, 0.05)
  
  #cell averages
  #catches
  marshallislands_thresher$catch <- rowMeans(marshallislands_thresher_catches[,1:1000])
  marshallislands_thresher$catch.sd = apply(marshallislands_thresher_catches[,1:1000], 1, sd)
  
  #DOA
  marshallislands_thresher$DOA <- rowMeans(marshallislands_thresher_DOA[,1:1000])
  marshallislands_thresher$DOA.sd = apply(marshallislands_thresher_DOA[,1:1000], 1, sd)
  
  #prm
  marshallislands_thresher$PRM = rowMeans(marshallislands_thresher_PRM[,1:1000])
  marshallislands_thresher$PRM.sd = apply(marshallislands_thresher_PRM[,1:1000], 1, sd)
  
  #total
  marshallislands_thresher$total = rowMeans(marshallislands_thresher_total[,1:1000])
  marshallislands_thresher$total.sd = apply(marshallislands_thresher_total[,1:1000], 1, sd)
  
  #### shortfinmako sharks ####
  #row 4 in mortality; need to change this for each species when drawing rates)
  r_mako = 4
  
  marshallislands_mako <- marshallislands_comb[,c(1:4,17,18)] #create dataframe
  
  marshallislands_mako_catches <- matrix(nrow=n, ncol=1000)
  marshallislands_mako_DOA <- matrix(nrow=n, ncol=1000)
  marshallislands_mako_released <- matrix(nrow=n, ncol=1000)
  marshallislands_mako_PRM<- matrix(nrow=n, ncol=1000)
  marshallislands_mako_total <- matrix(nrow=n, ncol=1000)
  
  marshallislands_mako_CM_rate <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako])
  marshallislands_mako_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      marshallislands_mako_catches[j,i] <- marshallislands_hook_preds[j,i]/1000 * rlnorm(1, marshallislands_comb$mako.logcpue[j], marshallislands_comb$mako.logcpue.se[j])
      marshallislands_mako_DOA[j,i] <- marshallislands_mako_catches[j,i] * marshallislands_mako_CM_rate[i]
      marshallislands_mako_released[j,i] <- marshallislands_mako_catches[j,i] - marshallislands_mako_DOA[j,i]
      marshallislands_mako_PRM[j,i] <- marshallislands_mako_released[j,i] * marshallislands_mako_PRM_rate[i]
      marshallislands_mako_total[j,i] <- marshallislands_mako_DOA[j,i] + marshallislands_mako_PRM[j,i]
    }
  }
  
  #run averages
  marshallislands_mako_proj <- data.frame("Species" = "Mako Shark",
                                   "catch" = NA,
                                   "catch.upr" = NA,
                                   "catch.lwr" = NA,
                                   "DOA" = NA,
                                   "DOA.upr" = NA,
                                   "DOA.lwr" = NA,
                                   "PRM" = NA,
                                   "PRM.upr" = NA,
                                   "PRM.lwr" = NA,
                                   "total" = NA,
                                   "total.upr" = NA,
                                   "total.lwr" = NA)
  
  marshallislands_mako_total_catches <- colSums(marshallislands_mako_catches)
  marshallislands_mako_proj$catch <- mean(marshallislands_mako_total_catches)
  marshallislands_mako_proj$catch.upr <- quantile(marshallislands_mako_total_catches, 0.95)
  marshallislands_mako_proj$catch.lwr <- quantile(marshallislands_mako_total_catches, 0.05)
  
  marshallislands_mako_total_DOA <- colSums(marshallislands_mako_DOA)
  marshallislands_mako_proj$DOA <- mean(marshallislands_mako_total_DOA)
  marshallislands_mako_proj$DOA.upr <- quantile(marshallislands_mako_total_DOA, 0.95)
  marshallislands_mako_proj$DOA.lwr <- quantile(marshallislands_mako_total_DOA, 0.05)
  
  marshallislands_mako_total_PRM <- colSums(marshallislands_mako_PRM)
  marshallislands_mako_proj$PRM <- mean(marshallislands_mako_total_PRM)
  marshallislands_mako_proj$PRM.upr <- quantile(marshallislands_mako_total_PRM, 0.95)
  marshallislands_mako_proj$PRM.lwr <- quantile(marshallislands_mako_total_PRM, 0.05)
  
  marshallislands_mako_total_total <- colSums(marshallislands_mako_total)
  marshallislands_mako_proj$total <- mean(marshallislands_mako_total_total)
  marshallislands_mako_proj$total.upr <- quantile(marshallislands_mako_total_total, 0.95)
  marshallislands_mako_proj$total.lwr <- quantile(marshallislands_mako_total_total, 0.05)
  
  #cell averages
  #catches
  marshallislands_mako$catch <- rowMeans(marshallislands_mako_catches[,1:1000])
  marshallislands_mako$catch.sd = apply(marshallislands_mako_catches[,1:1000], 1, sd)
  
  #DOA
  marshallislands_mako$DOA <- rowMeans(marshallislands_mako_DOA[,1:1000])
  marshallislands_mako$DOA.sd = apply(marshallislands_mako_DOA[,1:1000], 1, sd)
  
  #prm
  marshallislands_mako$PRM = rowMeans(marshallislands_mako_PRM[,1:1000])
  marshallislands_mako$PRM.sd = apply(marshallislands_mako_PRM[,1:1000], 1, sd)
  
  #total
  marshallislands_mako$total = rowMeans(marshallislands_mako_total[,1:1000])
  marshallislands_mako$total.sd = apply(marshallislands_mako_total[,1:1000], 1, sd)
  
  #### oceanic whitetip sharks ####
  #row 8 in mortality; need to change this for each species when drawing rates)
  r_oceanicwhitetip = 8
  
  marshallislands_oceanicwhitetip <- marshallislands_comb[,c(1:4,25,26)] #create dataframe
  
  marshallislands_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
  marshallislands_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
  marshallislands_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
  marshallislands_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
  marshallislands_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)
  
  marshallislands_oceanicwhitetip_CM_rate <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip])
  marshallislands_oceanicwhitetip_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      marshallislands_oceanicwhitetip_catches[j,i] <- marshallislands_hook_preds[j,i]/1000 * rlnorm(1, marshallislands_comb$oceanicwhitetip.logcpue[j], marshallislands_comb$oceanicwhitetip.logcpue.se[j])
      marshallislands_oceanicwhitetip_DOA[j,i] <- marshallislands_oceanicwhitetip_catches[j,i] * marshallislands_oceanicwhitetip_CM_rate[i]
      marshallislands_oceanicwhitetip_released[j,i] <- marshallislands_oceanicwhitetip_catches[j,i] - marshallislands_oceanicwhitetip_DOA[j,i]
      marshallislands_oceanicwhitetip_PRM[j,i] <- marshallislands_oceanicwhitetip_released[j,i] * marshallislands_oceanicwhitetip_PRM_rate[i]
      marshallislands_oceanicwhitetip_total[j,i] <- marshallislands_oceanicwhitetip_DOA[j,i] + marshallislands_oceanicwhitetip_PRM[j,i]
    }
  }
  
  #run averages
  marshallislands_oceanicwhitetip_proj <- data.frame("Species" = "Oceanic Whitetip",
                                              "catch" = NA,
                                              "catch.upr" = NA,
                                              "catch.lwr" = NA,
                                              "DOA" = NA,
                                              "DOA.upr" = NA,
                                              "DOA.lwr" = NA,
                                              "PRM" = NA,
                                              "PRM.upr" = NA,
                                              "PRM.lwr" = NA,
                                              "total" = NA,
                                              "total.upr" = NA,
                                              "total.lwr" = NA)
  
  marshallislands_oceanicwhitetip_total_catches <- colSums(marshallislands_oceanicwhitetip_catches)
  marshallislands_oceanicwhitetip_proj$catch <- mean(marshallislands_oceanicwhitetip_total_catches)
  marshallislands_oceanicwhitetip_proj$catch.upr <- quantile(marshallislands_oceanicwhitetip_total_catches, 0.95)
  marshallislands_oceanicwhitetip_proj$catch.lwr <- quantile(marshallislands_oceanicwhitetip_total_catches, 0.05)
  
  marshallislands_oceanicwhitetip_total_DOA <- colSums(marshallislands_oceanicwhitetip_DOA)
  marshallislands_oceanicwhitetip_proj$DOA <- mean(marshallislands_oceanicwhitetip_total_DOA)
  marshallislands_oceanicwhitetip_proj$DOA.upr <- quantile(marshallislands_oceanicwhitetip_total_DOA, 0.95)
  marshallislands_oceanicwhitetip_proj$DOA.lwr <- quantile(marshallislands_oceanicwhitetip_total_DOA, 0.05)
  
  marshallislands_oceanicwhitetip_total_PRM <- colSums(marshallislands_oceanicwhitetip_PRM)
  marshallislands_oceanicwhitetip_proj$PRM <- mean(marshallislands_oceanicwhitetip_total_PRM)
  marshallislands_oceanicwhitetip_proj$PRM.upr <- quantile(marshallislands_oceanicwhitetip_total_PRM, 0.95)
  marshallislands_oceanicwhitetip_proj$PRM.lwr <- quantile(marshallislands_oceanicwhitetip_total_PRM, 0.05)
  
  marshallislands_oceanicwhitetip_total_total <- colSums(marshallislands_oceanicwhitetip_total)
  marshallislands_oceanicwhitetip_proj$total <- mean(marshallislands_oceanicwhitetip_total_total)
  marshallislands_oceanicwhitetip_proj$total.upr <- quantile(marshallislands_oceanicwhitetip_total_total, 0.95)
  marshallislands_oceanicwhitetip_proj$total.lwr <- quantile(marshallislands_oceanicwhitetip_total_total, 0.05)
  
  #cell averages
  #catches
  marshallislands_oceanicwhitetip$catch <- rowMeans(marshallislands_oceanicwhitetip_catches[,1:1000])
  marshallislands_oceanicwhitetip$catch.sd = apply(marshallislands_oceanicwhitetip_catches[,1:1000], 1, sd)
  
  #DOA
  marshallislands_oceanicwhitetip$DOA <- rowMeans(marshallislands_oceanicwhitetip_DOA[,1:1000])
  marshallislands_oceanicwhitetip$DOA.sd = apply(marshallislands_oceanicwhitetip_DOA[,1:1000], 1, sd)
  
  #prm
  marshallislands_oceanicwhitetip$PRM = rowMeans(marshallislands_oceanicwhitetip_PRM[,1:1000])
  marshallislands_oceanicwhitetip$PRM.sd = apply(marshallislands_oceanicwhitetip_PRM[,1:1000], 1, sd)
  
  #total
  marshallislands_oceanicwhitetip$total = rowMeans(marshallislands_oceanicwhitetip_total[,1:1000])
  marshallislands_oceanicwhitetip$total.sd = apply(marshallislands_oceanicwhitetip_total[,1:1000], 1, sd)
  
  #### hammerhead sharks ####
  #row 9 in mortality; need to change this for each species when drawing rates)
  r_hammerhead = 9
  
  marshallislands_hammerhead <- marshallislands_comb[,c(1:4,29,30)] #create dataframe
  
  marshallislands_hammerhead_catches <- matrix(nrow=n, ncol=1000)
  marshallislands_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
  marshallislands_hammerhead_released <- matrix(nrow=n, ncol=1000)
  marshallislands_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
  marshallislands_hammerhead_total <- matrix(nrow=n, ncol=1000)
  
  marshallislands_hammerhead_CM_rate <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead])
  marshallislands_hammerhead_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      marshallislands_hammerhead_catches[j,i] <- marshallislands_hook_preds[j,i]/1000 * rlnorm(1, marshallislands_comb$hammerhead.logcpue[j], marshallislands_comb$hammerhead.logcpue.se[j])
      marshallislands_hammerhead_DOA[j,i] <- marshallislands_hammerhead_catches[j,i] * marshallislands_hammerhead_CM_rate[i]
      marshallislands_hammerhead_released[j,i] <- marshallislands_hammerhead_catches[j,i] - marshallislands_hammerhead_DOA[j,i]
      marshallislands_hammerhead_PRM[j,i] <- marshallislands_hammerhead_released[j,i] * marshallislands_hammerhead_PRM_rate[i]
      marshallislands_hammerhead_total[j,i] <- marshallislands_hammerhead_DOA[j,i] + marshallislands_hammerhead_PRM[j,i]
    }
  }
  
  #run averages
  marshallislands_hammerhead_proj <- data.frame("Species" = "Hammerhead",
                                         "catch" = NA,
                                         "catch.upr" = NA,
                                         "catch.lwr" = NA,
                                         "DOA" = NA,
                                         "DOA.upr" = NA,
                                         "DOA.lwr" = NA,
                                         "PRM" = NA,
                                         "PRM.upr" = NA,
                                         "PRM.lwr" = NA,
                                         "total" = NA,
                                         "total.upr" = NA,
                                         "total.lwr" = NA)
  
  marshallislands_hammerhead_total_catches <- colSums(marshallislands_hammerhead_catches)
  marshallislands_hammerhead_proj$catch <- mean(marshallislands_hammerhead_total_catches)
  marshallislands_hammerhead_proj$catch.upr <- quantile(marshallislands_hammerhead_total_catches, 0.95)
  marshallislands_hammerhead_proj$catch.lwr <- quantile(marshallislands_hammerhead_total_catches, 0.05)
  
  marshallislands_hammerhead_total_DOA <- colSums(marshallislands_hammerhead_DOA)
  marshallislands_hammerhead_proj$DOA <- mean(marshallislands_hammerhead_total_DOA)
  marshallislands_hammerhead_proj$DOA.upr <- quantile(marshallislands_hammerhead_total_DOA, 0.95)
  marshallislands_hammerhead_proj$DOA.lwr <- quantile(marshallislands_hammerhead_total_DOA, 0.05)
  
  marshallislands_hammerhead_total_PRM <- colSums(marshallislands_hammerhead_PRM)
  marshallislands_hammerhead_proj$PRM <- mean(marshallislands_hammerhead_total_PRM)
  marshallislands_hammerhead_proj$PRM.upr <- quantile(marshallislands_hammerhead_total_PRM, 0.95)
  marshallislands_hammerhead_proj$PRM.lwr <- quantile(marshallislands_hammerhead_total_PRM, 0.05)
  
  marshallislands_hammerhead_total_total <- colSums(marshallislands_hammerhead_total)
  marshallislands_hammerhead_proj$total <- mean(marshallislands_hammerhead_total_total)
  marshallislands_hammerhead_proj$total.upr <- quantile(marshallislands_hammerhead_total_total, 0.95)
  marshallislands_hammerhead_proj$total.lwr <- quantile(marshallislands_hammerhead_total_total, 0.05)
  
  #cell averages
  #catches
  marshallislands_hammerhead$catch <- rowMeans(marshallislands_hammerhead_catches[,1:1000])
  marshallislands_hammerhead$catch.sd = apply(marshallislands_hammerhead_catches[,1:1000], 1, sd)
  
  #DOA
  marshallislands_hammerhead$DOA <- rowMeans(marshallislands_hammerhead_DOA[,1:1000])
  marshallislands_hammerhead$DOA.sd = apply(marshallislands_hammerhead_DOA[,1:1000], 1, sd)
  
  #prm
  marshallislands_hammerhead$PRM = rowMeans(marshallislands_hammerhead_PRM[,1:1000])
  marshallislands_hammerhead$PRM.sd = apply(marshallislands_hammerhead_PRM[,1:1000], 1, sd)
  
  #total
  marshallislands_hammerhead$total = rowMeans(marshallislands_hammerhead_total[,1:1000])
  marshallislands_hammerhead$total.sd = apply(marshallislands_hammerhead_total[,1:1000], 1, sd)
  
  #### othersharks sharks ####
  #row 10 in mortality; need to change this for each species when drawing rates)
  r_othersharks = 10
  
  marshallislands_othersharks <- marshallislands_comb[,c(1:4,33,34)] #create dataframe
  
  marshallislands_othersharks_catches <- matrix(nrow=n, ncol=1000)
  marshallislands_othersharks_DOA <- matrix(nrow=n, ncol=1000)
  marshallislands_othersharks_released <- matrix(nrow=n, ncol=1000)
  marshallislands_othersharks_PRM<- matrix(nrow=n, ncol=1000)
  marshallislands_othersharks_total <- matrix(nrow=n, ncol=1000)
  
  marshallislands_othersharks_CM_rate <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks])
  marshallislands_othersharks_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      marshallislands_othersharks_catches[j,i] <- marshallislands_hook_preds[j,i]/1000 * rlnorm(1, marshallislands_comb$othersharks.logcpue[j], marshallislands_comb$othersharks.logcpue.se[j])
      marshallislands_othersharks_DOA[j,i] <- marshallislands_othersharks_catches[j,i] * marshallislands_othersharks_CM_rate[i]
      marshallislands_othersharks_released[j,i] <- marshallislands_othersharks_catches[j,i] - marshallislands_othersharks_DOA[j,i]
      marshallislands_othersharks_PRM[j,i] <- marshallislands_othersharks_released[j,i] * marshallislands_othersharks_PRM_rate[i]
      marshallislands_othersharks_total[j,i] <- marshallislands_othersharks_DOA[j,i] + marshallislands_othersharks_PRM[j,i]
    }
  }
  
  #run averages
  marshallislands_othersharks_proj <- data.frame("Species" = "Other Sharks",
                                          "catch" = NA,
                                          "catch.upr" = NA,
                                          "catch.lwr" = NA,
                                          "DOA" = NA,
                                          "DOA.upr" = NA,
                                          "DOA.lwr" = NA,
                                          "PRM" = NA,
                                          "PRM.upr" = NA,
                                          "PRM.lwr" = NA,
                                          "total" = NA,
                                          "total.upr" = NA,
                                          "total.lwr" = NA)
  
  marshallislands_othersharks_total_catches <- colSums(marshallislands_othersharks_catches)
  marshallislands_othersharks_proj$catch <- mean(marshallislands_othersharks_total_catches)
  marshallislands_othersharks_proj$catch.upr <- quantile(marshallislands_othersharks_total_catches, 0.95)
  marshallislands_othersharks_proj$catch.lwr <- quantile(marshallislands_othersharks_total_catches, 0.05)
  
  marshallislands_othersharks_total_DOA <- colSums(marshallislands_othersharks_DOA)
  marshallislands_othersharks_proj$DOA <- mean(marshallislands_othersharks_total_DOA)
  marshallislands_othersharks_proj$DOA.upr <- quantile(marshallislands_othersharks_total_DOA, 0.95)
  marshallislands_othersharks_proj$DOA.lwr <- quantile(marshallislands_othersharks_total_DOA, 0.05)
  
  marshallislands_othersharks_total_PRM <- colSums(marshallislands_othersharks_PRM)
  marshallislands_othersharks_proj$PRM <- mean(marshallislands_othersharks_total_PRM)
  marshallislands_othersharks_proj$PRM.upr <- quantile(marshallislands_othersharks_total_PRM, 0.95)
  marshallislands_othersharks_proj$PRM.lwr <- quantile(marshallislands_othersharks_total_PRM, 0.05)
  
  marshallislands_othersharks_total_total <- colSums(marshallislands_othersharks_total)
  marshallislands_othersharks_proj$total <- mean(marshallislands_othersharks_total_total)
  marshallislands_othersharks_proj$total.upr <- quantile(marshallislands_othersharks_total_total, 0.95)
  marshallislands_othersharks_proj$total.lwr <- quantile(marshallislands_othersharks_total_total, 0.05)
  
  #cell averages
  #catches
  marshallislands_othersharks$catch <- rowMeans(marshallislands_othersharks_catches[,1:1000])
  marshallislands_othersharks$catch.sd = apply(marshallislands_othersharks_catches[,1:1000], 1, sd)
  
  #DOA
  marshallislands_othersharks$DOA <- rowMeans(marshallislands_othersharks_DOA[,1:1000])
  marshallislands_othersharks$DOA.sd = apply(marshallislands_othersharks_DOA[,1:1000], 1, sd)
  
  #prm
  marshallislands_othersharks$PRM = rowMeans(marshallislands_othersharks_PRM[,1:1000])
  marshallislands_othersharks$PRM.sd = apply(marshallislands_othersharks_PRM[,1:1000], 1, sd)
  
  #total
  marshallislands_othersharks$total = rowMeans(marshallislands_othersharks_total[,1:1000])
  marshallislands_othersharks$total.sd = apply(marshallislands_othersharks_total[,1:1000], 1, sd)
  
  #### marshallislands Totals ####
  
  marshallislands_bsh$Species = "Blue Shark"
  marshallislands_silky$Species = "Silky Shark"
  marshallislands_thresher$Species="Thresher"
  marshallislands_mako$Species="Mako"
  marshallislands_oceanicwhitetip$Species="Oceanic Whitetip"
  marshallislands_hammerhead$Species="Hammerhead"
  marshallislands_othersharks$Species="Other Sharks"
  
  colnames(marshallislands_bsh)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(marshallislands_silky)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(marshallislands_thresher)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(marshallislands_mako)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(marshallislands_oceanicwhitetip)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(marshallislands_hammerhead)[c(5,6)] <- c("logcpue","logcpue.se")
  colnames(marshallislands_othersharks)[c(5,6)] <- c("logcpue","logcpue.se")
  
  marshallislands_total_preds <- rbind(marshallislands_bsh,
                                marshallislands_silky,
                                marshallislands_thresher,
                                marshallislands_mako,
                                marshallislands_oceanicwhitetip,
                                marshallislands_hammerhead,
                                marshallislands_othersharks)
  
  end.time <- Sys.time()
  end.time - start.time}

write.csv(marshallislands_total_preds, "marshallislands_total_preds.csv", row.names = F)

#### summary ####
marshallislands_summary <- rbind(marshallislands_bsh_proj,
                          marshallislands_silky_proj,
                          marshallislands_thresher_proj,
                          marshallislands_mako_proj,
                          marshallislands_oceanicwhitetip_proj,
                          marshallislands_hammerhead_proj,
                          marshallislands_othersharks_proj)

write.csv(marshallislands_summary, "marshallislands_summary.csv", row.names = F)

#plotting for Fig1
setwd('~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir')
marshallislands_mortality <- read.csv("marshallislands_total_preds.csv")
marshallislands_proj <- aggregate(data=marshallislands_mortality, total ~ Lat + Long, FUN='sum')
dev.off()
world <- map_data("world")
marshallislands_map <- ggplot(data=marshallislands_proj, aes(x=Long, y=Lat)) +
  geom_tile(aes(fill=total)) +
  scale_fill_gradient(name="Bycatch Mortality \n(# of individuals)", low="khaki1", high="red",
                      breaks=c(0,50,100,150,200),
                      labels=c(0,50,100,150,200),
                      limits=c(0,200)) +
  geom_polygon(data = mhl_shp, aes(x = long, y = lat, group = group), color="black", fill=NA, size = 0.6) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  scale_x_continuous(limits=c(156,177), breaks=seq(160,175,by=5)) +
  scale_y_continuous(limits=c(0.5,19), breaks=seq(5,15,by=5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill="white", colour = "black"),
        panel.grid.major = element_line(colour="grey97")) +
  labs(subtitle="Marshall Islands")




