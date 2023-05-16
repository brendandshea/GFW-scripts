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
  
  cookislands_bsh_catches <- matrix(nrow=n, ncol=1000)
  cookislands_bsh_DOA <- matrix(nrow=n, ncol=1000)
  cookislands_bsh_released <- matrix(nrow=n, ncol=1000)
  cookislands_bsh_PRM<- matrix(nrow=n, ncol=1000)
  cookislands_bsh_total <- matrix(nrow=n, ncol=1000)
  
  cookislands_bsh_CM_rate <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh])
  cookislands_bsh_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      cookislands_bsh_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$blueshark.logcpue[j], cookislands_comb$blueshark.logcpue.se[j])
      cookislands_bsh_DOA[j,i] <- cookislands_bsh_catches[j,i] *  cookislands_bsh_CM_rate[i]
      cookislands_bsh_released[j,i] <- cookislands_bsh_catches[j,i] - cookislands_bsh_DOA[j,i]
      cookislands_bsh_PRM[j,i] <- cookislands_bsh_released[j,i] * cookislands_bsh_PRM_rate[i] 
      cookislands_bsh_total[j,i] <- cookislands_bsh_DOA[j,i] + cookislands_bsh_PRM[j,i]
    }
  }
  
  #run averages
  cookislands_bsh_proj <- data.frame("Species" = "Blue Shark",
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
  
  cookislands_bsh_total_catches <- colSums(cookislands_bsh_catches)
  cookislands_bsh_proj$catch <- mean(cookislands_bsh_total_catches)
  cookislands_bsh_proj$catch.upr <- quantile(cookislands_bsh_total_catches, 0.95)
  cookislands_bsh_proj$catch.lwr <- quantile(cookislands_bsh_total_catches, 0.05)
  
  cookislands_bsh_total_DOA <- colSums(cookislands_bsh_DOA)
  cookislands_bsh_proj$DOA <- mean(cookislands_bsh_total_DOA)
  cookislands_bsh_proj$DOA.upr <- quantile(cookislands_bsh_total_DOA, 0.95)
  cookislands_bsh_proj$DOA.lwr <- quantile(cookislands_bsh_total_DOA, 0.05)
  
  cookislands_bsh_total_PRM <- colSums(cookislands_bsh_PRM)
  cookislands_bsh_proj$PRM <- mean(cookislands_bsh_total_PRM)
  cookislands_bsh_proj$PRM.upr <- quantile(cookislands_bsh_total_PRM, 0.95)
  cookislands_bsh_proj$PRM.lwr <- quantile(cookislands_bsh_total_PRM, 0.05)
  
  cookislands_bsh_total_total <- colSums(cookislands_bsh_total)
  cookislands_bsh_proj$total <- mean(cookislands_bsh_total_total)
  cookislands_bsh_proj$total.upr <- quantile(cookislands_bsh_total_total, 0.95)
  cookislands_bsh_proj$total.lwr <- quantile(cookislands_bsh_total_total, 0.05)
  
  #cell averages
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
  
  cookislands_silky_catches <- matrix(nrow=n, ncol=1000)
  cookislands_silky_DOA <- matrix(nrow=n, ncol=1000)
  cookislands_silky_released <- matrix(nrow=n, ncol=1000)
  cookislands_silky_PRM<- matrix(nrow=n, ncol=1000)
  cookislands_silky_total <- matrix(nrow=n, ncol=1000)
  
  cookislands_silky_CM_rate <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky])
  cookislands_silky_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      cookislands_silky_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$silkyshark.logcpue[j], cookislands_comb$silkyshark.logcpue.se[j])
      cookislands_silky_DOA[j,i] <- cookislands_silky_catches[j,i] * cookislands_silky_CM_rate[i]
      cookislands_silky_released[j,i] <- cookislands_silky_catches[j,i] - cookislands_silky_DOA[j,i]
      cookislands_silky_PRM[j,i] <- cookislands_silky_released[j,i] * cookislands_silky_PRM_rate[i]
      cookislands_silky_total[j,i] <- cookislands_silky_DOA[j,i] + cookislands_silky_PRM[j,i]
    }
  }
  cookislands_silky_proj <- data.frame("Species" = "Silky Shark",
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
  
  cookislands_silky_total_catches <- colSums(cookislands_silky_catches)
  cookislands_silky_proj$catch <- mean(cookislands_silky_total_catches)
  cookislands_silky_proj$catch.upr <- quantile(cookislands_silky_total_catches, 0.95)
  cookislands_silky_proj$catch.lwr <- quantile(cookislands_silky_total_catches, 0.05)
  
  cookislands_silky_total_DOA <- colSums(cookislands_silky_DOA)
  cookislands_silky_proj$DOA <- mean(cookislands_silky_total_DOA)
  cookislands_silky_proj$DOA.upr <- quantile(cookislands_silky_total_DOA, 0.95)
  cookislands_silky_proj$DOA.lwr <- quantile(cookislands_silky_total_DOA, 0.05)
  
  cookislands_silky_total_PRM <- colSums(cookislands_silky_PRM)
  cookislands_silky_proj$PRM <- mean(cookislands_silky_total_PRM)
  cookislands_silky_proj$PRM.upr <- quantile(cookislands_silky_total_PRM, 0.95)
  cookislands_silky_proj$PRM.lwr <- quantile(cookislands_silky_total_PRM, 0.05)
  
  cookislands_silky_total_total <- colSums(cookislands_silky_total)
  cookislands_silky_proj$total <- mean(cookislands_silky_total_total)
  cookislands_silky_proj$total.upr <- quantile(cookislands_silky_total_total, 0.95)
  cookislands_silky_proj$total.lwr <- quantile(cookislands_silky_total_total, 0.05)
  
  #cell averages
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
  
  cookislands_thresher_catches <- matrix(nrow=n, ncol=1000)
  cookislands_thresher_DOA <- matrix(nrow=n, ncol=1000)
  cookislands_thresher_released <- matrix(nrow=n, ncol=1000)
  cookislands_thresher_PRM<- matrix(nrow=n, ncol=1000)
  cookislands_thresher_total <- matrix(nrow=n, ncol=1000)
  
  cookislands_thresher_CM_rate <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher])
  cookislands_thresher_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      cookislands_thresher_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$thresher.logcpue[j], cookislands_comb$thresher.logcpue.se[j])
      cookislands_thresher_DOA[j,i] <- cookislands_thresher_catches[j,i] * cookislands_thresher_CM_rate[i]
      cookislands_thresher_released[j,i] <- cookislands_thresher_catches[j,i] - cookislands_thresher_DOA[j,i]
      cookislands_thresher_PRM[j,i] <- cookislands_thresher_released[j,i] * cookislands_thresher_PRM_rate[i]
      cookislands_thresher_total[j,i] <- cookislands_thresher_DOA[j,i] + cookislands_thresher_PRM[j,i]
    }
  }
  
  cookislands_thresher_proj <- data.frame("Species" = "Thresher",
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
  
  cookislands_thresher_total_catches <- colSums(cookislands_thresher_catches)
  cookislands_thresher_proj$catch <- mean(cookislands_thresher_total_catches)
  cookislands_thresher_proj$catch.upr <- quantile(cookislands_thresher_total_catches, 0.95)
  cookislands_thresher_proj$catch.lwr <- quantile(cookislands_thresher_total_catches, 0.05)
  
  cookislands_thresher_total_DOA <- colSums(cookislands_thresher_DOA)
  cookislands_thresher_proj$DOA <- mean(cookislands_thresher_total_DOA)
  cookislands_thresher_proj$DOA.upr <- quantile(cookislands_thresher_total_DOA, 0.95)
  cookislands_thresher_proj$DOA.lwr <- quantile(cookislands_thresher_total_DOA, 0.05)
  
  cookislands_thresher_total_PRM <- colSums(cookislands_thresher_PRM)
  cookislands_thresher_proj$PRM <- mean(cookislands_thresher_total_PRM)
  cookislands_thresher_proj$PRM.upr <- quantile(cookislands_thresher_total_PRM, 0.95)
  cookislands_thresher_proj$PRM.lwr <- quantile(cookislands_thresher_total_PRM, 0.05)
  
  cookislands_thresher_total_total <- colSums(cookislands_thresher_total)
  cookislands_thresher_proj$total <- mean(cookislands_thresher_total_total)
  cookislands_thresher_proj$total.upr <- quantile(cookislands_thresher_total_total, 0.95)
  cookislands_thresher_proj$total.lwr <- quantile(cookislands_thresher_total_total, 0.05)
  
  #cell averages
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
  
  cookislands_mako_catches <- matrix(nrow=n, ncol=1000)
  cookislands_mako_DOA <- matrix(nrow=n, ncol=1000)
  cookislands_mako_released <- matrix(nrow=n, ncol=1000)
  cookislands_mako_PRM<- matrix(nrow=n, ncol=1000)
  cookislands_mako_total <- matrix(nrow=n, ncol=1000)
  
  cookislands_mako_CM_rate <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako])
  cookislands_mako_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      cookislands_mako_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$mako.logcpue[j], cookislands_comb$mako.logcpue.se[j])
      cookislands_mako_DOA[j,i] <- cookislands_mako_catches[j,i] * cookislands_mako_CM_rate[i]
      cookislands_mako_released[j,i] <- cookislands_mako_catches[j,i] - cookislands_mako_DOA[j,i]
      cookislands_mako_PRM[j,i] <- cookislands_mako_released[j,i] * cookislands_mako_PRM_rate[i]
      cookislands_mako_total[j,i] <- cookislands_mako_DOA[j,i] + cookislands_mako_PRM[j,i]
    }
  }
  
  #run averages
  cookislands_mako_proj <- data.frame("Species" = "Mako Shark",
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
  
  cookislands_mako_total_catches <- colSums(cookislands_mako_catches)
  cookislands_mako_proj$catch <- mean(cookislands_mako_total_catches)
  cookislands_mako_proj$catch.upr <- quantile(cookislands_mako_total_catches, 0.95)
  cookislands_mako_proj$catch.lwr <- quantile(cookislands_mako_total_catches, 0.05)
  
  cookislands_mako_total_DOA <- colSums(cookislands_mako_DOA)
  cookislands_mako_proj$DOA <- mean(cookislands_mako_total_DOA)
  cookislands_mako_proj$DOA.upr <- quantile(cookislands_mako_total_DOA, 0.95)
  cookislands_mako_proj$DOA.lwr <- quantile(cookislands_mako_total_DOA, 0.05)
  
  cookislands_mako_total_PRM <- colSums(cookislands_mako_PRM)
  cookislands_mako_proj$PRM <- mean(cookislands_mako_total_PRM)
  cookislands_mako_proj$PRM.upr <- quantile(cookislands_mako_total_PRM, 0.95)
  cookislands_mako_proj$PRM.lwr <- quantile(cookislands_mako_total_PRM, 0.05)
  
  cookislands_mako_total_total <- colSums(cookislands_mako_total)
  cookislands_mako_proj$total <- mean(cookislands_mako_total_total)
  cookislands_mako_proj$total.upr <- quantile(cookislands_mako_total_total, 0.95)
  cookislands_mako_proj$total.lwr <- quantile(cookislands_mako_total_total, 0.05)
  
  #cell averages
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
  
  cookislands_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
  cookislands_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
  cookislands_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
  cookislands_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
  cookislands_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)
  
  cookislands_oceanicwhitetip_CM_rate <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip])
  cookislands_oceanicwhitetip_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      cookislands_oceanicwhitetip_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$oceanicwhitetip.logcpue[j], cookislands_comb$oceanicwhitetip.logcpue.se[j])
      cookislands_oceanicwhitetip_DOA[j,i] <- cookislands_oceanicwhitetip_catches[j,i] * cookislands_oceanicwhitetip_CM_rate[i]
      cookislands_oceanicwhitetip_released[j,i] <- cookislands_oceanicwhitetip_catches[j,i] - cookislands_oceanicwhitetip_DOA[j,i]
      cookislands_oceanicwhitetip_PRM[j,i] <- cookislands_oceanicwhitetip_released[j,i] * cookislands_oceanicwhitetip_PRM_rate[i]
      cookislands_oceanicwhitetip_total[j,i] <- cookislands_oceanicwhitetip_DOA[j,i] + cookislands_oceanicwhitetip_PRM[j,i]
    }
  }
  
  #run averages
  cookislands_oceanicwhitetip_proj <- data.frame("Species" = "Oceanic Whitetip",
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
  
  cookislands_oceanicwhitetip_total_catches <- colSums(cookislands_oceanicwhitetip_catches)
  cookislands_oceanicwhitetip_proj$catch <- mean(cookislands_oceanicwhitetip_total_catches)
  cookislands_oceanicwhitetip_proj$catch.upr <- quantile(cookislands_oceanicwhitetip_total_catches, 0.95)
  cookislands_oceanicwhitetip_proj$catch.lwr <- quantile(cookislands_oceanicwhitetip_total_catches, 0.05)
  
  cookislands_oceanicwhitetip_total_DOA <- colSums(cookislands_oceanicwhitetip_DOA)
  cookislands_oceanicwhitetip_proj$DOA <- mean(cookislands_oceanicwhitetip_total_DOA)
  cookislands_oceanicwhitetip_proj$DOA.upr <- quantile(cookislands_oceanicwhitetip_total_DOA, 0.95)
  cookislands_oceanicwhitetip_proj$DOA.lwr <- quantile(cookislands_oceanicwhitetip_total_DOA, 0.05)
  
  cookislands_oceanicwhitetip_total_PRM <- colSums(cookislands_oceanicwhitetip_PRM)
  cookislands_oceanicwhitetip_proj$PRM <- mean(cookislands_oceanicwhitetip_total_PRM)
  cookislands_oceanicwhitetip_proj$PRM.upr <- quantile(cookislands_oceanicwhitetip_total_PRM, 0.95)
  cookislands_oceanicwhitetip_proj$PRM.lwr <- quantile(cookislands_oceanicwhitetip_total_PRM, 0.05)
  
  cookislands_oceanicwhitetip_total_total <- colSums(cookislands_oceanicwhitetip_total)
  cookislands_oceanicwhitetip_proj$total <- mean(cookislands_oceanicwhitetip_total_total)
  cookislands_oceanicwhitetip_proj$total.upr <- quantile(cookislands_oceanicwhitetip_total_total, 0.95)
  cookislands_oceanicwhitetip_proj$total.lwr <- quantile(cookislands_oceanicwhitetip_total_total, 0.05)
  
  #cell averages
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
  
  cookislands_hammerhead_catches <- matrix(nrow=n, ncol=1000)
  cookislands_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
  cookislands_hammerhead_released <- matrix(nrow=n, ncol=1000)
  cookislands_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
  cookislands_hammerhead_total <- matrix(nrow=n, ncol=1000)
  
  cookislands_hammerhead_CM_rate <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead])
  cookislands_hammerhead_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      cookislands_hammerhead_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$hammerhead.logcpue[j], cookislands_comb$hammerhead.logcpue.se[j])
      cookislands_hammerhead_DOA[j,i] <- cookislands_hammerhead_catches[j,i] * cookislands_hammerhead_CM_rate[i]
      cookislands_hammerhead_released[j,i] <- cookislands_hammerhead_catches[j,i] - cookislands_hammerhead_DOA[j,i]
      cookislands_hammerhead_PRM[j,i] <- cookislands_hammerhead_released[j,i] * cookislands_hammerhead_PRM_rate[i]
      cookislands_hammerhead_total[j,i] <- cookislands_hammerhead_DOA[j,i] + cookislands_hammerhead_PRM[j,i]
    }
  }
  
  #run averages
  cookislands_hammerhead_proj <- data.frame("Species" = "Hammerhead",
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
  
  cookislands_hammerhead_total_catches <- colSums(cookislands_hammerhead_catches)
  cookislands_hammerhead_proj$catch <- mean(cookislands_hammerhead_total_catches)
  cookislands_hammerhead_proj$catch.upr <- quantile(cookislands_hammerhead_total_catches, 0.95)
  cookislands_hammerhead_proj$catch.lwr <- quantile(cookislands_hammerhead_total_catches, 0.05)
  
  cookislands_hammerhead_total_DOA <- colSums(cookislands_hammerhead_DOA)
  cookislands_hammerhead_proj$DOA <- mean(cookislands_hammerhead_total_DOA)
  cookislands_hammerhead_proj$DOA.upr <- quantile(cookislands_hammerhead_total_DOA, 0.95)
  cookislands_hammerhead_proj$DOA.lwr <- quantile(cookislands_hammerhead_total_DOA, 0.05)
  
  cookislands_hammerhead_total_PRM <- colSums(cookislands_hammerhead_PRM)
  cookislands_hammerhead_proj$PRM <- mean(cookislands_hammerhead_total_PRM)
  cookislands_hammerhead_proj$PRM.upr <- quantile(cookislands_hammerhead_total_PRM, 0.95)
  cookislands_hammerhead_proj$PRM.lwr <- quantile(cookislands_hammerhead_total_PRM, 0.05)
  
  cookislands_hammerhead_total_total <- colSums(cookislands_hammerhead_total)
  cookislands_hammerhead_proj$total <- mean(cookislands_hammerhead_total_total)
  cookislands_hammerhead_proj$total.upr <- quantile(cookislands_hammerhead_total_total, 0.95)
  cookislands_hammerhead_proj$total.lwr <- quantile(cookislands_hammerhead_total_total, 0.05)
  
  #cell averages
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
  
  cookislands_othersharks_catches <- matrix(nrow=n, ncol=1000)
  cookislands_othersharks_DOA <- matrix(nrow=n, ncol=1000)
  cookislands_othersharks_released <- matrix(nrow=n, ncol=1000)
  cookislands_othersharks_PRM<- matrix(nrow=n, ncol=1000)
  cookislands_othersharks_total <- matrix(nrow=n, ncol=1000)
  
  cookislands_othersharks_CM_rate <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks])
  cookislands_othersharks_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      cookislands_othersharks_catches[j,i] <- cookislands_hook_preds[j,i]/1000 * rlnorm(1, cookislands_comb$othersharks.logcpue[j], cookislands_comb$othersharks.logcpue.se[j])
      cookislands_othersharks_DOA[j,i] <- cookislands_othersharks_catches[j,i] * cookislands_othersharks_CM_rate[i]
      cookislands_othersharks_released[j,i] <- cookislands_othersharks_catches[j,i] - cookislands_othersharks_DOA[j,i]
      cookislands_othersharks_PRM[j,i] <- cookislands_othersharks_released[j,i] * cookislands_othersharks_PRM_rate[i]
      cookislands_othersharks_total[j,i] <- cookislands_othersharks_DOA[j,i] + cookislands_othersharks_PRM[j,i]
    }
  }
  
  #run averages
  cookislands_othersharks_proj <- data.frame("Species" = "Other Sharks",
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
  
  cookislands_othersharks_total_catches <- colSums(cookislands_othersharks_catches)
  cookislands_othersharks_proj$catch <- mean(cookislands_othersharks_total_catches)
  cookislands_othersharks_proj$catch.upr <- quantile(cookislands_othersharks_total_catches, 0.95)
  cookislands_othersharks_proj$catch.lwr <- quantile(cookislands_othersharks_total_catches, 0.05)
  
  cookislands_othersharks_total_DOA <- colSums(cookislands_othersharks_DOA)
  cookislands_othersharks_proj$DOA <- mean(cookislands_othersharks_total_DOA)
  cookislands_othersharks_proj$DOA.upr <- quantile(cookislands_othersharks_total_DOA, 0.95)
  cookislands_othersharks_proj$DOA.lwr <- quantile(cookislands_othersharks_total_DOA, 0.05)
  
  cookislands_othersharks_total_PRM <- colSums(cookislands_othersharks_PRM)
  cookislands_othersharks_proj$PRM <- mean(cookislands_othersharks_total_PRM)
  cookislands_othersharks_proj$PRM.upr <- quantile(cookislands_othersharks_total_PRM, 0.95)
  cookislands_othersharks_proj$PRM.lwr <- quantile(cookislands_othersharks_total_PRM, 0.05)
  
  cookislands_othersharks_total_total <- colSums(cookislands_othersharks_total)
  cookislands_othersharks_proj$total <- mean(cookislands_othersharks_total_total)
  cookislands_othersharks_proj$total.upr <- quantile(cookislands_othersharks_total_total, 0.95)
  cookislands_othersharks_proj$total.lwr <- quantile(cookislands_othersharks_total_total, 0.05)
  
  #cell averages
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
cookislands_summary <- rbind(cookislands_bsh_proj,
                          cookislands_silky_proj,
                          cookislands_thresher_proj,
                          cookislands_mako_proj,
                          cookislands_oceanicwhitetip_proj,
                          cookislands_hammerhead_proj,
                          cookislands_othersharks_proj)

write.csv(cookislands_summary, "cookislands_summary.csv", row.names = F)

#plotting for Fig3
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
