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
  
  newcaledonia_bsh_CM_rate <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh])
  newcaledonia_bsh_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      newcaledonia_bsh_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$blueshark.logcpue[j], newcaledonia_comb$blueshark.logcpue.se[j])
      newcaledonia_bsh_DOA[j,i] <- newcaledonia_bsh_catches[j,i] *  newcaledonia_bsh_CM_rate[i]
      newcaledonia_bsh_released[j,i] <- newcaledonia_bsh_catches[j,i] - newcaledonia_bsh_DOA[j,i]
      newcaledonia_bsh_PRM[j,i] <- newcaledonia_bsh_released[j,i] * newcaledonia_bsh_PRM_rate[i] 
      newcaledonia_bsh_total[j,i] <- newcaledonia_bsh_DOA[j,i] + newcaledonia_bsh_PRM[j,i]
    }
  }
  
  #run averages
  newcaledonia_bsh_proj <- data.frame("Species" = "Blue Shark",
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
  
  newcaledonia_bsh_total_catches <- colSums(newcaledonia_bsh_catches)
  newcaledonia_bsh_proj$catch <- mean(newcaledonia_bsh_total_catches)
  newcaledonia_bsh_proj$catch.upr <- quantile(newcaledonia_bsh_total_catches, 0.95)
  newcaledonia_bsh_proj$catch.lwr <- quantile(newcaledonia_bsh_total_catches, 0.05)
  
  newcaledonia_bsh_total_DOA <- colSums(newcaledonia_bsh_DOA)
  newcaledonia_bsh_proj$DOA <- mean(newcaledonia_bsh_total_DOA)
  newcaledonia_bsh_proj$DOA.upr <- quantile(newcaledonia_bsh_total_DOA, 0.95)
  newcaledonia_bsh_proj$DOA.lwr <- quantile(newcaledonia_bsh_total_DOA, 0.05)
  
  newcaledonia_bsh_total_PRM <- colSums(newcaledonia_bsh_PRM)
  newcaledonia_bsh_proj$PRM <- mean(newcaledonia_bsh_total_PRM)
  newcaledonia_bsh_proj$PRM.upr <- quantile(newcaledonia_bsh_total_PRM, 0.95)
  newcaledonia_bsh_proj$PRM.lwr <- quantile(newcaledonia_bsh_total_PRM, 0.05)
  
  newcaledonia_bsh_total_total <- colSums(newcaledonia_bsh_total)
  newcaledonia_bsh_proj$total <- mean(newcaledonia_bsh_total_total)
  newcaledonia_bsh_proj$total.upr <- quantile(newcaledonia_bsh_total_total, 0.95)
  newcaledonia_bsh_proj$total.lwr <- quantile(newcaledonia_bsh_total_total, 0.05)
  
  #cell averages
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
  
  newcaledonia_silky_CM_rate <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky])
  newcaledonia_silky_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      newcaledonia_silky_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$silkyshark.logcpue[j], newcaledonia_comb$silkyshark.logcpue.se[j])
      newcaledonia_silky_DOA[j,i] <- newcaledonia_silky_catches[j,i] * newcaledonia_silky_CM_rate[i]
      newcaledonia_silky_released[j,i] <- newcaledonia_silky_catches[j,i] - newcaledonia_silky_DOA[j,i]
      newcaledonia_silky_PRM[j,i] <- newcaledonia_silky_released[j,i] * newcaledonia_silky_PRM_rate[i]
      newcaledonia_silky_total[j,i] <- newcaledonia_silky_DOA[j,i] + newcaledonia_silky_PRM[j,i]
    }
  }
  newcaledonia_silky_proj <- data.frame("Species" = "Silky Shark",
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
  
  newcaledonia_silky_total_catches <- colSums(newcaledonia_silky_catches)
  newcaledonia_silky_proj$catch <- mean(newcaledonia_silky_total_catches)
  newcaledonia_silky_proj$catch.upr <- quantile(newcaledonia_silky_total_catches, 0.95)
  newcaledonia_silky_proj$catch.lwr <- quantile(newcaledonia_silky_total_catches, 0.05)
  
  newcaledonia_silky_total_DOA <- colSums(newcaledonia_silky_DOA)
  newcaledonia_silky_proj$DOA <- mean(newcaledonia_silky_total_DOA)
  newcaledonia_silky_proj$DOA.upr <- quantile(newcaledonia_silky_total_DOA, 0.95)
  newcaledonia_silky_proj$DOA.lwr <- quantile(newcaledonia_silky_total_DOA, 0.05)
  
  newcaledonia_silky_total_PRM <- colSums(newcaledonia_silky_PRM)
  newcaledonia_silky_proj$PRM <- mean(newcaledonia_silky_total_PRM)
  newcaledonia_silky_proj$PRM.upr <- quantile(newcaledonia_silky_total_PRM, 0.95)
  newcaledonia_silky_proj$PRM.lwr <- quantile(newcaledonia_silky_total_PRM, 0.05)
  
  newcaledonia_silky_total_total <- colSums(newcaledonia_silky_total)
  newcaledonia_silky_proj$total <- mean(newcaledonia_silky_total_total)
  newcaledonia_silky_proj$total.upr <- quantile(newcaledonia_silky_total_total, 0.95)
  newcaledonia_silky_proj$total.lwr <- quantile(newcaledonia_silky_total_total, 0.05)
  
  #cell averages
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
  
  newcaledonia_thresher_CM_rate <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher])
  newcaledonia_thresher_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      newcaledonia_thresher_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$thresher.logcpue[j], newcaledonia_comb$thresher.logcpue.se[j])
      newcaledonia_thresher_DOA[j,i] <- newcaledonia_thresher_catches[j,i] * newcaledonia_thresher_CM_rate[i]
      newcaledonia_thresher_released[j,i] <- newcaledonia_thresher_catches[j,i] - newcaledonia_thresher_DOA[j,i]
      newcaledonia_thresher_PRM[j,i] <- newcaledonia_thresher_released[j,i] * newcaledonia_thresher_PRM_rate[i]
      newcaledonia_thresher_total[j,i] <- newcaledonia_thresher_DOA[j,i] + newcaledonia_thresher_PRM[j,i]
    }
  }
  
  newcaledonia_thresher_proj <- data.frame("Species" = "Thresher",
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
  
  newcaledonia_thresher_total_catches <- colSums(newcaledonia_thresher_catches)
  newcaledonia_thresher_proj$catch <- mean(newcaledonia_thresher_total_catches)
  newcaledonia_thresher_proj$catch.upr <- quantile(newcaledonia_thresher_total_catches, 0.95)
  newcaledonia_thresher_proj$catch.lwr <- quantile(newcaledonia_thresher_total_catches, 0.05)
  
  newcaledonia_thresher_total_DOA <- colSums(newcaledonia_thresher_DOA)
  newcaledonia_thresher_proj$DOA <- mean(newcaledonia_thresher_total_DOA)
  newcaledonia_thresher_proj$DOA.upr <- quantile(newcaledonia_thresher_total_DOA, 0.95)
  newcaledonia_thresher_proj$DOA.lwr <- quantile(newcaledonia_thresher_total_DOA, 0.05)
  
  newcaledonia_thresher_total_PRM <- colSums(newcaledonia_thresher_PRM)
  newcaledonia_thresher_proj$PRM <- mean(newcaledonia_thresher_total_PRM)
  newcaledonia_thresher_proj$PRM.upr <- quantile(newcaledonia_thresher_total_PRM, 0.95)
  newcaledonia_thresher_proj$PRM.lwr <- quantile(newcaledonia_thresher_total_PRM, 0.05)
  
  newcaledonia_thresher_total_total <- colSums(newcaledonia_thresher_total)
  newcaledonia_thresher_proj$total <- mean(newcaledonia_thresher_total_total)
  newcaledonia_thresher_proj$total.upr <- quantile(newcaledonia_thresher_total_total, 0.95)
  newcaledonia_thresher_proj$total.lwr <- quantile(newcaledonia_thresher_total_total, 0.05)
  
  #cell averages
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
  
  newcaledonia_mako_CM_rate <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako])
  newcaledonia_mako_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      newcaledonia_mako_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$mako.logcpue[j], newcaledonia_comb$mako.logcpue.se[j])
      newcaledonia_mako_DOA[j,i] <- newcaledonia_mako_catches[j,i] * newcaledonia_mako_CM_rate[i]
      newcaledonia_mako_released[j,i] <- newcaledonia_mako_catches[j,i] - newcaledonia_mako_DOA[j,i]
      newcaledonia_mako_PRM[j,i] <- newcaledonia_mako_released[j,i] * newcaledonia_mako_PRM_rate[i]
      newcaledonia_mako_total[j,i] <- newcaledonia_mako_DOA[j,i] + newcaledonia_mako_PRM[j,i]
    }
  }
  
  #run averages
  newcaledonia_mako_proj <- data.frame("Species" = "Mako Shark",
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
  
  newcaledonia_mako_total_catches <- colSums(newcaledonia_mako_catches)
  newcaledonia_mako_proj$catch <- mean(newcaledonia_mako_total_catches)
  newcaledonia_mako_proj$catch.upr <- quantile(newcaledonia_mako_total_catches, 0.95)
  newcaledonia_mako_proj$catch.lwr <- quantile(newcaledonia_mako_total_catches, 0.05)
  
  newcaledonia_mako_total_DOA <- colSums(newcaledonia_mako_DOA)
  newcaledonia_mako_proj$DOA <- mean(newcaledonia_mako_total_DOA)
  newcaledonia_mako_proj$DOA.upr <- quantile(newcaledonia_mako_total_DOA, 0.95)
  newcaledonia_mako_proj$DOA.lwr <- quantile(newcaledonia_mako_total_DOA, 0.05)
  
  newcaledonia_mako_total_PRM <- colSums(newcaledonia_mako_PRM)
  newcaledonia_mako_proj$PRM <- mean(newcaledonia_mako_total_PRM)
  newcaledonia_mako_proj$PRM.upr <- quantile(newcaledonia_mako_total_PRM, 0.95)
  newcaledonia_mako_proj$PRM.lwr <- quantile(newcaledonia_mako_total_PRM, 0.05)
  
  newcaledonia_mako_total_total <- colSums(newcaledonia_mako_total)
  newcaledonia_mako_proj$total <- mean(newcaledonia_mako_total_total)
  newcaledonia_mako_proj$total.upr <- quantile(newcaledonia_mako_total_total, 0.95)
  newcaledonia_mako_proj$total.lwr <- quantile(newcaledonia_mako_total_total, 0.05)
  
  #cell averages
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
  
  newcaledonia_oceanicwhitetip_CM_rate <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip])
  newcaledonia_oceanicwhitetip_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      newcaledonia_oceanicwhitetip_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$oceanicwhitetip.logcpue[j], newcaledonia_comb$oceanicwhitetip.logcpue.se[j])
      newcaledonia_oceanicwhitetip_DOA[j,i] <- newcaledonia_oceanicwhitetip_catches[j,i] * newcaledonia_oceanicwhitetip_CM_rate[i]
      newcaledonia_oceanicwhitetip_released[j,i] <- newcaledonia_oceanicwhitetip_catches[j,i] - newcaledonia_oceanicwhitetip_DOA[j,i]
      newcaledonia_oceanicwhitetip_PRM[j,i] <- newcaledonia_oceanicwhitetip_released[j,i] * newcaledonia_oceanicwhitetip_PRM_rate[i]
      newcaledonia_oceanicwhitetip_total[j,i] <- newcaledonia_oceanicwhitetip_DOA[j,i] + newcaledonia_oceanicwhitetip_PRM[j,i]
    }
  }
  
  #run averages
  newcaledonia_oceanicwhitetip_proj <- data.frame("Species" = "Oceanic Whitetip",
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
  
  newcaledonia_oceanicwhitetip_total_catches <- colSums(newcaledonia_oceanicwhitetip_catches)
  newcaledonia_oceanicwhitetip_proj$catch <- mean(newcaledonia_oceanicwhitetip_total_catches)
  newcaledonia_oceanicwhitetip_proj$catch.upr <- quantile(newcaledonia_oceanicwhitetip_total_catches, 0.95)
  newcaledonia_oceanicwhitetip_proj$catch.lwr <- quantile(newcaledonia_oceanicwhitetip_total_catches, 0.05)
  
  newcaledonia_oceanicwhitetip_total_DOA <- colSums(newcaledonia_oceanicwhitetip_DOA)
  newcaledonia_oceanicwhitetip_proj$DOA <- mean(newcaledonia_oceanicwhitetip_total_DOA)
  newcaledonia_oceanicwhitetip_proj$DOA.upr <- quantile(newcaledonia_oceanicwhitetip_total_DOA, 0.95)
  newcaledonia_oceanicwhitetip_proj$DOA.lwr <- quantile(newcaledonia_oceanicwhitetip_total_DOA, 0.05)
  
  newcaledonia_oceanicwhitetip_total_PRM <- colSums(newcaledonia_oceanicwhitetip_PRM)
  newcaledonia_oceanicwhitetip_proj$PRM <- mean(newcaledonia_oceanicwhitetip_total_PRM)
  newcaledonia_oceanicwhitetip_proj$PRM.upr <- quantile(newcaledonia_oceanicwhitetip_total_PRM, 0.95)
  newcaledonia_oceanicwhitetip_proj$PRM.lwr <- quantile(newcaledonia_oceanicwhitetip_total_PRM, 0.05)
  
  newcaledonia_oceanicwhitetip_total_total <- colSums(newcaledonia_oceanicwhitetip_total)
  newcaledonia_oceanicwhitetip_proj$total <- mean(newcaledonia_oceanicwhitetip_total_total)
  newcaledonia_oceanicwhitetip_proj$total.upr <- quantile(newcaledonia_oceanicwhitetip_total_total, 0.95)
  newcaledonia_oceanicwhitetip_proj$total.lwr <- quantile(newcaledonia_oceanicwhitetip_total_total, 0.05)
  
  #cell averages
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
  
  newcaledonia_hammerhead_CM_rate <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead])
  newcaledonia_hammerhead_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      newcaledonia_hammerhead_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$hammerhead.logcpue[j], newcaledonia_comb$hammerhead.logcpue.se[j])
      newcaledonia_hammerhead_DOA[j,i] <- newcaledonia_hammerhead_catches[j,i] * newcaledonia_hammerhead_CM_rate[i]
      newcaledonia_hammerhead_released[j,i] <- newcaledonia_hammerhead_catches[j,i] - newcaledonia_hammerhead_DOA[j,i]
      newcaledonia_hammerhead_PRM[j,i] <- newcaledonia_hammerhead_released[j,i] * newcaledonia_hammerhead_PRM_rate[i]
      newcaledonia_hammerhead_total[j,i] <- newcaledonia_hammerhead_DOA[j,i] + newcaledonia_hammerhead_PRM[j,i]
    }
  }
  
  #run averages
  newcaledonia_hammerhead_proj <- data.frame("Species" = "Hammerhead",
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
  
  newcaledonia_hammerhead_total_catches <- colSums(newcaledonia_hammerhead_catches)
  newcaledonia_hammerhead_proj$catch <- mean(newcaledonia_hammerhead_total_catches)
  newcaledonia_hammerhead_proj$catch.upr <- quantile(newcaledonia_hammerhead_total_catches, 0.95)
  newcaledonia_hammerhead_proj$catch.lwr <- quantile(newcaledonia_hammerhead_total_catches, 0.05)
  
  newcaledonia_hammerhead_total_DOA <- colSums(newcaledonia_hammerhead_DOA)
  newcaledonia_hammerhead_proj$DOA <- mean(newcaledonia_hammerhead_total_DOA)
  newcaledonia_hammerhead_proj$DOA.upr <- quantile(newcaledonia_hammerhead_total_DOA, 0.95)
  newcaledonia_hammerhead_proj$DOA.lwr <- quantile(newcaledonia_hammerhead_total_DOA, 0.05)
  
  newcaledonia_hammerhead_total_PRM <- colSums(newcaledonia_hammerhead_PRM)
  newcaledonia_hammerhead_proj$PRM <- mean(newcaledonia_hammerhead_total_PRM)
  newcaledonia_hammerhead_proj$PRM.upr <- quantile(newcaledonia_hammerhead_total_PRM, 0.95)
  newcaledonia_hammerhead_proj$PRM.lwr <- quantile(newcaledonia_hammerhead_total_PRM, 0.05)
  
  newcaledonia_hammerhead_total_total <- colSums(newcaledonia_hammerhead_total)
  newcaledonia_hammerhead_proj$total <- mean(newcaledonia_hammerhead_total_total)
  newcaledonia_hammerhead_proj$total.upr <- quantile(newcaledonia_hammerhead_total_total, 0.95)
  newcaledonia_hammerhead_proj$total.lwr <- quantile(newcaledonia_hammerhead_total_total, 0.05)
  
  #cell averages
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
  
  newcaledonia_othersharks_CM_rate <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks])
  newcaledonia_othersharks_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      newcaledonia_othersharks_catches[j,i] <- newcaledonia_hook_preds[j,i]/1000 * rlnorm(1, newcaledonia_comb$othersharks.logcpue[j], newcaledonia_comb$othersharks.logcpue.se[j])
      newcaledonia_othersharks_DOA[j,i] <- newcaledonia_othersharks_catches[j,i] * newcaledonia_othersharks_CM_rate[i]
      newcaledonia_othersharks_released[j,i] <- newcaledonia_othersharks_catches[j,i] - newcaledonia_othersharks_DOA[j,i]
      newcaledonia_othersharks_PRM[j,i] <- newcaledonia_othersharks_released[j,i] * newcaledonia_othersharks_PRM_rate[i]
      newcaledonia_othersharks_total[j,i] <- newcaledonia_othersharks_DOA[j,i] + newcaledonia_othersharks_PRM[j,i]
    }
  }
  
  #run averages
  newcaledonia_othersharks_proj <- data.frame("Species" = "Other Sharks",
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
  
  newcaledonia_othersharks_total_catches <- colSums(newcaledonia_othersharks_catches)
  newcaledonia_othersharks_proj$catch <- mean(newcaledonia_othersharks_total_catches)
  newcaledonia_othersharks_proj$catch.upr <- quantile(newcaledonia_othersharks_total_catches, 0.95)
  newcaledonia_othersharks_proj$catch.lwr <- quantile(newcaledonia_othersharks_total_catches, 0.05)
  
  newcaledonia_othersharks_total_DOA <- colSums(newcaledonia_othersharks_DOA)
  newcaledonia_othersharks_proj$DOA <- mean(newcaledonia_othersharks_total_DOA)
  newcaledonia_othersharks_proj$DOA.upr <- quantile(newcaledonia_othersharks_total_DOA, 0.95)
  newcaledonia_othersharks_proj$DOA.lwr <- quantile(newcaledonia_othersharks_total_DOA, 0.05)
  
  newcaledonia_othersharks_total_PRM <- colSums(newcaledonia_othersharks_PRM)
  newcaledonia_othersharks_proj$PRM <- mean(newcaledonia_othersharks_total_PRM)
  newcaledonia_othersharks_proj$PRM.upr <- quantile(newcaledonia_othersharks_total_PRM, 0.95)
  newcaledonia_othersharks_proj$PRM.lwr <- quantile(newcaledonia_othersharks_total_PRM, 0.05)
  
  newcaledonia_othersharks_total_total <- colSums(newcaledonia_othersharks_total)
  newcaledonia_othersharks_proj$total <- mean(newcaledonia_othersharks_total_total)
  newcaledonia_othersharks_proj$total.upr <- quantile(newcaledonia_othersharks_total_total, 0.95)
  newcaledonia_othersharks_proj$total.lwr <- quantile(newcaledonia_othersharks_total_total, 0.05)
  
  #cell averages
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
newcaledonia_summary <- rbind(newcaledonia_bsh_proj,
                          newcaledonia_silky_proj,
                          newcaledonia_thresher_proj,
                          newcaledonia_mako_proj,
                          newcaledonia_oceanicwhitetip_proj,
                          newcaledonia_hammerhead_proj,
                          newcaledonia_othersharks_proj)

write.csv(newcaledonia_summary, "newcaledonia_summary.csv", row.names = F)

#plotting for Fig3
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


