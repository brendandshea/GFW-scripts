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
  
  kiribati_bsh_catches <- matrix(nrow=n, ncol=1000)
  kiribati_bsh_DOA <- matrix(nrow=n, ncol=1000)
  kiribati_bsh_released <- matrix(nrow=n, ncol=1000)
  kiribati_bsh_PRM<- matrix(nrow=n, ncol=1000)
  kiribati_bsh_total <- matrix(nrow=n, ncol=1000)
  
  kiribati_bsh_CM_rate <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh])
  kiribati_bsh_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      kiribati_bsh_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$blueshark.logcpue[j], kiribati_comb$blueshark.logcpue.se[j])
      kiribati_bsh_DOA[j,i] <- kiribati_bsh_catches[j,i] *  kiribati_bsh_CM_rate[i]
      kiribati_bsh_released[j,i] <- kiribati_bsh_catches[j,i] - kiribati_bsh_DOA[j,i]
      kiribati_bsh_PRM[j,i] <- kiribati_bsh_released[j,i] * kiribati_bsh_PRM_rate[i] 
      kiribati_bsh_total[j,i] <- kiribati_bsh_DOA[j,i] + kiribati_bsh_PRM[j,i]
    }
  }
  
  #run averages
  kiribati_bsh_proj <- data.frame("Species" = "Blue Shark",
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
  
  kiribati_bsh_total_catches <- colSums(kiribati_bsh_catches)
  kiribati_bsh_proj$catch <- mean(kiribati_bsh_total_catches)
  kiribati_bsh_proj$catch.upr <- quantile(kiribati_bsh_total_catches, 0.95)
  kiribati_bsh_proj$catch.lwr <- quantile(kiribati_bsh_total_catches, 0.05)
  
  kiribati_bsh_total_DOA <- colSums(kiribati_bsh_DOA)
  kiribati_bsh_proj$DOA <- mean(kiribati_bsh_total_DOA)
  kiribati_bsh_proj$DOA.upr <- quantile(kiribati_bsh_total_DOA, 0.95)
  kiribati_bsh_proj$DOA.lwr <- quantile(kiribati_bsh_total_DOA, 0.05)
  
  kiribati_bsh_total_PRM <- colSums(kiribati_bsh_PRM)
  kiribati_bsh_proj$PRM <- mean(kiribati_bsh_total_PRM)
  kiribati_bsh_proj$PRM.upr <- quantile(kiribati_bsh_total_PRM, 0.95)
  kiribati_bsh_proj$PRM.lwr <- quantile(kiribati_bsh_total_PRM, 0.05)
  
  kiribati_bsh_total_total <- colSums(kiribati_bsh_total)
  kiribati_bsh_proj$total <- mean(kiribati_bsh_total_total)
  kiribati_bsh_proj$total.upr <- quantile(kiribati_bsh_total_total, 0.95)
  kiribati_bsh_proj$total.lwr <- quantile(kiribati_bsh_total_total, 0.05)
  
  #cell averages
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
  
  kiribati_silky_catches <- matrix(nrow=n, ncol=1000)
  kiribati_silky_DOA <- matrix(nrow=n, ncol=1000)
  kiribati_silky_released <- matrix(nrow=n, ncol=1000)
  kiribati_silky_PRM<- matrix(nrow=n, ncol=1000)
  kiribati_silky_total <- matrix(nrow=n, ncol=1000)
  
  kiribati_silky_CM_rate <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky])
  kiribati_silky_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      kiribati_silky_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$silkyshark.logcpue[j], kiribati_comb$silkyshark.logcpue.se[j])
      kiribati_silky_DOA[j,i] <- kiribati_silky_catches[j,i] * kiribati_silky_CM_rate[i]
      kiribati_silky_released[j,i] <- kiribati_silky_catches[j,i] - kiribati_silky_DOA[j,i]
      kiribati_silky_PRM[j,i] <- kiribati_silky_released[j,i] * kiribati_silky_PRM_rate[i]
      kiribati_silky_total[j,i] <- kiribati_silky_DOA[j,i] + kiribati_silky_PRM[j,i]
    }
  }
  kiribati_silky_proj <- data.frame("Species" = "Silky Shark",
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
  
  kiribati_silky_total_catches <- colSums(kiribati_silky_catches)
  kiribati_silky_proj$catch <- mean(kiribati_silky_total_catches)
  kiribati_silky_proj$catch.upr <- quantile(kiribati_silky_total_catches, 0.95)
  kiribati_silky_proj$catch.lwr <- quantile(kiribati_silky_total_catches, 0.05)
  
  kiribati_silky_total_DOA <- colSums(kiribati_silky_DOA)
  kiribati_silky_proj$DOA <- mean(kiribati_silky_total_DOA)
  kiribati_silky_proj$DOA.upr <- quantile(kiribati_silky_total_DOA, 0.95)
  kiribati_silky_proj$DOA.lwr <- quantile(kiribati_silky_total_DOA, 0.05)
  
  kiribati_silky_total_PRM <- colSums(kiribati_silky_PRM)
  kiribati_silky_proj$PRM <- mean(kiribati_silky_total_PRM)
  kiribati_silky_proj$PRM.upr <- quantile(kiribati_silky_total_PRM, 0.95)
  kiribati_silky_proj$PRM.lwr <- quantile(kiribati_silky_total_PRM, 0.05)
  
  kiribati_silky_total_total <- colSums(kiribati_silky_total)
  kiribati_silky_proj$total <- mean(kiribati_silky_total_total)
  kiribati_silky_proj$total.upr <- quantile(kiribati_silky_total_total, 0.95)
  kiribati_silky_proj$total.lwr <- quantile(kiribati_silky_total_total, 0.05)
  
  #cell averages
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
  
  kiribati_thresher_catches <- matrix(nrow=n, ncol=1000)
  kiribati_thresher_DOA <- matrix(nrow=n, ncol=1000)
  kiribati_thresher_released <- matrix(nrow=n, ncol=1000)
  kiribati_thresher_PRM<- matrix(nrow=n, ncol=1000)
  kiribati_thresher_total <- matrix(nrow=n, ncol=1000)
  
  kiribati_thresher_CM_rate <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher])
  kiribati_thresher_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      kiribati_thresher_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$thresher.logcpue[j], kiribati_comb$thresher.logcpue.se[j])
      kiribati_thresher_DOA[j,i] <- kiribati_thresher_catches[j,i] * kiribati_thresher_CM_rate[i]
      kiribati_thresher_released[j,i] <- kiribati_thresher_catches[j,i] - kiribati_thresher_DOA[j,i]
      kiribati_thresher_PRM[j,i] <- kiribati_thresher_released[j,i] * kiribati_thresher_PRM_rate[i]
      kiribati_thresher_total[j,i] <- kiribati_thresher_DOA[j,i] + kiribati_thresher_PRM[j,i]
    }
  }
  
  kiribati_thresher_proj <- data.frame("Species" = "Thresher",
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
  
  kiribati_thresher_total_catches <- colSums(kiribati_thresher_catches)
  kiribati_thresher_proj$catch <- mean(kiribati_thresher_total_catches)
  kiribati_thresher_proj$catch.upr <- quantile(kiribati_thresher_total_catches, 0.95)
  kiribati_thresher_proj$catch.lwr <- quantile(kiribati_thresher_total_catches, 0.05)
  
  kiribati_thresher_total_DOA <- colSums(kiribati_thresher_DOA)
  kiribati_thresher_proj$DOA <- mean(kiribati_thresher_total_DOA)
  kiribati_thresher_proj$DOA.upr <- quantile(kiribati_thresher_total_DOA, 0.95)
  kiribati_thresher_proj$DOA.lwr <- quantile(kiribati_thresher_total_DOA, 0.05)
  
  kiribati_thresher_total_PRM <- colSums(kiribati_thresher_PRM)
  kiribati_thresher_proj$PRM <- mean(kiribati_thresher_total_PRM)
  kiribati_thresher_proj$PRM.upr <- quantile(kiribati_thresher_total_PRM, 0.95)
  kiribati_thresher_proj$PRM.lwr <- quantile(kiribati_thresher_total_PRM, 0.05)
  
  kiribati_thresher_total_total <- colSums(kiribati_thresher_total)
  kiribati_thresher_proj$total <- mean(kiribati_thresher_total_total)
  kiribati_thresher_proj$total.upr <- quantile(kiribati_thresher_total_total, 0.95)
  kiribati_thresher_proj$total.lwr <- quantile(kiribati_thresher_total_total, 0.05)
  
  #cell averages
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
  
  kiribati_mako_catches <- matrix(nrow=n, ncol=1000)
  kiribati_mako_DOA <- matrix(nrow=n, ncol=1000)
  kiribati_mako_released <- matrix(nrow=n, ncol=1000)
  kiribati_mako_PRM<- matrix(nrow=n, ncol=1000)
  kiribati_mako_total <- matrix(nrow=n, ncol=1000)
  
  kiribati_mako_CM_rate <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako])
  kiribati_mako_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      kiribati_mako_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$mako.logcpue[j], kiribati_comb$mako.logcpue.se[j])
      kiribati_mako_DOA[j,i] <- kiribati_mako_catches[j,i] * kiribati_mako_CM_rate[i]
      kiribati_mako_released[j,i] <- kiribati_mako_catches[j,i] - kiribati_mako_DOA[j,i]
      kiribati_mako_PRM[j,i] <- kiribati_mako_released[j,i] * kiribati_mako_PRM_rate[i]
      kiribati_mako_total[j,i] <- kiribati_mako_DOA[j,i] + kiribati_mako_PRM[j,i]
    }
  }
  
  #run averages
  kiribati_mako_proj <- data.frame("Species" = "Mako Shark",
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
  
  kiribati_mako_total_catches <- colSums(kiribati_mako_catches)
  kiribati_mako_proj$catch <- mean(kiribati_mako_total_catches)
  kiribati_mako_proj$catch.upr <- quantile(kiribati_mako_total_catches, 0.95)
  kiribati_mako_proj$catch.lwr <- quantile(kiribati_mako_total_catches, 0.05)
  
  kiribati_mako_total_DOA <- colSums(kiribati_mako_DOA)
  kiribati_mako_proj$DOA <- mean(kiribati_mako_total_DOA)
  kiribati_mako_proj$DOA.upr <- quantile(kiribati_mako_total_DOA, 0.95)
  kiribati_mako_proj$DOA.lwr <- quantile(kiribati_mako_total_DOA, 0.05)
  
  kiribati_mako_total_PRM <- colSums(kiribati_mako_PRM)
  kiribati_mako_proj$PRM <- mean(kiribati_mako_total_PRM)
  kiribati_mako_proj$PRM.upr <- quantile(kiribati_mako_total_PRM, 0.95)
  kiribati_mako_proj$PRM.lwr <- quantile(kiribati_mako_total_PRM, 0.05)
  
  kiribati_mako_total_total <- colSums(kiribati_mako_total)
  kiribati_mako_proj$total <- mean(kiribati_mako_total_total)
  kiribati_mako_proj$total.upr <- quantile(kiribati_mako_total_total, 0.95)
  kiribati_mako_proj$total.lwr <- quantile(kiribati_mako_total_total, 0.05)
  
  #cell averages
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
  
  kiribati_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
  kiribati_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
  kiribati_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
  kiribati_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
  kiribati_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)
  
  kiribati_oceanicwhitetip_CM_rate <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip])
  kiribati_oceanicwhitetip_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      kiribati_oceanicwhitetip_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$oceanicwhitetip.logcpue[j], kiribati_comb$oceanicwhitetip.logcpue.se[j])
      kiribati_oceanicwhitetip_DOA[j,i] <- kiribati_oceanicwhitetip_catches[j,i] * kiribati_oceanicwhitetip_CM_rate[i]
      kiribati_oceanicwhitetip_released[j,i] <- kiribati_oceanicwhitetip_catches[j,i] - kiribati_oceanicwhitetip_DOA[j,i]
      kiribati_oceanicwhitetip_PRM[j,i] <- kiribati_oceanicwhitetip_released[j,i] * kiribati_oceanicwhitetip_PRM_rate[i]
      kiribati_oceanicwhitetip_total[j,i] <- kiribati_oceanicwhitetip_DOA[j,i] + kiribati_oceanicwhitetip_PRM[j,i]
    }
  }
  
  #run averages
  kiribati_oceanicwhitetip_proj <- data.frame("Species" = "Oceanic Whitetip",
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
  
  kiribati_oceanicwhitetip_total_catches <- colSums(kiribati_oceanicwhitetip_catches)
  kiribati_oceanicwhitetip_proj$catch <- mean(kiribati_oceanicwhitetip_total_catches)
  kiribati_oceanicwhitetip_proj$catch.upr <- quantile(kiribati_oceanicwhitetip_total_catches, 0.95)
  kiribati_oceanicwhitetip_proj$catch.lwr <- quantile(kiribati_oceanicwhitetip_total_catches, 0.05)
  
  kiribati_oceanicwhitetip_total_DOA <- colSums(kiribati_oceanicwhitetip_DOA)
  kiribati_oceanicwhitetip_proj$DOA <- mean(kiribati_oceanicwhitetip_total_DOA)
  kiribati_oceanicwhitetip_proj$DOA.upr <- quantile(kiribati_oceanicwhitetip_total_DOA, 0.95)
  kiribati_oceanicwhitetip_proj$DOA.lwr <- quantile(kiribati_oceanicwhitetip_total_DOA, 0.05)
  
  kiribati_oceanicwhitetip_total_PRM <- colSums(kiribati_oceanicwhitetip_PRM)
  kiribati_oceanicwhitetip_proj$PRM <- mean(kiribati_oceanicwhitetip_total_PRM)
  kiribati_oceanicwhitetip_proj$PRM.upr <- quantile(kiribati_oceanicwhitetip_total_PRM, 0.95)
  kiribati_oceanicwhitetip_proj$PRM.lwr <- quantile(kiribati_oceanicwhitetip_total_PRM, 0.05)
  
  kiribati_oceanicwhitetip_total_total <- colSums(kiribati_oceanicwhitetip_total)
  kiribati_oceanicwhitetip_proj$total <- mean(kiribati_oceanicwhitetip_total_total)
  kiribati_oceanicwhitetip_proj$total.upr <- quantile(kiribati_oceanicwhitetip_total_total, 0.95)
  kiribati_oceanicwhitetip_proj$total.lwr <- quantile(kiribati_oceanicwhitetip_total_total, 0.05)
  
  #cell averages
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
  
  kiribati_hammerhead_catches <- matrix(nrow=n, ncol=1000)
  kiribati_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
  kiribati_hammerhead_released <- matrix(nrow=n, ncol=1000)
  kiribati_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
  kiribati_hammerhead_total <- matrix(nrow=n, ncol=1000)
  
  kiribati_hammerhead_CM_rate <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead])
  kiribati_hammerhead_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      kiribati_hammerhead_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$hammerhead.logcpue[j], kiribati_comb$hammerhead.logcpue.se[j])
      kiribati_hammerhead_DOA[j,i] <- kiribati_hammerhead_catches[j,i] * kiribati_hammerhead_CM_rate[i]
      kiribati_hammerhead_released[j,i] <- kiribati_hammerhead_catches[j,i] - kiribati_hammerhead_DOA[j,i]
      kiribati_hammerhead_PRM[j,i] <- kiribati_hammerhead_released[j,i] * kiribati_hammerhead_PRM_rate[i]
      kiribati_hammerhead_total[j,i] <- kiribati_hammerhead_DOA[j,i] + kiribati_hammerhead_PRM[j,i]
    }
  }
  
  #run averages
  kiribati_hammerhead_proj <- data.frame("Species" = "Hammerhead",
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
  
  kiribati_hammerhead_total_catches <- colSums(kiribati_hammerhead_catches)
  kiribati_hammerhead_proj$catch <- mean(kiribati_hammerhead_total_catches)
  kiribati_hammerhead_proj$catch.upr <- quantile(kiribati_hammerhead_total_catches, 0.95)
  kiribati_hammerhead_proj$catch.lwr <- quantile(kiribati_hammerhead_total_catches, 0.05)
  
  kiribati_hammerhead_total_DOA <- colSums(kiribati_hammerhead_DOA)
  kiribati_hammerhead_proj$DOA <- mean(kiribati_hammerhead_total_DOA)
  kiribati_hammerhead_proj$DOA.upr <- quantile(kiribati_hammerhead_total_DOA, 0.95)
  kiribati_hammerhead_proj$DOA.lwr <- quantile(kiribati_hammerhead_total_DOA, 0.05)
  
  kiribati_hammerhead_total_PRM <- colSums(kiribati_hammerhead_PRM)
  kiribati_hammerhead_proj$PRM <- mean(kiribati_hammerhead_total_PRM)
  kiribati_hammerhead_proj$PRM.upr <- quantile(kiribati_hammerhead_total_PRM, 0.95)
  kiribati_hammerhead_proj$PRM.lwr <- quantile(kiribati_hammerhead_total_PRM, 0.05)
  
  kiribati_hammerhead_total_total <- colSums(kiribati_hammerhead_total)
  kiribati_hammerhead_proj$total <- mean(kiribati_hammerhead_total_total)
  kiribati_hammerhead_proj$total.upr <- quantile(kiribati_hammerhead_total_total, 0.95)
  kiribati_hammerhead_proj$total.lwr <- quantile(kiribati_hammerhead_total_total, 0.05)
  
  #cell averages
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
  
  kiribati_othersharks_catches <- matrix(nrow=n, ncol=1000)
  kiribati_othersharks_DOA <- matrix(nrow=n, ncol=1000)
  kiribati_othersharks_released <- matrix(nrow=n, ncol=1000)
  kiribati_othersharks_PRM<- matrix(nrow=n, ncol=1000)
  kiribati_othersharks_total <- matrix(nrow=n, ncol=1000)
  
  kiribati_othersharks_CM_rate <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks])
  kiribati_othersharks_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      kiribati_othersharks_catches[j,i] <- kiribati_hook_preds[j,i]/1000 * rlnorm(1, kiribati_comb$othersharks.logcpue[j], kiribati_comb$othersharks.logcpue.se[j])
      kiribati_othersharks_DOA[j,i] <- kiribati_othersharks_catches[j,i] * kiribati_othersharks_CM_rate[i]
      kiribati_othersharks_released[j,i] <- kiribati_othersharks_catches[j,i] - kiribati_othersharks_DOA[j,i]
      kiribati_othersharks_PRM[j,i] <- kiribati_othersharks_released[j,i] * kiribati_othersharks_PRM_rate[i]
      kiribati_othersharks_total[j,i] <- kiribati_othersharks_DOA[j,i] + kiribati_othersharks_PRM[j,i]
    }
  }
  
  #run averages
  kiribati_othersharks_proj <- data.frame("Species" = "Other Sharks",
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
  
  kiribati_othersharks_total_catches <- colSums(kiribati_othersharks_catches)
  kiribati_othersharks_proj$catch <- mean(kiribati_othersharks_total_catches)
  kiribati_othersharks_proj$catch.upr <- quantile(kiribati_othersharks_total_catches, 0.95)
  kiribati_othersharks_proj$catch.lwr <- quantile(kiribati_othersharks_total_catches, 0.05)
  
  kiribati_othersharks_total_DOA <- colSums(kiribati_othersharks_DOA)
  kiribati_othersharks_proj$DOA <- mean(kiribati_othersharks_total_DOA)
  kiribati_othersharks_proj$DOA.upr <- quantile(kiribati_othersharks_total_DOA, 0.95)
  kiribati_othersharks_proj$DOA.lwr <- quantile(kiribati_othersharks_total_DOA, 0.05)
  
  kiribati_othersharks_total_PRM <- colSums(kiribati_othersharks_PRM)
  kiribati_othersharks_proj$PRM <- mean(kiribati_othersharks_total_PRM)
  kiribati_othersharks_proj$PRM.upr <- quantile(kiribati_othersharks_total_PRM, 0.95)
  kiribati_othersharks_proj$PRM.lwr <- quantile(kiribati_othersharks_total_PRM, 0.05)
  
  kiribati_othersharks_total_total <- colSums(kiribati_othersharks_total)
  kiribati_othersharks_proj$total <- mean(kiribati_othersharks_total_total)
  kiribati_othersharks_proj$total.upr <- quantile(kiribati_othersharks_total_total, 0.95)
  kiribati_othersharks_proj$total.lwr <- quantile(kiribati_othersharks_total_total, 0.05)
  
  #cell averages
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
kiribati_summary <- rbind(kiribati_bsh_proj,
                            kiribati_silky_proj,
                            kiribati_thresher_proj,
                            kiribati_mako_proj,
                            kiribati_oceanicwhitetip_proj,
                            kiribati_hammerhead_proj,
                            kiribati_othersharks_proj)

write.csv(kiribati_summary, "kiribati_summary.csv", row.names = F)


#plotting for Fig3
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
