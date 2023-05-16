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
  
  frenchpolynesia_bsh_catches <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_bsh_DOA <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_bsh_released <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_bsh_PRM<- matrix(nrow=n, ncol=1000)
  frenchpolynesia_bsh_total <- matrix(nrow=n, ncol=1000)
  
  frenchpolynesia_bsh_CM_rate <- runif(1000,mortality$HookMin[r_bsh],mortality$HookMax[r_bsh])
  frenchpolynesia_bsh_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_bsh],mortality$logit.prm.se[r_bsh])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      frenchpolynesia_bsh_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$blueshark.logcpue[j], frenchpolynesia_comb$blueshark.logcpue.se[j])
      frenchpolynesia_bsh_DOA[j,i] <- frenchpolynesia_bsh_catches[j,i] *  frenchpolynesia_bsh_CM_rate[i]
      frenchpolynesia_bsh_released[j,i] <- frenchpolynesia_bsh_catches[j,i] - frenchpolynesia_bsh_DOA[j,i]
      frenchpolynesia_bsh_PRM[j,i] <- frenchpolynesia_bsh_released[j,i] * frenchpolynesia_bsh_PRM_rate[i] 
      frenchpolynesia_bsh_total[j,i] <- frenchpolynesia_bsh_DOA[j,i] + frenchpolynesia_bsh_PRM[j,i]
    }
  }
  
  #run averages
  frenchpolynesia_bsh_proj <- data.frame("Species" = "Blue Shark",
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
  
  frenchpolynesia_bsh_total_catches <- colSums(frenchpolynesia_bsh_catches)
  frenchpolynesia_bsh_proj$catch <- mean(frenchpolynesia_bsh_total_catches)
  frenchpolynesia_bsh_proj$catch.upr <- quantile(frenchpolynesia_bsh_total_catches, 0.95)
  frenchpolynesia_bsh_proj$catch.lwr <- quantile(frenchpolynesia_bsh_total_catches, 0.05)
  
  frenchpolynesia_bsh_total_DOA <- colSums(frenchpolynesia_bsh_DOA)
  frenchpolynesia_bsh_proj$DOA <- mean(frenchpolynesia_bsh_total_DOA)
  frenchpolynesia_bsh_proj$DOA.upr <- quantile(frenchpolynesia_bsh_total_DOA, 0.95)
  frenchpolynesia_bsh_proj$DOA.lwr <- quantile(frenchpolynesia_bsh_total_DOA, 0.05)
  
  frenchpolynesia_bsh_total_PRM <- colSums(frenchpolynesia_bsh_PRM)
  frenchpolynesia_bsh_proj$PRM <- mean(frenchpolynesia_bsh_total_PRM)
  frenchpolynesia_bsh_proj$PRM.upr <- quantile(frenchpolynesia_bsh_total_PRM, 0.95)
  frenchpolynesia_bsh_proj$PRM.lwr <- quantile(frenchpolynesia_bsh_total_PRM, 0.05)
  
  frenchpolynesia_bsh_total_total <- colSums(frenchpolynesia_bsh_total)
  frenchpolynesia_bsh_proj$total <- mean(frenchpolynesia_bsh_total_total)
  frenchpolynesia_bsh_proj$total.upr <- quantile(frenchpolynesia_bsh_total_total, 0.95)
  frenchpolynesia_bsh_proj$total.lwr <- quantile(frenchpolynesia_bsh_total_total, 0.05)
  
  #cell averages
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
  
  frenchpolynesia_silky_catches <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_silky_DOA <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_silky_released <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_silky_PRM<- matrix(nrow=n, ncol=1000)
  frenchpolynesia_silky_total <- matrix(nrow=n, ncol=1000)
  
  frenchpolynesia_silky_CM_rate <- runif(1000,mortality$HookMin[r_silky],mortality$HookMax[r_silky])
  frenchpolynesia_silky_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_silky],mortality$logit.prm.se[r_silky])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      frenchpolynesia_silky_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$silkyshark.logcpue[j], frenchpolynesia_comb$silkyshark.logcpue.se[j])
      frenchpolynesia_silky_DOA[j,i] <- frenchpolynesia_silky_catches[j,i] * frenchpolynesia_silky_CM_rate[i]
      frenchpolynesia_silky_released[j,i] <- frenchpolynesia_silky_catches[j,i] - frenchpolynesia_silky_DOA[j,i]
      frenchpolynesia_silky_PRM[j,i] <- frenchpolynesia_silky_released[j,i] * frenchpolynesia_silky_PRM_rate[i]
      frenchpolynesia_silky_total[j,i] <- frenchpolynesia_silky_DOA[j,i] + frenchpolynesia_silky_PRM[j,i]
    }
  }
  frenchpolynesia_silky_proj <- data.frame("Species" = "Silky Shark",
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
  
  frenchpolynesia_silky_total_catches <- colSums(frenchpolynesia_silky_catches)
  frenchpolynesia_silky_proj$catch <- mean(frenchpolynesia_silky_total_catches)
  frenchpolynesia_silky_proj$catch.upr <- quantile(frenchpolynesia_silky_total_catches, 0.95)
  frenchpolynesia_silky_proj$catch.lwr <- quantile(frenchpolynesia_silky_total_catches, 0.05)
  
  frenchpolynesia_silky_total_DOA <- colSums(frenchpolynesia_silky_DOA)
  frenchpolynesia_silky_proj$DOA <- mean(frenchpolynesia_silky_total_DOA)
  frenchpolynesia_silky_proj$DOA.upr <- quantile(frenchpolynesia_silky_total_DOA, 0.95)
  frenchpolynesia_silky_proj$DOA.lwr <- quantile(frenchpolynesia_silky_total_DOA, 0.05)
  
  frenchpolynesia_silky_total_PRM <- colSums(frenchpolynesia_silky_PRM)
  frenchpolynesia_silky_proj$PRM <- mean(frenchpolynesia_silky_total_PRM)
  frenchpolynesia_silky_proj$PRM.upr <- quantile(frenchpolynesia_silky_total_PRM, 0.95)
  frenchpolynesia_silky_proj$PRM.lwr <- quantile(frenchpolynesia_silky_total_PRM, 0.05)
  
  frenchpolynesia_silky_total_total <- colSums(frenchpolynesia_silky_total)
  frenchpolynesia_silky_proj$total <- mean(frenchpolynesia_silky_total_total)
  frenchpolynesia_silky_proj$total.upr <- quantile(frenchpolynesia_silky_total_total, 0.95)
  frenchpolynesia_silky_proj$total.lwr <- quantile(frenchpolynesia_silky_total_total, 0.05)
  
  #cell averages
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
  
  frenchpolynesia_thresher_catches <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_thresher_DOA <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_thresher_released <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_thresher_PRM<- matrix(nrow=n, ncol=1000)
  frenchpolynesia_thresher_total <- matrix(nrow=n, ncol=1000)
  
  frenchpolynesia_thresher_CM_rate <- runif(1000,mortality$HookMin[r_thresher],mortality$HookMax[r_thresher])
  frenchpolynesia_thresher_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_thresher],mortality$logit.prm.se[r_thresher])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      frenchpolynesia_thresher_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$thresher.logcpue[j], frenchpolynesia_comb$thresher.logcpue.se[j])
      frenchpolynesia_thresher_DOA[j,i] <- frenchpolynesia_thresher_catches[j,i] * frenchpolynesia_thresher_CM_rate[i]
      frenchpolynesia_thresher_released[j,i] <- frenchpolynesia_thresher_catches[j,i] - frenchpolynesia_thresher_DOA[j,i]
      frenchpolynesia_thresher_PRM[j,i] <- frenchpolynesia_thresher_released[j,i] * frenchpolynesia_thresher_PRM_rate[i]
      frenchpolynesia_thresher_total[j,i] <- frenchpolynesia_thresher_DOA[j,i] + frenchpolynesia_thresher_PRM[j,i]
    }
  }
  
  frenchpolynesia_thresher_proj <- data.frame("Species" = "Thresher",
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
  
  frenchpolynesia_thresher_total_catches <- colSums(frenchpolynesia_thresher_catches)
  frenchpolynesia_thresher_proj$catch <- mean(frenchpolynesia_thresher_total_catches)
  frenchpolynesia_thresher_proj$catch.upr <- quantile(frenchpolynesia_thresher_total_catches, 0.95)
  frenchpolynesia_thresher_proj$catch.lwr <- quantile(frenchpolynesia_thresher_total_catches, 0.05)
  
  frenchpolynesia_thresher_total_DOA <- colSums(frenchpolynesia_thresher_DOA)
  frenchpolynesia_thresher_proj$DOA <- mean(frenchpolynesia_thresher_total_DOA)
  frenchpolynesia_thresher_proj$DOA.upr <- quantile(frenchpolynesia_thresher_total_DOA, 0.95)
  frenchpolynesia_thresher_proj$DOA.lwr <- quantile(frenchpolynesia_thresher_total_DOA, 0.05)
  
  frenchpolynesia_thresher_total_PRM <- colSums(frenchpolynesia_thresher_PRM)
  frenchpolynesia_thresher_proj$PRM <- mean(frenchpolynesia_thresher_total_PRM)
  frenchpolynesia_thresher_proj$PRM.upr <- quantile(frenchpolynesia_thresher_total_PRM, 0.95)
  frenchpolynesia_thresher_proj$PRM.lwr <- quantile(frenchpolynesia_thresher_total_PRM, 0.05)
  
  frenchpolynesia_thresher_total_total <- colSums(frenchpolynesia_thresher_total)
  frenchpolynesia_thresher_proj$total <- mean(frenchpolynesia_thresher_total_total)
  frenchpolynesia_thresher_proj$total.upr <- quantile(frenchpolynesia_thresher_total_total, 0.95)
  frenchpolynesia_thresher_proj$total.lwr <- quantile(frenchpolynesia_thresher_total_total, 0.05)
  
  #cell averages
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
  
  frenchpolynesia_mako_catches <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_mako_DOA <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_mako_released <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_mako_PRM<- matrix(nrow=n, ncol=1000)
  frenchpolynesia_mako_total <- matrix(nrow=n, ncol=1000)
  
  frenchpolynesia_mako_CM_rate <- runif(1000,mortality$HookMin[r_mako],mortality$HookMax[r_mako])
  frenchpolynesia_mako_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_mako],mortality$logit.prm.se[r_mako])
  
  
  for (j in 1:n) {
    for (i in 1:1000) {
      frenchpolynesia_mako_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$mako.logcpue[j], frenchpolynesia_comb$mako.logcpue.se[j])
      frenchpolynesia_mako_DOA[j,i] <- frenchpolynesia_mako_catches[j,i] * frenchpolynesia_mako_CM_rate[i]
      frenchpolynesia_mako_released[j,i] <- frenchpolynesia_mako_catches[j,i] - frenchpolynesia_mako_DOA[j,i]
      frenchpolynesia_mako_PRM[j,i] <- frenchpolynesia_mako_released[j,i] * frenchpolynesia_mako_PRM_rate[i]
      frenchpolynesia_mako_total[j,i] <- frenchpolynesia_mako_DOA[j,i] + frenchpolynesia_mako_PRM[j,i]
    }
  }
  
  #run averages
  frenchpolynesia_mako_proj <- data.frame("Species" = "Mako Shark",
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
  
  frenchpolynesia_mako_total_catches <- colSums(frenchpolynesia_mako_catches)
  frenchpolynesia_mako_proj$catch <- mean(frenchpolynesia_mako_total_catches)
  frenchpolynesia_mako_proj$catch.upr <- quantile(frenchpolynesia_mako_total_catches, 0.95)
  frenchpolynesia_mako_proj$catch.lwr <- quantile(frenchpolynesia_mako_total_catches, 0.05)
  
  frenchpolynesia_mako_total_DOA <- colSums(frenchpolynesia_mako_DOA)
  frenchpolynesia_mako_proj$DOA <- mean(frenchpolynesia_mako_total_DOA)
  frenchpolynesia_mako_proj$DOA.upr <- quantile(frenchpolynesia_mako_total_DOA, 0.95)
  frenchpolynesia_mako_proj$DOA.lwr <- quantile(frenchpolynesia_mako_total_DOA, 0.05)
  
  frenchpolynesia_mako_total_PRM <- colSums(frenchpolynesia_mako_PRM)
  frenchpolynesia_mako_proj$PRM <- mean(frenchpolynesia_mako_total_PRM)
  frenchpolynesia_mako_proj$PRM.upr <- quantile(frenchpolynesia_mako_total_PRM, 0.95)
  frenchpolynesia_mako_proj$PRM.lwr <- quantile(frenchpolynesia_mako_total_PRM, 0.05)
  
  frenchpolynesia_mako_total_total <- colSums(frenchpolynesia_mako_total)
  frenchpolynesia_mako_proj$total <- mean(frenchpolynesia_mako_total_total)
  frenchpolynesia_mako_proj$total.upr <- quantile(frenchpolynesia_mako_total_total, 0.95)
  frenchpolynesia_mako_proj$total.lwr <- quantile(frenchpolynesia_mako_total_total, 0.05)
  
  #cell averages
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
  
  frenchpolynesia_oceanicwhitetip_catches <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_oceanicwhitetip_DOA <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_oceanicwhitetip_released <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_oceanicwhitetip_PRM<- matrix(nrow=n, ncol=1000)
  frenchpolynesia_oceanicwhitetip_total <- matrix(nrow=n, ncol=1000)
  
  frenchpolynesia_oceanicwhitetip_CM_rate <- runif(1000,mortality$HookMin[r_oceanicwhitetip],mortality$HookMax[r_oceanicwhitetip])
  frenchpolynesia_oceanicwhitetip_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_oceanicwhitetip],mortality$logit.prm.se[r_oceanicwhitetip])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      frenchpolynesia_oceanicwhitetip_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$oceanicwhitetip.logcpue[j], frenchpolynesia_comb$oceanicwhitetip.logcpue.se[j])
      frenchpolynesia_oceanicwhitetip_DOA[j,i] <- frenchpolynesia_oceanicwhitetip_catches[j,i] * frenchpolynesia_oceanicwhitetip_CM_rate[i]
      frenchpolynesia_oceanicwhitetip_released[j,i] <- frenchpolynesia_oceanicwhitetip_catches[j,i] - frenchpolynesia_oceanicwhitetip_DOA[j,i]
      frenchpolynesia_oceanicwhitetip_PRM[j,i] <- frenchpolynesia_oceanicwhitetip_released[j,i] * frenchpolynesia_oceanicwhitetip_PRM_rate[i]
      frenchpolynesia_oceanicwhitetip_total[j,i] <- frenchpolynesia_oceanicwhitetip_DOA[j,i] + frenchpolynesia_oceanicwhitetip_PRM[j,i]
    }
  }
  
  #run averages
  frenchpolynesia_oceanicwhitetip_proj <- data.frame("Species" = "Oceanic Whitetip",
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
  
  frenchpolynesia_oceanicwhitetip_total_catches <- colSums(frenchpolynesia_oceanicwhitetip_catches)
  frenchpolynesia_oceanicwhitetip_proj$catch <- mean(frenchpolynesia_oceanicwhitetip_total_catches)
  frenchpolynesia_oceanicwhitetip_proj$catch.upr <- quantile(frenchpolynesia_oceanicwhitetip_total_catches, 0.95)
  frenchpolynesia_oceanicwhitetip_proj$catch.lwr <- quantile(frenchpolynesia_oceanicwhitetip_total_catches, 0.05)
  
  frenchpolynesia_oceanicwhitetip_total_DOA <- colSums(frenchpolynesia_oceanicwhitetip_DOA)
  frenchpolynesia_oceanicwhitetip_proj$DOA <- mean(frenchpolynesia_oceanicwhitetip_total_DOA)
  frenchpolynesia_oceanicwhitetip_proj$DOA.upr <- quantile(frenchpolynesia_oceanicwhitetip_total_DOA, 0.95)
  frenchpolynesia_oceanicwhitetip_proj$DOA.lwr <- quantile(frenchpolynesia_oceanicwhitetip_total_DOA, 0.05)
  
  frenchpolynesia_oceanicwhitetip_total_PRM <- colSums(frenchpolynesia_oceanicwhitetip_PRM)
  frenchpolynesia_oceanicwhitetip_proj$PRM <- mean(frenchpolynesia_oceanicwhitetip_total_PRM)
  frenchpolynesia_oceanicwhitetip_proj$PRM.upr <- quantile(frenchpolynesia_oceanicwhitetip_total_PRM, 0.95)
  frenchpolynesia_oceanicwhitetip_proj$PRM.lwr <- quantile(frenchpolynesia_oceanicwhitetip_total_PRM, 0.05)
  
  frenchpolynesia_oceanicwhitetip_total_total <- colSums(frenchpolynesia_oceanicwhitetip_total)
  frenchpolynesia_oceanicwhitetip_proj$total <- mean(frenchpolynesia_oceanicwhitetip_total_total)
  frenchpolynesia_oceanicwhitetip_proj$total.upr <- quantile(frenchpolynesia_oceanicwhitetip_total_total, 0.95)
  frenchpolynesia_oceanicwhitetip_proj$total.lwr <- quantile(frenchpolynesia_oceanicwhitetip_total_total, 0.05)
  
  #cell averages
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
  
  frenchpolynesia_hammerhead_catches <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_hammerhead_DOA <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_hammerhead_released <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_hammerhead_PRM<- matrix(nrow=n, ncol=1000)
  frenchpolynesia_hammerhead_total <- matrix(nrow=n, ncol=1000)
  
  frenchpolynesia_hammerhead_CM_rate <- runif(1000,mortality$HookMin[r_hammerhead],mortality$HookMax[r_hammerhead])
  frenchpolynesia_hammerhead_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_hammerhead],mortality$logit.prm.se[r_hammerhead])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      frenchpolynesia_hammerhead_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$hammerhead.logcpue[j], frenchpolynesia_comb$hammerhead.logcpue.se[j])
      frenchpolynesia_hammerhead_DOA[j,i] <- frenchpolynesia_hammerhead_catches[j,i] * frenchpolynesia_hammerhead_CM_rate[i]
      frenchpolynesia_hammerhead_released[j,i] <- frenchpolynesia_hammerhead_catches[j,i] - frenchpolynesia_hammerhead_DOA[j,i]
      frenchpolynesia_hammerhead_PRM[j,i] <- frenchpolynesia_hammerhead_released[j,i] * frenchpolynesia_hammerhead_PRM_rate[i]
      frenchpolynesia_hammerhead_total[j,i] <- frenchpolynesia_hammerhead_DOA[j,i] + frenchpolynesia_hammerhead_PRM[j,i]
    }
  }
  
  #run averages
  frenchpolynesia_hammerhead_proj <- data.frame("Species" = "Hammerhead",
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
  
  frenchpolynesia_hammerhead_total_catches <- colSums(frenchpolynesia_hammerhead_catches)
  frenchpolynesia_hammerhead_proj$catch <- mean(frenchpolynesia_hammerhead_total_catches)
  frenchpolynesia_hammerhead_proj$catch.upr <- quantile(frenchpolynesia_hammerhead_total_catches, 0.95)
  frenchpolynesia_hammerhead_proj$catch.lwr <- quantile(frenchpolynesia_hammerhead_total_catches, 0.05)
  
  frenchpolynesia_hammerhead_total_DOA <- colSums(frenchpolynesia_hammerhead_DOA)
  frenchpolynesia_hammerhead_proj$DOA <- mean(frenchpolynesia_hammerhead_total_DOA)
  frenchpolynesia_hammerhead_proj$DOA.upr <- quantile(frenchpolynesia_hammerhead_total_DOA, 0.95)
  frenchpolynesia_hammerhead_proj$DOA.lwr <- quantile(frenchpolynesia_hammerhead_total_DOA, 0.05)
  
  frenchpolynesia_hammerhead_total_PRM <- colSums(frenchpolynesia_hammerhead_PRM)
  frenchpolynesia_hammerhead_proj$PRM <- mean(frenchpolynesia_hammerhead_total_PRM)
  frenchpolynesia_hammerhead_proj$PRM.upr <- quantile(frenchpolynesia_hammerhead_total_PRM, 0.95)
  frenchpolynesia_hammerhead_proj$PRM.lwr <- quantile(frenchpolynesia_hammerhead_total_PRM, 0.05)
  
  frenchpolynesia_hammerhead_total_total <- colSums(frenchpolynesia_hammerhead_total)
  frenchpolynesia_hammerhead_proj$total <- mean(frenchpolynesia_hammerhead_total_total)
  frenchpolynesia_hammerhead_proj$total.upr <- quantile(frenchpolynesia_hammerhead_total_total, 0.95)
  frenchpolynesia_hammerhead_proj$total.lwr <- quantile(frenchpolynesia_hammerhead_total_total, 0.05)
  
  #cell averages
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
  
  frenchpolynesia_othersharks_catches <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_othersharks_DOA <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_othersharks_released <- matrix(nrow=n, ncol=1000)
  frenchpolynesia_othersharks_PRM<- matrix(nrow=n, ncol=1000)
  frenchpolynesia_othersharks_total <- matrix(nrow=n, ncol=1000)
  
  frenchpolynesia_othersharks_CM_rate <- runif(1000,mortality$HookMin[r_othersharks],mortality$HookMax[r_othersharks])
  frenchpolynesia_othersharks_PRM_rate <- rlogitnorm(1000, mortality$logit.prm[r_othersharks],mortality$logit.prm.se[r_othersharks])
  
  for (j in 1:n) {
    for (i in 1:1000) {
      frenchpolynesia_othersharks_catches[j,i] <- frenchpolynesia_hook_preds[j,i]/1000 * rlnorm(1, frenchpolynesia_comb$othersharks.logcpue[j], frenchpolynesia_comb$othersharks.logcpue.se[j])
      frenchpolynesia_othersharks_DOA[j,i] <- frenchpolynesia_othersharks_catches[j,i] * frenchpolynesia_othersharks_CM_rate[i]
      frenchpolynesia_othersharks_released[j,i] <- frenchpolynesia_othersharks_catches[j,i] - frenchpolynesia_othersharks_DOA[j,i]
      frenchpolynesia_othersharks_PRM[j,i] <- frenchpolynesia_othersharks_released[j,i] * frenchpolynesia_othersharks_PRM_rate[i]
      frenchpolynesia_othersharks_total[j,i] <- frenchpolynesia_othersharks_DOA[j,i] + frenchpolynesia_othersharks_PRM[j,i]
    }
  }
  
  #run averages
  frenchpolynesia_othersharks_proj <- data.frame("Species" = "Other Sharks",
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
  
  frenchpolynesia_othersharks_total_catches <- colSums(frenchpolynesia_othersharks_catches)
  frenchpolynesia_othersharks_proj$catch <- mean(frenchpolynesia_othersharks_total_catches)
  frenchpolynesia_othersharks_proj$catch.upr <- quantile(frenchpolynesia_othersharks_total_catches, 0.95)
  frenchpolynesia_othersharks_proj$catch.lwr <- quantile(frenchpolynesia_othersharks_total_catches, 0.05)
  
  frenchpolynesia_othersharks_total_DOA <- colSums(frenchpolynesia_othersharks_DOA)
  frenchpolynesia_othersharks_proj$DOA <- mean(frenchpolynesia_othersharks_total_DOA)
  frenchpolynesia_othersharks_proj$DOA.upr <- quantile(frenchpolynesia_othersharks_total_DOA, 0.95)
  frenchpolynesia_othersharks_proj$DOA.lwr <- quantile(frenchpolynesia_othersharks_total_DOA, 0.05)
  
  frenchpolynesia_othersharks_total_PRM <- colSums(frenchpolynesia_othersharks_PRM)
  frenchpolynesia_othersharks_proj$PRM <- mean(frenchpolynesia_othersharks_total_PRM)
  frenchpolynesia_othersharks_proj$PRM.upr <- quantile(frenchpolynesia_othersharks_total_PRM, 0.95)
  frenchpolynesia_othersharks_proj$PRM.lwr <- quantile(frenchpolynesia_othersharks_total_PRM, 0.05)
  
  frenchpolynesia_othersharks_total_total <- colSums(frenchpolynesia_othersharks_total)
  frenchpolynesia_othersharks_proj$total <- mean(frenchpolynesia_othersharks_total_total)
  frenchpolynesia_othersharks_proj$total.upr <- quantile(frenchpolynesia_othersharks_total_total, 0.95)
  frenchpolynesia_othersharks_proj$total.lwr <- quantile(frenchpolynesia_othersharks_total_total, 0.05)
  
  #cell averages
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
frenchpolynesia_summary <- rbind(frenchpolynesia_bsh_proj,
                          frenchpolynesia_silky_proj,
                          frenchpolynesia_thresher_proj,
                          frenchpolynesia_mako_proj,
                          frenchpolynesia_oceanicwhitetip_proj,
                          frenchpolynesia_hammerhead_proj,
                          frenchpolynesia_othersharks_proj)

write.csv(frenchpolynesia_summary, "frenchpolynesia_summary.csv", row.names = F)

#mortality plotting for Fig3
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


