library(tidyverse)

# IMPORT DATA
wildMSData <- read_rds("output_tables/WildDataRevised.Rds")
realmsRenamed <- readr::read_csv("./data/wild_ms_database/input_tables/realmsRenamed.csv")
sumData <- subset(wildMSData, wildBin=="wild") %>% select(everything()) %>% inner_join(realmsRenamed, by="Country")
#IMPORT SOME FUNCTIONS
source("./wild_codes/6_ImpFunctions.R")

#summarise the data grouped by Biorealms
(real_sum <-  sumData %>% group_by(Biorealms) %>%
    summarise(N = n(),
              #LAND USE
              lui_Base_mn=gm_mean(.1+base_lui, na.rm=TRUE),
              lui_Base_sd=sd(.1+base_lui, na.rm=TRUE),
              lui_Base_err=1.96*(lui_Base_sd/sqrt(N)),
              #lui_Base_err=qt(0.975, df = N-1)*(lui_Base_sd/sqrt(N)),
              lui_Base_95CI=paste0("(", paste(round(lui_Base_mn-lui_Base_err, 3), round(lui_Base_mn+lui_Base_err, 3), sep = ","), ")"),
              
              lui_SSP1_mn=gm_mean(.1+ssp1_lui, na.rm=TRUE),
              lui_SSP1_sd=sd(.1+ssp1_lui, na.rm=TRUE),
              lui_SSP1_err=1.96*(lui_SSP1_sd/sqrt(N)),
              #lui_SSP1_err=qt(0.975, df = N-1)*(lui_SSP1_sd/sqrt(N)),
              lui_SSP1_95CI=paste0("(", paste(round(lui_SSP1_mn-lui_Base_err, 3), round(lui_SSP1_mn+lui_Base_err, 3), sep = ","), ")"),
              
              lui_SSP5_mn=gm_mean(.1+ssp5_lui, na.rm=TRUE),
              lui_SSP5_sd=sd(.1+ssp5_lui, na.rm=TRUE),
              lui_SSP5_err=1.96*(lui_SSP5_sd/sqrt(N)),
              #lui_SSP5_err=qt(0.975, df = N-1)*(lui_SSP5_sd/sqrt(N)),
              lui_SSP5_95CI=paste0("(", paste(round(lui_SSP5_mn-lui_Base_err, 3), round(lui_SSP5_mn+lui_Base_err, 3), sep = ","), ")"),
              
              luiPDeviation = round(100*log(lui_SSP1_mn/lui_SSP5_mn)),
              
              #CLIMATE
              vcc_Base_mn=gm_mean(base_vcc, na.rm=TRUE),
              vcc_Base_sd=sd(base_vcc, na.rm=TRUE),
              vcc_Base_err=1.96*(vcc_Base_sd/sqrt(N)),
              #vcc_Base_err=qt(0.975, df = N-1)*(vcc_Base_sd/sqrt(N)),
              vcc_Base_95CI=paste0("(", paste(round(vcc_Base_mn-vcc_Base_err, 3), round(vcc_Base_mn+vcc_Base_err, 3), sep = ","), ")"),
              
              vcc_SSP1_mn=gm_mean(ssp1_vcc, na.rm=TRUE),
              vcc_SSP1_sd=sd(ssp1_vcc, na.rm=TRUE),
              vcc_SSP1_err=1.96*(vcc_SSP1_sd/sqrt(N)),
              #vcc_SSP1_err=qt(0.975, df = N-1)*(vcc_SSP1_sd/sqrt(N)),
              vcc_SSP1_95CI=paste0("(", paste(round(vcc_SSP1_mn-vcc_SSP1_err, 3), round(vcc_SSP1_mn+vcc_SSP1_err, 3), sep = ","), ")"),
              
              vcc_SSP5_mn=gm_mean(ssp5_vcc, na.rm=TRUE),
              vcc_SSP5_sd=sd(ssp5_vcc, na.rm=TRUE),
              vcc_SSP5_err=1.96*(vcc_SSP5_sd/sqrt(N)),
              #vcc_SSP5_err=qt(0.975, df = N-1)*(vcc_SSP5_sd/sqrt(N)),
              vcc_SSP5_95CI=paste0("(", paste(round(vcc_SSP5_mn-vcc_SSP5_err, 3), round(vcc_SSP5_mn+vcc_SSP5_err, 3), sep = ","), ")"),
              
              vccPDeviation = round(100*log(vcc_SSP1_mn/vcc_SSP5_mn) )
    )%>%mutate(cat = Biorealms)%>%
    select(
      cat,
      vcc_Base_mn,vcc_Base_95CI,
      vcc_SSP1_mn,vcc_SSP1_95CI,
      vcc_SSP5_mn,vcc_SSP5_95CI,vccPDeviation,
      lui_Base_mn,lui_Base_95CI,
      lui_SSP1_mn,lui_SSP1_95CI,
      lui_SSP5_mn,lui_SSP5_95CI,luiPDeviation,N
    ))

(pa_sum <-  subset(wildMSData, wildBin=="wild") %>% group_by(pa_dum) %>%
    summarise(N = n(),
              #LAND USE
              lui_Base_mn=gm_mean(.1+base_lui, na.rm=TRUE),
              lui_Base_sd=sd(.1+base_lui, na.rm=TRUE),
              lui_Base_err=1.96*(lui_Base_sd/sqrt(N)),
              #lui_Base_err=qt(0.975, df = N-1)*(lui_Base_sd/sqrt(N)),
              lui_Base_95CI=paste0("(", paste(round(lui_Base_mn-lui_Base_err, 3), round(lui_Base_mn+lui_Base_err, 3), sep = ","), ")"),
              
              lui_SSP1_mn=gm_mean(.1+ssp1_lui, na.rm=TRUE),
              lui_SSP1_sd=sd(.1+ssp1_lui, na.rm=TRUE),
              lui_SSP1_err=1.96*(lui_SSP1_sd/sqrt(N)),
              #lui_SSP1_err=qt(0.975, df = N-1)*(lui_SSP1_sd/sqrt(N)),
              lui_SSP1_95CI=paste0("(", paste(round(lui_SSP1_mn-lui_Base_err, 3), round(lui_SSP1_mn+lui_Base_err, 3), sep = ","), ")"),
              
              lui_SSP5_mn=gm_mean(.1+ssp5_lui, na.rm=TRUE),
              lui_SSP5_sd=sd(.1+ssp5_lui, na.rm=TRUE),
              lui_SSP5_err=1.96*(lui_SSP5_sd/sqrt(N)),
              #lui_SSP5_err=qt(0.975, df = N-1)*(lui_SSP5_sd/sqrt(N)),
              lui_SSP5_95CI=paste0("(", paste(round(lui_SSP5_mn-lui_Base_err, 3), round(lui_SSP5_mn+lui_Base_err, 3), sep = ","), ")"),
              
              luiPDeviation = round(100*log(lui_SSP1_mn/lui_SSP5_mn)),
              
              #CLIMATE
              vcc_Base_mn=gm_mean(base_vcc, na.rm=TRUE),
              vcc_Base_sd=sd(base_vcc, na.rm=TRUE),
              vcc_Base_err=1.96*(vcc_Base_sd/sqrt(N)),
              #vcc_Base_err=qt(0.975, df = N-1)*(vcc_Base_sd/sqrt(N)),
              vcc_Base_95CI=paste0("(", paste(round(vcc_Base_mn-vcc_Base_err, 3), round(vcc_Base_mn+vcc_Base_err, 3), sep = ","), ")"),
              
              vcc_SSP1_mn=gm_mean(ssp1_vcc, na.rm=TRUE),
              vcc_SSP1_sd=sd(ssp1_vcc, na.rm=TRUE),
              vcc_SSP1_err=1.96*(vcc_SSP1_sd/sqrt(N)),
              #vcc_SSP1_err=qt(0.975, df = N-1)*(vcc_SSP1_sd/sqrt(N)),
              vcc_SSP1_95CI=paste0("(", paste(round(vcc_SSP1_mn-vcc_SSP1_err, 3), round(vcc_SSP1_mn+vcc_SSP1_err, 3), sep = ","), ")"),
              
              vcc_SSP5_mn=gm_mean(ssp5_vcc, na.rm=TRUE),
              vcc_SSP5_sd=sd(ssp5_vcc, na.rm=TRUE),
              vcc_SSP5_err=1.96*(vcc_SSP5_sd/sqrt(N)),
              #vcc_SSP5_err=qt(0.975, df = N-1)*(vcc_SSP5_sd/sqrt(N)),
              vcc_SSP5_95CI=paste0("(", paste(round(vcc_SSP5_mn-vcc_SSP5_err, 3), round(vcc_SSP5_mn+vcc_SSP5_err, 3), sep = ","), ")"),
              
              vccPDeviation = round(100*log(vcc_SSP1_mn/vcc_SSP5_mn) )
    )%>%mutate(cat = pa_dum)%>%
    select(
      cat, 
      vcc_Base_mn,vcc_Base_95CI,
      vcc_SSP1_mn,vcc_SSP1_95CI,
      vcc_SSP5_mn,vcc_SSP5_95CI,vccPDeviation,
      lui_Base_mn,lui_Base_95CI,
      lui_SSP1_mn,lui_SSP1_95CI,
      lui_SSP5_mn,lui_SSP5_95CI,luiPDeviation,N
    ))

(wild_sum <- wildMSData %>% group_by(wildBin) %>%
    summarise(N = n(),
              #LAND USE
              lui_Base_mn=gm_mean(.1+base_lui, na.rm=TRUE),
              lui_Base_sd=sd(.1+base_lui, na.rm=TRUE),
              lui_Base_err=1.96*(lui_Base_sd/sqrt(N)),
              #lui_Base_err=qt(0.975, df = N-1)*(lui_Base_sd/sqrt(N)),
              lui_Base_95CI=paste0("(", paste(round(lui_Base_mn-lui_Base_err, 3), round(lui_Base_mn+lui_Base_err, 3), sep = ","), ")"),
              
              lui_SSP1_mn=gm_mean(.1+ssp1_lui, na.rm=TRUE),
              lui_SSP1_sd=sd(.1+ssp1_lui, na.rm=TRUE),
              lui_SSP1_err=1.96*(lui_SSP1_sd/sqrt(N)),
              #lui_SSP1_err=qt(0.975, df = N-1)*(lui_SSP1_sd/sqrt(N)),
              lui_SSP1_95CI=paste0("(", paste(round(lui_SSP1_mn-lui_Base_err, 3), round(lui_SSP1_mn+lui_Base_err, 3), sep = ","), ")"),
              
              lui_SSP5_mn=gm_mean(.1+ssp5_lui, na.rm=TRUE),
              lui_SSP5_sd=sd(.1+ssp5_lui, na.rm=TRUE),
              lui_SSP5_err=1.96*(lui_SSP5_sd/sqrt(N)),
              #lui_SSP5_err=qt(0.975, df = N-1)*(lui_SSP5_sd/sqrt(N)),
              lui_SSP5_95CI=paste0("(", paste(round(lui_SSP5_mn-lui_Base_err, 3), round(lui_SSP5_mn+lui_Base_err, 3), sep = ","), ")"),
              
              luiPDeviation = round(100*log(lui_SSP1_mn/lui_SSP5_mn)),
              
              #CLIMATE
              vcc_Base_mn=gm_mean(base_vcc, na.rm=TRUE),
              vcc_Base_sd=sd(base_vcc, na.rm=TRUE),
              vcc_Base_err=1.96*(vcc_Base_sd/sqrt(N)),
              #vcc_Base_err=qt(0.975, df = N-1)*(vcc_Base_sd/sqrt(N)),
              vcc_Base_95CI=paste0("(", paste(round(vcc_Base_mn-vcc_Base_err, 3), round(vcc_Base_mn+vcc_Base_err, 3), sep = ","), ")"),
              
              vcc_SSP1_mn=gm_mean(ssp1_vcc, na.rm=TRUE),
              vcc_SSP1_sd=sd(ssp1_vcc, na.rm=TRUE),
              vcc_SSP1_err=1.96*(vcc_SSP1_sd/sqrt(N)),
              #vcc_SSP1_err=qt(0.975, df = N-1)*(vcc_SSP1_sd/sqrt(N)),
              vcc_SSP1_95CI=paste0("(", paste(round(vcc_SSP1_mn-vcc_SSP1_err, 3), round(vcc_SSP1_mn+vcc_SSP1_err, 3), sep = ","), ")"),
              
              vcc_SSP5_mn=gm_mean(ssp5_vcc, na.rm=TRUE),
              vcc_SSP5_sd=sd(ssp5_vcc, na.rm=TRUE),
              vcc_SSP5_err=1.96*(vcc_SSP5_sd/sqrt(N)),
              #vcc_SSP5_err=qt(0.975, df = N-1)*(vcc_SSP5_sd/sqrt(N)),
              vcc_SSP5_95CI=paste0("(", paste(round(vcc_SSP5_mn-vcc_SSP5_err, 3), round(vcc_SSP5_mn+vcc_SSP5_err, 3), sep = ","), ")"),
              
              vccPDeviation = round(100*log(vcc_SSP1_mn/vcc_SSP5_mn)) 
    )%>%mutate(cat = wildBin)%>%
    select(
      cat, 
      vcc_Base_mn,vcc_Base_95CI,
      vcc_SSP1_mn,vcc_SSP1_95CI,
      vcc_SSP5_mn,vcc_SSP5_95CI,vccPDeviation,
      lui_Base_mn,lui_Base_95CI,
      lui_SSP1_mn,lui_SSP1_95CI,
      lui_SSP5_mn,lui_SSP5_95CI,luiPDeviation,N
    ))

View(sumStats<-rbind(
  real_sum, 
  pa_sum,
  wild_sum
))
#sumStats%>%mutate_at(2:16, round(., 2))
write.csv(sumStats, "summStatistics.csv")


# How much does outside differ from inside wilderness
100*log(wild_sum$vcc_SSP5_mn[2]/wild_sum$vcc_SSP5_mn[1]) #55.76656%
100*log(wild_sum$lui_SSP5_mn[2]/wild_sum$lui_SSP5_mn[1]) #-50.74368%

100*log(wild_sum$vcc_SSP1_mn[2]/wild_sum$vcc_SSP1_mn[1]) #43.92377%
100*log(wild_sum$lui_SSP1_mn[2]/wild_sum$lui_SSP1_mn[1]) #-55.95811%



#summarise the data grouped by Biorealms
names(sumData)
(climate_var_sum <-  sumData %>% group_by(Biorealms) %>%
    summarise(vcc_Base_temp=gm_mean(velocity_temp_hist_2005, na.rm=TRUE),
              vcc_Base_prec=gm_mean(velocity_prec_hist_2005, na.rm=TRUE),
              
              vcc_SSP1_temp=gm_mean(velocity_temp_rcp26_2050, na.rm=TRUE),
              vcc_SSP1_prec=gm_mean(velocity_prec_rcp26_2050, na.rm=TRUE),
              
              vcc_SSP5_temp=gm_mean(velocity_temp_rcp85_2050, na.rm=TRUE),
              vcc_SSP5_prec=gm_mean(velocity_prec_rcp85_2050, na.rm=TRUE)))

(climate_var_sum2 <-  sumData %>%
    summarise(Biorealms = "Wilderness (All)",
              vcc_Base_temp=gm_mean(velocity_temp_hist_2005, na.rm=TRUE),
              vcc_Base_prec=gm_mean(velocity_prec_hist_2005, na.rm=TRUE),
              
              vcc_SSP1_temp=gm_mean(velocity_temp_rcp26_2050, na.rm=TRUE),
              vcc_SSP1_prec=gm_mean(velocity_prec_rcp26_2050, na.rm=TRUE),
              
              vcc_SSP5_temp=gm_mean(velocity_temp_rcp85_2050, na.rm=TRUE),
              vcc_SSP5_prec=gm_mean(velocity_prec_rcp85_2050, na.rm=TRUE)))

climate_var_sum <- rbind(climate_var_sum2,climate_var_sum)
write.csv(climate_var_sum, "climateVariblesSumms.csv")

