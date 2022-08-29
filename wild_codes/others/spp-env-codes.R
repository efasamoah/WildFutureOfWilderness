library(tidyverse)
#gm_mean <- psych::geometric.mean

extrafont::loadfonts(device = "win")
theme_sleek<-function(base_size = 11, base_family = "Helvetica") {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey30"),
      strip.text.y = element_text(colour = "grey30"),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(colour = "black"),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 0.5),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey30", size = rel(1)),
      plot.subtitle = element_text(colour = "grey30", size = rel(.85))
    )
}


gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

library(tidyverse)
library(raster)

##IMPORT VELCOTIY RASTERS
luv_stack <- stack(list.files("data/wild_ms_database/landuseNew", pattern = ".tif$", full.names = TRUE, recursive = TRUE))
nlayers(luv_stack)
#plot((luv_stack))

clim_stack <- stack(list.files("data/wild_ms_database/climate", pattern = ".tif$", full.names = TRUE))
#plot(clim_stack)

myRLists <- list.files("data/wild_ms_database/others", pattern = ".tif$", full.names = TRUE)

ecozones <- raster(myRLists[grep("ecoregions", myRLists)]); names(ecozones)<-"ecozones"
protect <- raster(myRLists[grep("largeWDPA", myRLists)]); names(protect)<-"protect"
barenLand <- raster(myRLists[grep("baren", myRLists)]); names(barenLand)<-"barenLand"
elevation <- raster(myRLists[grep("elevation", myRLists)]); names(elevation) <-"elevation"
continents <- raster(myRLists[grep("continents", myRLists)]); names(continents) <-"continents"
ctry_iso3 <- raster(myRLists[grep("ctry_", myRLists)]); names(ctry_iso3) <-"iso3_num"
thrt_spp <- raster(myRLists[grep("thr_", myRLists)]); names(thrt_spp) <-"thrt_tetrapods"
all_spp <- raster(myRLists[grep("allRich_", myRLists)]); names(all_spp) <-"tetrapods"
wilderness <- raster(myRLists[grep("earth_wilderness", myRLists)]); names(wilderness) <-"wilderness"
dist_ocean <- raster(myRLists[grep("dist_ocean", myRLists)]); names(dist_ocean) <-"dist_ocean"
intactness <- raster(myRLists[grep("intactness", myRLists)]); names(intactness) <-"intactness"

ecozones <- resample(ecozones, clim_stack, method = "ngb")
wilderness <- resample(wilderness, clim_stack, method = "ngb")
continents <- resample(continents, clim_stack, method = "ngb")
ctry_iso3 <- resample(ctry_iso3, clim_stack, method = "ngb")
protect <- resample(protect, clim_stack, method = "ngb")
barenLand <- resample(barenLand, clim_stack, method = "ngb")

elevation <- resample(elevation, clim_stack, method = "bilinear")
thrt_spp <- resample(thrt_spp, clim_stack, method = "bilinear")
all_spp <- resample(all_spp, clim_stack, method = "bilinear")
dist_ocean <- resample(dist_ocean, clim_stack, method = "bilinear")
intactness <- resample(intactness, clim_stack, method = "bilinear")
luv_stack <- resample(luv_stack, clim_stack, method = "bilinear")

multi_data <- stack(clim_stack,luv_stack,elevation,continents,ctry_iso3,all_spp,thrt_spp,protect,wilderness,ecozones,dist_ocean,intactness,barenLand)
crs(multi_data) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
multi_data <- projectRaster(multi_data, crs ="+proj=moll +ellps=WGS84 +datum=WGS84 +no_defs",method="ngb", res = 24000)
writeRaster(multi_data, filename=paste0("data/wild_ms_database/ProcessedRastersMoll/", names(multi_data), ".tif"), bylayer=TRUE, format="GTiff", overwrite=TRUE)

# choroData <- as.data.frame(multi_data, xy = TRUE)
# dim(choroData)
# dplyr::glimpse(choroData)
# 
# choroData <- choroData[!is.na(choroData$iso3_num),]
# choroData <- merge(choroData, nation_attr[, c("FID", "GID_0", "Country")], by.x = "iso3_num", by.y = "FID", all.x = TRUE)
# choroData <- merge(choroData, eco_attr[, c("ECO_ID_U", "ECO_CODE", "ECO_NAME", "ECO_NUM", "WWF_REALM2", "WWF_MHTNUM", "WWF_MHTNAM")], by.x = "ecozones", by.y = "ECO_ID_U", all.x = TRUE)
# #choroData$pa_dum <- ifelse(choroData$newProtect >0, "PA", "nonPA")
# choroData$pa_dum <- ifelse(choroData$protect >0, "PA", "nonPA")
# choroData$pa_dum[is.na(choroData$pa_dum)] <- "nonPA"
# factor(choroData$pa_dum)
# 
# #choroData <- merge(choroData, pa_attr, by.x = "newProtect", by.y = "WDPAID", all.x = TRUE)
# choroData$realms<-plyr::revalue(factor(choroData$WWF_REALM2),
#                                      c("Afrotropic"="Afrotropical", 
#                                        "Australasia"="Australasia",
#                                        "Indo-Malay" = "Indomalaya",
#                                        "Nearctic" = "Nearctic", 
#                                        "Neotropic" = "Neotropical", 
#                                        "Oceania" = "Australasia", 
#                                        "Palearctic" = "Palearctic"))
# names(choroData)
# choroData$crop2050_ssp585<-(rowSums(choroData[, c("c3ann2050_585",
#                                                      "c3nfx2050_585",
#                                                      "c3per2050_585",
#                                                      "c4ann2050_585",
#                                                      "c4per2050_585")], na.rm = T))
# 
# choroData$lu_ssp5_max <- apply(choroData[, c("crop2050_ssp585",
#                                                  "pastr2050_585",
#                                                  "range2050_585", 
#                                                  "primf2050_585",
#                                                  #"secdf2050_585", 
#                                                  "urban2050_585")], 1, function(x) max(x, na.rm = T))
# 
# choroData$lu_ssp5_max<-round(choroData$lu_ssp5_max, 3)
# hist(choroData$lu_ssp5_max, breaks = 100)
# mean((choroData$lu_ssp5_max), na.rm = T)
# 
# choroData$crop2050_ssp126<-rowSums(choroData[, c("c3ann2050_126",
#                                                     "c3nfx2050_126",
#                                                     "c3per2050_126", 
#                                                     "c4ann2050_126",
#                                                     "c4per2050_126")], na.rm = T)
# 
# choroData$lu_ssp1_max<-apply(choroData[, c("crop2050_ssp126",
#                                                "pastr2050_126",
#                                                "range2050_126",
#                                                "primf2050_126",
#                                                #"secdf2050_126",
#                                                "urban2050_126")], 1, function(x) max(x, na.rm = T))
# choroData$lu_ssp1_max<-round(choroData$lu_ssp1_max, 3)
# hist((choroData$lu_ssp1_max+0.1), breaks = 100)
# gm_mean((choroData$lu_ssp1_max+0.1), na.rm = T)
# 
# choroData$crop_hist<-rowSums(choroData[, c("c3ann2050_hist",
#                                            "c3nfx2050_hist",
#                                            "c3per2050_hist",
#                                            "c4ann2050_hist",
#                                            "c4per2050_hist")], na.rm = T)
# 
# choroData$lu_Base_max<-apply(choroData[, c("crop_hist",
#                                            "pastr2050_hist",
#                                            "range2050_hist",
#                                            "primf2050_hist",
#                                            #"secdf2050_hist", 
#                                            "urban2050_hist")], 1, function(x) max(x, na.rm = T))
# 
# choroData$lu_Base_max<-round(choroData$lu_Base_max, 3)
# hist((choroData$lu_Base_max), breaks = 50)
# mean(choroData$lu_Base_max, na.rm = T)
# 
# choroData$cc_Base<-apply(choroData[, c("velocity_temp_hist_2005", "velocity_prec_hist_2005")], 1, function(x) max(x, na.rm = T))
# choroData$cc_ssp5<-apply(choroData[, c("velocity_temp_rcp85_2050", "velocity_prec_rcp85_2050")], 1, function(x) max(x, na.rm = T))
# choroData$cc_ssp1<-apply(choroData[, c("velocity_temp_rcp26_2050", "velocity_prec_rcp26_2050")], 1, function(x) max(x, na.rm = T))
# 
# hist(choroData$cc_Base, breaks = 100)
# gm_mean(choroData$cc_Base, na.rm = T)
# 
# choroData$wild_dummy<-ifelse(choroData$wilderness<65535, "wild","nonwild")
# choroData<-choroData[!is.na(choroData$wild_dummy),]
# 
# choroData<-as.data.frame(subset(choroData)) %>%
#   select(everything())%>%
#   filter(is.finite(lu_Base_max), is.finite(cc_Base))%>%
#   group_by(wilderness)%>%
#   mutate(vlength = length(wilderness))
# 
# 
# ##SUBSET GLOBAL DATASET AND WORK WITH ONLY WILDERNESS AREAS
# dWild<-subset(as.data.frame(choroData), wild_dummy == "wild")
# dim(dWild) #41563 CORDEX PIXELS ~19.5% of the global dataset (41563/212826)
# dWild<-subset(dWild)%>% 
#   #group_by(wilderness)%>%
#   mutate(wld.size=n())
# 
# library(spatstat.geom)
# choroData%>% 
#   group_by(wild_dummy)%>%
#   mutate(wld.size=n())%>%
#   summarise(metric=weighted.median(lu_Base_max*100, w = wld.size, na.rm = TRUE))
# 
# 
# #CALCULATE GEOMETRIC AVERAGE OF CLIMATE VARIABLES
# names(dWild)
# dWildSum<-dWild%>%
#   select(everything())%>%
#   group_by(wilderness)%>%
#   summarise(TempBase = mean(velocity_temp_hist_2005, na.rm = T),
#             TempSus = mean(velocity_temp_rcp26_2050, na.rm = T),
#             TempBAU = mean(velocity_temp_rcp85_2050, na.rm = T),
#             PrecBase = mean(velocity_prec_hist_2005, na.rm = T),
#             PrecSus = mean(velocity_prec_rcp26_2050, na.rm = T),
#             PrecBAU = mean(velocity_prec_rcp85_2050, na.rm = T),
#             ClimBase = mean(cc_Base, na.rm = T),
#             sdBase = sd(cc_Base, na.rm = T),
#             ClimSus = mean(cc_ssp1, na.rm = T),
#             sdSus = sd(cc_ssp1, na.rm = T),
#             ClimBAU = mean(cc_ssp5, na.rm = T),
#             sdBAU = sd(cc_ssp5, na.rm = T))%>%
#   na.omit()%>%
#   ungroup()
# 
# #Calculate proportion of wilderness with velocity greater than 1 km/yr
# 100*(dim(subset(dWild, cc_Base>1))[[1]]/dim(dWild)[[1]]) #ca. 84%
# 
# 
# ##CLIMATE NON-WILDERNESS AREAS
# subset(choroData, wild_dummy=="nonwild")%>%select(everything())%>%
#   summarise(
#     ClimBase = gm_mean(cc_Base, na.rm = T),
#     sdBase = sd(cc_Base, na.rm = T),
#     ClimSus = gm_mean(cc_ssp1, na.rm = T),
#     sdSus = sd(cc_ssp1, na.rm = T),
#     ClimBAU = gm_mean(cc_ssp5, na.rm = T),
#     sdBAU = sd(cc_ssp5, na.rm = T))%>%
#   na.omit()%>%
#   ungroup()
# 
# #SUMMARY OF LAND-USE INSTABILITY
# dWild%>%select(everything())%>%
#   group_by(pa_dum)%>%
#   #mutate(size = n())%>%
#   summarise(n_cell = n(),
#             mnBase = gm_mean(lu_Base_max+0.1, na.rm = T),
#             sdbase = sd(lu_Base_max+0.1,na.rm = T),
#             mnSus = gm_mean(lu_ssp1_max+0.1, na.rm = T),
#             sdSus = sd(lu_ssp1_max+0.1, na.rm = T),
#             mnBAU = gm_mean(lu_ssp5_max+0.1, na.rm = T),
#             sdBAU = sd(lu_ssp5_max+0.1, na.rm = T))%>%
#   na.omit()%>%ungroup()
# 
# ##Provide summary for NON WILDERNESS AREAS
# subset(choroData, wild_dummy=="nonwild")%>%select(everything())%>%
#   #group_by(pa_dum)%>%
#   summarise(size = length(realms),
#             mnBase = gm_mean(lu_Base_max+0.1, na.rm = T),
#             sdbase = sd(lu_Base_max+0.1, na.rm = T),
#             mnSus = gm_mean(lu_ssp1_max+0.1, na.rm = T),
#             sdSus = sd(lu_ssp1_max+0.1, na.rm = T),
#             mnBAU = gm_mean(lu_ssp5_max+0.1, na.rm = T),
#             sdBAU = sd(lu_ssp5_max+0.1, na.rm = T))%>%
#   na.omit()%>%ungroup()
# 
# 
# ##Calculate and plot estimates for the baseline as bars
# velPWild<-as.data.frame(dWild)%>% 
#   group_by(pa_dum)%>%##summarise the data grouped by protection vs unprotected
#   summarise(wld.size=length(pa_dum),
#             vClim = gm_mean(cc_Base, na.rm = T),
#             sdClim = sd(cc_Base, na.rm = T))
# names(velPWild)<-c("status", "size", "vClim","sdClim")
# 
# velPWild2<-dWild%>% ##Geometric mean across all wilderness areas
#   group_by(wild_dummy)%>%
#   summarise(size=length(wild_dummy),
#             vClim = gm_mean(cc_Base,na.rm = T),
#             sdClim = sd(cc_Base, na.rm = T))
# names(velPWild2)<-c("status", "size", "vClim","sdClim")
# velBaseClim<-rbind(velPWild,velPWild2)
# velBaseClim
# 
# 
# velPWild<-subset(dWild)%>% ##summarise the data grouped by protection status
#   group_by(pa_dum)%>%
#   summarise(wld.size=n(),
#             vClim = gm_mean(lu_Base_max+0.1, na.rm = T),
#             sdClim = sd(lu_Base_max+0.1, na.rm = T))
# names(velPWild)<-c("status", "size", "vLuse","sdLuse")
# 
# velPWild2<-subset(dWild)%>% ##summarise the data grouped by protection status
#   group_by(wild_dummy)%>%
#   summarise(size=n(),
#             vLuse = gm_mean(lu_Base_max+0.1, na.rm = T),
#             sdClim = sd(lu_Base_max+0.1, na.rm = T))
# names(velPWild2)<-c("status", "size", "vLuse","sdLuse")
# velBaseLuse<-rbind(velPWild,velPWild2)
# velBaseLuse
# 
# #Calculate standard error of mean
# velBaseClim$errorC<-(qt(0.975,df=(velBaseClim$size)-1)*(velBaseClim$sdClim)/sqrt(velBaseClim$size))
# velBaseLuse$errorL<-(qt(0.975,df=(velBaseLuse$size)-1)*(velBaseLuse$sdLuse)/sqrt(velBaseLuse$size))
# 
# velBaseClim$vClim+velBaseClim$errorC
# velBaseClim$vClim-velBaseClim$errorC
# 
# velBaseLuse$vLuse+velBaseLuse$errorL
# velBaseLuse$vLuse-velBaseLuse$errorL
# 
# ##melt data into long format
# velBase <- rbind(
#   data.frame(what="Climate", status=velBaseClim$status, mean=velBaseClim$vClim, error=velBaseClim$errorC),
#   data.frame(what="Land use", status=velBaseLuse$status, mean=velBaseLuse$vLuse, error=velBaseLuse$errorL)
# )
# velBase$status <-plyr::revalue(velBase$status, #Rename protection status
#                                 c("PA" = "PW",
#                                   "nonPA" = "UW", 
#                                   #"nonwild" = "OW",
#                                   "wild" = "AW"))
# velBase$status<-factor(velBase$status, levels=c("AW","PW","UW")) ##sort factors
# 
# #Plot bars with errors
# ggplot(as.data.frame(velBase), aes(status, mean, fill = status))+
#   geom_bar(stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=mean-error, ymax=mean+error), width=.15, position=position_dodge(.9))+
#   scale_fill_viridis_d(option = "B", direction = -1)+
#   scale_y_continuous(expand = expansion(mult = c(0, .1)), labels = scales::number_format(accuracy = 0.01))+
#   facet_wrap(~what, scales = "free")+
#   labs(x = "Status", y = expression("Velocity or instability" ~ paste("(","km", yr^-1,")")))+
#   theme_sleek(base_size = 14)+
#   theme(legend.position = "top",
#         legend.text = element_text(size = 8),
#         legend.title = element_blank(),
#         axis.text = element_text(size = 10)
#   )
# ggsave(here::here("Fig1A.tiff"), dpi = 600, width=4, height = 4.5)
# 
# #Create a Kernel density plot for the correlations
# #https://ggplot2.tidyverse.org/reference/geom_density_2d.html
# vsum_patch<-dWild%>% ##summarise the data grouped by protection status
#   group_by(wilderness)%>%
#   #filter(lu_Base_max >0.18)%>%
#   summarise(size = n(),
#             vClim = mean(cc_Base, na.rm = T),
#             vLand = mean(lu_Base_max, na.rm = T),
#             x = mean(x, na.rm = T),
#             y = mean(y, na.rm = T))%>%
#   filter(size >2)%>% ##limit estimate to less than three CORDEX pixels to limit miscalculations
#   ungroup()
# 
# library(SpatialPack)
# coords <- vsum_patch[c("x","y")]
# modified.ttest(log(vsum_patch$vClim), log(vsum_patch$vLand+.01), coords) # r = ~.43, p<1x10-6
# 
# ggplot(subset(vsum_patch), 
#        aes(x = (vLand+.001), y = (vClim))) +
#   geom_density_2d_filled(contour_var = "ndensity")+
#   scale_fill_viridis_d(name = "Density",labels =c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
#   scale_x_continuous(expand = c(0,0),trans = "log",labels = scales::number_format(accuracy = 1), breaks = c(0.001,1, 10))+
#   scale_y_continuous(expand = c(0,0), trans = "log", labels = scales::number_format(accuracy = 1), breaks = c(0,1, 10))+
#   #annotate("text", x = 0.1, y = 0.1, label = "paste(italic(r[spatial]), \" = -0.41\", \" ; P < 1 x 10^-6\")", parse = TRUE, size=6, colour = "white")+
#   annotate("text", x = 0.1, y = 0.12, label = expression(italic(r[spatial]) ~ paste(" = -0.44, P < 1 x" , 10^-6,)),size=4, colour = "white")+
#   theme_sleek(base_size = 14)+
#   labs(x = expression("Land-use instability" ~ paste("(","km", yr^-1,")")), 
#        y = expression("Climate velocity" ~ paste("(","km", yr^-1,")")))+
#   theme(legend.position="top", 
#         legend.direction = "horizontal", 
#         legend.text = element_text(size = 8),
#         axis.text = element_text(size = 10),
#         legend.title = element_blank())+
#   guides(fill = guide_legend(nrow = 2))
# ggsave("Fig1B.tiff", dpi = 600, width=4, height = 4.5)
# 
# 
# 
# #test statistically for Differences between projected estimated under ssp1 and ssp5
# my_data<-rbind(
#   data.frame(velocity=dWild$cc_ssp1, scenario="SSP1-RCP2.6"),
#   data.frame(velocity=dWild$cc_ssp5, scenario="SSP5-RCP8.5")
# )
# wilcox.test(velocity ~ scenario, data = my_data, exact = FALSE, alternative = "less")
# 
# #summarise the data grouped by realms
# (realmsSum<-choroData%>% ##
#   group_by(wild_dummy)%>%
#   summarise(size=length(realms),
#             vLand = gm_mean(lu_Base_max+0.1, na.rm = T),
#             sdLand = sd(lu_Base_max+0.1, na.rm = T))%>%na.omit())
# realmsSum$error<-(qt(0.975,df=(realmsSum$size)-1)*(realmsSum$sdLand)/sqrt(realmsSum$size))
# realmsSum$vLand+realmsSum$error
# realmsSum$vLand-realmsSum$error
# 
# 
# realmsSum<-dWild%>% ##summarise the data grouped by realms
#   group_by(pa_dum)%>%
#   summarise(size=n(),
#             vClim = gm_mean(cc_ssp5,na.rm = T),
#             sdClim = sd(cc_ssp5, na.rm = T))%>%na.omit()
# realmsSum$error<-(qt(0.975,df=(realmsSum$size)-1)*(realmsSum$sdClim)/sqrt(realmsSum$size))
# realmsSum$vClim+realmsSum$error
# realmsSum$vClim-realmsSum$error
# 
# 
# 
# dWild$sens85<-(dWild$cc_ssp5*(dWild$lu_ssp5_max))
# summary(dWild$sens85)
# cor.test(dWild$sens85, dWild$intactness, method = "spearman")
# qplot(dWild$sens85,geom = "histogram", bins = 100)#+xlim(c(0, 200))
# 
# ntile_na <- function(x,n)
# {
#   notna <- !is.na(x)
#   out <- rep(NA_real_,length(x))
#   out[notna] <- ntile(x[notna],n)
#   return(out)
# }
# 
# choro <- as.data.frame(dWild) %>% 
#   select(everything())%>%
#   mutate(ccBrkBase = ntile_na(cc_Base, 3),
#          ccBrk85 = ntile_na(cc_ssp5, 3),
#          ccBrk26 = ntile_na(cc_ssp1, 3),
#          cmbBrk85 = ntile_na(sens85, 3),
#          cntxt = ntile(intactness, 3))
# 
# choro <- choro%>% select(everything())%>% #0.08*1.204627(mean + SD of recovery pixels)
#   mutate(luBrkBase = ifelse(lu_Base_max <0.16,1,2),
#          pfBrkBase = ifelse(primf2050_hist <0.16,1,2),
#          luBrk85 = ifelse(lu_ssp5_max <0.16,1,2),
#          pfBrk85 = ifelse(primf2050_585 <0.16,1,2))
# 
# choro <- subset(as.data.frame(choro)) %>% ##bivariate colour combinations
#   select(everything()) %>%
#   filter(is.finite(luBrkBase), is.finite(ccBrkBase), is.finite(cntxt), is.finite(pfBrkBase)) %>%
#   mutate(sns85 = rgb(red = round(cmbBrk85/3, 1), green = round(cntxt/3, 1), blue = 0.5),
#          mixBs.main = rgb(red = round(luBrkBase/2.5, 1), green = round(ccBrkBase/3, 1), blue = 0.5),
#          mixBs.pfor = rgb(red = round(pfBrkBase/2.5, 1), green = round(ccBrkBase/3, 1), blue = 0.5),
#          mix85.main = rgb(red = round(luBrk85/2.5, 1), green = round(ccBrk85/3, 1), blue = 0.5),
#          mix85.pfor = rgb(red = round(pfBrk85/2.5, 1), green = round(ccBrk85/3, 1), blue = 0.5))
# 
# library(rgdal)
# #terrPAs <- readOGR("D:/R - HoMe/SPP-ENV2020/wpda_region_wise/terr_PAs_mollweide", layer="terrPAsIUCN_WildOnly")
# #terrWilderness <- rgdal::readOGR("D:/R - HoMe/LULC velocity/ProcessingHausz/Data/Wilderness_maps_R.1", layer="LoW_2009_current")
# #terrWilderness<-fortify(terrWilderness)
# 
# bb_realms <- readOGR("C:/Users/45019738/Dropbox/PNAS_Revision/Generalised_Biogeographic_Realms_2004", layer="brpol_fin_dd")
# bb_realms <- spTransform(bb_realms, CRS("+proj=moll"))
# realms <- fortify(bb_realms, region = "REALMCODE")
# 
# centroids.realms <- as.data.frame(coordinates(bb_realms))
# names(centroids.realms) <- c("long", "lat")
# 
# idList <- bb_realms@data$REALMCODE
# realm.df <- data.frame(id = idList, centroids.realms) #Using centroid positions text on keep priority areas so Manually adjust labels in map
# #realm.labels<-cbind.data.frame(id = c("PA","NA","IM","AT","NT","AA"),
# #                               long = c(4214966, -7839085, 9242787, 2094967, -6222277, 13047461),
# #                               lat = c(5386746.3, 5491240.4, 480038.6, -1347711.5, -1478926.6, -2980095.8))
# 
# realm.labels<-cbind.data.frame(id = c("PA","NA","IM","AT","NT","AA"),
#                                long = c(4214966, -10839085, 8242787, 104967, -8522277, 15047461),
#                                lat = c(5386746.3, 5491240.4, 100038.6, -1347711.5, -1478926.6, -2980095.8))
# 
# 
# ggplot() +##Create map for the baseline instabilities
#   geom_polygon(data = cont, aes(long,lat, group=group), fill="grey70") +
#   geom_tile(data = choro, aes(x, y, fill = mixBs.main), color=NA) +
#   geom_polygon(data = bb_realms, aes(long, lat, group=group), fill=NA, size=0.2, colour = "grey50") +
#   geom_polygon(data = bbox_moll_df, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
#   scale_fill_identity() + theme_void(base_size = 11) + theme(legend.position = "none")+
#   coord_equal()
# ggsave(here::here("1_Fig1C.tiff"), dpi = 600, width = 5.5, height = 3)
# 
# ggplot() + ##Areas of biodiversity importance map
#   geom_polygon(data=cont, aes(long,lat, group=group), fill="grey70") +
#   geom_tile(data=choro, aes(x, y, fill = sns85), color=NA) +
#   scale_fill_identity() +
#   geom_polygon(data = bbox_moll_df, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
#   geom_polygon(data = bb_realms, aes(long, lat, group=group), fill=NA, size=0.2, colour = "grey50")+
#   geom_text(data = realm.labels, aes(label = id, x = long, y = lat), size=2)+
#   theme_void(base_size = 11) +coord_equal()+theme(legend.position = "none")
# ggsave(here::here("4_Fig4A.tiff"), dpi = 600, width = 5.5, height = 3)
# 
# legCI<-choro%>%
#   group_by(cmbBrk85, cntxt, sns85)%>%
#   summarise(vlength = n())%>%ungroup()
# legCI$propCells = round(100*(legCI$vlength/sum(legCI$vlength)), 1)
# legCI$propCells<-paste(legCI$propCells, "%", sep = "")
# glimpse(legCI)
# 
# ggplot(legCI, aes(x = (cmbBrk85/3), y = (cntxt/3))) + 
#   geom_raster(aes(fill = sns85)) + scale_fill_identity() +
#   theme_sleek(base_size = 24)+
#   geom_text(aes(label=propCells),size = 5)+
#   scale_x_continuous(expand = c(0,0))+
#   scale_y_continuous(expand = c(0,0))+
#   theme(plot.background = element_rect(fill = NA),
#         axis.ticks = element_blank(), axis.title.y = element_text(angle = 90),
#         axis.text = element_blank(),legend.position = "none")+
#   labs(y = "Conservation value ⟶️", x = "Climate * land-use ⟶️")
# ggsave(here::here("Fig4aLeg.tiff"), dpi = 600, width = 5, height = 5)
# 
# 
# #Forest instability
# df1<-rbind.data.frame(
#   data.frame(x = choro$x, y = choro$y, fill=choro$mixBs.main, what = "Land-use instability", when = "Baseline"),
#   data.frame(x = choro$x, y = choro$y, fill=choro$mixBs.pfor, what = "Forest instability", when = "Baseline"),
#   data.frame(x = choro$x, y = choro$y, fill=choro$mix85.main, what = "Land-use instability", when = "SSP5-RCP8.5"),
#   data.frame(x = choro$x, y = choro$y, fill=choro$mix85.pfor, what = "Forest instability", when = "SSP5-RCP8.5")
# )
# ggplot() + 
#   geom_polygon(data = cont, aes(long,lat, group=group), fill="grey70") +
#   geom_tile(data = df1, aes(x, y, fill = fill), color=NA) +
#   facet_grid(when~what)+
#   #geom_polygon(data = bb_realms, aes(long, lat, group=group), fill=NA, size=0.2, colour = "grey50") +
#   geom_polygon(data = bbox_moll_df, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
#   scale_fill_identity() + theme_void(base_size = 11) + theme(legend.position = "none")+
#   coord_equal()
# ggsave(here::here("SI_FigS6.tiff"), dpi = 600, width = 5.5, height = 3.5)
# 
# #Plot legend
# legBaseline<-choro%>%group_by(ccBrkBase, luBrkBase, mix1)%>%summarise(vlength = n())%>%ungroup()
# ggplot(legBaseline, aes(x = luBrkBase, y = ccBrkBase)) + 
#   geom_tile(aes(fill = mix1)) + scale_fill_identity() +
#   theme_sleek(base_size = 25)+
#   scale_x_continuous(expand = c(0,0))+
#   scale_y_continuous(expand = c(0,0))+
#   theme(
#     plot.background = element_rect(fill = NA),
#     axis.ticks = element_blank(),
#     axis.title.y = element_text(angle = 90),
#     axis.text = element_blank(),
#     legend.position = "none")+
#   labs(x = "Land-use instability"~symbol("\256"), y = "Climate velocity"~symbol("\256"))
# #labs(x ="Land-use velocity⟶️", y = "Climate velocity⟶️")
# ggsave(here::here("legFig1C.tiff"), dpi = 600, width = 4, height = 4.73)
# 
