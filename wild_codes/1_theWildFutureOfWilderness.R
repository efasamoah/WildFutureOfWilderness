rm(list = ls())
library(raster);library(tidyverse);library(rgdal)

bbox <- rnaturalearth::ne_download(scale=50, type="wgs84_bounding_box", category="physical")%>%spTransform(CRS("+proj=moll"))%>%fortify()
land <- rnaturalearth::ne_download(scale=10, type="land", category="physical")%>%spTransform(CRS("+proj=moll"))%>%fortify()

# # IMPORT DATA
# choroData <- stack(list.files("data/wild_ms_database/ProcessedRastersMoll", pattern = ".tif$", full.names = TRUE, recursive = TRUE)) %>% as.data.frame(xy = TRUE)
# # names(choroData)
# eco_attr <- readxl::read_excel("data/wild_ms_database/input_tables/eco_attr.xlsx")
# nation_attr <- readxl::read_excel("data/wild_ms_database/input_tables/nation_attr.xlsx")
# #
# choroData <- merge(choroData, nation_attr[, c("FID", "GID_0", "Country")], by.x = "iso3_num", by.y = "FID", all.x = TRUE)
# choroData <- merge(choroData, eco_attr[, c("ECO_ID_U", "ECO_CODE", "ECO_NAME", "ECO_NUM", "WWF_REALM2", "WWF_MHTNUM", "WWF_MHTNAM")], by.x = "ecozones", by.y = "ECO_ID_U", all.x = TRUE)
# choroData$realms <- plyr::revalue(factor(choroData$WWF_REALM2),
#                                   c("Afrotropic"="AT",
#                                     "Australasia"="AA",
#                                     "Indo-Malay" = "IM",
#                                     "Nearctic" = "NA",
#                                     "Neotropic" = "NT",
#                                     "Oceania" =  NA,
#                                     "Palearctic" = "PA"))
# 
# trySet <- choroData%>%
#   filter(!is.na(ecozones),
#          !is.na(realms),
#          !is.na(velocity_temp_hist_2005),
#          !is.na(velocity_temp_rcp85_2050),
# 
#          WWF_MHTNUM!=99,
#          #!ECO_NAME %in% deserts,
#          !WWF_MHTNAM %in% c("Rock and Ice", "Inland Water"),
#          Country!="Greenland"
#   )%>%
# 
#   mutate(lat = y, long = x) %>%
#   dplyr::select(-x,-y) %>%
# 
#   mutate(
#     # #SSP5-RCP126
#     ssp5_c3ann = abs(ifelse(c3ann_ssp5 <=0, 0, c3ann_ssp5)),
#     ssp5_c3nfx = abs(ifelse(c3nfx_ssp5 <=0, 0, c3nfx_ssp5)),
#     ssp5_c3per = abs(ifelse(c3per_ssp5 <=0, 0, c3per_ssp5)),
#     ssp5_c4ann = abs(ifelse(c4ann_ssp5 <=0, 0, c4ann_ssp5)),
#     ssp5_c4per = abs(ifelse(c4per_ssp5 <=0, 0, c4per_ssp5)),
#     ssp5_pastr = abs(ifelse(pastr_ssp5 <=0, 0, pastr_ssp5)),
#     ssp5_urban = abs(ifelse(urban_ssp5 <=0, 0, urban_ssp5)),
#     ssp5_range = abs(ifelse(range_ssp5 >=0, 0, range_ssp5)),
#     ssp5_primf = abs(ifelse(primf_ssp5 >=0, 0, primf_ssp5)),
#     ssp5_secdf = abs(ifelse(secdf_ssp5 >=0, 0, secdf_ssp5)),
# 
#     #Positive transitions under SSP5
#     ssp5_c3ann_1 = abs(ifelse(c3ann_ssp5 >0, 0, c3ann_ssp5)),
#     ssp5_c3nfx_1 = abs(ifelse(c3nfx_ssp5 >0, 0, c3nfx_ssp5)),
#     ssp5_c3per_1 = abs(ifelse(c3per_ssp5 >0, 0, c3per_ssp5)),
#     ssp5_c4ann_1 = abs(ifelse(c4ann_ssp5 >0, 0, c4ann_ssp5)),
#     ssp5_c4per_1 = abs(ifelse(c4per_ssp5 >0, 0, c4per_ssp5)),
#     ssp5_pastr_1 = abs(ifelse(pastr_ssp5 >0, 0, pastr_ssp5)),
#     ssp5_urban_1 = abs(ifelse(urban_ssp5 >0, 0, urban_ssp5)),
#     ssp5_range_1 = abs(ifelse(range_ssp5 <0, 0, range_ssp5)),
#     ssp5_primf_1 = abs(ifelse(primf_ssp5 <0, 0, primf_ssp5)),
#     ssp5_secdf_1 = abs(ifelse(secdf_ssp5 <0, 0, secdf_ssp5)),
# 
#     #SSP1-RCP126
#     ssp1_c3ann = abs(ifelse(c3ann_ssp1 <=0, 0, c3ann_ssp1)),
#     ssp1_c3nfx = abs(ifelse(c3nfx_ssp1 <=0, 0, c3nfx_ssp1)),
#     ssp1_c3per = abs(ifelse(c3per_ssp1 <=0, 0, c3per_ssp1)),
#     ssp1_c4ann = abs(ifelse(c4ann_ssp1 <=0, 0, c4ann_ssp1)),
#     ssp1_c4per = abs(ifelse(c4per_ssp1 <=0, 0, c4per_ssp1)),
#     ssp1_pastr = abs(ifelse(pastr_ssp1 <=0, 0, pastr_ssp1)),
#     ssp1_urban = abs(ifelse(urban_ssp1 <=0, 0, urban_ssp1)),
#     ssp1_range = abs(ifelse(range_ssp1 >=0, 0, range_ssp1)),
#     ssp1_primf = abs(ifelse(primf_ssp1 >=0, 0, primf_ssp1)),
#     ssp1_secdf = abs(ifelse(secdf_ssp1 >=0, 0, secdf_ssp1)),
# 
#     #Positive transitions under SSP1
#     ssp1_c3ann_1 = abs(ifelse(c3ann_ssp1 >0, 0, c3ann_ssp1)),
#     ssp1_c3nfx_1 = abs(ifelse(c3nfx_ssp1 >0, 0, c3nfx_ssp1)),
#     ssp1_c3per_1 = abs(ifelse(c3per_ssp1 >0, 0, c3per_ssp1)),
#     ssp1_c4ann_1 = abs(ifelse(c4ann_ssp1 >0, 0, c4ann_ssp1)),
#     ssp1_c4per_1 = abs(ifelse(c4per_ssp1 >0, 0, c4per_ssp1)),
#     ssp1_pastr_1 = abs(ifelse(pastr_ssp1 >0, 0, pastr_ssp1)),
#     ssp1_urban_1 = abs(ifelse(urban_ssp1 >0, 0, urban_ssp1)),
#     ssp1_range_1 = abs(ifelse(range_ssp1 <0, 0, range_ssp1)),
#     ssp1_primf_1 = abs(ifelse(primf_ssp1 <0, 0, primf_ssp1)),
#     ssp1_secdf_1 = abs(ifelse(secdf_ssp1 <0, 0, secdf_ssp1)),
# #
#     #Baseline
#     base_c3ann = abs(ifelse(c3ann_hist <=0, 0, c3ann_hist)),
#     base_c3nfx = abs(ifelse(c3nfx_hist <=0, 0, c3nfx_hist)),
#     base_c3per = abs(ifelse(c3per_hist <=0, 0, c3per_hist)),
#     base_c4ann = abs(ifelse(c4ann_hist <=0, 0, c4ann_hist)),
#     base_c4per = abs(ifelse(c4per_hist <=0, 0, c4per_hist)),
#     base_pastr = abs(ifelse(pastr_hist <=0, 0, pastr_hist)),
#     base_urban = abs(ifelse(urban_hist <=0, 0, urban_hist)),
#     base_range = abs(ifelse(range_hist >=0, 0, range_hist)),
#     base_primf = abs(ifelse(primf_hist >=0, 0, primf_hist)),
#     base_secdf = abs(ifelse(secdf_hist >=0, 0, secdf_hist)),
# 
#     #Positive transitions under baseline conditions
#     base_c3ann_1 = abs(ifelse(c3ann_hist >0, 0, c3ann_hist)),
#     base_c3nfx_1 = abs(ifelse(c3nfx_hist >0, 0, c3nfx_hist)),
#     base_c3per_1 = abs(ifelse(c3per_hist >0, 0, c3per_hist)),
#     base_c4ann_1 = abs(ifelse(c4ann_hist >0, 0, c4ann_hist)),
#     base_c4per_1 = abs(ifelse(c4per_hist >0, 0, c4per_hist)),
#     base_pastr_1 = abs(ifelse(pastr_hist >0, 0, pastr_hist)),
#     base_urban_1 = abs(ifelse(urban_hist >0, 0, urban_hist)),
#     base_range_1 = abs(ifelse(range_hist <0, 0, range_hist)),
#     base_primf_1 = abs(ifelse(primf_hist <0, 0, primf_hist)),
#     base_secdf_1 = abs(ifelse(secdf_hist <0, 0, secdf_hist)))%>%
# #
#   rowwise()%>%
#   mutate(ssp5_lui = sum(c(ssp5_primf,ssp5_secdf,ssp5_range,ssp5_c3ann,ssp5_c3nfx,ssp5_c3per,ssp5_c4ann,ssp5_c4per,ssp5_pastr,ssp5_urban), na.rm = TRUE),
#          ssp5_lui_1 = sum(c(ssp5_primf_1,ssp5_secdf_1,ssp5_range_1,ssp5_c3ann_1,ssp5_c3nfx_1,ssp5_c3per_1,ssp5_c4ann_1,ssp5_c4per_1,ssp5_pastr_1,ssp5_urban_1), na.rm = TRUE),
# 
#          ssp1_lui = sum(c(ssp1_primf,ssp1_secdf,ssp1_range,ssp1_c3ann,ssp1_c3nfx,ssp1_c3per,ssp1_c4ann,ssp1_c4per,ssp1_pastr,ssp1_urban), na.rm = TRUE),
#          ssp1_lui_1 = sum(c(ssp1_primf_1,ssp1_secdf_1,ssp1_range_1,ssp1_c3ann_1,ssp1_c3nfx_1,ssp1_c3per_1,ssp1_c4ann_1,ssp1_c4per_1,ssp1_pastr_1,ssp1_urban_1), na.rm = TRUE),
# 
#          base_lui = sum(c(base_primf,base_secdf,base_range,base_c3ann,base_c3nfx,base_c3per,base_c4ann,base_c4per,base_pastr,base_urban), na.rm = TRUE),
#          base_lui_1 = sum(c(base_primf_1,base_secdf_1,base_range_1,base_c3ann_1,base_c3nfx_1,base_c3per_1,base_c4ann_1,base_c4per_1,base_pastr_1,base_urban_1), na.rm = TRUE),
# 
#          #Multivariate VoCC
#          base_vcc=max(c(velocity_temp_hist_2005,velocity_prec_hist_2005), na.rm = TRUE),
#          ssp1_vcc=max(c(velocity_temp_rcp26_2050,velocity_prec_rcp26_2050), na.rm = TRUE),
#          ssp5_vcc=max(c(velocity_temp_rcp85_2050,velocity_prec_rcp85_2050), na.rm = TRUE),
# 
#          dev_lui = log((.01+ssp1_lui)/(.01+ssp5_lui)),
#          dev_vcc = log(ssp1_vcc/ssp5_vcc))%>%
#   data.frame()%>%filter(!is.na(iso3_num))
#
# trySet$pa_dum <- ifelse(trySet$protect >0, "PA", "nonPA"); trySet$pa_dum[is.na(trySet$pa_dum)] <- "nonPA"
# # summary(factor(trySet$pa_dum))
# trySet$wildBin <- ifelse(trySet$wilderness<65535, "wild","nonwild")
# trySet <- trySet[!is.na(trySet$wildBin),]
# summary(factor(trySet$wildBin))
# 
# trySet <- trySet %>%
#   mutate(diff_base = (base_lui-base_lui_1),
#          diff_ssp1 = (ssp1_lui-ssp1_lui_1),
#          diff_ssp5 = (ssp5_lui-ssp5_lui_1),
#          # diffBin = ifelse(diff_base >= 1,6,
#          #                  ifelse(diff_base>=.5,5,
#          #                         ifelse(diff_base>=0,4,
#          #                                ifelse(diff_base>=-.5,3,
#          #                                       ifelse(diff_base>= -1,2,1))))),
#          unchange_base = ifelse(base_lui==base_lui_1, 1,0),
#          unchange_ssp1 = ifelse(ssp1_lui==ssp1_lui_1, 1,0),
#          unchange_ssp5 = ifelse(ssp5_lui==ssp5_lui_1, 1,0))
# 
# write_rds(trySet, "./output_tables/WildDataRevised.Rds")


################################################################################
#                   MAP BASELINE OVERLAPS (FIGURE 1)
################################################################################
# IMPORT DATA
wildMSData <- read_rds("./output_tables/WildDataRevised.Rds")
names(wildMSData)

#IMPORT SOME FUNCTIONS
source("./wild_codes/6_ImpFunctions.R")
#######################################################################################
#                                    PLOTS SUPPLEMENTARY FIG_S5 and S6
######################################################################################
ggplot()+
  geom_polygon(data = land, aes(long, lat, group=group), fill = "grey80")+
  geom_polygon(data = bbox, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
  geom_raster(data = subset(wildMSData, wildBin=="wild"&unchange_base==0), 
              aes(long,lat, fill=factor(ntile(diff_base,7))))+
  coord_equal()+ theme_void(base_size = 15) +
  #guides(fill = guide_legend(ncol = 1))+
  theme(legend.position="none",
        legend.direction="horizontal",
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(.1, 'cm'),
        #legend.text=element_blank(),
        #legend.spacing.x = unit(0, 'cm'),
        #axis.text=element_text(size=10),
        legend.title=element_blank())+
  scale_fill_viridis_d(option="C")+geom_raster(data = subset(wildMSData, wildBin=="wild"&unchange_base==1),aes(long,lat), fill="grey30", alpha=.5)
ggsave(here::here("./out_plots/SI_FigS5A.tiff"), dpi = 600, width = 5.5, height = 3)

summary(wildMSData)
df_map <- rbind.data.frame(
  data.frame(trnsType = "Progressive", scenario = "base", long = wildMSData$long, lat = wildMSData$lat, value = wildMSData$base_lui, wilderness = wildMSData$wildBin),
  data.frame(trnsType = "Progressive", scenario = "ssp1", long = wildMSData$long, lat = wildMSData$lat, value = wildMSData$ssp1_lui, wilderness = wildMSData$wildBin),
  data.frame(trnsType = "Progressive", scenario = "ssp5", long = wildMSData$long, lat = wildMSData$lat, value = wildMSData$ssp5_lui, wilderness = wildMSData$wildBin),
  
  data.frame(trnsType = "Retrogressive", scenario = "base", long = wildMSData$long, lat = wildMSData$lat, value = wildMSData$base_lui_1, wilderness = wildMSData$wildBin),
  data.frame(trnsType = "Retrogressive", scenario = "ssp1", long = wildMSData$long, lat = wildMSData$lat, value = wildMSData$ssp1_lui_1, wilderness = wildMSData$wildBin),
  data.frame(trnsType = "Retrogressive", scenario = "ssp5", long = wildMSData$long, lat = wildMSData$lat, value = wildMSData$ssp5_lui_1, wilderness = wildMSData$wildBin))

# hist(subset(df_map, wilderness=="wild")$value, breaks=50)
# max(df_map$value, na.rm = TRUE)
ggplot()+ 
  geom_polygon(data = land, aes(long, lat, group=group), fill = "grey80")+
  geom_polygon(data = bbox, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
  geom_raster(data = subset(df_map, wilderness=="wild"), aes(long,lat, fill=value+0.1))+
  coord_equal()+ theme_void(base_size = 15) +
  #guides(fill = guide_legend(ncol = 1))+
  facet_grid(scenario~trnsType, scales = "fixed")+
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(.1, 'cm'),
        #legend.text=element_blank(),
        #legend.spacing.x = unit(0, 'cm'),
        #axis.text=element_text(size=10),
        legend.title=element_blank())+
  scale_fill_viridis_c(option="C", trans="log", breaks=c(0.1,25), label = c("Low", "High"))#+
#geom_raster(data = subset(df_map, wilderness=="wild"&unchanged==1),aes(long,lat), fill="grey30", alpha=.5)
ggsave(here::here("./out_plots/SI_FigS6.tiff"), dpi = 600, width = 5.5, height = 4)



# Calculate and plot estimates for the baseline as bars
# Summarize data by protection status and calculate standard error of mean at 95% using non-central t-distribution
wildMSData <- wildMSData %>% group_by(wilderness) %>% mutate(N = n(), Ww = N/sum(N)) %>% ungroup()
velSum1 <- wildMSData %>% filter(wildBin=="wild") %>% group_by(pa_dum) %>%
  summarise(N=n(),
            mnVoCC=gm_mean(base_vcc,na.rm=TRUE),
            #mnVoCC_n=weighted.mean(base_vcc,Ww,na.rm=TRUE),
            sdVoCC=sd(base_vcc, na.rm=TRUE),
            #vccUP=spatstat.geom::weighted.quantile(base_vcc,Ww,na.rm=TRUE)[[4]],
            #vccLW=spatstat.geom::weighted.quantile(base_vcc,Ww,na.rm=TRUE)[[2]],
            errVoCC=qt(0.975, df = N-1)*(sdVoCC/sqrt(N)))%>%
  mutate(status=pa_dum)%>%select(-pa_dum)

velSum2 <- wildMSData %>% filter(wildBin=="wild") %>% group_by(wildBin)%>%
  summarise(N = n(),
            mnVoCC=gm_mean(base_vcc,na.rm=TRUE),
            #mnVoCC_n=weighted.mean(base_vcc,Ww,na.rm=TRUE),
            sdVoCC=sd(base_vcc, na.rm=TRUE),
            #vccUP=spatstat.geom::weighted.quantile(base_vcc,Ww,na.rm=TRUE)[[4]],
            #vccLW=spatstat.geom::weighted.quantile(base_vcc,Ww,na.rm=TRUE)[[2]],
            errVoCC=qt(0.975, df = N-1)*(sdVoCC/sqrt(N)))%>% 
  mutate(status=wildBin)%>%select(-wildBin)
(vlSum <- rbind(velSum1, velSum2))

#hist(subset(wildMSData, wildBin=="wild")$base_lui, breaks=50)
ins_Sum1 <- wildMSData %>% filter(wildBin=="wild") %>% group_by(pa_dum) %>%
  summarise(N=n(),
            mnLUI=gm_mean(.1+base_lui,na.rm=TRUE),
            #mnLUI_n=weighted.mean(.1+base_lui,Ww,na.rm=TRUE),
            sdLUI=sd(.1+base_lui, na.rm=TRUE),
            #luiUP=spatstat.geom::weighted.quantile(base_lui,Ww,na.rm=TRUE)[[4]],
            #luiLW=spatstat.geom::weighted.quantile(base_lui,Ww,na.rm=TRUE)[[2]],
            errLUI = qt(0.975, df = N-1)*(sdLUI/sqrt(N)))%>% 
  mutate(status = pa_dum)%>%select(-pa_dum)

ins_Sum2 <- wildMSData %>% filter(wildBin=="wild") %>% group_by(wildBin) %>%
  summarise(N=n(),
            mnLUI=gm_mean(.1+base_lui,na.rm=TRUE),
            #mnLUI_n=weighted.mean(.1+base_lui,Ww,na.rm=TRUE),
            sdLUI=sd(.1+base_lui, na.rm=TRUE),
            #luiUP=spatstat.geom::weighted.quantile(base_lui,Ww,na.rm=TRUE)[[4]],
            #luiLW=spatstat.geom::weighted.quantile(base_lui,Ww,na.rm=TRUE)[[2]],
            errLUI = qt(0.975, df = N-1)*(sdLUI/sqrt(N))) %>% 
  mutate(status = wildBin) %>% select(-wildBin)

(luSum <- rbind(ins_Sum1, ins_Sum2))

# melt data into long format
df1 <- rbind(
  data.frame(what="Climate", status=vlSum$status, g.mean=vlSum$mnVoCC, UpperL=vlSum$mnVoCC+vlSum$errVoCC, LowerL=vlSum$mnVoCC-vlSum$errVoCC),
  data.frame(what="Land use", status=luSum$status, g.mean=luSum$mnLUI, UpperL=luSum$mnLUI+luSum$errLUI, LowerL=luSum$mnLUI-luSum$errLUI))%>%
  mutate(status = ifelse(status=="PA", "PW", status),
         status = ifelse(status=="nonPA", "UW", status),
         status = ifelse(status=="wild", "AW", status),
         status = factor(status, levels=c("AW","PW","UW"))) #revalue and sort factors
#Plot bars with errors
(Fig1A <- ggplot(df1, aes(status, g.mean, fill=status))+
    geom_bar(stat="identity", position=position_dodge(), width = 0.8) +
    geom_errorbar(aes(ymin=LowerL, ymax=UpperL), width=.15, position=position_dodge(.9))+
    scale_fill_viridis_d(option = "A", direction = -1, begin = 0.70, end = 1)+
    #scale_fill_manual(name = "", values = c("#e9e9e9","#9a9a9a","#454545"))+
    scale_y_continuous(expand = expansion(mult = c(0, .1)), labels=scales::number_format(accuracy = 0.01))+
    facet_wrap(~what, scales="free")+
    labs(x="Status", y=expression("Velocity or instability" ~ paste("(","km", yr^-1,")")))+
    theme_sleek(base_size=8)+
    theme(legend.position="top",
          #legend.text=element_text(size=8),
          legend.title=element_blank(),
          #axis.text=element_text(size=10)
    ))
# 
# expData <- wildMSData %>% filter(wildBin=="wild")
# expData2 <- rbind(data.frame(what="Climate", rbind(cbind.data.frame(status=expData$pa_dum, value=expData$base_vcc), cbind.data.frame(status=expData$wildBin, value=expData$base_vcc))),
#                   data.frame(what="Land use", rbind(cbind.data.frame(status=expData$pa_dum, value=expData$base_lui), cbind.data.frame(status=expData$wildBin, value=expData$base_lui))))%>%
#   mutate(status = ifelse(status=="PA", "PW", status),
#          status = ifelse(status=="nonPA", "UW", status),
#          status = ifelse(status=="wild", "AW", status),
#          status = factor(status, levels=c("AW","PW","UW"))) 
# (Fig1A <- ggplot(expData2, aes(status, 0.01+value, fill=status))+
#   geom_violin() +
#   geom_boxplot(colour = "black", width=0.05)+
#   scale_fill_viridis_d(option = "A", direction = -1, begin = 0.70, end = 1)+
#   scale_y_continuous(expand = expansion(mult = c(0, .1)), labels=scales::number_format(accuracy = 0.01), trans = "log")+
#   facet_wrap(~what, scales="free")+
#   labs(x="Status", y=expression("Velocity or instability" ~ paste("(","km", yr^-1,")")))+
#   theme_sleek(base_size=8)+
#   theme(legend.position="top",legend.title=element_blank()))

# Plot Fig1B
vsum_patch <- subset(wildMSData, wildBin=="wild") %>% 
  group_by(GID_0)%>% #previous was at the patch-level
  summarise(N = n(),
            vClim_base = gm_mean(base_vcc, na.rm = TRUE),
            vLand_base = gm_mean(.1+base_lui, na.rm = TRUE),
            x = mean(long, na.rm = T), # find centroids
            y = mean(long, na.rm = T))%>%
  filter(N >5)%>% # limit estimate to less than three CORDEX pixels to limit miscalculations
  ungroup()%>%na.omit()
summary(vsum_patch);(vsum_patch)

library(SpatialPack)
coords <- vsum_patch[c("x","y")]
modified.ttest(log(vsum_patch$vClim_base), log(vsum_patch$vLand_base), coords)
# Corrected Pearson's correlation for spatial autocorrelation
# 
# data: x and y ; coordinates: x and y 
# F-statistic: 13.6209 on 1 and 37.0572 DF, p-value: 7e-04 
# alternative hypothesis: true autocorrelation is not equal to 0
# sample correlation: -0.5184
(
  Fig1B1 <- ggplot(vsum_patch, aes(x=(vLand_base), y=vClim_base)) +
    geom_density_2d_filled(contour_var="ndensity")+
    scale_fill_viridis_d(name="", labels=c(.1,.2,.3,.4,.5,.6,.7,.8,.9, 1), option = "A")+
    scale_x_continuous(expand = c(0,0), trans = "log", labels = scales::number_format(accuracy = 1), breaks = c(0.01,1,3))+
    scale_y_continuous(expand = c(0,0), trans = "log", labels = scales::number_format(accuracy = 1), breaks = c(0.01,1,8))+
    annotate("text", x = 0.5, y = 0.25, label = expression(italic(r[spatial]) ~ paste(" = -0.518, P < 7 x" , 10^-4,)),size=2.5, colour = "white")+
    theme_sleek(base_size = 8)+
    labs(x=expression("Land-use instability" ~ paste("(","km", yr^-1,")")),
         y=expression("Climate velocity" ~ paste("(","km", yr^-1,")")))+
    theme(legend.position="top", 
          legend.direction="horizontal", 
          legend.key.width = unit(.5, 'cm'),
          legend.key.height = unit(.25, 'cm'),
          #legend.text=element_text(size=8),
          #axis.text=element_text(size=10),
          legend.title=element_blank())+
    guides(fill=guide_legend(nrow=2))
)

#MAPS (FIGURE 1C)
source("wild_codes/others/BivariateMap.R") #import bivariate codes
# Define the number of breaks
nBreaks <- 50

# Create the colour matrix
col.matVul <- colmat(nbreaks = nBreaks, breakstyle = "quantile", 
                     xlab = "X", ylab = "Y",
                     upperleft = "dodgerblue4",
                     upperright = "darkred", 
                     bottomleft =  "#BEBEBE", 
                     bottomright = "yellow",
                     saveLeg = FALSE, plotLeg = TRUE)
# Retrieve bivariate colour pallet data
lgd <- BivLegend$data; names(lgd) <- c("binY", "binX", "HEXCode", "UID")

(globalAv = gm_mean(0.1+subset(wildMSData)$base_lui)) #Save the global average
choro <- subset(wildMSData) %>%
  select(long, lat, base_vcc, base_lui, wildBin, base_lui_1, diff_base)%>%
  mutate(binY = ntile(base_vcc, 50),
         #binX = ntile(diff_base, 50),
         binX = ifelse((.1+base_lui)<=globalAv,1,50),
         alpa = atan(binX/binY))%>%
  inner_join(y = lgd, by = c("binY", "binX"))

(expMapsBase <- ggplot() +
    geom_polygon(data = land, aes(long, lat, group=group), fill = "grey80")+
    geom_tile(data = subset(choro, wildBin=="wild"), aes(long, lat, fill = HEXCode), color=NA)+
    scale_fill_identity() +
    geom_polygon(data = bbox, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
    theme_void(base_size = 11) + coord_equal()+theme(legend.position = "none"))

# Generate data for legend
(legCI <- choro %>%
    group_by(binY, binX, HEXCode) %>% summarise(N=n()) %>% ungroup() %>%
    ggplot(aes(x=binX, y=binY, fill=HEXCode))+#geom_raster()+
    geom_col(position = "stack")+ 
    scale_fill_identity()+coord_flip()+
    theme_minimal(base_size=5)+
    scale_x_continuous(expand=c(0,0), breaks=c(0, 50), labels=c("Low LUI", "High LUI"))+
    scale_y_continuous(expand=c(0,0))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())+
    labs(y = expression("Climate velocity," ~ paste("km", yr^-1,) ~ symbol("\256")),
         #y = Delta~ "Climate change velocity" ~symbol("\256"),
         x = "")+
    theme(legend.position = "none"))
(Fig1C <- expMapsBase+annotation_custom(ggplotGrob(legCI), ymin=-1.5e6, ymax = -4e6, xmin = -17e6, xmax = -8.5e6))
(Fig1 <- cowplot::plot_grid(cowplot::plot_grid(Fig1A, Fig1B1, labels = c("A", "B")), Fig1C, labels = c("", "C"), nrow = 2))
#ggsave(plot = Fig1, here::here("./out_plots/1_Fig1_base.tiff"), dpi=600, width = 5.5, height = 6)

#library(metR)
#library(magrittr)
#source("./wild_codes/others/new_scales.R")
#choro <- WildernessData %>% select(everything()) %>%
#  mutate(luBrkBase=ifelse(LUI_N_base <0.08,1,2),
#         pfBrkBase=ifelse(abs(primf2050_hist) <0.08,1,2),
#         luBrk85=ifelse(LUI_N_ssp585 <0.08,1,2),
#         pfBrk85=ifelse(abs(primf2050_585) <0.08,1,2),
#         
#         BrkBase=ntile_na(VoCC_N_base, 10)) %>% as.data.frame()

#(PlA <- ggplot() +
#    geom_polygon(data=land, aes(long,lat, group=group), fill="grey70") +
#    geom_tile(data=subset(choro, luBrkBase==2), aes(long, lat, fill=BrkBase), color=NA) +
#    geom_polygon(data=bb_realms, aes(long, lat, group=group), fill=NA, size=0.2, colour = "grey50") +
#    #scale_fill_viridis_c(name = "High LUI", option = "C", breaks = c(1, 10), label = c("", "High"), 
#    #                     guide = guide_colorbar(reverse=F, title.hjust = 0.5, title.position = "top")) + 
#    scale_fill_gradient2("High LUI", limits = c(1, 10), low="#762A83", high="#1B7837", breaks = c(1, 10), label = c("", "High"), 
#                         guide = guide_colorbar(reverse=F, title.hjust = 0.5, title.position = "top"))+
#    theme_void(base_size = 11) + theme(legend.position="bottom",
#                                       legend.key.width=unit(.8, 'cm'),
#                                       legend.key.height=unit(.2, 'cm'),
#                                       legend.spacing.x=unit(0, "cm")
#                                       )+
#    coord_equal() + new_scale_fill() +
#    geom_tile(data = subset(choro, luBrkBase==1), aes(long, lat, fill=BrkBase), color=NA) +
#    #scale_fill_viridis_c(name = "Low LUI", option = "D", breaks = c(1, 10), label = c("Low", "High"), 
#    #                     guide = guide_colorbar(reverse=T,title.hjust = 0.5, title.position = "top"))#+
#    #scale_fill_viridis_c(name = expression("LUI" < "0.08" ~ paste("km", yr^-1,)), option = "D", breaks = c(1, 10), label = c("Low", "High"), guide = guide_colorbar(title.hjust = 0.5, title.position = "top"))#+
#    scale_fill_gradient2("Low LUI", limits=c(1, 10), low="#1B7837", high="#762A83",
#                         breaks = c(1, 10), label = c("Low", "High"),
#                         guide = guide_colorbar(reverse=TRUE, title.hjust=0.5, title.position = "top"))
#    )


############################################################################################################################################
#                     EXAMINE PROJECTED CHANGES IN VELOCITIES AND INSTABILITIES (FIGURE 2)
###########################################################################################################################################
# Store the global averages (gMean)
# (gMean_cc <- gm_mean(subset(wildMSData, wildBin=="wild")$base_vcc, na.rm=TRUE))
# (gMean_lu <- gm_mean(subset(wildMSData, wildBin=="wild")$base_lui+0.1, na.rm=TRUE))

# Calculate and plot estimates for the baseline as bars
velPWild <- subset(wildMSData, wildBin=="wild") %>%
  group_by(GID_0)%>% 
  mutate(base_sum = gm_mean(base_vcc, na.rm=TRUE)) %>% ungroup() %>%
  mutate(diff_ssp5 = (ssp5_vcc-base_sum)/base_sum, diff_ssp1 = (ssp1_vcc-base_sum)/base_sum)%>% 
  group_by(pa_dum) %>%
  summarise(cSSP5=mean(diff_ssp5, na.rm=TRUE),
            cSSP1=mean(diff_ssp1, na.rm=TRUE)); names(velPWild)<-c("status", "perVcSSP5","perVcSSP1")

velPWild2 <- subset(wildMSData) %>%
  group_by(GID_0)%>% 
  mutate(base_sum = gm_mean(base_vcc, na.rm=TRUE)) %>% ungroup() %>%
  mutate(diff_ssp5 = (ssp5_vcc-base_sum)/base_sum, diff_ssp1 = (ssp1_vcc-base_sum)/base_sum)%>%
  group_by(wildBin)%>%
  summarise(cSSP5=mean(diff_ssp5, na.rm=TRUE),
            cSSP1=mean(diff_ssp1, na.rm=TRUE)); names(velPWild2) <- c("status", "perVcSSP5","perVcSSP1")
velSSPClim <- rbind(velPWild, velPWild2)
velSSPClim

velPWild <- subset(wildMSData, wildBin=="wild") %>%
  group_by(GID_0)%>% 
  mutate(base_sum = gm_mean(base_lui, na.rm=TRUE)) %>% ungroup() %>%
  mutate(diff_ssp5 = (ssp5_lui-base_sum)/base_sum, diff_ssp1 = (ssp1_lui-base_sum)/base_sum) %>% 
  group_by(pa_dum) %>%
  summarise(perVcSSP5=mean(diff_ssp5, na.rm=TRUE),
            perVcSSP1=mean(diff_ssp1, na.rm=TRUE));names(velPWild) <- c("status","perVlSSP5", "perVlSSP1")

velPWild2 <- subset(wildMSData) %>%
  group_by(wilderness)%>% 
  mutate(base_sum = gm_mean(base_lui, na.rm=TRUE)) %>% ungroup() %>%
  mutate(diff_ssp5 = (ssp5_lui-base_sum)/base_sum, diff_ssp1 = (ssp1_lui-base_sum)/base_sum) %>% 
  group_by(wildBin) %>%
  summarise(perVcSSP5=mean(diff_ssp5, na.rm=TRUE),
            perVcSSP1=mean(diff_ssp1, na.rm=TRUE)); names(velPWild2) <- c("status","perVlSSP5", "perVlSSP1")
velSSPLuse <- rbind(velPWild,velPWild2)
velSSPLuse

# melt data into long format
velGlobal <- rbind(
  data.frame(what="Climate", scenario="SSP1-RCP2.6", status=velSSPClim$status, mean=velSSPClim$perVcSSP1),
  data.frame(what="Climate", scenario="SSP5-RCP8.5", status=velSSPClim$status, mean=velSSPClim$perVcSSP5),
  data.frame(what="Land use", scenario="SSP1-RCP2.6", status=velSSPLuse$status, mean=velSSPLuse$perVlSSP1),
  data.frame(what="Land use", scenario="SSP5-RCP8.5", status=velSSPLuse$status, mean=velSSPLuse$perVlSSP5))

#velGlobal$status <- plyr::revalue(velGlobal$status, c("nonPA" = "UW", "PA" = "PW", "wild" = "AW"))
#velGlobal$status <- factor(velGlobal$status, levels=c("AW","PW","UW")) 

#Plot bars
ggplot(velGlobal, aes(scenario, mean, fill = status))+
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_viridis_d(option = "A", direction = -1, begin = 0.70, end = 1)+
  scale_y_continuous(expand = c(0,NA), #limits = c(-.25, .55), breaks = c(-.25, 0, .25, .50),
                     label = scales::percent)+
  facet_wrap(~what, scales = "free_x")+
  labs(x = "Scenario", y = "Percent change")+
  theme_sleek(base_size = 24)+
  theme(legend.position = c(0.8, 0.2),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        plot.background = element_rect(size = 5),
        #strip.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 11,  angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11)
  )
ggsave("out_plots/Manuscript/2_Fig2xx.tiff", dpi = 600, width = 5.4, height = 6.15)
#write.csv(velGlobal, "averageProjectedChange.csv")


#######################################################################################################################################
#                                   FIGURE 3: PLOT CLIMATE POLAR
#######################################################################################################################################
realmsRenamed <- readr::read_csv("./data/wild_ms_database/input_tables/realmsRenamed.csv")
climCircular <- subset(wildMSData, wildBin=="wild") %>% 
  select(everything()) %>% inner_join(realmsRenamed, by="Country")%>%
  group_by(Country, Biorealms) %>%
  summarise(N = n(),
            vBase = gm_mean(base_vcc, na.rm=TRUE),
            vSus = gm_mean(ssp1_vcc, na.rm=TRUE),
            vHigh = gm_mean(ssp5_vcc, na.rm=TRUE)) %>% 
  na.omit() %>% filter(N >5) %>%  # filter out Greenland and limit the >5 CORDEX pixels
  ungroup() %>%
  mutate(Biorealms = factor(Biorealms), # Create new variables percentages of countries baseline
         vBAU = 100*(vHigh-vBase)/vBase, 
         vSUS = 100*(vSus-vBase)/vBase, 
         
         Biorealms = ifelse(Biorealms == "NA1", "NA", Biorealms))


subset(climCircular, N<20)$Country
# [1] "Congo, Dem. Rep." "Ethiopia"         "Kenya"            "Madagascar"       "Morocco"         
# [6] "Mozambique"       "Norway"           "South Africa"     "Tajikistan"       "Thailand"        
# [11] "Vietnam"          "Zambia"  
100*(nrow(subset(climCircular, vBAU>=0))/nrow(climCircular))
# # Nearly 90% (51 out 57) nations could experience higher changes in the future than the baseline.
100*(nrow(subset(climCircular, vBAU>0&vBAU>=50))/nrow(subset(climCircular, vBAU>0)))
# # Over half (~54%; 27 of 50 nations) of these could experience over 50% that of the baseline on the average.

dim(climCircular) # 65 countries remaining
climCircular <- rbind( # convert data to long format by scenarios 
  data.frame(realms=climCircular$Biorealms, Country=climCircular$Country, size=climCircular$N, velocity=climCircular$vSUS, scenario="SSP1-RCP2.6"),
  data.frame(realms=climCircular$Biorealms, Country=climCircular$Country, size=climCircular$N, velocity=climCircular$vBAU, scenario="SSP5-RCP8.5"))

# Rename some countries and Biogeographical realms
climCircular$Country <- car::recode(climCircular$Country,"'Venezuela, RB'='Venezuela'")
climCircular$Country <- car::recode(climCircular$Country,"'Russian Federation'='Russia'")
climCircular$Country <- car::recode(climCircular$Country,"'Egypt, Arab Rep.'='Egypt'")
climCircular$Country <- car::recode(climCircular$Country,"'Congo, Dem. Rep.'='Democratic Rep. of Congo'")
climCircular$Country <- car::recode(climCircular$Country,"'Congo, Rep.'='Republic of Congo'")
climCircular$Country <- car::recode(climCircular$Country,"'Lao PDR'='Laos'")
climCircular$Country <- car::recode(climCircular$Country,"'Yemen, Rep.'='Yemen'")

climPolar_data <- climCircular %>% group_by(realms, Country) %>% mutate(tot=sum(velocity)) %>% ungroup()
climPolar_data <- climPolar_data %>% select(everything()) %>% arrange(realms, tot) %>% 
  add_row(realms = rep(unique(climPolar_data$realms), 4)) %>% # add a couple of empty rows to separate continents
  arrange(realms) %>% ungroup()

climPolar_data$id <- rep(seq(1, nrow(climPolar_data)/nlevels(as.factor(climPolar_data$scenario))), each=nlevels(as.factor(climPolar_data$scenario)))

# Get the name and the y position of each label
label_data <- climPolar_data %>% group_by(id, Country) %>% summarize(tot=sum(velocity))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

realms_breaks <- climPolar_data %>% 
  group_by(realms) %>% 
  summarize(start = min(id), end = max(id) - 2) %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end))) %>% 
  ungroup() %>% 
  mutate(end = data.table::shift(end + 1, n = 1, type = "shift", fill = max(end) + 1), start = start -1) 

realms_breaks$start[realms_breaks$realms == "AA"] <- -1
realms_breaks$end[realms_breaks$realms == "AA"] <- 0

max_value <- max(climPolar_data$tot, na.rm = T); y_max <- 20 * ceiling(max_value/20); v <- c(-50,0,50,100, 200)
realms_breaks <- realms_breaks %>% mutate(v = list(v)) %>% unnest()

climPolar_data$scenario <- factor(climPolar_data$scenario, levels = c("SSP1-RCP2.6","SSP5-RCP8.5"))
climPolar_data %>% ggplot()+
  geom_bar(aes(x = as.factor(id), y = velocity, fill = scenario), 
           position="stack", stat="identity", width = 0.5) +
  geom_segment(data = realms_breaks, aes(x = end, y = v, xend = start, yend = v), colour = "grey", size=0.3 , inherit.aes = FALSE ) +
  annotate("text",
           x = rep(max(climPolar_data$id[climPolar_data$realms == "PA"]), length(v)),
           y = v -5, label = paste0(head(v), "%"), color = "grey", size = 3, angle = 0, fontface = "bold", hjust = 0.7)+
  ylim(-200, y_max+200)+
  #scale_fill_manual(values = c("#be64ac", "#3b4994"))+
  paletteer::scale_fill_paletteer_d(palette = "rcartocolor::Pastel")+
  theme_minimal(base_size = 15) +
  guides(fill = guide_legend(nrow = 1))+
  coord_polar()+
  theme(
    legend.position = c(0.5, 0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(1,-1,-1,-1), "cm")
  )+
  geom_text(data = label_data, aes(x = id, y = y_max, label = Country, hjust = hjust), color="black",  size = 3.5, angle = label_data$angle, inherit.aes = FALSE)+
  geom_text(data = realms_breaks, aes(x = title, y = 200, label = realms),  colour = "black", alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE)
ggsave("3_Fig3x.tiff", dpi = 300, height = 11, width = 10)

