# library(raster)
# library(dplyr)
# library(rgdal)
# library(sf)
library(tidyverse)
#IMPORT SOME FUNCTIONS
source("./wild_codes/6_ImpFunctions.R")
# 
# terrWilderness <- readOGR("./data/wild_ms_database/shp_vectors/Wilderness_maps_R.1", layer = "LoW_2009_current")%>%spTransform(CRS("+proj=longlat")) # Wilderness polygons are available online at 
# clim_stack <- stack(list.files("D:/R - HoMe/3_PNAS-WILDERNESS/WLD_35YR-ENV2021/data/wild_ms_database/climate", pattern = ".tif$", full.names = TRUE)) # Raster layers of velocity of climate change are archeived in Figshare under the Identifier https://doi.org/10.6084/m9.figshare.16892629
# crs(clim_stack) <-"+proj=longlat"
# wildExtract <- extract(x=clim_stack, y=terrWilderness, fun = mean, sp=T, df=T)#%>%as.data.frame()
# nrow(wildExtract)
# 
# #calculate diameter based on resTime in VoCC
# wildExtract$VoCC_N_base <- apply(wildExtract[, c("velocity_temp_hist_2005", "velocity_prec_hist_2005")],1, function(x) max(x, na.rm = TRUE))
# wildExtract$VoCC_N_ssp585 <- apply(wildExtract[, c("velocity_temp_rcp85_2050", "velocity_prec_rcp85_2050")],1, function(x) max(x, na.rm = TRUE))
# wildExtract$VoCC_N_ssp126 <- apply(wildExtract[, c("velocity_temp_rcp26_2050", "velocity_prec_rcp26_2050")],1, function(x) max(x, na.rm = TRUE))
# 
# resTimeData <- wildExtract  %>% rowwise() %>%
#   mutate(VoCC_N_base = max(c(velocity_temp_hist_2005,velocity_prec_hist_2005), na.rm = TRUE),
#          VoCC_N_ssp585 = max(c(velocity_temp_rcp85_2050,velocity_prec_rcp85_2050), na.rm = TRUE),
#          VoCC_N_ssp126 = max(c(velocity_temp_rcp26_2050,velocity_prec_rcp26_2050), na.rm = TRUE))%>%ungroup() %>%
#   mutate(patch_diam = 2*sqrt(area_km2/pi)) %>%
#   select(realm,area_km2,id,velocity_temp_hist_2005,
#          velocity_temp_rcp26_2050,velocity_temp_rcp85_2050,
#          velocity_prec_hist_2005,velocity_prec_rcp26_2050,
#          velocity_prec_rcp85_2050,patch_diam,VoCC_N_base,
#          VoCC_N_ssp585,VoCC_N_ssp126) %>%
#   mutate(CRT_base = patch_diam/VoCC_N_base,
#          CRT_ssp585 = patch_diam/VoCC_N_ssp585,
#          CRT_ssp126 = patch_diam/VoCC_N_ssp126)
# 
# nrow(resTimeData)
# resTimeData$TempRT_ssp585 <- resTimeData$patch_diam/resTimeData$velocity_temp_rcp85_2050
# resTimeData$TempRT_ssp126 <- resTimeData$patch_diam/resTimeData$velocity_temp_rcp26_2050
# resTimeData$PrecRT_ssp585 <- resTimeData$patch_diam/resTimeData$velocity_prec_rcp85_2050
# resTimeData$PrecRT_ssp126 <- resTimeData$patch_diam/resTimeData$velocity_prec_rcp26_2050
# write_rds(resTimeData, "./output_tables/ClimateResidenceTime.Rds")

resTimeData <- read_rds("./output_tables/ClimateResidenceTime.Rds")
names(resTimeData)
# Compute year of shifts
df.YOS1 <- rbind(
  data.frame(YearOfShifts = 2016+resTimeData$CRT_ssp585, scenario = "SSP5-RCP8.5"),
  data.frame(YearOfShifts = 2016+resTimeData$CRT_ssp126, scenario = "SSP1-RCP2.6"))

df.YOS1 <- df.YOS1%>%
  filter(!is.na(YearOfShifts)) %>% group_by(scenario)%>%
  summarise(YearOfShifts = unique(YearOfShifts),
            ecdf = ecdf(YearOfShifts)(unique(YearOfShifts)))

# feed YearOfShifts into ggplot. 
df.YOS1$scenario <- factor(df.YOS1$scenario, levels=c("SSP1-RCP2.6","SSP5-RCP8.5"))
(p1 <- ggplot(df.YOS1, aes(x=YearOfShifts, y=ecdf)) + 
    geom_line(aes(x=YearOfShifts,color=scenario), size = 1.2)+
    paletteer::scale_colour_paletteer_d(palette = "rcartocolor::Pastel")+
    paletteer::scale_fill_paletteer_d(palette = "rcartocolor::Pastel")+
    theme_sleek(base_size = 18)+
    theme(legend.position = c(0.5, 0.9), 
        legend.title = element_blank(),
        plot.margin = unit(rep(1,4), "cm"), 
        axis.text.x = element_text(angle = 45, hjust = 1))+
    guides(color = guide_legend(nrow = 1))+
    geom_ribbon(data=df.YOS1[which(df.YOS1$YearOfShifts<2050),],aes(ymin=0,ymax=ecdf, fill=scenario, alpha=scenario))+
    scale_alpha_discrete(range = c(0.3, 0.1))+
    scale_y_continuous(expand = c(0,0), label = scales::percent)+
    
    annotate("linerange", x = 2050, ymin = 0, ymax = .4914, colour = "red", size = 1.1)+
    annotate("linerange", y = .4914, xmin = 2016, xmax = 2050, colour = "orange", size = 1.1, lty = 2)+
    annotate("linerange", y = .3461, xmin = 2016, xmax = 2050, colour = "lightblue", size = 1.1, lty = 2)+
    
    scale_x_continuous(expand = c(0,0), limits = c(2016, 2100),breaks = c(2016,2030,2050,2065,2100))+
  labs(y = "Percent of wilderness areas", x = "Year of climate shift")
)

#Plot Violins
(p2 <- resTimeData%>%
    ggplot()+
    geom_violin(aes(y = CRT_ssp126, x = 1, fill = "SSP1-RCP2.6"), size = .5)+
    geom_boxplot(aes(y = CRT_ssp126, x = 1), colour = "black", width=0.1)+
    
    geom_violin(aes(y = CRT_ssp585, x = 2, fill = "SSP5-RCP8.5"), size = .5)+
    geom_boxplot(aes(y = CRT_ssp585, x = 2), colour = "black", width=0.1)+
    
    scale_y_continuous(limits = c(0, 100))+
    scale_x_continuous(limits = c(0.5, 2.5))+
    geom_vline(aes(xintercept = 0.5), width = 1.2)+
    geom_hline(aes(yintercept = 0), width = 1.2)+
    paletteer::scale_fill_paletteer_d(palette = "rcartocolor::Pastel")+
    #scale_fill_manual(values=c("#999999", "#56B4E9","#E69F00"))+
    
    labs(x = "Scenarios", y = "Climate residence time \n(years)")+
    theme_sleek(base_size = 12)+
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 0),
          axis.title.x = element_blank(),
          axis.text.x.bottom = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank()
        ))

(plt_fig4<- p1+annotation_custom(ggplotGrob(p2), xmin = 2065, ymin = .01, xmax = 2099, ymax = .40))
#save as pdf
cairo_pdf(filename = here::here("./out_plots/4_Fig4.pdf"), width = 8, height = 8, fallback_resolution = 600)
print(plt_fig4)
dev.off()

# manipulating into cdf
names(resTimeData)

df.YOS2 <- rbind(
  data.frame(YearOfShifts = 2016+resTimeData$TempRT_ssp585, scenario = "SSP5-RCP8.5", what = "Temperature"),
  data.frame(YearOfShifts = 2016+resTimeData$TempRT_ssp126, scenario = "SSP1-RCP2.6", what = "Temperature"),
  data.frame(YearOfShifts = 2016+resTimeData$PrecRT_ssp585, scenario = "SSP5-RCP8.5", what = "Precipitation"),
  data.frame(YearOfShifts = 2016+resTimeData$PrecRT_ssp126, scenario = "SSP1-RCP2.6", what = "Precipitation")
)

df.YOS2 <- df.YOS2 %>%select(everything())%>%filter(is.finite(YearOfShifts))%>%
  group_by(what, scenario) %>% summarise(YearOfShifts = unique(YearOfShifts), ecdf = ecdf(YearOfShifts)(unique(YearOfShifts)))

# feeding into ggplot. 
df.YOS2$scenario <- factor(df.YOS2$scenario, levels = c("SSP1-RCP2.6","SSP5-RCP8.5")); df.YOS2$what <- factor(df.YOS2$what, levels = c("Temperature","Precipitation"))

(tmPlt<-ggplot(df.YOS2,aes(x = YearOfShifts, y = ecdf)) + facet_wrap(what~.)+
    geom_line(aes(x=YearOfShifts, col=scenario), size = 1.2)+
    paletteer::scale_colour_paletteer_d(palette = "rcartocolor::Pastel")+
    paletteer::scale_fill_paletteer_d(palette = "rcartocolor::Pastel")+
    theme_sleek(base_size = 18)+
    theme(panel.spacing = unit(2, "lines"), 
          legend.position = c(0.25, 0.9), 
          legend.title = element_blank(),
          plot.margin = unit(rep(1,4), "cm"), 
          axis.text.x = element_text(angle = 45, hjust = 1))+
    guides(color = guide_legend(nrow = 2))+
    geom_ribbon(data=df.YOS2[which(df.YOS2$YearOfShifts<=2050),], aes(ymin=0,ymax=ecdf, fill=scenario, alpha=scenario))+
    scale_alpha_discrete(range = c(0.3, 0.1))+
    scale_x_continuous(expand = c(0,0), 
                       limits = c(2016, 2100),
                       breaks = c(2016,2030,2050,2065,2100))+
    scale_y_continuous(expand = c(0,0), limits = c(0, 1), labels = scales::percent)+
    labs(y = "Percent of wilderness areas", x = "Year of climate shift"))
ggsave("./out_plots/SI_FigS3.tiff", dpi=600, width = 7, height = 6.5)



################################################################################################################################
#Examine dispersal velocities of nonvolant mammals vs climate change velocity
###############################################################################################################################

#Data on dispersal velocity were retrieved from Schloss et al 2012 (https://doi.org/10.1073/pnas.1116791109)
spp_disp_vel <- readxl::read_excel("./data/wild_ms_database/input_tables/nonvlant_mammals.xlsx");names(spp_disp_vel)<-c("Species","Order","Family","Mass","diet","GenLength","dispersal")
nrow(subset(spp_disp_vel, dispersal<4.44))/nrow(spp_disp_vel) # ~85% has dispersal velocities lesser than average climate velocity of 4.41. However, we considered only unique dispersal velocities only (see below)

spp_disp_vel%>%group_by(Order)%>%
  summarise(N=n(),dispersal=mean(dispersal))

spp_disp_vel <- spp_disp_vel%>%select(everything()) %>% summarise(dispersal = unique(dispersal),ecdf = ecdf(dispersal)(unique(dispersal)))
# How much of these unique dispersal velocities are lower than our average gVoCC estimates
nrow(subset(spp_disp_vel, dispersal<4.44))/nrow(spp_disp_vel)
# = 74.7331% of unique dispersal velocities among 493 nonvolant mammals are be
(plt_fig5 <- ggplot(spp_disp_vel,aes(x=dispersal, y=ecdf)) + 
  geom_line(aes(x=dispersal), size = 1)+
  theme_sleek(base_size = 18)+
  theme(legend.position = "top", 
        legend.title = element_blank(),
        plot.margin = unit(rep(1,4), "cm"))+
  geom_ribbon(data=spp_disp_vel[which(spp_disp_vel$dispersal<4.44),], aes(ymin=0,ymax=ecdf), alpha=0.3)+
  scale_x_continuous(expand = c(0,0), limits = c(0, 50))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels=scales::percent)+
  labs(x=expression("Dispersal velocity" ~ paste("(","km", yr^-1,")")),
       y="Cummulative percent")+
  paletteer::scale_colour_paletteer_d(palette = "rcartocolor::Pastel"))

#save as pdf
cairo_pdf(filename = here::here("./out_plots/5_Fig5.pdf"), width = 6.19, height = 7.15, fallback_resolution = 600)
print(plt_fig5)
dev.off()






