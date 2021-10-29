#Import resTime function from VoCC package
resTime<-function(pg, vel, areapg = NA){
  RT <- suppressWarnings(data.table(ID = sp::getSpPPolygonsIDSlots(pg)))
  RT[, v := raster::extract(vel, pg, small=TRUE, fun=mean, na.rm=TRUE)]
  # If not provided, calculate the area of the polygon
  if(all(is.na(areapg))){
    areapg <- geosphere::areaPolygon(pg)/1000000              # area polygon in km2
  }
  RT[, d := 2*sqrt((areapg/pi))]
  RT[, resTim := abs(d/v)]
  return(RT[])
}


library(raster)
library(dplyr)
library(rgdal)
terrWilderness <- readOGR("./data/vector/Wilderness_maps_R.1", layer = "LoW_2009_current") # Wilderness polygons are available online at 
clim_stack <- stack(list.files("./data/raster/climate/", pattern = ".tiff$", full.names = TRUE)) # Raster layers of velocity of climate change are archeived in Figshare under the Identifier https://doi.org/10.6084/m9.figshare.16892629

wildExtract <- extract(clim_stack, terrWilderness, fun = mean, sp=T, df=T)%>%as.data.frame()
nrow(wildExtract)

#calculate diameter based on resTime in VoCC
wildExtract$diameter <- 2*sqrt(wildExtract$area_km2/pi)
wildExtract$cc_Base_max <- apply(wildExtract[, c("velocity_temp_hist_2005", "velocity_prec_hist_2005")],1, function(x) max(x, na.rm = TRUE))
wildExtract$cc_rcp85_max <- apply(wildExtract[, c("velocity_temp_rcp85_2050", "velocity_prec_rcp85_2050")],1, function(x) max(x, na.rm = TRUE))
wildExtract$cc_rcp26_max <- apply(wildExtract[, c("velocity_temp_rcp26_2050", "velocity_prec_rcp26_2050")],1, function(x) max(x, na.rm = TRUE))

names(wildExtract)
resTimeData <- na.omit(wildExtract[, c("realm","area_km2","id", "velocity_temp_hist_2005", "velocity_temp_rcp26_2050",
                                      "velocity_temp_rcp85_2050","velocity_prec_hist_2005","velocity_prec_rcp26_2050",
                                      "velocity_prec_rcp85_2050", "diameter","cc_Base_max","cc_rcp85_max","cc_rcp26_max")])
nrow(resTimeData)

resTimeData$ccRT.base <- resTimeData$diameter/resTimeData$cc_Base_max
resTimeData$ccRT.rcp85 <- resTimeData$diameter/resTimeData$cc_rcp85_max
resTimeData$ccRT.rcp26 <- resTimeData$diameter/resTimeData$cc_rcp26_max

resTimeData$TempRT.rcp85 <- resTimeData$diameter/resTimeData$velocity_temp_rcp85_2050
resTimeData$TempRT.rcp265 <- resTimeData$diameter/resTimeData$velocity_temp_rcp26_2050
resTimeData$PrecRT.rcp85 <- resTimeData$diameter/resTimeData$velocity_prec_rcp85_2050
resTimeData$PrecRT.rcp26 <- resTimeData$diameter/resTimeData$velocity_prec_rcp26_2050
# write.csv(resTimeData, "data/spreadsheets/climate_residence_time.csv")

# Compute year of shifts
df.YOS1 <- rbind(
  data.frame(YearOfShifts = 2015+resTimeData$ccRT.rcp85, scenario = "SSP5-RCP8.5"),
  data.frame(YearOfShifts = 2015+resTimeData$ccRT.rcp26, scenario = "SSP1-RCP2.6"))

df.YOS1 <- df.YOS1%>%select(everything())%>%filter(!is.na(YearOfShifts))%>%
  group_by(scenario)%>%
  summarise(YearOfShifts = unique(YearOfShifts),
            ecdf = 100*ecdf(YearOfShifts)(unique(YearOfShifts)))

# feed YearOfShifts into ggplot. 
df.YOS1$scenario<-factor(df.YOS1$scenario,levels=c("SSP1-RCP2.6","SSP5-RCP8.5"))
ggplot(df.YOS1,aes(x=YearOfShifts, y=ecdf)) + 
  geom_line(aes(x=YearOfShifts,color=scenario), size = 1.2)+
  paletteer::scale_colour_paletteer_d(palette = "rcartocolor::Pastel")+
  paletteer::scale_fill_paletteer_d(palette = "rcartocolor::Pastel")+
  theme_sleek(base_size = 18)+
  theme(legend.position = c(0.5, 0.9), 
        legend.title = element_blank(),
        plot.margin = unit(rep(1,4), "cm"), 
        axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color = guide_legend(nrow = 1))+
  geom_ribbon(data=df_resTimz[which(df_resTimz$YearOfShifts<2050),], 
              aes(ymin=0,ymax=ecdf, fill=scenario, alpha=scenario))+
  scale_alpha_discrete(range = c(0.3, 0.1))+
  scale_x_continuous(expand = c(0,0), 
                     limits = c(2015, 2100),
                     breaks = c(2015,2030,2050,2065,2100)
  )+
  scale_y_continuous(expand = c(0,0), limits = c(0, 100.05))+
  labs(y = "Percent of wilderness areas [%]", x = "Year of climate shift")
ggsave("5_Fig5.tiff", dpi=600, width = 6.19, height = 7.15)



#Plot Violins
resTimeData%>%ggplot()+
  geom_violin(aes(y = ccRT.base, x = "Baseline", fill = "Past (baseline)"), size=1)+
  geom_boxplot(aes(y = ccRT.base, x = "Baseline"), colour = "black", width=0.1)+
  geom_violin(aes(y = ccRT.rcp26, x = "SSP1-RCP2.6", fill = "SSP1-RCP2.6"), size = 1)+
  geom_boxplot(aes(y = ccRT.rcp26, x = "SSP1-RCP2.6"), colour = "black", width=0.1)+
  geom_violin(aes(y = ccRT.rcp85, x = "SSP5-RCP8.5", fill = "SSP5-RCP8.5"), size = 1)+
  geom_boxplot(aes(y = ccRT.rcp85, x = "SSP5-RCP8.5"), colour = "black", width=0.1)+
  scale_y_continuous(limits = c(0, 100))+
  scale_fill_manual(values=c("#999999", "#56B4E9","#E69F00"))+
  labs(x = "Scenarios", y = "Climate residence time (years)")+
  theme_sleek(base_size = 18)+
  theme(legend.position = "none",
        legend.title = element_blank(), axis.text.x = element_text(angle = 0))
ggsave("./figs/SI_FigS3.tiff", dpi = 600, width = 6, height = 6)



# manipulating into cdf
df.YOS2 <- rbind(
  data.frame(YearOfShifts = 2015+resTimeData$TempRT.rcp85, scenario = "SSP5-RCP8.5", what = "Temperature"),
  data.frame(YearOfShifts = 2015+resTimeData$TempRT.rcp85, scenario = "SSP1-RCP2.6", what = "Temperature"),
  data.frame(YearOfShifts = 2015+resTimeData$PrecRT.rcp85, scenario = "SSP5-RCP8.5", what = "Precipitation"),
  data.frame(YearOfShifts = 2015+resTimeData$PrecRT.rcp26, scenario = "SSP1-RCP2.6", what = "Precipitation")
)

df.YOS2 <- df.YOS2%>%
  select(everything())%>%filter(is.finite(YearOfShifts))%>%
  group_by(what, scenario)%>%
  summarise(YearOfShifts = unique(YearOfShifts),
            ecdf = 100*ecdf(YearOfShifts)(unique(YearOfShifts)))

# feeding into ggplot. 
df.YOS2$scenario<-factor(df.YOS2$scenario, levels = c("SSP1-RCP2.6","SSP5-RCP8.5")); df.YOS$what<-factor(df.YOS$what, levels = c("Temperature","Precipitation"))

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
    geom_ribbon(data=df.YOS[which(df.YOS$YearOfShifts<=2050),],
                aes(ymin=0,ymax=ecdf, fill=scenario, alpha=scenario))+
    scale_alpha_discrete(range = c(0.3, 0.1))+
    scale_x_continuous(expand = c(0,0), 
                       limits = c(2015, 2101),
                       breaks = c(2015,2030,2050,2065,2100)
    )+
    scale_y_continuous(expand = c(0,0), limits = c(0, 100.05))+
    labs(y = "Percent of wilderness areas [%]", x = "Year of climate shift"))
ggsave("./figs/SI_FigS4.tiff", dpi=600, width = 7, height = 7.15)





################################################################################################################################
#Examine dispersal velocities of nonvolant mammals vs climate change velocity
###############################################################################################################################


#Data on dispersal velocity were retrieved from Schloss et al 2012 (https://doi.org/10.1073/pnas.1116791109)
spp_disp_vel <- readxl::read_excel("./data/spreadsheets/nonvlant_mammals.xlsx");names(spp_disp_vel)<-c("Species","Order","Family","Mass","diet","GenLength","dispersal")
nrow(subset(spp_disp_vel, dispersal<4.41))/nrow(spp_disp_vel) # ~85% has dispersal velocities lesser than average climate velocity of 4.41. However, we considered only unique dispersal velocities only (see below)

spp_disp_vel <- spp_disp_vel%>%select(everything())%>%
  summarise(dispersal = unique(dispersal),
            ecdf = ecdf(dispersal)(unique(dispersal)))

ggplot(spp_disp_vel,aes(x=dispersal, y=ecdf)) + 
  geom_line(aes(x=dispersal), size = 1)+
  theme_sleek(base_size = 18)+
  theme(legend.position = "top", 
        legend.title = element_blank(),
        plot.margin = unit(rep(1,4), "cm"))+
  geom_ribbon(data=spp_disp_vel[which(spp_disp_vel$dispersal<4.41),], aes(ymin=0,ymax=ecdf), alpha=0.3)+
  scale_x_continuous(expand = c(0,0), limits = c(0, 50))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05))+
  labs(x=expression("Dispersal velocity" ~ paste("(","km", yr^-1,")")), y="f(dispersal velocity)")+
  paletteer::scale_colour_paletteer_d(palette = "rcartocolor::Pastel")
ggsave("SI_FigS2.tiff", dpi = 600, width = 6.19, height = 7.15)







