library(data.table)
library(rgeos)
library(sp)
library(raster)
wildStack<-stack(luv_stack, clim_stack, ccBase, ccRCP26, ccRCP85)
wildExtract<-extract(wildStack, terrWilderness, fun = mean, sp=T, df=T)
wildExtract<-as.data.frame(wildExtract)
dim(wildExtract)
summary(wildExtract)
summary(na.omit(wildExtract))


ccBase<-calc(clim_stack[[c("velocity_temp_hist_2005","velocity_prec_hist_2005")]], function(x) max(x))
plot(ccBase)
terrWilderness <- rgdal::readOGR("D:/R - HoMe/LULC velocity/ProcessingHausz/Data/Wilderness_maps_R.1", layer="LoW_2009_current")
terrWilderness <- spTransform(terrWilderness, CRS("+proj=longlat")) 
wildResTime1<-resTime(pg=terrWilderness, vel = ccBase, 
                     areapg = as.numeric(as.numeric(levels(terrWilderness$area_km2))[terrWilderness$area_km2]))

ccRCP26<-calc(clim_stack[[c("velocity_temp_rcp26_2050","velocity_prec_rcp26_2050")]], function(x) max(x))
plot(ccRCP26)
wildResTime2<-resTime(pg=terrWilderness, vel = ccRCP26, 
                      areapg = as.numeric(as.numeric(levels(terrWilderness$area_km2))[terrWilderness$area_km2]))

ccRCP85<-calc(clim_stack[[c("velocity_temp_rcp85_2050","velocity_prec_rcp85_2050")]], function(x) max(x))

wildResTime3<-resTime(pg=terrWilderness, vel = ccRCP85, 
                      areapg = as.numeric(as.numeric(levels(terrWilderness$area_km2))[terrWilderness$area_km2]))

theo_res<-cbind(hist=wildResTime1, rcp26=wildResTime2, rcp85=wildResTime3)
theo_res<-merge(wildExtract, theo_res[, c("hist.ID","hist.d", "hist.resTim",
                                         "rcp26.resTim", "rcp85.resTim")], by.x="id", by.y="hist.ID", all.x = T)

dplyr::glimpse(theo_res)
dim(theo_res)


theo_res<-na.omit(theo_res)
dim(theo_res)
summary(theo_res)
#dim(subset(theo_res, area_km2>2500))
write.csv(theo_res, "resTimeWild.csv")

plot((theo_res$primf2050_126), log(theo_res$layer.3))
summary(theo_res)
#write.csv(theo_duration, "climate_resid_time.csv")

theo_res%>%ggplot()+
  geom_violin(aes(y = hist.resTim, x = "Baseline", fill = "Past (baseline)"), size=1)+
  geom_boxplot(aes(y = hist.resTim, x = "Baseline"), colour = "black", width=0.1)+
  geom_violin(aes(y = rcp26.resTim, x = "SSP1-RCP2.6", fill = "SSP1-RCP2.6"), size = 1)+
  geom_boxplot(aes(y = rcp26.resTim, x = "SSP1-RCP2.6"), colour = "black", width=0.1)+
  geom_violin(aes(y = rcp85.resTim, x = "SSP5-RCP8.5", fill = "SSP5-RCP8.5"), size = 1)+
  geom_boxplot(aes(y = rcp85.resTim, x = "SSP5-RCP8.5"), colour = "black", width=0.1)+
  scale_y_continuous(limits = c(0, 100))+
  scale_fill_manual(values=c("#999999", "#56B4E9","#E69F00"))+
  labs(x = "Scenarios", y = "Climate residence time (years)")+
  theme_sleek(base_size = 18)+
  #coord_flip()+
  theme(legend.position = "none",
        legend.title = element_blank(), axis.text.x = element_text(angle = 0))
ggsave("5_FigS5.tiff", dpi = 600, width = 6, height = 6)




dtF2 <-theo_res%>%select(everything())%>%na.omit()%>%
  #group_by(realm, biome)%>%
  summarise(
    N=length(biome),
    hist.resTim = median(hist.resTim, na.rm = T),
    rcp26.resTim = median(rcp26.resTim, na.rm = T),
    rcp85.resTim = median(rcp85.resTim, na.rm = T)
  )%>%ungroup()

dtF2 %>%
  ggplot() +
  geom_point(data=dtF2, aes(rcp85.resTim, y=hist.resTim, colour = "RCP2.6", pch = realm),size=1.5) +
  geom_point(data=dtF2, aes(rcp26.resTim-rcp85.resTim, y=hist.resTim, colour = "RCP8.5", pch = realm),size=1.5) +
  geom_segment(data=dtF2,aes(x=rcp85.resTim,y=hist.resTim, xend=rcp26.resTim-rcp85.resTim, 
                             yend=hist.resTim), size=0.5, color="grey70",alpha=0.5,
               arrow = arrow(type = "closed",length=unit(0.075, "inches")))+
  #scale_x_reverse()+scale_y_reverse()+
  scale_colour_manual(values = c("blue", "red"))+
  theme_sleek(base_size = 15)+
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.2),
        plot.margin = unit(rep(1,4), "cm"))+
  labs(x = "Projected climate residence time (yr)", y = "Contemporary climate residence time (yr)")
ggsave("resid_time.png", dpi = 600)


##Plot cdf
#manipulating into cdf, as I've seen in other examples
df_resTimz <- rbind(
  data.frame(resTimz=theo_res$rcp85.resTim+2015, scenario="SSP5-RCP8.5"),
  data.frame(resTimz=theo_res$rcp26.resTim+2015, scenario="SSP1-RCP2.6"))

df_resTimz <- df_resTimz%>%
  select(everything())%>%
  group_by(scenario)%>%
  summarise(resTimz = unique(resTimz),
            ecdf = 100*ecdf(resTimz)(unique(resTimz)))

# feeding into ggplot. 
df_resTimz$scenario<-factor(df_resTimz$scenario,
                            levels=c("SSP1-RCP2.6","SSP5-RCP8.5")) ##sort factors
ggplot(df_resTimz,aes(x=resTimz, y=ecdf)) + 
  geom_line(aes(x=resTimz,color=scenario), size = 1.2)+
  paletteer::scale_colour_paletteer_d(palette = "rcartocolor::Pastel")+
  paletteer::scale_fill_paletteer_d(palette = "rcartocolor::Pastel")+
  theme_sleek(base_size = 18)+
  theme(legend.position = c(0.5, 0.9), 
        legend.title = element_blank(),
        plot.margin = unit(rep(1,4), "cm"), 
        axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color = guide_legend(nrow = 1))+
  #geom_ribbon(data=resTim.below2, aes(ymin=0,ymax=ecdf, fill=scenario), alpha=0.3)+
  geom_ribbon(data=df_resTimz[which(df_resTimz$resTimz<2050),], 
              aes(ymin=0,ymax=ecdf, fill=scenario, alpha=scenario))+
  scale_alpha_discrete(range = c(0.3, 0.1))+
  scale_x_continuous(expand = c(0,0), 
                     limits = c(2015, 2100),
                     breaks = c(2015,2030,2050,2065,2100)
  )+
  scale_y_continuous(expand = c(0,0), limits = c(0, 100.05))+
  labs(y = "Percent of wilderness areas [%]", x = "Year of climate shift")
ggsave("5_Fig5_n.tiff", dpi=600, width = 6.19, height = 7.15)


dWild$value<-1
names(dWild)
wildProtect<-reshape2::dcast((dWild), wilderness~pa_dum, length, value.var = "value")
dplyr::glimpse(wildProtect)
wildProtect$sumProt<-wildProtect$nonPA+wildProtect$PA
wildProtect$percProtected<-wildProtect$PA/wildProtect$sumProt
hist(wildProtect$percProtected)
wildProtect<-merge(wildProtect, theo_res[, c("id","hist.resTim",
                                             "rcp26.resTim","rcp85.resTim")],
                   by.x="wilderness", by.y="id", all.x = T)
dim(wildProtect)
cor.test(wildProtect$percProtected, wildProtect$rcp26.resTim, method = "spearman")
ggplot(data=wildProtect, aes(percProtected, rcp85.resTim))+
  geom_point()+
  #geom_smooth(method = "lm", se=F)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Percent protected [%]", y = "Climate residence time")+
  theme_sleek()


##EXamine dispersal velocity
spp_disp_vel <- readxl::read_excel("spp disp vel.xlsx")
dim(spp_disp_vel)
names(spp_disp_vel)<-c("Species","Order","Family","Mass","diet",
                       "GenLength","dispersal")
dim(subset(spp_disp_vel,dispersal<4.41))

spp_disp_vel <- spp_disp_vel%>%
  select(everything())%>%
  #group_by(scenario)%>%
  summarise(dispersal = unique(dispersal),
            ecdf = ecdf(dispersal)(unique(dispersal)))
dispersal.below1 <- spp_disp_vel[which(spp_disp_vel$dispersal<4.41),]; dim(dispersal.below1)
dim(dispersal.below1)

ggplot(spp_disp_vel,aes(x=dispersal, y=ecdf)) + 
  geom_line(aes(x=dispersal), size = 1)+
  theme_sleek(base_size = 18)+
  theme(legend.position = "top", 
        legend.title = element_blank(),
        plot.margin = unit(rep(1,4), "cm"))+
  geom_ribbon(data=dispersal.below1, aes(ymin=0,ymax=ecdf), alpha=0.3)+
  scale_x_continuous(expand = c(0,0), limits = c(0, 50))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05))+
  labs(x=expression("Dispersal velocity" ~ paste("(","km", yr^-1,")")), y="f(dispersal velocity)")+
  paletteer::scale_colour_paletteer_d(palette = "rcartocolor::Pastel")
ggsave("FigS2.tiff", dpi = 600, width = 6.19, height = 7.15)




