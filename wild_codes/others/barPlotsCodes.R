#EXAMINE PROJECTED CHANGES IN VELOCITIES AND INSTABILITIES
##Store the gloabl averages (gMean)
gMean_cc = gm_mean(dWild$cc_Base)
gMean_lu = gm_mean(dWild$lu_Base_max+0.1)


##Calculate and plot estimates for the baseline as bars
velPWild<-dWild%>% 
  group_by(pa_dum)%>%##summarise the data grouped by protection vs unprotected
  summarise(cSSP5 = gm_mean(cc_ssp5,na.rm = T),
            cSSP1 = gm_mean(cc_ssp1,na.rm = T),
            cBase = gm_mean(cc_Base,na.rm = T))
names(velPWild)<-c("status", "vcSSP5","vcSSP1", "cBase")

velPWild2<-dWild%>% ##Geomrtic mean across all wilderness areas
  group_by(wild_dummy)%>%
  summarise(cSSP5 = gm_mean(cc_ssp5,na.rm = T),
            cSSP1 = gm_mean(cc_ssp1,na.rm = T),
            cBase = gm_mean(cc_Base,na.rm = T))
names(velPWild2)<-c("status", "vcSSP5","vcSSP1", "cBase")
velSSPClim<-rbind(velPWild,velPWild2)
velSSPClim

velPWild<-subset(dWild)%>% ##summarise the data grouped by protection status
  group_by(pa_dum)%>%
  summarise(vlSSP5 = gm_mean(lu_ssp5_max+0.1,na.rm = T),
            vlSSP1 = gm_mean(lu_ssp1_max+0.1,na.rm = T), 
            vlBase = gm_mean(lu_Base_max+0.1,na.rm = T))
names(velPWild)<-c("status","vlSSP5", "vlSSP1", "vlBase")

velPWild2<-subset(dWild)%>% ##summarise the data grouped by protection status
  group_by(wild_dummy)%>%
  summarise(vlSSP5 = gm_mean(lu_ssp5_max+0.1,na.rm = T),
            vlSSP1 = gm_mean(lu_ssp1_max+0.1,na.rm = T), 
            vlBase = gm_mean(lu_Base_max+0.1,na.rm = T))
names(velPWild2)<-c("status","vlSSP5", "vlSSP1", "vlBase")
velSSPLuse<-rbind(velPWild,velPWild2)
velSSPLuse

velSSPClim$perVcSSP5<-100*log(velSSPClim$vcSSP5/gMean_cc)
velSSPClim$perVcSSP1<-100*log(velSSPClim$vcSSP1/gMean_cc)
velSSPLuse$perVluSSP5<-100*log(velSSPLuse$vlSSP5/gMean_lu)
velSSPLuse$perVluSSP1<-100*log(velSSPLuse$vlSSP1/gMean_lu)

#melt data into long format
velGlobal <- rbind(
  data.frame(what="Climate", scenario="SSP1-RCP2.6", status=velSSPClim$status, mean=velSSPClim$perVcSSP1),
  data.frame(what="Climate", scenario="SSP5-RCP8.5", status=velSSPClim$status, mean=velSSPClim$perVcSSP5),
  data.frame(what="Land use", scenario="SSP1-RCP2.6", status=velSSPLuse$status, mean=velSSPLuse$perVluSSP1),
  data.frame(what="Land use", scenario="SSP5-RCP8.5", status=velSSPLuse$status, mean=velSSPLuse$perVluSSP5))
velGlobal$status<-plyr::revalue(velGlobal$status, c("nonPA" = "UW", "PA" = "PW", "wild" = "AW"))

#Plot bars with errors
velGlobal$status<-factor(velGlobal$status, levels=c("AW","PW","UW")) #sort factors
ggplot(subset(velGlobal), aes(scenario, mean, fill = status))+
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_viridis_d(option = "B", direction = -1)+
  scale_y_continuous(expand = c(0,NA), limits = c(-25, 50), breaks = c(-25, 0, 25, 50))+
  facet_wrap(~what, scales = "free_x")+
  labs(x = "Scenario", y = "Percent change [%]")+
  theme_sleek(base_size = 24)+
  theme(legend.position = c(0.8, 0.2),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        plot.background = element_rect(size = 5),
        #strip.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 11,  angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11)
  )
ggsave("2_Fig2.tiff", dpi = 600, width = 5.4, height = 6.15)


