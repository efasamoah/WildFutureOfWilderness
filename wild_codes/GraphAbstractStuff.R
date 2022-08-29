# IMPORT DATA
library(tidyverse)
# 
# sensData <- read_rds("output_tables/wildData.Rds")%>%subset(wild_dummy=="wild")%>%as.data.frame()
# names(sensData)
# 
# sensData$percForest <- (sensData$base_primf)/(0.1+sensData$LUI_N_base)
# plot(log(sensData$LUI_N_base), log(sensData$percForest))
# 
# cor.test(sensData$LUI_N_base, sensData$percForest, method = "spearman")
# sensData %>%#filter(base_primf>0)%>%
#   ggplot(aes(x=(0.1+LUI_N_base), y=(percForest)))+
#   geom_point()+
#   geom_hline(yintercept = .5, colour = "green", lwd=1)+
#   geom_vline(xintercept = gm_mean(0.1+sensData$LUI_N_base), colour = "red", lwd=1)+
#   scale_y_continuous(expand = c(0,0), name="Percent of forest instability", label = scales::percent)+
#   scale_x_continuous(expand = c(0,0), name="LUI (log, km/yr", trans = "log")+
#   theme_bw(base_size = 20)
# ggsave("suppl_try1.png", dpi=600)
# 
# sensData %>%#filter(base_primf>0)%>%
#   ggplot(aes(x=(0.1+LUI_N_base), y=(0.1+base_primf)))+
#   geom_point()+
#   geom_hline(yintercept = gm_mean(0.1+sensData$base_primf), colour = "green", lwd=1)+
#   geom_vline(xintercept = gm_mean(0.1+sensData$LUI_N_base), colour = "red", lwd=1)+
#   scale_y_continuous(expand = c(0,0), name="Forest instability (log, km/yr)", trans = "log")+
#   scale_x_continuous(expand = c(0,0), name="Integrated LUI (log, km/yr)", trans = "log")+
#   theme_bw(base_size = 20)
# ggsave("suppl_try2.png", dpi=600)

#Afrotropical
ggplot(data = tibble(gp = c("A","B"),
                     cc = c(4.72653635,0),
                     lu = c(0,0.69823705)), aes( x=gp)) +
  geom_col( aes(y=cc), fill = "darkred", colour = "darkred") + 
  geom_col( aes(y=lu*10), fill = "yellow3", colour = "yellow3") +
  scale_y_continuous(expand = c(0, NA),limits = c(0,8), name = "Climate",sec.axis = sec_axis(~./10, name="Land use"))+
  #coord_flip()+ 
  theme_sleek(base_size = 10)+  xlab("")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
ggsave("AT.png", dpi = 600, height = 2, width = 1.5)

#Australasia
ggplot(data = tibble(gp = c("A","B"),
                     cc = c(6.487569934,0),
                     lu = c(0,0.111145452)), aes( x=gp)) +
  geom_col( aes(y=cc), fill = "darkred", colour = "darkred") + 
  geom_col( aes(y=lu*10), fill = "yellow3", colour = "yellow3") +
  scale_y_continuous(expand = c(0, NA),limits = c(0,7), name = "Climate",sec.axis = sec_axis(~./10, name="Land use"))+
  #coord_flip()+ 
  theme_sleek(base_size = 10)+  xlab("")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
ggsave("AA.png", dpi = 600, height = 2, width = 1.5)


#Indomalaya
ggplot(data = tibble(gp = c("A","B"),
                     cc = c(1.337252765,0),
                     lu = c(0,1.015410808)), aes( x=gp)) +
  geom_col( aes(y=cc), fill = "darkred", colour = "darkred") + 
  geom_col( aes(y=lu), fill = "yellow3", colour = "yellow3") +
  scale_y_continuous(expand = c(0, NA),limits = c(0,1.5), name = "Climate", sec.axis = sec_axis(~./1, name="Land use"))+
  #coord_flip()+ 
  theme_sleek(base_size = 10)+  xlab("")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
ggsave("IM.png", dpi = 600, height = 2, width = 1.5)

#Nearctic
ggplot(data = tibble(gp = c("A","B"),
                     cc = c(4.075786486,0),
                     lu = c(0,0.272181369)), aes( x=gp)) +
  geom_col( aes(y=cc), fill = "darkred", colour = "darkred") + 
  geom_col( aes(y=lu*10), fill = "yellow3", colour = "yellow3") +
  scale_y_continuous(expand = c(0, NA),limits = c(0, 5), name = "Climate", sec.axis = sec_axis(~./10, name="Land use"))+
  #coord_flip()+ 
  theme_sleek(base_size = 10)+  xlab("")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
ggsave("NA.png", dpi = 600, height = 2, width = 1.5)


#Neotropical
ggplot(data = tibble(gp = c("A","B"),
                     cc = c(4.976938761,0),
                     lu = c(0,0.501889612)), aes( x=gp)) +
  geom_col( aes(y=cc), fill = "darkred", colour = "darkred") + 
  geom_col( aes(y=lu*10), fill = "yellow3", colour = "yellow3") +
  scale_y_continuous(expand = c(0, NA),limits = c(0, 6), name = "Climate", sec.axis = sec_axis(~./10, name="Land use"))+
  #coord_flip()+ 
  theme_sleek(base_size = 10)+  xlab("")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
ggsave("NT.png", dpi = 600, height = 2, width = 1.5)

#Palearctic
ggplot(data = tibble(gp = c("A","B"),
                     cc = c(4.431847063,0),
                     lu = c(0,0.203852526)), aes( x=gp)) +
  geom_col( aes(y=cc), fill = "darkred", colour = "darkred") + 
  geom_col( aes(y=lu*10), fill = "yellow3", colour = "yellow3") +
  scale_y_continuous(expand = c(0, NA),limits = c(0, 5), name = "Climate", sec.axis = sec_axis(~./10, name="Land use"))+
  #coord_flip()+ 
  theme_sleek(base_size = 10)+  xlab("")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
ggsave("PA.png", dpi = 600, height = 2, width = 1.5)



names(RiskData)
RiskData <- read_rds("output_tables/wildData.Rds")%>%
  as.data.frame()%>%
  select(base_crop)%>%
  mutate(base_crop = log10(base_crop+0.001))%>% 
  dplyr::arrange(-desc(base_crop))%>%
  mutate(ssp5_cdf.crop= cumsum(base_crop)/sum(base_crop, na.rm = TRUE))
hist(RiskData$base_crop)

ggplot(data = RiskData, aes( x=base_crop)) +
  geom_histogram(bins = 50,fill = "darkred", colour = "darkred") + 
  geom_point( aes(y=ssp5_cdf.crop*100000), fill = "yellow3", colour = "yellow3") +
  scale_y_continuous(expand = c(0, NA),
                     #limits = c(0, 5), 
                     name = "No. of cells", sec.axis = sec_axis(~./100000, name=""))+
  #coord_flip()+ 
  theme_bw(base_size = 12)+  xlab("log10(Speed of cropland change, km/yr)")+
  theme(
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  )
ggsave("cdf_standardised.tiff", dpi = 600, height = 3, width = 4)


