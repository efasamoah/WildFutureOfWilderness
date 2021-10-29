library(tidyverse)
library(raster)


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

ntile_na <- function(x,n)
{
  notna <- !is.na(x)
  out <- rep(NA_real_,length(x))
  out[notna] <- ntile(x[notna],n)
  return(out)
}

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


#IMPORT DATA
#write.csv(choroData, "choroData.csv")



# SUBSET GLOBAL DATASET AND WORK WITH ONLY WILDERNESS AREAS
dWild<-subset(choroData, wild_dummy == "wild")

# Percentage of wilderness with climate velocity greater than 1 km/yr
100*(nrow(subset(dWild, cc_Base_max>1))/nrow(dWild)[[1]]) #ca. 84%

# Calculate and plot estimates for the baseline as bars
velSum1<-dWild%>% group_by(pa_dum)%>%
  summarise(N = n(),
            vClim = gm_mean(cc_Base_max, na.rm = T),
            sdClim = sd(cc_Base_max, na.rm = T)); names(velSum1) <- c("status", "N", "vClim","sdClim")

velSum2<-dWild %>% group_by(wild_dummy)%>%
  summarise(N = n(), 
            vClim = gm_mean(cc_Base_max,na.rm = T), 
            sdClim = sd(cc_Base_max, na.rm = T)); names(velSum2) <- c("status", "N", "vClim","sdClim")
(vlSum<-rbind(velSum1,velSum2))


# summarise the data grouped by protection status
hist(dWild$lu_Base_max, breaks = 100)
ins_Sum1 <- dWild %>% group_by(pa_dum)%>%
  summarise(N = n(),
            vClim = gm_mean(lu_Base_max+0.1, na.rm = T),
            sdClim = sd(lu_Base_max+0.1, na.rm = T)); names(ins_Sum1) <- c("status", "N", "vLuse","sdLuse")

ins_Sum2 <- subset(dWild)%>% group_by(wild_dummy)%>%
  summarise(N = n(),
            vLuse = gm_mean(lu_Base_max+0.1, na.rm = T),
            sdClim = sd(lu_Base_max+0.1, na.rm = T)); names(ins_Sum2) <- c("status", "N", "vLuse","sdLuse")
(luSum <- rbind(ins_Sum1, ins_Sum2))

# Calculate error of mean at 95% 
vlSum$errorC <- (qt(0.975, df = (vlSum$N)-1)*(vlSum$sdClim)/sqrt(vlSum$N))
luSum$errorL <- (qt(0.975, df = (luSum$N)-1)*(luSum$sdLuse)/sqrt(luSum$N))

# melt data into long format
df1 <- rbind(
  data.frame(what = "Climate", status = vlSum$status, g.mean = vlSum$vClim, error = vlSum$errorC),
  data.frame(what = "Land use", status = luSum$status, g.mean = luSum$vLuse, error = luSum$errorL)
)
df1$status <-plyr::revalue(df1$status, c("PA" = "PW", "nonPA" = "UW", "wild" = "AW"))
df1$status <- factor(df1$status, levels=c("AW","PW","UW")) #sort factors
write.csv(df1, "./data/spreadsheets/BaselineSum.csv")

#Plot bars with errors
ggplot(df1, aes(status, g.mean, fill = status))+
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=g.mean-error, ymax=g.mean+error), width=.15, position=position_dodge(.9))+
  scale_fill_viridis_d(option = "B", direction = -1)+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), labels = scales::number_format(accuracy = 0.01))+
  facet_wrap(~what, scales = "free")+
  labs(x = "Status", y = expression("Velocity or instability" ~ paste("(","km", yr^-1,")")))+
  theme_sleek(base_size = 14)+
  theme(legend.position = "top",
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        axis.text = element_text(size = 10)
  )
ggsave(here::here("Fig1A.tiff"), dpi = 600, width=4, height = 4.5)

# Plot Fig1B
vsum_patch <- dWild %>% group_by(wilderness)%>%
  summarise(N = n(),
            vClim = mean(cc_Base_max, na.rm = T),
            vLand = mean(lu_Base_max, na.rm = T),
            x = mean(x, na.rm = T), # find centroids
            y = mean(y, na.rm = T))%>%
  filter(N >2)%>% # limit estimate to less than three CORDEX pixels to limit miscalculations
  ungroup()

library(SpatialPack)
coords <- vsum_patch[c("x","y")]
modified.ttest(log(vsum_patch$vClim), log(vsum_patch$vLand+.001), coords) # r = ~.44, p<1x10-6

ggplot(vsum_patch, aes(x = vLand+.001, y = vClim)) +
  geom_density_2d_filled(contour_var = "ndensity")+
  scale_fill_viridis_d(name = "Density",labels =c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  scale_x_continuous(expand = c(0,0), trans = "log", labels = scales::number_format(accuracy = 1), breaks = c(0.001,1, 10))+
  scale_y_continuous(expand = c(0,0), trans = "log", labels = scales::number_format(accuracy = 1), breaks = c(0,1, 10))+
  annotate("text", x = 0.1, y = 0.12, label = expression(italic(r[spatial]) ~ paste(" = -0.44, P < 1 x" , 10^-6,)),size=4, colour = "white")+
  theme_sleek(base_size = 14)+
  labs(x = expression("Land-use instability" ~ paste("(","km", yr^-1,")")), y = expression("Climate velocity" ~ paste("(","km", yr^-1,")")))+
  theme(legend.position="top", 
        legend.direction = "horizontal", 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        legend.title = element_blank())+
  guides(fill = guide_legend(nrow = 2))
ggsave("Fig1B.tiff", dpi = 600, width=4, height = 4.5)


#dWild$combPressures<-dWild$cc_ssp5*dWild$lu_ssp5_max
choro <- dWild %>% select(everything())%>%
  mutate(ccBrkBase = ntile_na(cc_Base_max, 3),
         ccBrk85 = ntile_na(cc_rcp85_max, 3),
         ccBrk26 = ntile_na(cc_rcp26_max, 3),
         cmbBrk85 = ntile_na(sens85, 3),
         cntxt = ntile_na(intactness, 3))

choro <- choro%>% select(everything())%>%
  mutate(luBrkBase = ifelse(lu_Base_max <0.08,1,2),
         pfBrkBase = ifelse(primf2050_hist <0.08,1,2),
         luBrk85 = ifelse(lu_ssp5_max <0.08,1,2),
         pfBrk85 = ifelse(primf2050_585 <0.08,1,2))

choro <- choro %>% #bivariate colour combinations
  select(everything()) %>%
  filter(is.finite(luBrkBase), is.finite(ccBrkBase), is.finite(cntxt), is.finite(pfBrkBase)) %>%
  mutate(sns85 = rgb(red = round(cmbBrk85/3, 1), green = round(cntxt/3, 1), blue = 0.5),
         mixBs.main = rgb(red = round(luBrkBase/2.5, 1), green = round(ccBrkBase/3, 1), blue = 0.5),
         mixBs.pfor = rgb(red = round(pfBrkBase/2.5, 1), green = round(ccBrkBase/3, 1), blue = 0.5),
         mix85.main = rgb(red = round(luBrk85/2.5, 1), green = round(ccBrk85/3, 1), blue = 0.5),
         mix85.pfor = rgb(red = round(pfBrk85/2.5, 1), green = round(ccBrk85/3, 1), blue = 0.5))

#write.csv(choro, "./data/spreadsheets/MainDataWilderness.csv")

library(rgdal)
bbox <- readOGR("./natural-earth-vector-master/natural-earth-vector-master/110m_physical/ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box")
bbox_moll <- spTransform(bbox, CRS("+proj=moll"))
bbox_moll <- fortify(bbox_moll)


land <- readOGR("./natural-earth-vector-master/natural-earth-vector-master/10m_physical", layer="ne_10m_land")
land <- spTransform(land, CRS("+proj=moll"))
land <- fortify(land)


bb_realms <- readOGR("C:/Users/45019738/Dropbox/PNAS_Revision/Generalised_Biogeographic_Realms_2004", layer="brpol_fin_dd")
bb_realms <- spTransform(bb_realms, CRS("+proj=moll"))
realms <- fortify(bb_realms, region = "REALMCODE")

centroids.realms <- as.data.frame(coordinates(bb_realms))
names(centroids.realms) <- c("long", "lat")

idList <- bb_realms@data$REALMCODE
realm.df <- data.frame(id = idList, centroids.realms) #Using centroid positions text on key priority areas so Manually adjust labels in map
#realm.labels<-cbind.data.frame(id = c("PA","NA","IM","AT","NT","AA"),
#                               long = c(4214966, -7839085, 9242787, 2094967, -6222277, 13047461),
#                               lat = c(5386746.3, 5491240.4, 480038.6, -1347711.5, -1478926.6, -2980095.8))

realm.labels<-cbind.data.frame(id = c("PA","NA","IM","AT","NT","AA"),
                               long = c(4214966, -10839085, 8242787, 104967, -8522277, 15047461),
                               lat = c(5386746.3, 5491240.4, 100038.6, -1347711.5, -1478926.6, -2980095.8))

#Map baseline overlaps
ggplot() +
  geom_polygon(data = land, aes(long,lat, group=group), fill="grey70") +
  geom_tile(data = choro, aes(x, y, fill = mixBs.main), color=NA) +
  geom_polygon(data = bb_realms, aes(long, lat, group=group), fill=NA, size=0.2, colour = "grey50") +
  geom_polygon(data = bbox_moll, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
  scale_fill_identity() + theme_void(base_size = 11) + theme(legend.position = "none")+
  coord_equal()

ggsave("./figs/1_Fig1C.tiff", dpi = 600, width = 5.5, height = 3)

# Fig1C - Legend
legBaseline<-choro%>%group_by(ccBrkBase, luBrkBase, mix1)%>%summarise(vlength = n())%>%ungroup()
ggplot(legBaseline, aes(x = luBrkBase, y = ccBrkBase)) + 
  geom_tile(aes(fill = mix1)) + scale_fill_identity() +
  theme_sleek(base_size = 25)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(
    plot.background = element_rect(fill = NA),
    axis.ticks = element_blank(),
    axis.title.y = element_text(angle = 90),
    axis.text = element_blank(),
    legend.position = "none")+
  labs(x = "Land-use instability"~symbol("\256"), y = "Climate velocity"~symbol("\256"))

ggsave("./figs/Fig1CLeg.tiff", dpi = 600, width = 4, height = 4.73)







######################################################################################
#Vulnerability of areas of biodiversity importance map to combined pressures (FIGURE 4)
#####################################################################################
ggplot() + 
  geom_polygon(data = land, aes(long,lat, group=group), fill="grey70") +
  geom_tile(data = choro, aes(x, y, fill = sns85), color=NA) +
  scale_fill_identity() +
  geom_polygon(data = bbox_moll, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
  geom_polygon(data = bb_realms, aes(long, lat, group=group), fill=NA, size=0.2, colour = "grey50")+
  geom_text(data = realm.labels, aes(label = id, x = long, y = lat), size=2)+
  theme_void(base_size = 11) +coord_equal()+theme(legend.position = "none")
ggsave(here::here("./figs/4_Fig4A.tiff"), dpi = 600, width = 5.5, height = 3)

#Generate data for legend
legCI <- ch.df4%>%group_by(cmbBrk85, cntxt, sns85)%>%summarise(vlength = n())%>%ungroup()
legCI$propCells = round(100*(legCI$vlength/sum(legCI$vlength)), 1)
legCI$propCells <- paste(legCI$propCells, "%", sep = "")
glimpse(legCI)

ggplot(legCI, aes(x = cmbBrk85, y = cntxt)) + geom_raster(aes(fill = sns85)) + scale_fill_identity() +
  theme_sleek(base_size = 24)+
  geom_text(aes(label=propCells),size = 5)+
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))+
  theme(plot.background = element_rect(fill = NA),
        axis.ticks = element_blank(), axis.title.y = element_text(angle = 90),
        axis.text = element_blank(),legend.position = "none")+ labs(y = "Conservation value"~symbol("\256"), x = "Climate * land-use"~symbol("\256"))

ggsave("./figs/Fig4ALeg.tiff", dpi = 600, width = 5, height = 5)


#Protection status bars
mydataBars<-choro%>%
  group_by(sns85, pa_dum)%>%
  summarise(vlength = length(sns85))%>%ungroup()
mydataBars<-reshape2::dcast(mydataBars, sns85~pa_dum, sum, value.var = "vlength")
mydataBars$Sum<-(mydataBars$PA+mydataBars$nonPA)
mydataBars$percProtect<-100*(mydataBars$PA/mydataBars$Sum)
mydataBars$percVul<-100*(mydataBars$Sum/sum(mydataBars$Sum))

mydataBars<-rbind(
  data.frame(bicol=mydataBars$sns85, valuez=mydataBars$percProtect, what = "Protected", prt=mydataBars$percProtect),
  data.frame(bicol=mydataBars$sns85, valuez=100-mydataBars$percProtect, what = "Unprotected", prt=mydataBars$percProtect)
)
mydataBars%>%
  ggplot()+
  geom_bar(aes(reorder(bicol, prt), valuez, fill=what), stat = "identity", width = 0.5)+
  scale_fill_manual(values = c("green4", "grey80"))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Percent [%]", x = "Exposure risk")+
  theme_sleek(base_size = 18)+
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)),
        legend.position = "top")
ggsave("./figs/barProtected.tiff", width=5.29, height = 5)


mydataBars%>%ggplot()+
  geom_bar(aes(reorder(bicol, prt), valuez, fill=bicol), stat = "identity", width = 0.5)+
  scale_fill_identity()+
  theme_void()+scale_y_continuous(expand = c(0,0))
ggsave("./figs/exp_gradient.tiff", width=5.29, height = 1.04)


#Plot Fig 4B
choro$value <- 1
mydataAgg <- reshape2::dcast(choro, Country+realms~sns85, length, value.var = "value")
mydataAgg <- na.omit(mydataAgg)
dim(mydataAgg)
mydataAgg$N <- rowSums(mydataAgg[c(3:11)], na.rm = T)

mydataAgg<-subset(mydataAgg, N>5)
mydataAgg$`#005480`<-100*(mydataAgg$`#005480`/mydataAgg$sum) #Note HTML codes may change depending on the rgb colour range selected
mydataAgg$`#00AB80`<-100*(mydataAgg$`#00AB80`/mydataAgg$sum)
mydataAgg$`#00FF80`<-100*(mydataAgg$`#00FF80`/mydataAgg$sum)
mydataAgg$`#755480`<-100*(mydataAgg$`#755480`/mydataAgg$sum)
mydataAgg$`#75AB80`<-100*(mydataAgg$`#75AB80`/mydataAgg$sum)
mydataAgg$`#75FF80`<-100*(mydataAgg$`#75FF80`/mydataAgg$sum)
mydataAgg$`#FF5480`<-100*(mydataAgg$`#FF5480`/mydataAgg$sum)
mydataAgg$`#FFAB80`<-100*(mydataAgg$`#FFAB80`/mydataAgg$sum)
mydataAgg$`#FFFF80`<-100*(mydataAgg$`#FFFF80`/mydataAgg$sum)

mydataAgg<-mydataAgg[, -12]
data <- mydataAgg %>% gather(key = "observation", value="value", -c(1,2)); colnames(data)<-c("Individual","group","observation","value")

data <- data %>% filter(Individual != "Greenland") %>% droplevels

data$Individual<-car::recode(data$Individual,"'United States'='USA'")
data$Individual<-car::recode(data$Individual,"'Venezuela, RB'='Venezuela'")
data$Individual<-car::recode(data$Individual,"'Russian Federation'='Russia'")
data$Individual<-car::recode(data$Individual,"'Egypt, Arab Rep.'='Egypt'")
data$Individual<-car::recode(data$Individual,"'Congo, Dem. Rep.'='Democratic Republic of Congo'")
data$Individual<-car::recode(data$Individual,"'Congo, Rep.'='Republic of Congo'")
data$Individual<-car::recode(data$Individual,"'Lao PDR'='Laos'")
data$Individual<-car::recode(data$Individual,"'Yemen, Rep.'='Yemen'")

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group, Individual)
summary(factor(data$Individual))
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, Individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare realm labels
base_data <- data %>% group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

base_data$group<-car::recode(base_data$group,"'Australasia'='AA'")
base_data$group<-car::recode(base_data$group,"'Indomalaya'='IM'")
base_data$group<-car::recode(base_data$group,"'Nearctic'='NA'")
base_data$group<-car::recode(base_data$group,"'Neotropical'='NT'")
base_data$group<-car::recode(base_data$group,"'Palearctic'='PA'")
base_data$group<-car::recode(base_data$group,"'Afrotropical'='AT'")


(p1 <- ggplot(data) +      
  geom_bar(aes(x=(as.factor(id)), y=value, fill=observation), stat="identity") +
  scale_fill_identity() +
  ggplot2::annotate("text", x = rep(max(data$id),3), y = c(0, 50, 100), label = c("0%", "50%","100%") , color="black", size=4, angle=0, fontface="bold", hjust=1) +
  
  scale_y_continuous( limits = c(-100, max(label_data$tot, na.rm=T)+50)) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(1,-1,-1,-1), "cm")
  ) +
  coord_polar() +
  # Add country names on top of each bar
  geom_text(data=label_data, aes(x=(id), y=max(label_data$tot, na.rm = T)+5, label=Individual, hjust=hjust), color="black", size=4, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add realm names
  geom_text(data=base_data, aes(x = title, y = -15, label=group),colour = "black", size=5, fontface="bold", inherit.aes = FALSE))

ggsave("./figs/Fig4B.tiff", dpi = 600, height = 11, width = 10)








###########################################################################
#SUPPLEMENTARY FIGURE S6: Vulnerability of forest instability compares to land-use composite
##########################################################################

# Compare bi-variates for forest instability and land-use composites
df2<-rbind.data.frame(
  data.frame(x = choro$x, y = choro$y, fill=choro$mixBs.main, what = "Land-use instability", when = "Baseline"),
  data.frame(x = choro$x, y = choro$y, fill=choro$mixBs.pfor, what = "Forest instability", when = "Baseline"),
  data.frame(x = choro$x, y = choro$y, fill=choro$mix85.main, what = "Land-use instability", when = "SSP5-RCP8.5"),
  data.frame(x = choro$x, y = choro$y, fill=choro$mix85.pfor, what = "Forest instability", when = "SSP5-RCP8.5")
)

ggplot() + 
  geom_polygon(data = land, aes(long,lat, group=group), fill="grey70") +
  geom_tile(data = df2, aes(x, y, fill = fill), color=NA) +
  facet_grid(when~what)+
  geom_polygon(data = bbox_moll, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
  scale_fill_identity() + theme_void(base_size = 11) + theme(legend.position = "none")+
  coord_equal()

ggsave("./figs/SI_FigS6.tiff", dpi = 600, width = 5.5, height = 3.5)





############################################################################################################################################
#EXAMINE PROJECTED CHANGES IN VELOCITIES AND INSTABILITIES
###########################################################################################################################################

#Store the global averages (gMean)
gMean_cc <- gm_mean(dWild$cc_Base_max)
gMean_lu <- gm_mean(dWild$lu_Base_max+0.1)


#Calculate and plot estimates for the baseline as bars
velPWild <- dWild%>% 
  group_by(pa_dum)%>%#summarise the data grouped by protected vs unprotected
  summarise(cSSP5 = gm_mean(cc_ssp5_max,na.rm = T),
            cSSP1 = gm_mean(cc_ssp1_max,na.rm = T),
            cBase = gm_mean(cc_Base_max,na.rm = T))
names(velPWild)<-c("status", "vcSSP5","vcSSP1", "cBase")

velPWild2 <- dWild%>%
  group_by(wild_dummy)%>%
  summarise(cSSP5 = gm_mean(cc_ssp5,na.rm = T),
            cSSP1 = gm_mean(cc_ssp1,na.rm = T),
            cBase = gm_mean(cc_Base,na.rm = T))
names(velPWild2)<-c("status", "vcSSP5","vcSSP1", "cBase")
velSSPClim<-rbind(velPWild,velPWild2)
velSSPClim

velPWild<-subset(dWild)%>% #summarise the data grouped by protection status
  group_by(pa_dum)%>%
  summarise(vlSSP5 = gm_mean(lu_ssp5_max+0.1,na.rm = T),
            vlSSP1 = gm_mean(lu_ssp1_max+0.1,na.rm = T), 
            vlBase = gm_mean(lu_Base_max+0.1,na.rm = T))
names(velPWild)<-c("status","vlSSP5", "vlSSP1", "vlBase")

velPWild2<-subset(dWild)%>% #summarise the data grouped by protection status
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
velGlobal$status<-factor(velGlobal$status, levels=c("AW","PW","UW")) 
ggplot(velGlobal, aes(scenario, mean, fill = status))+
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

#write.csv(velGlobal, "averageProjectedChange.csv")


########################################################################################################################################
# PLOT CLIMATE POLAR
#######################################################################################################################################
climCircular<-dWild
climCircular<-climCircular%>%select(everything())%>%group_by(Country, realms)%>%
  summarise(N = n(),
            vBase = gm_mean(cc_Base, na.rm = T),
            vSus = gm_mean(cc_ssp1, na.rm = T),
            vHigh = gm_mean(cc_ssp5, na.rm = T))%>%
  na.omit()%>% filter(N >5, Country != "Greenland")%>%droplevels()%>%  # filter out Greenland and limit the 5 cordex data
  ungroup() %>%
  mutate(realms = factor(realms), # Create new variables percentages of countries baseline
         vBAU = 100*log(vHigh/vBase), 
         vSUS = 100*log(vSus/vBase))

dim(climCircular) # 65 countries remaining
climCircular <- rbind( # convert data to long format by scenarios 
  data.frame(realms=climCircular$realms,Country=climCircular$Country,size=climCircular$N, velocity=climCircular$SUS, scenario="SSP1-RCP2.6"),
  data.frame(realms=climCircular$realms,Country=climCircular$Country,size=climCircular$N, velocity=climCircular$BAU, scenario="SSP5-RCP8.5"))

climCircular$Country<-car::recode(climCircular$Country,"'United States'='USA'")
climCircular$Country<-car::recode(climCircular$Country,"'Venezuela, RB'='Venezuela'")
climCircular$Country<-car::recode(climCircular$Country,"'Russian Federation'='Russia'")
climCircular$Country<-car::recode(climCircular$Country,"'Egypt, Arab Rep.'='Egypt'")
climCircular$Country<-car::recode(climCircular$Country,"'Congo, Dem. Rep.'='Democratic Replublic of Congo'")
climCircular$Country<-car::recode(climCircular$Country,"'Congo, Rep.'='Republic of Congo'")
climCircular$Country<-car::recode(climCircular$Country,"'Lao PDR'='Laos'")
climCircular$Country<-car::recode(climCircular$Country,"'Yemen, Rep.'='Yemen'")

climCircular$realms<-car::recode(climCircular$realms,"'Australasia'='AA'")
climCircular$realms<-car::recode(climCircular$realms,"'Indomalaya'='IM'")
climCircular$realms<-car::recode(climCircular$realms,"'Nearctic'='NA'")
climCircular$realms<-car::recode(climCircular$realms,"'Neotropical'='NT'")
climCircular$realms<-car::recode(climCircular$realms,"'Palearctic'='PA'")
climCircular$realms<-car::recode(climCircular$realms,"'Afrotropical'='AT'")

climPolar_data<-climCircular %>% select(everything()) %>% group_by(realms, Country) %>% mutate(tot=sum(velocity)) %>% ungroup()

climPolar_data <- climPolar_data %>% 
  select(everything()) %>% 
  arrange(realms, tot) %>% 
  add_row(realms = rep(unique(climPolar_data$realms), 4)) %>% # add a couple of empty rows to separate continents
  arrange(realms)%>% 
  ungroup()

plotting_data$id <- rep(seq(1, nrow(climPolar_data)/nlevels(as.factor(climPolar_data$scenario))), each=nlevels(as.factor(climPolar_data$scenario)))

# Get the name and the y position of each label
label_data <- climPolar_data %>% group_by(id, Country) %>% summarize(tot=sum(velocity))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


realms_breaks <- climPolar_data %>% 
  group_by(realms) %>% 
  summarize(start = min(id), 
            end = max(id) - 2) %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end))) %>% 
  ungroup() %>% 
  mutate(end = data.table::shift(end + 1, n = 1, type = "shift", fill = max(end) + 1),
         start = start -1) 

realms_breaks$start[realms_breaks$realms == "AA"] <- -1
realms_breaks$end[realms_breaks$realms == "AA"] <- 0

max_value <- max(climPolar_data$tot, na.rm = T); y_max <- 20 * ceiling(max_value/20); v <- c(-50,0,50,100, 200)
realms_breaks <- realms_breaks %>% mutate(v = list(v)) %>% unnest()

climPolar_data$scenario <- factor(climPolar_data$scenario, levels = c("SSP5-RCP8.5","SSP1-RCP2.6"))
climatePolar <- climPolar_data %>% ggplot()+
  geom_bar(aes(x = as.factor(id), y = velocity, fill = scenario), position="stack", stat="identity", width = 0.8) +
  geom_segment(data = continent_breaks, aes(x = end, y = v, xend = start, yend = v), colour = "grey", size=0.3 , inherit.aes = FALSE ) +
  annotate("text",
           x = rep(max(climPolar_data$id[climPolar_data$realms == "PA"])+1, length(v)),
           y = v -5, label = paste0(head(v), "%"), color = "grey", size = 3, angle = 0, fontface = "bold", hjust = 0.7)+
  ylim(-200, y_max+200)+
  scale_fill_manual(values = c("#be64ac", "#3b4994"))+
  theme_minimal(base_family = "Helvetica", base_size = 12) +
  guides(fill = guide_legend(nrow = 1))+
  coord_polar()+
  theme(
    legend.position = c(0.5, 0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm")
  )+
  geom_text(data = label_data, aes(x = id, y = y_max, label = Country, hjust = hjust), color="black",  size = 3, angle = label_data$angle, inherit.aes = FALSE)+
  geom_text(data = continent_breaks, 
            aes(x = title, y = 140, label = realms),  colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE)

print(climatePolar)
ggsave("3_Fig3.tiff", dpi = 600, height = 8, width = 8)





##########################################################################################################################
# LAND-USE POLAR for CoUNTRIES
##########################################################################################################################
ins_coverage<-dWild

ins_coverage$secdf2050_hist <- round(ins_coverage$secdf2050_hist, 3)
hist(ins_coverage$secdf2050_hist, breaks = 50)
gm_mean(ins_coverage$secdf2050_hist+0.1, na.rm = TRUE)/2
sd(ins_coverage$secdf2050_hist+0.1, na.rm = TRUE)

ins_coverage$value <- 1
ins_coverage$d1 <- ifelse(ins_coverage$lu_Base_max>0.08, 1,0)
ins_coverage$d2 <- ifelse(ins_coverage$lu_ssp5_max>0.08, 1,0)
ins_coverage$d1_d2 <- paste(ins_coverage$d1, dWild$d2)
ins_coverage$d1_d2 <- plyr::revalue(factor(ins_coverage$d1_d2), c("0 0" = "unchanged", "0 1" = "ssp5", "1 0" ="base", "1 1"="base")) #All baseline changes that were reflected in projected areas ("1 0") where considered as base change

lu_ISO3<-reshape2::dcast(ins_coverage, Country+realms~d1_d2, length, value.var = "value")
lu_ISO3<-na.omit(lu_ISO3); # nrow(mydataAgg)
lu_ISO3$sum<-rowSums(lu_ISO3[c(3:5)], na.rm = T) # Store rowsums
lu_ISO3$unchanged<-100*(lu_ISO3$unchanged/lu_ISO3$sum);lu_ISO3$ssp5<-100*(lu_ISO3$ssp5/lu_ISO3$sum);lu_ISO3$base<-100*(lu_ISO3$base/lu_ISO3$sum)

lu_ISO3<-lu_ISO3 %>% filter(sum >5, Country != "Greenland") %>% droplevels() # Plot Polar for countries hosting ~2500km2 wilderness areas
nrow(mydataAgg) # 65 countries remaining

lu_ISO3 <- rbind( # convert data to long format by scenarios 
  data.frame(realms=lu_ISO3$realms,Country=lu_ISO3$Country,size=lu_ISO3$sum, perc_cov=lu_ISO3$unchanged, scenario="Unchanged"),
  data.frame(realms=lu_ISO3$realms,Country=lu_ISO3$Country,size=lu_ISO3$sum, perc_cov=lu_ISO3$base, scenario="1970-2005"),
  data.frame(realms=lu_ISO3$realms,Country=lu_ISO3$Country,size=lu_ISO3$sum, perc_cov=lu_ISO3$ssp5, scenario="2015-2050"))

# Rename some countries and Biogeographical realms
lu_ISO3$Country<-car::recode(lu_ISO3$Country,"'United States'='USA'")
lu_ISO3$Country<-car::recode(lu_ISO3$Country,"'Venezuela, RB'='Venezuela'")
lu_ISO3$Country<-car::recode(lu_ISO3$Country,"'Russian Federation'='Russia'")
lu_ISO3$Country<-car::recode(lu_ISO3$Country,"'Egypt, Arab Rep.'='Egypt'")
lu_ISO3$Country<-car::recode(lu_ISO3$Country,"'Congo, Dem. Rep.'='Democratic Replublic of Congo'")
lu_ISO3$Country<-car::recode(lu_ISO3$Country,"'Congo, Rep.'='Republic of Congo'")
lu_ISO3$Country<-car::recode(lu_ISO3$Country,"'Lao PDR'='Laos'")
lu_ISO3$Country<-car::recode(lu_ISO3$Country,"'Yemen, Rep.'='Yemen'")

lu_ISO3$realms<-car::recode(lu_ISO3$realms,"'Australasia'='AA'")
lu_ISO3$realms<-car::recode(lu_ISO3$realms,"'Indomalaya'='IM'")
lu_ISO3$realms<-car::recode(lu_ISO3$realms,"'Nearctic'='NA'")
lu_ISO3$realms<-car::recode(lu_ISO3$realms,"'Neotropical'='NT'")
lu_ISO3$realms<-car::recode(lu_ISO3$realms,"'Palearctic'='PA'")
lu_ISO3$realms<-car::recode(lu_ISO3$realms,"'Afrotropical'='AT'")

table(lu_ISO3$Country, lu_ISO3$scenario) #
lu_ISO3$size<-ifelse(lu_ISO3$Country == "Ethiopia", lu_ISO3$size+0.5, lu_ISO3$size) # add small value to overlapping size

coverage_data <- lu_ISO3 %>% 
  select(everything()) %>% 
  arrange(realms, size) %>% 
  add_row(realms = rep(unique(coverage_data$realms), 6)) %>% # add a couple of empty rows to separate continents
  arrange(realms)%>% 
  ungroup()

coverage_data$id <- rep(seq(1, nrow(coverage_data)/nlevels(as.factor(coverage_data$scenario))), each=nlevels(as.factor(coverage_data$scenario)))

# Get the name and the y position of each label
label_data <- coverage_data %>% group_by(id, Country) %>% summarize(tot=sum(perc_cov))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar # Subtract 0.5 for letters to have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

realms_breaks <- coverage_data %>% 
  group_by(realms) %>% 
  summarize(start = min(id), 
            end = max(id) - 3) %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end))) %>% 
  ungroup() %>% 
  mutate(end = data.table::shift(end + 1, n = 1, type = "shift", fill = max(end) + 1), start = start -1) 

realms_breaks$start[realms_breaks$realms == "AA"] <- -1
realms_breaks$end[realms_breaks$realms == "AA"] <- 0

max_value <- max(coverage_data$perc_cov, na.rm = T); y_max <-max(coverage_data$perc_cov, na.rm = T); v <- c(0,50,100)
realms_breaks <- realms_breaks %>% mutate(v = list(v)) %>% unnest()


coverage_data$scenario <- factor(coverage_data$scenario, levels = c("2015-2050", "1970-2005", "Unchanged")) #reorder scenarios
coverage_data %>% ggplot()+
  geom_bar(aes(x = as.factor(id), y = per_size, fill = scenario), position="stack", stat="identity", width = 0.8) +
  geom_segment(data = continent_breaks, aes(x = end, y = v, xend = start, yend = v), colour = "grey", size=0.3 , inherit.aes = FALSE ) +
  annotate("text",
           x = rep(max(coverage_data$id[coverage_data$realms == "PA"])+1, length(v)),
           y = v, label = paste0(head(v), "%"), color = "grey", size = 3, angle = 0, fontface = "bold", hjust = 0.7)+
  ylim(-100, y_max+80)+
  scale_fill_manual(values = c("#DDCC77", "#88CCEE", "#117733"))+guides(fill = guide_legend(nrow = 1))+
  theme_minimal(base_family = "Helvetica", base_size = 12) +
  coord_polar()+
  theme(
    legend.position = c(0.5, 0.08),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm")
  )+
  geom_text(data = label_data, aes(x = id, y = y_max+10, label = Country, hjust = hjust), 
            color="black",  size = 3.5, angle = label_data$angle, inherit.aes = FALSE)+
  geom_text(data = continent_breaks, aes(x = title, y = -15, label = realms),
            colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE)
ggsave("SI_Fig1.tiff", dpi = 600, height = 8.5, width = 8)

# Prepare data for plotting inset
wildloss <- reshape2::dcast(dWild, d1_d2~value, length, value.var = "value"); names(wildloss)<-c("category", "change")
wildloss$change <- 100*(wildloss$change/sum(wildloss$change))
wildloss$labels <- as.factor(as.character(round(wildloss$change, 0)))
wildloss$labels <- paste(wildloss$labels, "%", sep = "")

wildloss$category <- plyr::revalue(factor(wildloss$category), c("unchanged"="Unchanged", "ssp5"="2015-2050", "base"="1971-2005"))
wildloss$category <- factor(wildloss$category, levels = c("2015-2050", "1971-2005", "Unchanged"))
ggplot(data = wildloss)+
  geom_bar(aes(x = reorder(category, change), y = change, fill = category),
           position="stack", stat="identity", width = 0.8)+
  geom_text(aes(x = reorder(category, change), y = change+1.5, label=labels),size = 5, colour = "black")+
  scale_fill_manual(values = c("#DDCC77", "#88CCEE", "#117733"))+
  theme_void()+
  theme(legend.position = "none")
ggsave("SI_FigS1_inset.tiff", dpi = 600, height = 4, width = 2)
