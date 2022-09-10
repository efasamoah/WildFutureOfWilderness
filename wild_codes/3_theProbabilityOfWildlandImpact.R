######################################################################################
#Vulnerability of areas of biodiversity importance map to combined pressures (FIGURE 4)
#####################################################################################
library(raster)
library(rgdal)
library(tidyverse)

bbox <- rnaturalearth::ne_download(scale=50, type="wgs84_bounding_box", category="physical")%>%spTransform(CRS("+proj=moll"))%>%fortify()
land <- rnaturalearth::ne_download(scale=10, type="land", category="physical")%>%spTransform(CRS("+proj=moll"))%>%fortify()

bb_realms <- readOGR("./data/wild_ms_database/shp_vectors/Generalised_Biogeographic_Realms_2004", layer="brpol_fin_dd")%>%spTransform(CRS("+proj=moll"))#%>%fortify(region = "REALMCODE")
centroids.realms <- as.data.frame(coordinates(bb_realms))
names(centroids.realms) <- c("long", "lat")

idList <- bb_realms@data$REALMCODE
realm.df <- data.frame(id=idList, centroids.realms) #Using centroid positions text on key priority areas so Manually adjust labels in map
realm.labels <- cbind.data.frame(id=c("PA","NA","IM","AT","NT","AA"),long=c(4214966, -10839085, 8242787, 104967, -8522277, 15047461),lat=c(5386746.3, 5491240.4, 100038.6, -1347711.5, -1478926.6, -2980095.8))

# IMPORT DATA
wildMSData <- read_rds("output_tables/WildDataRevised.Rds") %>% 
  select(Country, realms, long, lat, intactness, pa_dum, wildBin, 
         velocity_temp_rcp85_2050, velocity_prec_rcp85_2050, ssp5_lui, base_lui)

#IMPORT SOME FUNCTIONS
source("./wild_codes/6_ImpFunctions.R")
source("./wild_codes/others/BivariateMap.R") #import bivariate codes
# Define the number of breaks
nBreaks <- 3
# Create the colour matrix
col.matVul <- colmat(nbreaks = nBreaks, breakstyle = "quantile", xlab = "X", ylab = "Y",
                      upperleft = "dodgerblue4", upperright = "darkred", bottomleft =  "#BEBEBE", bottomright = "yellow", #main
                     # upperleft = "#64ACBE", upperright = "#574249", bottomleft = "#E8E8E8", bottomright = "#C85A5A",
                     # upperleft = "#BE64AC", upperright = "#3B4994", bottomleft = "#E8E8E8", bottomright = "#5AC8C8",
                     # upperleft = "#73AE80", upperright = "#2A5A5B", bottomleft = "#E8E8E8", bottomright = "#6C83B5", 
                     # upperleft = "#9972AF", upperright = "#804D36", bottomleft = "#E8E8E8", bottomright = "#C8B35A",
                     # upperleft = "#DA8DC8", upperright = "#697AA2", bottomleft = "#E8E8E8", bottomright = "#73BCA0",
                     # upperleft = "#F7900A", upperright = "#993A65", bottomleft = "#44B360", bottomright = "#3A88B5",
                     # upperleft = "#FEF287", upperright = "#21908D", bottomleft = "#E8F4F3", bottomright = "#9874A1",
                     # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
                     # upperleft = "#0096EB", upperright = "#820050", bottomleft= "#BEBEBE", bottomright = "#FFE60F", #default
                     # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
                     # upperleft = "dodgerblue4", upperright = "#820050", bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                     saveLeg = FALSE, plotLeg = TRUE)

# Retrieve bivariate colour pallet data
lgd <- BivLegend$data; names(lgd) <- c("binY", "binX", "HEXCode", "UID")

#add small (+.001) to instability to values
#exp(0.827) #2.286449
#exp(0.852) #2.344331
Vul_Wilderness <- wildMSData %>%
  mutate(ImpactProb = ssp5_lui+0.001 * (2.286449*velocity_temp_rcp85_2050 + 2.344331*velocity_prec_rcp85_2050),
         ImpactProb2 = ImpactProb/(1+ImpactProb))%>%
  mutate(binY = ntile(intactness, 3),
         binX = ntile(sqrt(ImpactProb2), 3))%>%
  inner_join(y = lgd, by = c("binY", "binX"))
hist(sqrt(Vul_Wilderness$ImpactProb2), breaks=100)

(expMaps <- ggplot() +
    geom_polygon(data = land, aes(long,lat, group=group), fill = "grey80") +
    geom_tile(data = subset(Vul_Wilderness, wildBin=="wild"), aes(long, lat, fill = HEXCode), color=NA) +
    scale_fill_identity()+
    theme_void(base_size = 11) +coord_equal()+theme(legend.position = "none") +
    #geom_polygon(data = bbox_moll, aes(long,lat, group=group), fill=NA, size=0.2, colour = "grey50") +
    geom_polygon(data = bb_realms, aes(long, lat, group=group), fill=NA, size=0.2, colour = "grey50")+
    geom_text(data = realm.labels, aes(label = id, x = long, y = lat), size=3)
)
#ggsave(plot = expMaps, here::here("./out_plots/4_Fig4A.tiff"), dpi = 600, width = 5.5, height = 3)

#Generate data for legend
(legCI <- subset(Vul_Wilderness, wildBin=="wild") %>%
    group_by(binY, binX, HEXCode) %>%
    summarise(N = n()) %>% 
    ungroup() %>%
    mutate(propCells = round(100*(N/sum(N)), 1),
           propCells = paste0(propCells, "%")) %>%
    ggplot(aes(x = binX, y = binY, fill = HEXCode)) +
    geom_raster() + scale_fill_identity() +
    geom_text(aes(label=propCells),size = 7)+
    theme_minimal(base_size = 25)+
    scale_x_discrete(expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.title.y = element_text(angle = 90))+
    labs(x = "Impact probability" ~symbol("\256"), 
         y = "Conservation value" ~symbol("\256"))+
    theme(legend.position = "none"))
#ggsave(plot = legCI, "./out_plots/Fig4B1.tiff", dpi = 600, width = 5.5, height = 5.5)

######################################################################################################
##                               PROTECTION STATUS (Fig 4b.1)                                       ##
######################################################################################################
mydataBars <- subset(Vul_Wilderness, wildBin=="wild") %>% group_by(HEXCode, pa_dum)%>% summarise(vlength = n())%>%ungroup()
mydataBars <- reshape2::dcast(mydataBars, HEXCode ~ pa_dum, sum, value.var = "vlength")
mydataBars$Protected <- 100*(mydataBars$PA/rowSums(mydataBars[, c("PA", "nonPA")]))

mydataBars <- rbind(
  data.frame(bicol=mydataBars$HEXCode, value = 100-mydataBars$Protected, what = "Unprotected", prt=mydataBars$Protected),
  data.frame(bicol=mydataBars$HEXCode, value = mydataBars$Protected, what = "Protected", prt=mydataBars$Protected)
)

mydataBars$what <- factor(mydataBars$what, levels = c("Unprotected", "Protected"))
(p1a <- mydataBars %>%
    ggplot()+
    geom_bar(aes(reorder(bicol, prt), value, fill=what), stat = "identity", width = 0.5)+
    scale_fill_manual(values = c("grey80", "green4"))+
    scale_y_continuous(expand = c(0,0))+
    labs(y = "Percent [%]", x = "")+
    theme_sleek(base_size = 25)+
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_text(margin = margin(t = -30, r = 0, b = 0, l = 0)),
          legend.position = "top"))

(p2a <- mydataBars %>% ggplot()+
    geom_bar(aes(reorder(bicol, prt), value, fill=bicol), stat = "identity", width = 0.5)+
    scale_fill_identity()+labs(y = "", x = "Exposure risk")+
    theme_sleek(base_size = 25)+scale_y_continuous(expand = c(0,0))+
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_text(margin = margin(t = 1, r = 0, b = -1, l = 0)),
          panel.border = element_blank(), 
          axis.text.y = element_text(colour = "white"), 
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank())
)
(PAnonPA <- cowplot::plot_grid(p1a, p2a, rel_heights = c(1, 0.2), ncol=1))
# ggsave(plot = PAnonPA, filename = "./out_plots/Fig4B2.tiff", width = 5.5, height = 5.5)


# Plot Fig 4B
realmsRemained <- readr::read_csv("./data/wild_ms_database/input_tables/realmsRemained.csv")
mydataAgg <- Vul_Wilderness %>% filter(wildBin=="wild")%>%
  inner_join(realmsRemained, by="Country")%>%
  mutate(value = 1,
         Biorealms = ifelse(Biorealms == "NA1", "NA", Biorealms)) %>% na.omit()
mydataAgg <- reshape2::dcast(mydataAgg, Country + Biorealms ~ HEXCode, length, value.var = "value")
mydataAgg$N <- rowSums(mydataAgg[c(3:11)], na.rm = T); mydataAgg <- subset(mydataAgg, N>5)
mydataAgg <- bind_cols(mydataAgg[, 1:2], 100*(mydataAgg[, 3:11]/rowSums(mydataAgg[, 3:11])))

data <- mydataAgg %>% gather(key = "observation", value="value", -c(1,2)); colnames(data)<-c("Individual","group","observation","value")

# Rename some countries and Biogeographical realms
data$Individual <- car::recode(data$Individual,"'Venezuela, RB'='Venezuela'")
data$Individual <- car::recode(data$Individual,"'Russian Federation'='Russia'")
data$Individual <- car::recode(data$Individual,"'Egypt, Arab Rep.'='Egypt'")
data$Individual <- car::recode(data$Individual,"'Congo, Dem. Rep.'='Democratic Replublic of Congo'")
data$Individual <- car::recode(data$Individual,"'Congo, Rep.'='Republic of Congo'")
data$Individual <- car::recode(data$Individual,"'Lao PDR'='Laos'")
data$Individual <- car::recode(data$Individual,"'Yemen, Rep.'='Yemen'")

data <- data %>% select(everything()) %>% group_by(group, Individual) %>% mutate(tot = sum(value)) %>% ungroup()
nc_uni <- unique(data$group)
data <- data %>% 
  select(everything()) %>% 
  arrange(group, tot) %>% 
  add_row(group = rep(nc_uni, 18)) %>% # add a couple of empty rows to separate continents
  arrange(group, Individual)%>% 
  ungroup()
data$id <- rep(seq(1, nrow(data)/nlevels(as.factor(data$observation))),each = nlevels(as.factor(data$observation)))

# Get the name and the y position of each label
label_data <- data %>% group_by(id, Individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Prepare realm labels
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start = min(id), end = max(id) - 2) %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end))) %>% 
  ungroup() %>% 
  mutate(end = data.table::shift(end + 1, n = 1, type = "shift", fill = max(end) + 1),start = start -1) 

base_data$start[base_data$group == "AA"] <- -1
base_data$end[base_data$group == "AA"] <- 0

max_value <- max(data$tot, na.rm = T); y_max <- 20 * ceiling(max_value/20); v <- c(0, 50, 100)
base_data <- base_data %>% mutate(v = list(v)) %>% unnest()
# Prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

(p1 <- ggplot(data) +      
    geom_bar(aes(x=(as.factor(id)), y=value, fill=observation), position = "stack", stat="identity", width = 0.5) +
    scale_fill_identity() +
    geom_segment(data = base_data, aes(x = end, y = v, xend = start, yend = v), colour = "grey", size=0.3 , inherit.aes = FALSE ) +
    annotate("text", x = rep(max(data$id[data$group == "PA"]), length(v)), 
             y = v-5, label = paste0(head(v), "%"), color = "black", size = 5, angle = 0, fontface = "bold", hjust = 0.7)+
    scale_y_continuous( limits = c(-100, max(label_data$tot, na.rm=T)+50)) +
    theme_minimal(base_family = "Helvetica", base_size = 5) +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(c(1,-1,-1,-1), "cm")
    ) +
    coord_polar() +
    geom_text(data = label_data, aes(x= id, y=max(label_data$tot, na.rm = T)+5, label=Individual, hjust=hjust), color="black", size=5, angle= label_data$angle, inherit.aes = FALSE ) +
    geom_text(data = base_data, aes(x = title, y = -15, label=group),colour = "black", size=5, fontface="bold", inherit.aes = FALSE))
ggsave(plot=p1, filename="./out_plots/Fig4C_n.tiff", dpi = 300, height = 11, width = 10)

# # Prepare data for graph abstract
# VulBarsBiomes <- Vul_Wilderness %>% filter(wildBin=="wild")%>%
#   inner_join(realmsRemained, by="Country")%>%
#   mutate(value = 1,Biorealms = ifelse(Biorealms == "NA1", "NA", Biorealms)) %>% na.omit()
# VulBarsBiomes <- subset(VulBarsBiomes, wildBin=="wild") %>% group_by(HEXCode, Biorealms)%>% summarise(vlength = n())%>%ungroup()
# VulBarsBiomes <- reshape2::dcast(VulBarsBiomes, HEXCode ~ Biorealms, sum, value.var = "vlength")
# VulBarsBiomes <- bind_cols(VulBarsBiomes["HEXCode"], sweep(VulBarsBiomes[, 2:7], 2, colSums(VulBarsBiomes[, 2:7]),`/`))
# 
# #Australasia
# VulBarsBiomes %>% ggplot() +      
#   geom_col(aes(x="A", y=AA, fill=HEXCode), position = "stack", width = 0.5) +
#   scale_fill_identity() + coord_flip()+
#   scale_x_discrete(expand = c(0,0))+scale_y_continuous(name = "Percent of Area", expand = c(0,0), label=scales::percent)+
#   theme_minimal(base_size = 20)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank())
# 
# #Afrotropical
# VulBarsBiomes %>% ggplot() +      
#   geom_col(aes(x="A", y=AT, fill=HEXCode), position = "stack", width = 0.5) +
#   scale_fill_identity() + coord_flip()+
#   scale_x_discrete(expand = c(0,0))+scale_y_continuous(name = "Percent of Area", expand = c(0,0), label=scales::percent)+
#   theme_minimal(base_size = 20)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank())
# ggsave("Afrotropics.tiff")
# 



##########################################################################################################################
#                          LAND-USE POLAR FOR COUNTRIES (FIGURE S1)
##########################################################################################################################
realmsRemained <- readr::read_csv("./data/wild_ms_database/input_tables/realmsRenamed.csv")
(gMean1<- gm_mean(subset(wildMSData, wildBin=="wild")$base_lui+0.1, na.rm = TRUE))

ins_coverage <- subset(wildMSData, wildBin=="wild") %>%
  inner_join(realmsRemained, by="Country")%>%
  mutate(Biorealms = ifelse(Biorealms == "NA1", "NA", Biorealms),
         
         d1=ifelse(base_lui>gMean1, "H", "L"),
         d2=ifelse(ssp5_lui>gMean1, "H", "L"),
         
         d1_d2=paste0(d1, d2), 
         
         d1_d2=ifelse(d1_d2=="LL", "Unchanged", d1_d2),
         d1_d2=ifelse(d1_d2=="LH", "NewHotspot", d1_d2),
         d1_d2=ifelse(d1_d2=="HL", "DimiHotspot", d1_d2),
         d1_d2=ifelse(d1_d2=="HH", "PersistHotspot", d1_d2), 
         
         value = 1)

lu_ISO3 <- reshape2::dcast(ins_coverage, Country+Biorealms~d1_d2, length, value.var = "value")
lu_ISO3$sum <- rowSums(lu_ISO3[c(3:6)], na.rm = T) # Store rowsums
lu_ISO3 <- lu_ISO3 %>% 
  filter(sum >5, Country!="Greenland")%>%
  mutate(Unchanged=100*(Unchanged/sum),
         NewHotspot=100*(NewHotspot/sum),
         DimiHotspot=100*(DimiHotspot/sum),
         PersistHotspot=100*(PersistHotspot/sum), 
         total=Unchanged+NewHotspot+DimiHotspot+PersistHotspot)
# Plot Polar for countries hosting ~2500km2 wilderness areas
nrow(lu_ISO3) # 65 countries remaining

lu_ISO3 <- rbind( # convert data to long format by scenarios 
  data.frame(realms=lu_ISO3$Biorealms,Country=lu_ISO3$Country,size=lu_ISO3$sum, perc_cov=lu_ISO3$Unchanged, scenario="Unchanged", ordBy = lu_ISO3$Unchanged),
  data.frame(realms=lu_ISO3$Biorealms,Country=lu_ISO3$Country,size=lu_ISO3$sum, perc_cov=lu_ISO3$DimiHotspot, scenario="Diminishing Hotspot",  ordBy = lu_ISO3$Unchanged),
  data.frame(realms=lu_ISO3$Biorealms,Country=lu_ISO3$Country,size=lu_ISO3$sum, perc_cov=lu_ISO3$NewHotspot, scenario="New Hotspot",  ordBy = lu_ISO3$Unchanged),
  data.frame(realms=lu_ISO3$Biorealms,Country=lu_ISO3$Country,size=lu_ISO3$sum, perc_cov=lu_ISO3$PersistHotspot, scenario="Persistent Hotspot",  ordBy = lu_ISO3$Unchanged))

# Rename some countries and Biogeographical realms
lu_ISO3$Country <- car::recode(lu_ISO3$Country,"'Venezuela, RB'='Venezuela'")
lu_ISO3$Country <- car::recode(lu_ISO3$Country,"'Russian Federation'='Russia'")
lu_ISO3$Country <- car::recode(lu_ISO3$Country,"'Egypt, Arab Rep.'='Egypt'")
lu_ISO3$Country <- car::recode(lu_ISO3$Country,"'Congo, Dem. Rep.'='Democratic Replublic of Congo'")
lu_ISO3$Country <- car::recode(lu_ISO3$Country,"'Congo, Rep.'='Republic of Congo'")
lu_ISO3$Country <- car::recode(lu_ISO3$Country,"'Lao PDR'='Laos'")
lu_ISO3$Country <- car::recode(lu_ISO3$Country,"'Yemen, Rep.'='Yemen'")

#table(lu_ISO3$Country, lu_ISO3$scenario) #
lu_ISO3$size <- ifelse(lu_ISO3$Country=="Ethiopia", lu_ISO3$size+0.5, lu_ISO3$size) # add small value to overlapping size
coverage_data <- lu_ISO3 %>% 
  select(everything()) %>% 
  arrange(realms, size) %>% 
  add_row(realms = rep(unique(lu_ISO3$realms), 12)) %>% # add a couple of empty rows to separate continents
  arrange(realms)%>%
  ungroup()

coverage_data$id = rep(seq(1, nrow(coverage_data)/nlevels(as.factor(coverage_data$scenario))), each=nlevels(as.factor(coverage_data$scenario)))
# Get the name and the y position of each label
label_data <- coverage_data %>% group_by(id, Country) %>% summarize(tot=sum(perc_cov))%>%ungroup()
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar # Subtract 0.5 for letters to have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse( angle < -90, angle+180, angle)

realms_breaks <- coverage_data %>% 
  group_by(realms) %>% 
  summarize(start=min(id), 
            end=max(id) - 3) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end))) %>% 
  ungroup() %>% 
  mutate(end=data.table::shift(end+1, n=1, type="shift", fill=max(end)+1), start=start-1) 

realms_breaks$start[realms_breaks$realms=="AA"] <- -1
realms_breaks$end[realms_breaks$realms=="AA"] <- 0

max_value <- max(coverage_data$perc_cov, na.rm = T); y_max <- max(coverage_data$perc_cov, na.rm = T); v <- c(0, 50, 100)
realms_breaks <- realms_breaks %>% mutate(v=list(v)) %>% unnest()

coverage_data$scenario <- factor(coverage_data$scenario, levels = c("Persistent Hotspot", "New Hotspot", "Diminishing Hotspot", "Unchanged")) #reorder scenarios
(p1<-coverage_data %>% ggplot()+
    #geom_bar(aes(x = as.factor(id), y = perc_cov, fill = scenario), position="stack", stat="identity", width = 0.8) +
    geom_bar(aes(x=as.factor(id), y=perc_cov, fill=scenario),
             position="stack", stat="identity", width = 0.5) +
    geom_segment(data = realms_breaks, aes(x=end, y=v, xend=start, yend=v), colour="grey", size=0.3, inherit.aes = FALSE ) +
    annotate("text",
             x=rep(max(coverage_data$id[coverage_data$realms=="PA"]), length(v)),
             y=v, label=paste0(head(v),"%"), color="grey", size=3, angle=0, fontface="bold", hjust=0.7)+
    ylim(-120, y_max+80)+
    #scale_fill_manual(values = c("darkred", "#DDCC77", "#88CCEE", "#117733"))+
    scale_fill_manual(values=c("#FFFF9F", "#F2BA49","#C32148", "#800000"))+
    guides(fill=guide_legend(nrow=1))+
    theme_minimal(base_size=12) +
    coord_polar()+
    theme(
      legend.position=c(0.5, 0.05),
      legend.title=element_blank(),
      legend.text=element_text(size = 10),
      axis.text=element_blank(),
      axis.title=element_blank(),
      panel.grid=element_blank(),
      plot.margin=unit(rep(0,4), "cm")
    )+
    geom_text(data=label_data, aes(x=id, y=y_max+10, label=Country, hjust=hjust), color="black",  size=4, angle=label_data$angle, inherit.aes = FALSE)+
    geom_text(data=realms_breaks, aes(x=title, y=-15, label=realms), colour="black", alpha=0.8, size=4, fontface="bold", inherit.aes=FALSE)
)
#ggsave("SI_Fig1N.tiff", dpi=600, height = 11, width = 10)

# Prepare data for plotting inset
wildloss <- reshape2::dcast(ins_coverage, d1_d2 ~ value, length, value.var = "value"); names(wildloss)<-c("category", "change")
(p2 <- wildloss %>%
    mutate(change = 100*(change/sum(change)),
           labels = as.factor(as.character(round(change, 0))),
           labels = paste(labels, "%", sep = ""),
           
           category = ifelse(category == "Unchanged", "Unchanged", category),
           category = ifelse(category == "NewHotspot", "New Hotspot", category),
           category = ifelse(category == "DimiHotspot", "Diminishing Hotspot", category),
           category = ifelse(category == "PersistHotspot", "Persistent Hotspot", category), 
           
           category = factor(category, levels = c("Persistent Hotspot", "New Hotspot", "Diminishing Hotspot", "Unchanged")))%>%
    ggplot()+
    geom_bar(aes(x = reorder(category, change), y = change, fill = category),position="stack", stat="identity", width = 0.5)+
    geom_text(aes(x = reorder(category, change), y = change+2, label=labels),size = 3, colour = "black")+
    #scale_fill_manual(values = c("darkred","#DDCC77", "#88CCEE", "#117733"))+
    scale_fill_manual(values = c("#FFFF9F", "#F2BA49","#C32148", "#800000"))+
    theme_void()+theme(legend.position = "none")
)
ggsave(plot = p2, "SI_FigS1_inset.png", dpi = 600, height = 2, width = 1.5)
