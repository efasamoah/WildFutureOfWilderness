#EXAMINE PROJECTED CHANGES IN VELOCITIES AND INSTABILITIES
#Store the gloabl averages (gMean)
gMean_cc = gm_mean(dWild$cc_Base)
gMean_lu = gm_mean(dWild$lu_Base_max+0.1)


#Calculate and plot estimates for the baseline as bars
velPWild<-dWild%>% 
  group_by(pa_dum)%>%#summarise the data grouped by protection vs unprotected
  summarise(cSSP5 = gm_mean(cc_ssp5,na.rm = T),
            cSSP1 = gm_mean(cc_ssp1,na.rm = T),
            cBase = gm_mean(cc_Base,na.rm = T))
names(velPWild)<-c("status", "vcSSP5","vcSSP1", "cBase")

velPWild2<-dWild%>%
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

write.csv(velGlobal, "avergeProjectedChange.csv")



# PLOT CLIMATE POLAR
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










# LAND-USE POLAR for CoUNTRIES
ins_coverage<-dWild

ins_coverage$secdf2050_hist<-round(ins_coverage$secdf2050_hist, 3)
hist(ins_coverage$secdf2050_hist, breaks = 50)
gm_mean(ins_coverage$secdf2050_hist+0.1, na.rm = TRUE)/2
sd(ins_coverage$secdf2050_hist+0.1, na.rm = TRUE)

ins_coverage$value<-1
ins_coverage$d1<-ifelse(ins_coverage$lu_Base_max>0.08, 1,0)
ins_coverage$d2<-ifelse(ins_coverage$lu_ssp5_max>0.08, 1,0)
ins_coverage$d1_d2<-paste(ins_coverage$d1, dWild$d2)
ins_coverage$d1_d2<-plyr::revalue(factor(ins_coverage$d1_d2), c("0 0" = "unchanged", "0 1" = "ssp5", "1 0" ="base", "1 1"="base"))

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