# library
library(tidyverse)
library(viridis)
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
ggsave("barProtected.tiff", width=5.29, height = 5)


mydataBars%>%
  ggplot()+
  geom_bar(aes(reorder(bicol, prt), valuez, fill=bicol), stat = "identity", width = 0.5)+
  #scale_fill_manual(values = c("green4", "grey80"))+
  scale_fill_identity()+
  theme_void()+
  scale_y_continuous(expand = c(0,0))
ggsave("barProtLeg.tiff", width=5.29, height = 1.04)




choro$value<-1
mydataAgg<-reshape2::dcast((choro), Country+realms~sns85, length, value.var = "value")
mydataAgg<-na.omit(mydataAgg)
dim(mydataAgg)
mydataAgg$sum<-rowSums(mydataAgg[c(3:11)], na.rm = T)

mydataAgg<-subset(mydataAgg, sum>5)
mydataAgg$`#005480`<-100*(mydataAgg$`#005480`/mydataAgg$sum)
mydataAgg$`#00AB80`<-100*(mydataAgg$`#00AB80`/mydataAgg$sum)
mydataAgg$`#00FF80`<-100*(mydataAgg$`#00FF80`/mydataAgg$sum)
mydataAgg$`#755480`<-100*(mydataAgg$`#755480`/mydataAgg$sum)
mydataAgg$`#75AB80`<-100*(mydataAgg$`#75AB80`/mydataAgg$sum)
mydataAgg$`#75FF80`<-100*(mydataAgg$`#75FF80`/mydataAgg$sum)
mydataAgg$`#FF5480`<-100*(mydataAgg$`#FF5480`/mydataAgg$sum)
mydataAgg$`#FFAB80`<-100*(mydataAgg$`#FFAB80`/mydataAgg$sum)
mydataAgg$`#FFFF80`<-100*(mydataAgg$`#FFFF80`/mydataAgg$sum)

mydataAgg<-mydataAgg[, -12]
data <- mydataAgg %>% gather(key = "observation", value="value", -c(1,2))
colnames(data)<-c("Individual","group","observation","value")


#mydataAgg<-subset(mydataAgg, vulength>4)
#mydataAgg <- mydataAgg %>% filter(Country != "Bahamas, The") %>% droplevels ## drop intercept
data <- data %>% filter(Individual != "Greenland") %>% droplevels ## drop intercept
#data <- data %>% filter(Individual != "Iceland") %>% droplevels ## drop intercept
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
#data$percent <- 100*((data$vSus-data$vHigh)/data$vHigh)
#data$p.ssp5 <- 100*((data$vHigh-data$vBase)/data$vBase)
data <- data %>% arrange(group, Individual)
summary(factor(data$Individual))
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, Individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
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

#data$observation<-car::recode(data$observation,"'black'=NA")
#data$observation <- factor(data$observation, levels = c("NA", "#555580", "#55FF80", "#FF5580", "#FFFF80"))
# Make the plot
p <- ggplot(subset(data)) +      
  
  # Add the stacked bar
  geom_bar(aes(x=(as.factor(id)), y=value, fill=observation), stat="identity") +
  #geom_bar(aes(x=(as.factor(id)), y=p.ssp5, fill="red"), stat="identity")+
  scale_fill_identity() +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  #geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),3), y = c(0, 50, 100), label = c("0%", "50%","100%") , color="black", size=4, angle=0, fontface="bold", hjust=1) +
  
  scale_y_continuous( limits = c(-100, max(label_data$tot, na.rm=T)+50)) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(1,-1,-1,-1), "cm") ## margin(t, r, l, b) 
  ) +
  coord_polar() +
  #scale_y_reverse()+
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=(id), y=max(label_data$tot, na.rm = T)+5, label=Individual, hjust=hjust), color="black", size=4, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  #geom_segment(data=base_data, aes(x = start, y = 50, xend = end, yend = 50), colour = "grey90", alpha=0.3, lty=2,size=0.3, inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -15, label=group),colour = "black", size=5, fontface="bold", inherit.aes = FALSE)
p
ggsave("Fig4b.tiff", dpi = 600, height = 11, width = 10)



