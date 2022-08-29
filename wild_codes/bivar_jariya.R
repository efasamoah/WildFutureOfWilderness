rm(list = ls())
library(raster)
library(ncdf4)
library(sf)
library(rgdal)
mainDir = "D:/R - HoMe/3_PNAS-WILDERNESS/WLD_35YR-ENV2021/"
setwd(mainDir)

rprim <-brick(paste0(mainDir,"data/states.nc"), varname ="primf")
rsec <- brick(paste0(mainDir,"data/states.nc"), varname ="secdf")

#r2000 <-calc(stack(rprim[[1151]], rsec[[1151]]),max); names(r2000) <- "forest.2000"
r2001 <-calc(stack(rprim[[1152]], rsec[[1152]]),max); names(r2001) <- "forest.2001"
r2002 <-calc(stack(rprim[[1153]], rsec[[1153]]),max); names(r2002) <- "forest.2002"
r2003 <-calc(stack(rprim[[1154]], rsec[[1154]]),max); names(r2003) <- "forest.2003"
r2004 <-calc(stack(rprim[[1155]], rsec[[1155]]),max); names(r2004) <- "forest.2004"
r2005 <-calc(stack(rprim[[1156]], rsec[[1156]]),max); names(r2005) <- "forest.2005"
r2006 <-calc(stack(rprim[[1157]], rsec[[1157]]),max); names(r2006) <- "forest.2006"
r2007 <-calc(stack(rprim[[1158]], rsec[[1158]]),max); names(r2007) <- "forest.2007"
r2008 <-calc(stack(rprim[[1159]], rsec[[1159]]),max); names(r2008) <- "forest.2008"
r2009 <-calc(stack(rprim[[1160]], rsec[[1160]]),max); names(r2009) <- "forest.2009"
r2010 <-calc(stack(rprim[[1161]], rsec[[1161]]),max); names(r2010) <- "forest.2010"
r2011 <-calc(stack(rprim[[1162]], rsec[[1162]]),max); names(r2011) <- "forest.2011"
r2012 <-calc(stack(rprim[[1163]], rsec[[1163]]),max); names(r2012) <- "forest.2012"
r2013 <-calc(stack(rprim[[1164]], rsec[[1164]]),max); names(r2013) <- "forest.2013"
r2014 <-calc(stack(rprim[[1165]], rsec[[1165]]),max); names(r2014) <- "forest.2014"
r2015 <-calc(stack(rprim[[1166]], rsec[[1166]]),max); names(r2015) <- "forest.2015"

luc15yr <- stack(r2001, r2002, r2003, r2004, r2005, r2006, r2007, r2008, r2009, r2010, 
                 r2011, r2012, r2013, r2014, r2015)
#Reproject stack to Mollweide
mollProj<- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"
luh15yr_prj <- projectRaster(luc15yr, crs = mollProj, res = 25000, method = "bilinear")
plot(luh15yr_prj[[1:4]])

#Extract land-use information to the Planing Unit (100km)
EcoGridInd <- st_read("C:/Users/45019738/Dropbox/SPP_PROJ/gridindex_mollweide/gridindex_mollweide.shp")
luh15yr_joined <- raster::extract(luh15yr_prj, EcoGridInd, fun=sum, df=TRUE, sp=TRUE)

#Determine which grid are forested
ForestedGrid <- luh15yr_joined %>% data.frame()%>%
  mutate(isThereForestIn20001 = ifelse(forest.2001>0, 1,0))%>%
  select(PageNumber, isThereForestIn20001)

library(dplyr)
library(tidyr)
dataNest <- luh15yr_joined %>% as.data.frame() %>% na.omit() %>% 
  select(-starts_with("output")) %>%          
  pivot_longer(forest.2001:forest.2015) %>%
  separate(name, into = c("var", "time")) %>%  
  pivot_wider(names_from=var, values_from=value) %>%
  nest(data=-PageNumber) %>%                             
  mutate(reg = purrr::map(data, ~lm(forest~time, .))) %>%   # do the regression
  mutate(slope = purrr::map_dbl(reg, ~coefficients(.)[2]))

hist(dataNest$slope, breaks=100)

#Join linear slope back to the PUs 
ecogrid_slope <- ecogrid_join %>% 
  mutate(PageNumber = as.numeric(as.character(PageNumber)))%>% 
  inner_join(y = dataNest[, c("slope", "PageNumber")], by="PageNumber")

#import bivariate codes
source("./ColMatrix.R") 
# Define the number of breaks
nBreaks <- 3
# Create the colour matrix
col.matVul <- colmat(nbreaks = nBreaks, breakstyle = "quantile",
                     xlab = "X", ylab = "Y",
                     upperleft = "dodgerblue4", 
                     upperright = "darkred", 
                     bottomleft =  "#BEBEBE", 
                     bottomright = "yellow",
                     saveLeg = FALSE, plotLeg = TRUE)

# Retrieve bivariate colour pallet data
lgd <- BivLegend$data; names(lgd) <- c("binY", "binX", "BivCol", "UID")

ecogrid_slope <- ecogrid_slope %>%
  inner_join(y = ForestedGrid, by = "PageNumber") %>%
  filter(isThereForestIn20001==1) %>%
  mutate(binY = ntile(Completeness, 3), 
         binX = ntile(-1*slope, 3),
         #binX = ifelse(slope>0,3,1)
         )%>%
  inner_join(y = lgd, by = c("binY", "binX"))
class(ecogrid_slope)


bbox <- rnaturalearth::ne_download(scale=50, type="wgs84_bounding_box", category="physical",returnclass = "sf")%>%st_transform(crs = "+proj=moll")
land <- rnaturalearth::ne_download(scale=10, type="land", category="physical",returnclass = "sf")%>%st_transform(crs = "+proj=moll")
(Fig1 <- ggplot()+ 
  geom_sf(data = land, fill = "grey50", size=NA) +
  geom_sf(data = bbox, fill=NA, size=0.2, colour = "grey50") +
  geom_sf(data = ecogrid_slope, aes(fill=BivCol), lwd=NA)+
  scale_fill_identity()+theme_void())

#Generate data for legend
(legCI <- ecogrid_slope %>%
    group_by(binY, binX, BivCol) %>%
    summarise(N = n()) %>% 
    ungroup() %>%
    mutate(propCells = round(100*(N/sum(N)), 1),
           propCells = paste0(propCells, "%")) %>%
    ggplot(aes(x = binX, y = binY, fill = BivCol)) +
    geom_raster() + scale_fill_identity() +
    geom_text(aes(label=propCells),size = 1.5)+
    theme_minimal(base_size = 4)+
    scale_x_discrete(expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.title.y = element_text(angle = 90))+
    labs(x = "Degree of Forest Change" ~symbol("\256"), 
         y = "Tree Species Completeness" ~symbol("\256"))+
    theme(legend.position = "none"))

(FigComp <- Fig1 +
    annotation_custom(ggplotGrob(legCI), 
                      ymin=-.5e6, 
                      ymax = -8e6, 
                      xmin = -17e6, 
                      xmax = -8.5e6))

ggsave(here::here("./FigOpporunties.tiff"), dpi = 600, width = 5.5, height = 3)



