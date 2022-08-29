#OCCEAN VELCOTIY - MAINA JM
rm(list = ls())
mainDir = "D:/R - HoMe/3_PNAS-WILDERNESS/WLD_35YR-ENV2021/"
setwd(mainDir)
ang_ulo <- "wild_codes/VoCC/angulo.R"
sGrad <- "wild_codes/VoCC/spatGrad.R"

#Open all files
library(ncdf4)
library(raster)
mainDir1 <- list.files(paste0(mainDir, "data/"), pattern='*.nc',full.names=TRUE)
#varname = "tos"
#r <- do.call("stack", lapply(1:length(mainDir1), function(x){raster::stack(mainDir1[[x]], varname = "tos")}))

mySED_fun <- function(path1, HistVar, PastYrs, FutYrs, varname){
  r_hist = brick(path1[[1]], varname = varname)[[HistVar]]
  r_sd = raster::calc(r_hist, mean)
  
  r_past = brick(path1[[1]], varname = varname)[[PastYrs]];  r_past = raster::calc(r_past, mean)
  r_fut = brick(path1[[3]], varname = varname)[[FutYrs]];  r_fut = raster::calc(r_fut, mean)
  
  myRec<-function(x) {ifelse(x < 0, 0, x)}
  r_change = r_fut-r_past; r_changeN = calc(r_change, myRec)
  output = (r_changeN)^2/((r_sd+1)^2)
  
  names(output) <- paste0("SED_", varname)
  return(output)
}

mySED_fun2 <- function(path1, HistVar, PastYrs, FutYrs, varname){
  r_hist = brick(path1[[1]], varname = varname)[[HistVar]]
  r_sd = raster::calc(r_hist, mean)
  
  r_past = brick(path1[[1]], varname = varname)[[PastYrs]];  r_past = raster::calc(r_past, mean)
  r_fut = brick(path1[[3]], varname = varname)[[FutYrs]];  r_fut = raster::calc(r_fut, mean)
  
  myRec<-function(x) {ifelse(x > 0, 0, x)}
  r_change = r_fut-r_past; r_changeN = calc(r_change, myRec)
  output = (r_changeN)^2/((r_sd+1)^2)
  
  names(output) <- paste0("SED_", varname)
  return(output)
}

SED_VoCC <- function(r, spatGrad){
  
  library("rgeos")
  library("rasterVis")
  library("gridExtra")
  library("doParallel")
  library("foreach")
  library("scales")
  library("data.table")
  library("repmis")
  library("rgdal")
  library("xts")
  library("geosphere")
  
  angulo <- source(paste(mainDir, ang_ulo, sep = ""))
  spatGrad <- source(paste(mainDir, spatGrad, sep = ""))
  
  # Temporal trend
  vt <- r
  
  # spatial gradient
  vg <- spatGrad(r, th = 0.00001, projected = FALSE)
  
  #Velocity
  VoCC <- vt[[1]]/vg[[1]]
  ind <- which(getValues(VoCC) > 0)
  VoCCang <- vg[[2]]
  VoCCang[ind] <- vg[[2]][ind] + 180
  VoCCang[] <- ifelse(VoCCang[] >= 360, VoCCang[] - 360, VoCCang[])
  output <- VoCC
  names(output) <- c("voccMag")
  return(output)
}

#C3ann
lui_c3ann <- mySED_fun(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "c3ann")
sed_c3ann <- SED_VoCC(r = lui_c3ann, spatGrad = sGrad)
#c3per
lui_c3per <- mySED_fun(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "c3per")
sed_c3per <- SED_VoCC(r = lui_c3per, spatGrad = sGrad)

lui_c4ann <- mySED_fun(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "c4ann")
sed_c4ann <- SED_VoCC(r = lui_c4ann, spatGrad = sGrad)

lui_c4per <- mySED_fun(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "c4per")
sed_c4per <- SED_VoCC(r = lui_c4per, spatGrad = sGrad)

lui_c3nfx <- mySED_fun(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "c3nfx")
sed_c3nfx <- SED_VoCC(r = lui_c3nfx, spatGrad = sGrad)

lui_range <- mySED_fun(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "range")
sed_range <- SED_VoCC(r = lui_range, spatGrad = sGrad)

lui_urban <- mySED_fun(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "urban")
sed_urban <- SED_VoCC(r = lui_urban, spatGrad = sGrad)




lui_pastr <- mySED_fun2(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "pastr")
sed_pastr <- SED_VoCC(r = lui_pastr, spatGrad = sGrad)

lui_primf <- mySED_fun2(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "primf")
sed_primf <- SED_VoCC(r = lui_primf, spatGrad = sGrad)

lui_secdf <- mySED_fun2(mainDir1, HistVar = 1097:1131, PastYrs = 1131:1165, FutYrs = 1:35, varname = "secdf")
sed_secdf <- SED_VoCC(r = lui_secdf, spatGrad = sGrad)

MultiInstability <- (1/35)*raster::calc(
  stack(sed_c3ann[[1]],sed_c3per[[1]],sed_c4ann[[1]],sed_c4per[[1]],lui_c3nfx[[1]],sed_range[[1]],
        sed_pastr[[1]],sed_primf[[1]],sed_urban[[1]]),
  sum, na.rm=TRUE)

plot(log(MultiInstability+1))
#gVoCC <- SED_VoCC(r = MultiInstability, spatGrad = sGrad)
#plot(log(1+gVoCC[[1]]))
raster::writeRaster(MultiInstability, "instablity_ssp5_cond.tif", overwrite=TRUE)

df_vocc <- as.data.frame(MultiInstability, xy=TRUE)
summary(df_vocc)
hist(log(df_vocc$layer))

library(ggplot2)
library(dplyr)
df_vocc%>%filter(!is.na(layer), is.finite(layer))%>%
  mutate(layer2=ifelse(layer>1000, 1000, layer))%>%
  ggplot(aes(x,y,fill=layer2+1))+
  scale_fill_viridis_c(trans = "log", na.value = "white")+
  geom_raster()+coord_equal()





p = "2015-01/2100-12" #required subset
yr0 = "2000-01-16" #base year
sumSeries = "VoCC/sumSeries.R"
tempTrend = "VoCC/tempTrend.R"
angulo <- "VoCC/angulo.R"
spatGrad <- "VoCC/spatGrad.R"

gVoCC <- function(r, p, yr0, sumSeries, tempTrend, spatGrad){
  
  library("rgeos")
  library("rasterVis")
  library("gridExtra")
  library("doParallel")
  library("foreach")
  library("scales")
  library("data.table")
  library("repmis")
  library("rgdal")
  library("xts")
  library("geosphere")
  
  sumSeries <- source(paste(mainDir, sumSeries, sep = ""))
  tempTrend <- source(paste(mainDir, tempTrend, sep = ""))
  angulo <- source(paste(mainDir, angulo, sep = ""))
  spatGrad <- source(paste(mainDir, spatGrad, sep = ""))
  
  # Monthly to annual averages
  rr <- sumSeries(r, p = p, yr0 = yr0, l = nlayers(r),fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

  # Temporal trend
  vt <- tempTrend(rr, th = 10)

  # spatial gradient
  rr_mean = calc(rr, mean)
  vg <- spatGrad(rr_mean, th = 0.0001, projected = FALSE)

# climate velocity
  VoCC <- vt[[1]]/vg[[1]]
    # velocity angles have opposite direction to the spatial climatic gradient if warming and same direction (cold to warm) if cooling
    ind <- which(getValues(VoCC) > 0)
    VoCCang <- vg[[2]]
    VoCCang[ind] <- vg[[2]][ind] + 180
    VoCCang[] <- ifelse(VoCCang[] >= 360, VoCCang[] - 360, VoCCang[])
    output <- stack(VoCC,VoCCang)
    names(output) <- c("voccMag", "voccAng")
    return(output)
}
gv_ocean <- gVoCC(r = r, p = p, yr0 = yr0, sumSeries = sumSeries, tempTrend = tempTrend, spatGrad = spatGrad)

# first compare both velocities
my.at <- seq(0, 100, by = 1)
rasterVis::levelplot(gv_ocean[[1]], par.settings = BuRdTheme, at=my.at, main = 'Gradient-based vocc', margin = FALSE)
