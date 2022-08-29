rm(list = ls())
mainDir = "D:/R - HoMe/3_PNAS-WILDERNESS/WLD_35YR-ENV2021/"
setwd(mainDir)
library(raster)
library(ncdf4)

mainDir1 <- list.files(paste0(mainDir, "data/"), pattern='*.nc',full.names=TRUE)
annualTrend = "wild_codes/VoCC/tempTrend.R"
angu <- "wild_codes/VoCC/angulo.R"
sGrad <- "wild_codes/VoCC/spatGrad.R"

gV_wild <- function(path1,yr0,scenario,angulo,tempTrend,spatGrad,varname){
  
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
  
  #After Molinos et al
  #sumSeries <- source(paste0(mainDir, sumS))
  tempTrend <- source(paste0(mainDir, annualTrend))
  angulo <- source(paste0(mainDir, angu))
  spatGrad <- source(paste0(mainDir, sGrad))
 
  # Temporal trend
  r = brick(path1[grep(scenario, path1)], varname = varname)[[yr0]]
  vt <- tempTrend(r, th = 10)
  #vt = (1/35)*log(calc(r[[30:35]], mean)/calc(r[[1:5]], mean))
  
  # spatial gradient
  rr_mean = calc(r, mean)
  vg <- spatGrad(rr_mean, th = 0.001, projected = FALSE)
  
  # climate velocity
  VoCC <- vt[[1]]/vg[[1]]
  # velocity angles have opposite direction to the spatial climatic gradient if warming and same direction (cold to warm) if cooling
  ind <- which(getValues(VoCC) > 0)
  VoCCang <- vg[[2]]
  VoCCang[ind] <- vg[[2]][ind] + 180
  VoCCang[] <- ifelse(VoCCang[] >= 360, VoCCang[] - 360, VoCCang[])
  #output <- stack(VoCC,VoCCang)
  output <- VoCC
  names(output) <- paste(varname, scenario, sep="_")
  return(output)
}

#Historical 
primf_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "primf", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
secdf_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "secdf", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
range_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "range", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3ann_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "c3ann", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c4ann_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "c4ann", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3per_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "c3per", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c4per_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "c4per", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3nfx_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "c3nfx", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
pastr_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "pastr", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
urban_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "urban", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
#primn_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "primn", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
#secdn_base <- gV_wild(path1 = mainDir1, yr0=1131:1165, scenario="hist",varname = "secdn", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)


#SSP5-RCP8.5
primf_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "primf", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
secdf_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "secdf", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
range_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "range", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3ann_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "c3ann", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c4ann_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "c4ann", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3per_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "c3per", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c4per_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "c4per", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3nfx_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "c3nfx", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
pastr_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "pastr", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
urban_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5",varname = "urban", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
#primn_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5", varname = "primn", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
#secdn_ssp5 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp5", varname = "secdn", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)

#SSP1-RCP2.6
primf_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "primf", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
secdf_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "secdf", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
range_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "range", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3ann_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "c3ann", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c4ann_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "c4ann", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3per_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "c3per", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c4per_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "c4per", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
c3nfx_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "c3nfx", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
pastr_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "pastr", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
urban_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "urban", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
#primn_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "primn", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)
#secdn_ssp1 <- gV_wild(path1 = mainDir1, yr0=1:35, scenario="ssp1",varname = "secdn", angulo = angu, tempTrend = annualTrend, spatGrad = sGrad)

multi_data <- stack(primf_base,secdf_base,range_base,c3ann_base,c4ann_base,c3per_base,c4per_base,c3nfx_base,pastr_base,urban_base,
                    primf_ssp1,secdf_ssp1,range_ssp1,c3ann_ssp1,c4ann_ssp1,c3per_ssp1,c4per_ssp1,c3nfx_ssp1,pastr_ssp1,urban_ssp1,
                    primf_ssp5,secdf_ssp5,range_ssp5,c3ann_ssp5,c4ann_ssp5,c3per_ssp5,c4per_ssp5,c3nfx_ssp5,pastr_ssp5,urban_ssp5)
writeRaster(multi_data, filename=paste0("data/wild_ms_database/landuseNew/", names(multi_data), ".tif"), bylayer=TRUE, format="GTiff", overwrite=TRUE)
