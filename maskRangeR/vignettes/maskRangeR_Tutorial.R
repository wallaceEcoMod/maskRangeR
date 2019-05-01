## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = F)
knitr::opts_chunk$set(eval = T)
mc.cores=1

## ---- include=F----------------------------------------------------------
path = "/home/pgalante/Projects/NASA/maskrangeR/UseCaseData"
if(Sys.info()['user']=='ctg') path ='/Users/ctg/Dropbox/Projects/Wallace/MymaskRangeR/demoData'

## ------------------------------------------------------------------------
library(raster)
library(maskRangeR)
library(lubridate)
## Remember that the 'path' is the directory in which you saved the demonstration data 
dataDir = paste0(path, '/dataDriven/olinguito/') 

## ------------------------------------------------------------------------
dateScale <- "year" # or "month", "day"
env <- stack(list.files(path = paste0(dataDir), pattern = "olinguito_Modis.tif$", full.names = T))
# this should be a formal date object of class "POSIXct" "POSIXt"
envDates <- parse_date_time((c('2005','2006','2008','2009','2010')),orders = c("Y", "Ym"))
datedOccs <- read.csv(paste0(dataDir,'All_new_records_by_year.csv'))
sdm <- raster(paste0(dataDir,'olinguito_SDM.tif'))
# crop climate data to study region
env <- crop(env, extent(sdm))

## ------------------------------------------------------------------------
# convert dates to formal date objects
datedOccs$date <- parse_date_time(datedOccs$date,orders = c("Y", "Ym"))
# convert to spatial object
coordinates(datedOccs) <- c('long','lat')
projection(datedOccs) <- projection(env)

## ------------------------------------------------------------------------
datedOccs <- maskRangeR::annotate(datedOccs,env,envDates,dateScale)
# Note the warnings:
# [1] "Environmental layers were missing for date 2013"
# [1] "Environmental layers were missing for date 2016"
# [1] "Environmental layers were missing for date 2015"
# These indicate that you have occurrencee records that do not match some environmental data.
# You may want to look into this, but it is not an issue for using the package.

## ------------------------------------------------------------------------
(bounds <- quantile(datedOccs$env,prob=c(0,.025,.25,.5,.75,.975,1),na.rm=T))

## ------------------------------------------------------------------------
logicString <- paste0('maskLayers>',quantile(datedOccs$env,prob=.25,na.rm=T))
forest <- env[[5]]
names(forest) <- 'forest'
maskedDist <- maskRangeR::maskRanger(potentialDist=sdm,maskLayers=forest,logicString=logicString)

## ------------------------------------------------------------------------
(nOccupiedCells <- apply(values(maskedDist[[1:2]]),2,sum,na.rm=T))

## ----fig=T,fig.height=6,fig.width=7--------------------------------------
par(mfrow = c(1,3))
plot(forest,main = 'Raw Forest Data')
plot(maskedDist[['forestMask']],main = 'Forest Mask + Potential Distribution')
plot(sdm,add = T,col = c(grey(0,0),grey(.4,.7)))
plot(maskedDist[[1]],col = c(grey(.6),'red1'),main = 'Masked Distribution')

## ----results='hide'------------------------------------------------------
library(devtools)
library(maskRangeR)
library(lubridate)
library(raster)
library(rgdal)
dataDir = paste0(path, '/dataDriven/Alouatta/')

## ------------------------------------------------------------------------
original = stack(paste0(dataDir, 'Alouatta_seniculus_0_mx.tif'))
expertAddedPoly = rgdal::readOGR(paste0(dataDir, 'Alouatta_seniculus_add_HO.shp'))

## ----fig.height=6,fig.width=7--------------------------------------------
par(mfrow = c(1,1))
plot(original, main = "Range map")

## ------------------------------------------------------------------------
expertAddRaster <- rasterize(expertAddedPoly,original, 1)
expertAddRaster[is.na(expertAddRaster)] <- 0
final = original+expertAddRaster
# remove overlap
final[final>1] <- 1

## ----fig.height=6,fig.width=7--------------------------------------------
plot(final, main = "Range map with \n expert area added")
plot(expertAddedPoly,add = T)

## ------------------------------------------------------------------------
cellStats(original,sum)
cellStats(final,sum)

## ----results='hide'------------------------------------------------------
library(maskRangeR)
library(raster)
library(rgdal)
dataDir = paste0(path, '/expertDriven/swampForestCrab/')

## ------------------------------------------------------------------------
expertMap <- readOGR(paste0(dataDir,'IUCNshape/data_0.shp'))

#CM: pH and Water bounds are mutually exclusive, so we can only choose one, otherwise there is no suitable habitat. as I understand it, the soil grids shouldn't be taken too seriously at fine grain
#masks
maskListRaw <- list(treeCover = raster(paste0(dataDir,'/Hansen_percent_treecover.tif')), 
                    dem = raster(paste0(dataDir,'/DEM.tif')),
                    mat = raster(paste0(dataDir,'/MATsingapore.tif')),
                    pH = raster(paste0(dataDir,'/PHIHOX_M_sl1_250m.tif')))

# define the limits for each mask
maskBounds <- read.csv(paste0(dataDir,'/crabInfo.csv'))

## ------------------------------------------------------------------------
# rename the limits to match the raster names in maskListRaw
maskBounds$Layer <- c('water','treeCover','dem','pH','mat')
# make sure the units match between the layers and the bounds
maskListRaw$mat <- maskListRaw$mat/10
maskListRaw$pH <- maskListRaw$pH/10

## ------------------------------------------------------------------------
crt <- maskRangeR::cropResampleTrim(expertMap,maskListRaw)

## ------------------------------------------------------------------------
expertRaster <- crt$expertRaster
maskStack <- crt$maskStack
realized <- maskRangeR::lotsOfMasks(expertRaster,maskStack,maskBounds)

## ----fig=T,fig.height=6,fig.width=7--------------------------------------
plot(stack(realized))

## ----fig=T,fig.height=6,fig.width=7--------------------------------------
real <- raster::trim(realized$realizedDist)
plot(real,col = c('white','red'))
plot(expertMap,add = T)

## ------------------------------------------------------------------------
cellStats(expertRaster,sum)
cellStats(real,sum)

## ------------------------------------------------------------------------
library(maskRangeR)
library(raster)
library(maptools)

## ------------------------------------------------------------------------
dataDir = paste0(path, '/dataDriven/bradypus/')

# Species occurrence coordinates
variegatus <- read.csv(paste0(dataDir,'variegatus_occ.csv'))
tridactylus <- read.csv(paste0(dataDir,'tridactylus_occ.csv'))
torquatus <- read.csv(paste0(dataDir,'torquatus_occ.csv'))

# SDMs projected to the same extent
var_sdm <- raster(paste0(dataDir,'variegatus_sdm.tif'))
tri_sdm <- raster(paste0(dataDir,'tridactylus_sdm.tif'))
tor_sdm <- raster(paste0(dataDir,'torquatus_sdm.tif'))

# Crop tridactyus SDM to a 2-degree bounding box
ext_tri <- extent(c(min(tridactylus$longitude)-2, max(tridactylus$longitude)+2, min(tridactylus$latitude)-2, max(tridactylus$latitude)+2))
tri_sdm_mask <- mask(tri_sdm, as(ext_tri, "SpatialPolygons"))

# Crop torquatus SDM to a 2-degree bounding box
ext_tor <- extent(c(min(torquatus$longitude)-2, max(torquatus$longitude)+2, min(torquatus$latitude)-2, max(torquatus$latitude)+2))
tor_sdm_mask <- mask(tor_sdm, as(ext_tor, "SpatialPolygons"))

## ----fig.height=6,fig.width=7--------------------------------------------
# Get world map
data("wrld_simpl")
# Create list of South American countries for visualization
country_list <- c("Honduras", "Nicaragua", "Costa Rica", "Panama", "Colombia",
                  "Ecuador", "Peru", "Bolivia", "Chile", "Argentina", "Uruguay",
                  "Paraguay", "Brazil", "French Guiana", "Suriname", "Guyana",
                  "Venezuela")

# Create 3-panel figure
par(mfrow = c(1,3))

# Plot B. variegatus SDM
plot(var_sdm, xlim = c(-90, -30), ylim = c(-27, 16))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)
# Plot B. tridactylus SDM
plot(tri_sdm_mask, xlim = c(-70, -49), ylim = c(-6, 11))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)
# Plot B. torquatus SDM
plot(tor_sdm_mask, xlim = c(-46, -34), ylim = c(-24, -8))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)

## ---- message=FALSE------------------------------------------------------
set.seed(1234) # to reproduce our exact results
# Create SVM
svmSP <- rangeSVM(variegatus[,2:3], tridactylus[,2:3], 
                  torquatus[,2:3], nrep = 8, mc.cores=mc.cores)

# Use SVM to create a raster of predicted regions
sloth_svmSP <- rangeSVM_predict(svm = svmSP, r = var_sdm)

## ----fig.height=6,fig.width=7--------------------------------------------
plot(sloth_svmSP, col=c("yellow","pink","lightblue"))
points(variegatus[,2:3], pch = 20, cex = 0.75, col = "orange")
points(tridactylus[,2:3], pch = 20, cex = 0.75, col = "red")
points(torquatus[,2:3], pch = 20, cex = 0.75, col = "blue")

## ------------------------------------------------------------------------
# masked SDM predictions for variegatus
var_svmSP <- sloth_svmSP == 1
var_svmSP[var_svmSP == 0] <- NA
var_svmSP_mask <- mask(var_sdm, var_svmSP)

# masked SDM predictions for tridactylus
tri_svmSP <- sloth_svmSP == 2
tri_svmSP[tri_svmSP == 0] <- NA
tri_svmSP_mask <- mask(tri_sdm_mask, tri_svmSP)

# masked SDM predictions for torquatus
tor_svmSP <- sloth_svmSP == 3
tor_svmSP[tor_svmSP == 0] <- NA
tor_svmSP_mask <- mask(tor_sdm_mask, tor_svmSP)

## ----fig.height=6,fig.width=7--------------------------------------------
# Create 3-panel figure
par(mfrow = c(1,3))

plot(var_svmSP_mask, xlim = c(-90, -30), ylim = c(-27, 16))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)
plot(tri_svmSP_mask, xlim = c(-70, -49), ylim = c(-6, 11))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)
plot(tor_svmSP_mask, xlim = c(-46, -34), ylim = c(-24, -8))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)

## ---- message = FALSE----------------------------------------------------
set.seed(1234)
# Create SVM: we used a few more reps here to be sure a set of optimal parameters was found
svmHYB <- rangeSVM(variegatus[,2:3], tridactylus[,2:3], torquatus[,2:3], 
                   sdm = raster::stack(var_sdm, tri_sdm, tor_sdm), 
                   nrep = 14, mc.cores=mc.cores)

# Use SVM to create a raster of predicted regions
sloth_svmHYB <- rangeSVM_predict(svm = svmHYB, r = var_sdm, 
                                 sdm = raster::stack(var_sdm, tri_sdm, tor_sdm))

## ----fig.height=6,fig.width=7--------------------------------------------
plot(sloth_svmHYB, col=c("yellow","pink","lightblue"))
points(variegatus[,2:3], pch = 20, cex = 0.75, col = "orange")
points(tridactylus[,2:3], pch = 20, cex = 0.75, col = "red")
points(torquatus[,2:3], pch = 20, cex = 0.75, col = "blue")

## ------------------------------------------------------------------------
# masked SDM predictions for variegatus
var_svmHYB <- sloth_svmHYB == 1
var_svmHYB[var_svmHYB == 0] <- NA
var_svmHYB_mask <- mask(var_sdm, var_svmHYB)

# masked SDM predictions for tridactylus
tri_svmHYB <- sloth_svmHYB == 2
tri_svmHYB[tri_svmHYB == 0] <- NA
tri_svmHYB_mask <- mask(tri_sdm_mask, tri_svmHYB)

# masked SDM predictions for torquatus
tor_svmHYB <- sloth_svmHYB == 3
tor_svmHYB[tor_svmHYB == 0] <- NA
tor_svmHYB_mask <- mask(tor_sdm_mask, tor_svmHYB)

## ----fig.height=6,fig.width=7--------------------------------------------
# Create 3-panel figure
par(mfrow = c(1,3))

plot(var_svmHYB_mask, xlim = c(-90, -30), ylim = c(-27, 16))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)
plot(tri_svmHYB_mask, xlim = c(-70, -49), ylim = c(-6, 11))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)
plot(tor_svmHYB_mask, xlim = c(-46, -34), ylim = c(-24, -8))
plot(wrld_simpl[wrld_simpl$NAME %in% country_list,], add = TRUE)

