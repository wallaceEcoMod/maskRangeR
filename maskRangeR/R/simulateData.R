#=============================================================================
#=============================================================================
#' @title Simulate data to run function examples 
#'
#' @description Run function with no arguments to generate example data for all of the use cases found in the vignette.
#' @details 
#' See Examples
#' 
#' @examples 
#' \dontrun{
#' ########## Forest Cover Mask 
#' # Declare the date scale
#' dateScale = "year"
#' # Declare dates for simulated env data
#' envDates <- parse_date_time(paste0("",2010:2014, ""), orders = c("Y", "Ym"))
#' ###  Preparing data
#' # convert to spatial object
#' coordinates(datedOccs) <- c("x", "y")
#' ##  Performing data-driven masking. First find the values of the environment at point locations
#' datedOccs <- annotate(datedOccs, env, envDates, dateScale)
#' # find suitable bounds
#' bounds <- min(datedOccs$env)
#' ## Create mask, and use it on SDM
#' logicString = paste0('maskLayers >', bounds)
#' # use 'most recent' environmental variable as base for mask
#' SDMmask <- env[[5]]
#' names(SDMmask) <- "SDM_mask"
#' maskedDist <- maskRanger(potentialDist = sdm, maskLayers = SDMmask, logicString = logicString)
#' # Notice that the minimum observed value was masked from the 'most recent' environmental raster
#' par(mfrow=c(1,3))
#' plot(SDMmask, main = "Most recent env")
#' plot(maskedDist[['SDM_maskMask']], main = "Mask + SDM")
#' plot(sdm, add=T, col = c(grey(0,0), grey(0.4,0.7)))
#' plot(maskedDist[[1]], col=c(grey(0.6), 'red1'), main = "Masked Distribution")
#' 
#' ##########  Expert Maps
#' sdm[sdm>200000] <- NA
#' sdm[!is.na(sdm)] <- 1
#' expertRast<-rasterize(polyg, sdm)
#' final <- sum(sdm, expertRast, na.rm=T)
#' final[final>1] <- 1
#' par(mfrow=c(1,2))
#' plot(sdm)
#' plot(final)
#' plot(polyg, add=T)
#' 
#' ##########  Multiple Expert Maps
#' expertRaster <- rasterize(polyg, r1)
#' maskStack <- stack(env1, env2, env3)
#' names(maskStack) <- c("env1", "env2", "env3")
#' # Get list of tolerances for environmental data
#' env1Vals <- quantile(values(env1), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = T)
#' env2Vals <- quantile(values(env2), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = T)
#' env3Vals <- quantile(values(env3), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = T)
#' maskBounds <- data.frame(rbind(cbind(env1Vals[[3]], 
#'                                      env1Vals[[5]]), 
#'                                      cbind(env2Vals[[3]], 
#'                                      env2Vals[[5]]), 
#'                                      cbind(env3Vals[[3]], 
#'                                      env3Vals[[5]])))
#' maskBounds <- cbind(names(maskStack), maskBounds)
#' colnames(maskBounds) <- c("Layer", "min", "max")
#' # mask range by these tolerance masks
#' realized <- lotsOfMasks(expertRaster, maskStack, maskBounds)
#' plot(stack(realized))
#' plot(realized$realizedDist)
#' plot(polyg, add = T)
#' 
#' ########## Maksing by biotic interactions
#' # Spatial SVMs
#' svm.SP <- rangeSVM(sp1.xy, sp2.xy, sp3.xy, nrep=10)
#' # Use SVM to create a raster of predicted regions
#' rand_svm.SP <- rangeSVM_predict(svm = svm.SP, r = r1.svm)
#' # Plot the results
#' plot(rand_svm.SP, col=c("yellow","pink","lightblue"))
#' points(sp1.xy, pch = 20, cex = 0.75, col = "orange")
#' points(sp2.xy, pch = 20, cex = 0.75, col = "green")
#' points(sp3.xy, pch = 20, cex = 0.75, col = "blue")
#' # We can use this SVM as a mask over our original SDM predictions.
#' # masked SDM predictions for variegatus
#' sp1_svm.SP <- rand_svm.SP == 1
#' sp1_svm.SP[sp1_svm.SP == 0] <- NA
#' sp1_svmSP_mask <- mask(r1.sdm, sp1_svm.SP)
#' # masked SDM predictions for tridactylus
#' sp2_svm.SP <- rand_svm.SP == 2
#' sp2_svm.SP[sp2_svm.SP == 0] <- NA
#' sp2_svmSP_mask <- mask(r2.sdm, sp2_svm.SP)
#' # masked SDM predictions for torquatus
#' sp3_svm.SP <- rand_svm.SP == 3
#' sp3_svm.SP[sp3_svm.SP == 0] <- NA
#' sp3_svmSP_mask <- mask(r3.sdm, sp3_svm.SP)
#' ##Plot the predicted realized distributions for each species.
#' # Create 3-panel figure
#' par(mfrow = c(1,3))
#' plot(sp1_svmSP_mask)
#' plot(sp2_svmSP_mask)
#' plot(sp3_svmSP_mask)
#' ## Hybrid SVMs
#' # Create SVM
#' svmHYB <- rangeSVM(sp1.xy, sp2.xy, sp3.xy, sdm = raster::stack(r1.sdm, r2.sdm, r3.sdm), nrep = 10)
#' # Use SVM to create a raster of predicted regions
#' rand_svmHYB <- rangeSVM_predict(svm = svmHYB, 
#'                                 r = r1.sdm, 
#'                                 sdm = raster::stack(r1.sdm, r2.sdm, r3.sdm))
#' ## Plot the SVM results.
#' plot(rand_svmHYB, col=c("yellow","pink","lightblue"))
#' points(sp1.xy, pch = 20, cex = 0.75, col = "orange")
#' points(sp2.xy, pch = 20, cex = 0.75, col = "green")
#' points(sp3.xy, pch = 20, cex = 0.75, col = "blue")
#' ## Use the hybrid SVM as a mask over our SDM predictions.
#' # masked SDM predictions for variegatus
#' sp1_svmHYB <- rand_svmHYB == 1
#' sp1_svmHYB[rand_svmHYB == 0] <- NA
#' sp1_svmHYB_mask <- mask(r1.sdm, sp1_svmHYB)
#' # masked SDM predictions for tridactylus
#' sp2_svmHYB <- rand_svmHYB == 2
#' sp2_svmHYB[sp2_svmHYB == 0] <- NA
#' sp2_svmHYB_mask <- mask(r2.sdm, sp2_svmHYB)
#' # masked SDM predictions for torquatus
#' sp3_svmHYB <- rand_svmHYB == 3
#' sp3_svmHYB[sp3_svmHYB == 0] <- NA
#' sp3_svmHYB_mask <- mask(r3.sdm, sp3_svmHYB)
#' ## Plot the predicted realized distributions for each species.
#' # Create 3-panel figure
#' par(mfrow = c(1,3))
#' plot(sp1_svmHYB_mask)
#' plot(sp2_svmHYB_mask)
#' plot(sp3_svmHYB_mask)
#' }
#' @export

#=============================================================================
# FUNCTION TO CREATE ALL SIMULATED DATA AND EXPORT A LIST USABLE FOR EACH EXAMPLE IN USECASE
simulateData <- function(){
  options(warn=-1)
    ###  Initiating data for use case # 1
    # Initiate empty raster object with extent and resolution 
    r1 <<- raster(extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
    # Generate random occurrence points
    datedOccs<<-data.frame(randomPoints(r1, 5))
    # Pair dates (as years) with the occurrences
    datedOccs$date <<- 2010:2014
    # convert dates to formal date objects
    datedOccs$date <<- parse_date_time(datedOccs$date, orders = c("Y", "Ym"))
    # Create values showing distance from random points to simulate suitability surface
    sdm <<- distanceFromPoints(r1, datedOccs[,1:2])
    # Create 5 different rasters as env data
    env1 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
    env2 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
    env3 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
    env4 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
    env5 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
    env <<- stack(env1, env2, env3, env4, env5)
    ### Initiating data for use case # 2
    ## Generate random polygon
    coords <- randomPoints(sdm, 3)
    polyg <- Polygon(coords)
    polyg <<- SpatialPolygons(list(Polygons(list(polyg), ID = "a")))
    ### Initiating data for use case # 3
    ## not needed
    ### Initiating data for use case # 4
    ## Generate some species occurrence records
    r1.sdm <<- raster(extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
    values(r1.sdm) <<- (1:ncell(r1.sdm))^2
    r2.sdm <<- raster(extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
    values(r2.sdm) <<- (ncell(r2.sdm):1)^2
    r3.sdm <<- raster(extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
    r3.sdm [1] <<- 10
    r3.sdm <<- raster::distance(r3.sdm)
    sp1.xy <<- data.frame(randomPoints(r1.sdm, 15, prob = T)); colnames(sp1.xy) <<- c("longitude", "latitude")
    sp2.xy <<- data.frame(randomPoints(r2.sdm, 15, prob = T)); colnames(sp2.xy) <<- c("longitude", "latitude")
    sp3.xy <<- data.frame(randomPoints(r3.sdm, 15, prob = T)); colnames(sp3.xy) <<- c("longitude", "latitude")
  }
#system.time({simulateData()})