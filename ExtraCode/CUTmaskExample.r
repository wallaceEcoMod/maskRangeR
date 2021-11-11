#' \donttest{
#' # Here's a more realistic example
#' op=par()
#' ########## Forest Cover Mask 
#' ###  Initiating data for use case # 1
#' ##Initiate empty raster object with extent and resolution 
#' r1 <- raster::raster(raster::extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
#' # Generate random occurrence points
#' datedOccs <- data.frame(dismo::randomPoints(r1, 5))
#' # Pair dates (as years) with the occurrences
#' datedOccs$date <- 2010:2014
#' # convert dates to formal date objects
#' datedOccs$date <- lubridate::parse_date_time(datedOccs$date, orders = c("Y", "Ym"))
#' # Create values showing distance from random points to simulate suitability surface
#' sdm <- raster::distanceFromPoints(r1, datedOccs[,1:2])
#' # Create 5 different rasters as env data
#' env1 <- raster::distanceFromPoints(r1, data.frame(dismo::randomPoints(sdm, 3)))
#' env2 <- raster::distanceFromPoints(r1, data.frame(dismo::randomPoints(sdm, 3)))
#' env3 <- raster::distanceFromPoints(r1, data.frame(dismo::randomPoints(sdm, 3)))
#' env4 <- raster::distanceFromPoints(r1, data.frame(dismo::randomPoints(sdm, 3)))
#' env5 <- raster::distanceFromPoints(r1, data.frame(dismo::randomPoints(sdm, 3)))
#' env <- raster::stack(env1, env2, env3, env4, env5)
#' # Declare the date scale
#' dateScale = "year"
#' # Declare dates for simulated env data
#' envDates <- lubridate::parse_date_time(paste0("",2010:2014, ""), orders = c("Y", "Ym"))
#' ###  Preparing data
#' # convert to spatial object
#' sp::coordinates(datedOccs) <- c("x", "y")
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
#' }
#' 