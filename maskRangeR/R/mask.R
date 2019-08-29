
#=============================================================================
#=============================================================================
#' @title Make a matrix of modeling decisions to be used to specify clipping rules
#'
#' @description Performs data driven masking of potential species distributions.
#' @details
#' See Examples.
#' @param potentialDist A raster stack of binary or continuous values. Supplying more than one layer will be interepreted as different time periods. Layers should follow the naming convention `Y2000`, `Y2001`, etc.
#' @param maskLayers A single raster or a raster stack. If a single raster, the same mask will be applied to each layer of `potentialDist`. If a stack it must have the same number of layers as potentialDist, and each layer corresponds to a different time period.
#' @param logicString a character indicating the logical conditions to use for masking.
# @param rsLower A list of lower bounds of suitable values in the same order as `rsList`
# @param rsUpper A list of upper bounds of suitable values in the same order as `rsList`
#' @param method A list of strings defining methods to be used, in the same order as `rsList`. 
#' If a single value is provided it will be applied to all rasters in `rsList`. Options include:
#' \itemize{
#'  \item{mask}{mask cells with values outside the bounds}
#' }
#' @examples
#' \donttest{
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
#' datedOccs$date <- parse_date_time(datedOccs$date, orders = c("Y", "Ym"))
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
#' 
#' ##########  Multiple Expert Maps
#' ## Generate random polygon
#' coords <- dismo::randomPoints(sdm, 3)
#' polyg <- sp::Polygon(coords)
#' polyg <- sp::SpatialPolygons(list(sp::Polygons(list(polyg), ID = "a")))
#' expertRaster <- raster::rasterize(polyg, r1)
#' maskStack <- stack(env1, env2, env3)
#' names(maskStack) <- c("env1", "env2", "env3")
#' # Get list of tolerances for environmental data
#' env1Vals <- quantile(values(env1), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = T)
#' env2Vals <- quantile(values(env2), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = T)
#' env3Vals <- quantile(values(env3), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = T)
#' maskBounds <- data.frame(rbind(cbind(env1Vals[[3]], env1Vals[[5]]), 
#'                          cbind(env2Vals[[3]], env2Vals[[5]]),
#'                          cbind(env3Vals[[3]], env3Vals[[5]])))
#' maskBounds <- cbind(names(maskStack), maskBounds)
#' colnames(maskBounds) <- c("Layer", "min", "max")
#' # mask range by these tolerance masks
#' realized <- lotsOfMasks(expertRaster, maskStack, maskBounds)
#' plot(stack(realized))
#' plot(realized$realizedDist)
#' plot(polyg, add = T)
#' par(op)
#' }
# @return
#' @author Cory Merow <cory.merow@@gmail.com>,
#' @note To apply multiple masks, e.g., elevation and forest cover, use separate calls to maskRS.
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

maskRanger=function(initialDist,
                    maskLayers,
                    logicString,
                    method='mask'){
  #  for testing
  #  potentialDist=stack(spA,spA-.1,spA+.1); maskLayers=stack(spB,spB+.1,spB-.1); logicString='maskLayers>0.5'
  
  
  if(method=='mask'){
    mask.bin=eval(parse(text=logicString))
    if(raster::nlayers(maskLayers)==1){
      out=raster::mask(initialDist,mask=mask.bin,maskvalue=0)
    } else {
      if(!raster::nlayers(potentialDist)==raster::nlayers(maskLayers)) {
        stop('Error: you must either supply a single layer to mask all layers 
             in potentialDist or ensure that maskLayers has the same number 
             of layers as potentialDist')
      }
      out=raster::stack(lapply(1:raster::nlayers(mask.bin),function(x){
        raster::mask(initiallDist[[x]],mask=mask.bin[[x]],maskvalue=1)
      }))
    }
  } # end if method=='mask'
  
  out=raster::stack(out,initialDist,mask.bin)
  names(out)=c('refinedDist','initialDist',paste0(names(maskLayers),'Mask'))
  return(out)
}


#============================================================================
#============================================================================
#' @title Generate and apply multiple masks to a map
#'
#' @description Based on a potential distribution, environmental rasters, and bounds for suitable habitat on the environmental rasters
#' @details
#' See Examples.
#' @param expertRaster The binary expert map (1s and 0s), rasterized with the same projection as `maskStack`
#' @param maskStack A stack of *named* layers from which masks will be made
#' @param maskBounds A data.frame with columns indicating the layer name (matching the names in maskStack), and the min and max values of that layer to be used for masking.
# @examples
#'
# @return
#' @author Cory Merow <cory.merow@@gmail.com>,
# @note 
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

lotsOfMasks=function(expertRaster,maskStack,maskBounds){
  if(!any(class(expertRaster)==c('RasterLayer','RasterStack'))) stop('The expertRaster must be a raster')
  # make all the masks 
  binaryMasks=raster::stack(lapply(1:raster::nlayers(maskStack),function(x,maskBounds){
    keep=which(maskBounds[,1]==names(maskStack[[x]]))
    if(length(keep)==0) stop(paste0('The names in maskBounds may not match names(maskStack). Check ', names(x)))
    maskBin=(maskStack[[x]]>maskBounds[keep,2] & 
            maskStack[[x]]<maskBounds[keep,3])
    maskBin[!maskBin==1]=NA
    maskBin
  },maskBounds=maskBounds))
  names(binaryMasks)=names(maskStack)
  realizedDist=expertRaster
  for(i in 1:raster::nlayers(binaryMasks)){
    realizedDist=raster::mask(realizedDist,binaryMasks[[i]])
  }
  out=stack(realizedDist,expertRaster,binaryMasks)
  names(out)=c('refinedDist','initialDist',paste0(names(binaryMasks),'Mask'))
  return(out)
}








