
#=============================================================================
#=============================================================================
#' @title Make a matrix of modeling decisions to be used to specify clipping rules
#'
#' @description Performs data driven masking of potential species distributions.
#' @details
#' See Examples.
#' @param initialDist A raster showing a previously created optimally tuned SDM. Must have same extent and resolution as maskLayers.
#' @param potentialDist A raster stack of binary or continuous values. Supplying more than one layer will be interepreted as different time periods. Layers should follow the naming convention `Y2000`, `Y2001`, etc. Must have same extent and resolution as maskLayers.
#' @param maskLayers A single raster or a raster stack. If a single raster, the same mask will be applied to each layer of `potentialDist`. If a stack it must have the same number of layers as potentialDist, and each layer corresponds to a different time period. Must have same extent and resolution as initialDist.
#' @param logicString a character indicating the logical conditions to use for masking.
# @param rsLower A list of lower bounds of suitable values in the same order as `rsList`
# @param rsUpper A list of upper bounds of suitable values in the same order as `rsList`
#' @param method A list of strings defining methods to be used, in the same order as `rsList`. If a single value is provided it will be applied to all rasters in `rsList`. Options currently include only `mask` to mask cells with values outside the bounds.
#' @examples
#' # Multiple Expert Maps
#' # Generate random polygon
#' env1 <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
#' env2 <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
#' env3 <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
#' raster::values(env1)<- sort(runif(n = (108*21)))  
#' raster::values(env2)<- runif(n = (108*21))
#' raster::values(env3)<- runif(n = (108*21))
#' sdm <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
#' raster::values(sdm)<- sort(runif(n = (108*21)))  
#' coords <- dismo::randomPoints(sdm, 3)
#' polyg <- sp::Polygon(coords)
#' polyg <- sp::SpatialPolygons(list(sp::Polygons(list(polyg), ID = "a")))
#' expertRaster <- raster::rasterize(polyg, sdm)
#' maskStack <-raster:: stack(env1, env2, env3)
#' names(maskStack) <- c("env1", "env2", "env3")
#' # Get list of tolerances for environmental data
#' env1Vals <- quantile(raster::values(env1), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), 
#'                      na.rm = TRUE)
#' env2Vals <- quantile(raster::values(env2), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), 
#'                      na.rm = TRUE)
#' env3Vals <- quantile(raster::values(env3), prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), 
#'                      na.rm = TRUE)
#' maskBounds <- data.frame(rbind(cbind(env1Vals[[3]], env1Vals[[5]]), 
#'                                cbind(env2Vals[[3]], env2Vals[[5]]),
#'                                cbind(env3Vals[[3]], env3Vals[[5]])))
#' maskBounds <- cbind(names(maskStack), maskBounds)
#' colnames(maskBounds) <- c("Layer", "min", "max")
#' # mask range by these tolerance masks
#' realized <- lotsOfMasks(expertRaster, maskStack, maskBounds)

#' @return a raster stack
#' @author Cory Merow <cory.merow@@gmail.com>,
#' @note To apply multiple masks, e.g., elevation and forest cover, use separate calls to maskRS.
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

maskRanger=function(potentialDist,
                    initialDist= NULL,
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
        raster::mask(initialDist[[x]],mask=mask.bin[[x]],maskvalue=1)
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
#' @examples
#' r1 <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
#' raster::values(r1)<- sort(runif(n = (108*21)))
#' r1[r1>0.5] <- 1
#' r1[r1<0.5] <- 0
#' r2 <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
#' raster::values(r2) <- runif(n=(108*21))
#' r3 <- raster::raster(nrows=108, ncols=21, xmn=0, xmx=10)
#' raster::values(r3) <- runif(n=(108*21))
#' maskStack <- raster::stack(r2, r3)
#' names(maskStack) <- c("r2", "r3")
#' minbounds <- c(0.3, 0.4)
#' maxbounds <- c(0.4, 0.5)
#' maskBounds <- data.frame(cbind(c("r2", "r3"), minbounds, maxbounds))
#' colnames(maskBounds)<- c("Layer", "Min Value", "Max Value")
#' maskBounds[,2] <- as.numeric(as.character(maskBounds[,2]))
#' maskBounds[,3] <- as.numeric(as.character(maskBounds[,3]))
#' out <- lotsOfMasks(expertRaster = r1, maskStack = maskStack, maskBounds = maskBounds)
#' 
#' 
#' @return a RasterStack
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
  out=raster::stack(realizedDist,expertRaster,binaryMasks)
  names(out)=c('refinedDist','initialDist',paste0(names(binaryMasks),'Mask'))
  return(out)
}








