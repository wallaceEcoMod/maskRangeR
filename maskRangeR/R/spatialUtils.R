#==================================================================================
#==================================================================================
#' @title Line two rasters or stacks or lists of rasters
#'
#' @description Obtain the same extents and resample to the finest resolution layer.
#' @details
#' See Examples.
#' @param expertMap A binary map, either as a polygon or a raster.
#' @param maskListRaw A list of rasters, each corresponding to layers from which masks will eventually be made (in another function).
# @examples
#'
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


cropResampleTrim=function(expertMap,maskListRaw){
  # reduce the size of everything as much as possible before resampling to save time
  crops=lapply(maskListRaw,function(x,expertMap){
    raster::crop(x,expertMap)
  },expertMap=expertMap)
  # find finest res 
  ress=lapply(crops,raster::res)
  minRes=which.min(do.call(rbind,ress)[,1])
  # resample everything ot finest res
  print('Resampling, which can be a little slow...')
  resamples=lapply(crops,raster::resample,y=crops[[minRes]])
  # set up the expert map on the same grid
  expertRaster=NULL
  if(any(class(expertMap)==c("SpatialPolygons","SpatialPolygonsDataFrame"))){
    expertRaster=raster::rasterize(expertMap,crops[[minRes]])
  }
  if(any(class(expertMap)==c("raster","stack"))){
    expertRaster=raster::resample(expertMap,crops[[minRes]])
  }
  if(is.null(expertRaster)) stop('expertMap must be an object of one of the following
                                 classes: SpatialPolygons, SpatialPolygonsDataFrame,
                                 raster, stack')
  # just in case there's junk
  expertRaster=raster::trim(expertRaster)
  # final crop to truly align
  finalCrops=lapply(resamples,function(x,expertRaster){
    raster::crop(x,expertRaster)
  },expertRaster=expertRaster)
  
  maskStack=raster::stack(finalCrops)
  return(list(expertRaster=expertRaster,maskStack=maskStack))
}