#=============================================================================
#=============================================================================
#' @title Update a binary raster (species range map) to continuous values that describe the proportion of cell that is suitable or the quality of the cell.
#'
#' @description The use case envision is updating a binary map to continuous values that describe the proportion of the cell that is suitable, based on land  use/land cover classes
#' @details
#' See Examples.
#' @param contStack a stack of layers with continuous values.
#' @param suitable a vector of names of suitable layers of `contStack`. These can be substrings of the layer names that can be `grep`ped from `names(contStack)`
#' @param binaryRange a binary raster
#' @param maskValue numeric. The value in `binaryRange` that indicates the unsuitable cell
#' @param ... arguments to be passed to `raster::mask`
#' @return a raster
#' @author Cory Merow <cory.merow@@gmail.com>,
# @note 
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
# For testing
# contStack=stack('/Users/ctg/Dropbox/Projects/MoL/Range_Bagging/Data/LandUse/aim_crop_2015.tif','/Users/ctg/Dropbox/Projects/MoL/Range_Bagging/Data/LandUse/aim_forest_2015.tif', '/Users/ctg/Dropbox/Projects/MoL/Range_Bagging/Data/LandUse/aim_nonforest_2015.tif','/Users/ctg/Dropbox/Projects/MoL/Range_Bagging/Data/LandUse/aim_range_2015.tif','/Users/ctg/Dropbox/Projects/MoL/Range_Bagging/Data/LandUse/aim_urban_2015.tif')
# suitable=c('urban','crop')
# binaryRange=raster('/Users/ctg/Dropbox/Projects/MoL/Range_Bagging/Data/Species/Expert_Rasters_QD/Birds/Abroscopus_superciliaris.tif')
# out=continuousMask(contStack,suitable,binaryRange,maskValue=NA)


continuousMask=function(contStack,suitable,binaryRange,maskValue=NA,...){
  contStack.c=raster::crop(contStack,binaryRange)
  # in case there's a little misalignment, just tweek the binary map.
  if(!raster::extent(contStack.c)==raster::extent(binaryRange)){
    binaryRange=raster::resample(binaryRange,contStack.c,method='ngb')
    message("Maps didn't line up perfectly, so I resampled the binaryRange. Probably has only a little effect at range edges.")
  }
  cont.keep=mapply(function(x){grep(x,names(contStack))},suitable)
  suitable.cont=raster::calc(contStack.c[[cont.keep]],sum,na.rm=T)
  raster::mask(suitable.cont,binaryRange,maskValue=maskValue,...)
}




