
#==================================================================================
#==================================================================================
#' @title Make a matrix of modeling decisions to be used to specify clipping rules
#'
#' @description to do
#' @details
#' See Examples.
#' @param potentialDist A raster stack of binary or continuous values. Supplying more than one layer will be interepreted as different time periods. Layers should follow the naming convention `Y2000`, `Y2001`, etc.
#' @param maskLayers A single raster or a raster stack. If a single raster, the same mask will be applied to each layer of `potentialDist`. If a stack it must have the same number of layers as potentialDist, and each layer corresponds to a different time period.
#' @param logicString a character indicating the logical conditions to use for masking
# @param rsLower A list of lower bounds of suitable values in the same order as `rsList`
# @param rsUpper A list of upper bounds of suitable values in the same order as `rsList`
#' @param method A list of strings defining methods to be used, in the same order as `rsList`. If a single value is provided it will be applied to all rasters in `rsList`. Options include:
#' \itemize{
#'  \item{mask}{mask cells with values outside the bounds}
#' }
# @examples
#'
#'
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

# would we ever have the case where you'd have suitable < X < Y< suitable?

maskedRangeR=function(potentialDist,
                      maskLayers,
                      logicString,
                      method='mask'){
  #  for testing
  #  potentialDist=stack(spA,spA-.1,spA+.1); maskLayers=stack(spB,spB+.1,spB-.1); logicString='maskLayers>0.5'
  
  
  if(method=='mask'){
    mask.bin=eval(parse(text=logicString))
    if(raster::nlayers(maskLayers)==1){
      out=raster::mask(potentialDist,mask=mask.bin,maskvalue=1)
    } else {
      if(!raster::nlayers(potentialDist)==raster::nlayers(maskLayers)) {
        stop('Error: you must either supply a single layer to mask all layers 
             in potentialDist or ensure that maskLayers has the same number 
             of layers as potentialDist')
      }
      out=raster::stack(lapply(1:raster::nlayers(mask.bin),function(x){
        raster::mask(potentialDist[[x]],mask=mask.bin[[x]],maskvalue=1)
      }))
    }
  } # end if method=='mask'
  
  
  return(out)
}










