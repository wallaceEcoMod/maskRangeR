#=============================================================================
#=============================================================================
#' @title Generate layers based on different focal windows
#'
#' @description Aids in exploring how different focal regions may affect masks. 
#' @details
#' See Examples.
#' @param layer A single raster layer to be summarized.
#' @param windowSizes A vector of the number of cell in each direction to buffer for the focal summary. E.g., a value of 1 indicates the 8 cells immediately surrounding the focal cell, i.e., which are 1 cell away.
#' @param fun The function fun should take multiple numbers, and return a single number. For example mean, modal, min or max. It should also accept a na.rm argument (or ignore it, e.g. as one of the 'dots' arguments. For example, length will fail, but function(x, ...){na.omit(length(x))} works. (Specifications from `raster::focal`)
#' @param mc.cores Number of cores for (optional) parallelization

#' @examples
#' r <- raster::raster(ncols=36, nrows=18, xmn=0)
#' r[] <- runif(raster::ncell(r)) 
#' r15 <- focalCompare(r, windowSizes = c(1:5),mc.cores=1,fun=mean) 
#' @return Raster object
#' @author Cory Merow <cory.merow@@gmail.com>,
#' @note This may be particularly useful if for mobile species when their movement patterns cover a much larger extent than the single cell in which they were observed. 
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export


focalCompare=function(layer,windowSizes,fun,mc.cores=1){
  #  for testing
  #  layer=r; windowSizes = c(1:5); mc.cores=1; fun=mean
  intfunc=function(i){
    window=matrix(1,1+2*i,1+2*i)
    raster::focal(layer,window,fun)
  }
  if(mc.cores>1) {
    out=parallel::mclapply(windowSizes,function(x) intfunc(x),mc.cores=mc.cores)
  } else { out=lapply(windowSizes,function(x) intfunc(x))}
  out1=raster::stack(out)
  names(out1)=paste0(names(layer),'window_',windowSizes)
  out1
}


