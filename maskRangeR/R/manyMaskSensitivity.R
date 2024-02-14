#===========================================================================
#===========================================================================
#' @title Sensitivity testing for masks
#' @author Peter Galante <pgalante@@amnh.org>
#' @export
#' @importFrom grDevices dev.new rainbow
#' @importFrom stats na.omit quantile  setNames
#' @importFrom utils combn 
#' @description Compare how masks of climate tolerances affect predicted area
#' @details
#' See Examples.
#' @param crt A raster stack; the output from the function maskRangeR::cropResampleTrim
#' @param rasProj (optional) a character string: a proj4string showing the projection of the environmental layers. If NULL, areas will be estimated using the raster package.
#' @param maskBounds A data.frame with columns indicating the layer name (matching the names in maskStack), and the min and max values of that layer to be used for masking.
#' @param expertRaster The binary expert map (1s and 0s), rasterized with the same projection as `maskStack`
#' 
#' @examples 
#' #See lotsOfMasks and maskRanger examples
#' 
#' @return returns a data.frame where row names are the environmental layer name combinations, and Area is expressed in square km, unless a projection is supplied
#'  


manyMaskSensitivity <- function(crt, rasProj = NULL, maskBounds, expertRaster){
  ## define function for calculating areas
  calcAreas <- function(r2){
    tapply(raster::area(r2), r2[], sum)
  }
  ### create combinations of mask layers at list of matrices
  masks <- crt$maskStack
  x <- names(masks)
  m <- 1:(raster::nlayers(masks)-1)
  combinos <- lapply(m, function(r){utils::combn(x,r)})
  ### Create a list of names of mask layers as list of lists and subset maskList by each sublist
  combiNamesList <- lapply(combinos, function(x) lapply(apply(x, 2, list), unlist))
  combiRastList <- unlist(lapply(combiNamesList, function(y) lapply(y, function(x) raster::subset(masks, x))))
  ### Get many outputs; each combination
  manyOuts <- lapply(combiRastList, function(x) lotsOfMasks(expertRaster = expertRaster, 
                                                            maskStack = x, 
                                                            maskBounds = maskBounds))
  if (is.null(rasProj)){
    ### Calculate areas for each "refinedDist"
    manyRefined <- lapply(manyOuts, function(x) x$refinedDist)
    manyMasksArea <- stats::setNames(as.data.frame(
      cbind(unlist(lapply(manyRefined, function(r2){tapply(raster::area(r2), r2[], sum)})), 
            lapply(combiRastList, names)), 
      row.names = lapply(combiRastList, names)), c("Area", "Layer"))
    
    v1 <- manyMasksArea[with(manyMasksArea, order(unlist(manyMasksArea$Area))), ]
  } 
  if (!is.null(rasProj)){
    proj.FUN<-function(rasterList, proj4){
      raster::crs(rasterList) <- proj4
    }
    ### Reproject
    lapply(manyOuts, proj.FUN, rasProj)
    ### Calculate areas for each "refinedDist"
    manyRefined <- lapply(manyOuts, function(x) x$refinedDist)
    manyMasksArea <- stats::setNames(as.data.frame(
      cbind(unlist(lapply(manyRefined, calcAreas)), lapply(combiRastList, names)), 
      row.names = lapply(combiRastList, names)),  c("Area", "Layer"))
    v1 <- manyMasksArea[with(manyMasksArea, order(unlist(manyMasksArea$Area))), ]
  }
  v1 <- manyMasksArea[with(manyMasksArea, order(unlist(manyMasksArea$Area))), ]
  # ## Plotting function
  # .manyMaskSensitivityPlots <- function(v1){
  #   oldpar <- par(no.readonly = TRUE)
  #   on.exit(par(oldpar))  
  #   graphics::barplot(height = unlist(v1$Area), space = 1, xlab = "", names.arg = "")
  #   graphics::text(seq(1.5, (0.5 + nrow(v1) + nrow(v1)-1), 2), graphics::par("usr")[3]-.25, srt = 60, adj=1, xpd=T, labels = cbind(unlist(lapply(v1$Layer, toString))), cex= 0.65)
  # }
  .manyMaskSensitivityPlots(v1)
  return(v1)
}

## Plotting function
#' @importFrom graphics barplot legend par plot points text
.manyMaskSensitivityPlots <- function(v1){
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))  
  graphics::barplot(height = unlist(v1$Area), space = 1, xlab = "", names.arg = "", 
                    ylab = "area")
  graphics::text(seq(1.5, (0.5 + nrow(v1) + nrow(v1)-1), 2), par("usr")[3]-.25, srt = 60, 
                 adj=1, xpd=T, labels = cbind(unlist(lapply(v1$Layer, toString))), cex= 0.65)
}
