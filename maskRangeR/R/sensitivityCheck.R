#===========================================================================
#===========================================================================
#' @title Sensitivity testing for thresholds
#'
#' @description Measure a number of reasonable thresholds and calculate areas based on these thresholds
#' @details
#' See Examples.

#' @param datedOccs a `SpatialPointsDataFrame` where one column is labeled `date` and has class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time` 
#' @param maskLayer the layer from which you will build the mask. Usually the most recent satellite derived raster
#' @param maskClass either top5, aroundSelected, quants, or userSpecified. "top5" ranks the environmental values at occurrences and returns the 5 highest observed values. 
#' "aroundSelected" selects two observed values above and two below a user-specified threshold value. "quants" returns quantile values as thresholds. 
#' "userSpecified" will take a list of values defined by the user.
#'  @param sdm previously generated species distribution model
#'  
#'  @author Peter Galante <pgalante@@amnh.org>
#'  @export

sensitivityMasks <- function(datedOccs, maskLayer, maskClass, sdm){
  require(maskRangeR)
  require(raster)
  require(lubridate)
  
  if (maskClass == "top5"){
    ### multiple sensible masks
    # Top 5 thresholds
    maskThresh <- rev(sort(unique(na.omit(datedOccs$env))))[1:5]
  } else if (maskClass == "aroundSelected"){
    # buffer around selected value
    aroundSelected <- function(datedOccs, selectedValue){
      maskVals <- rev(sort(unique(na.omit(datedOccs$env))))[(which.min(abs(rev(sort(unique(na.omit(datedOccs$env)))) - selectedValue)) - 1) : (which.min(abs(rev(sort(unique(na.omit(datedOccs$env)))) - selectedValue)) + 2)]
      return(maskVals)
    }
    maskThresh <- aroundSelected(datedOccs, selectedValue = 55) 
  } else if (maskClass == "quants"){
    # Quantile values
    maskThresh <- as.numeric(quantile(datedOccs$env, na.rm=T))
  } else if (maskClass == "userSpecified"){
    # user defined
    maskThresh <- paste0('maskLayers>',quantile(datedOccs$env,prob=.25,na.rm=T))
  }
  
  ### makes several logicStrings
  # get most recent forest cover
  stringsOfLogic <- paste0('maskLayers<', maskThresh)
  
  ### create a list of rasters
  sensitivityStack <- lapply(stringsOfLogic, function(x) maskRanger(initialDist=sdm, maskLayers=maskLayer, logicString=x)$refinedDist)
  
  ## Calculate areas for each refinedDist
  # Set all values in each reginedDist = 1, calculate area of 1's
  ras.FUN <- function(r1){
    r1[!is.na(r1)] <- 1
    return(r1)
  }
  # change all values !=na to 1
  sensitivityOnes <- lapply(sensitivityStack, ras.FUN)
  # fcn to calculate area based on 1's
  calcAreas <- function(r2){
    tapply(area(r2), r2[], sum)
  }
  # apply area calculation to list of rasters. results are square km
  sensitivityAreas <- lapply(sensitivityOnes, calcAreas)
  
  ## Plotting each mask and one plot of area vs threshold
  # area vs. threshold
  maskValues <- gsub(".*<","",stringsOfLogic)
  names(sensitivityStack) <- paste0("mask threshold of ", maskValues)
  dev.new()
  par(mfrow=c(2,(length(stringsOfLogic)/2)+1))
  colPal <- rainbow(5)
  lapply(names(sensitivityStack), function(x) plot(sensitivityStack[[x]], main = x, xlab = "long", ylab = "lat"))
  plot(maskValues, sensitivityAreas, ylab = "Area (square km)", xlab = "Mask values", main = "Mask Threshold Area Sensitivity", col = colPal, pch = 19)
  # Get values to add to plot
  nums <- gsub(".*= ", "", sensitivityAreas)
  nums <- gsub( ").*$", "", nums)
  nums <- format(round(unlist(lapply(nums, function(x) as.numeric(as.character(x)))), 3))
  # add values
  text(maskValues, sensitivityAreas, labels = nums, cex= 1, pos=3)
  # add legend
  legend(x = "topleft", col = colPal, legend = names(sensitivityStack), pch = 19, cex=0.7)
}
