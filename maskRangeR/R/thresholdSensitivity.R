#===========================================================================
#===========================================================================
#' @title Sensitivity testing for thresholds
#'
#' @description Measure a number of reasonable thresholds and calculate areas based on these thresholds
#' @details
#' See Examples.

#' @param datedOccs a `SpatialPointsDataFrame` where one column is labeled `date` and has class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time` 
#' @param maskLayer the layer from which you will build the mask. Usually the most recent satellite derived raster
#' @param maskClass either top5, aroundSelected, quants, or userSpecified. "top5" ranks the environmental values at occurrences and returns the 5 highest observed values."aroundSelected" selects two observed values above and two below a user-specified threshold value. "quants" returns quantile values as thresholds. "userSpecified" will take a list of values defined by the user.
#' @param sdm previously generated species distribution model
#' @param maskProjection (optional) a proj4string showing the projection of the maskLayer. If NULL, areas will be estimated using the raster package.
#' @param maskVal (optional) a user defined value for thresholding when using the "aroundSelected" maskClass, or if "userSpecified", a list of thresholds to use.
#' @param selectedValue  (optional) a user selected value around which masks will be selected using the nearest two threshold on either side
#' 
#' @return returns a list containing two items. The first is a rasterstack of the masked distributions. The second item is a table of thresholds and areas
#'  
#' @author Peter Galante <pgalante@@amnh.org>
#' @export


########################

thresholdSensitivity <- function(datedOccs, maskLayer, maskClass, sdm, maskProjection = NULL, maskVal = NULL, selectedValue = NULL){
  ## Maskthresholds
  .maskThresh <- function(datedOccs, maskClass, selectedValue = NULL, maskVal = NULL){
    if (maskClass == "top5"){
      ### multiple sensible masks
      # Top 5 thresholds
      maskThresh <- rev(sort(unique(stats::na.omit(datedOccs$env))))[1:5]
    } else if (maskClass == "aroundSelected"){
      # buffer around selected value
      aroundSelected <- function(datedOccs, selectedValue){
        maskVals <- rev(sort(unique(stats::na.omit(datedOccs$env))))[(which.min(abs(rev(sort(unique(stats::na.omit(datedOccs$env)))) - selectedValue)) - 1) : (which.min(abs(rev(sort(unique(stats::na.omit(datedOccs$env)))) - selectedValue)) + 2)]
        return(maskVals)
      }
      maskThresh <- aroundSelected(datedOccs, selectedValue = maskVal) 
    } else if (maskClass == "quants"){
      # Quantile values
      maskThresh <- as.numeric(raster::quantile(datedOccs$env, na.rm=T))
    } else if (maskClass == "userSpecified"){
      # user defined
      maskThresh <- maskVal
      return(maskThresh)
    }
  }
  maskThresh <- .maskThresh(datedOccs, maskClass, selectedValue, maskVal)
  ## Bounded omission/commission
  
  ### makes several logicStrings
  # get most recent forest cover
  stringsOfLogic <- paste0('maskLayers>', maskThresh)
  
  ### create a list of rasters
  sensitivityStack <- lapply(stringsOfLogic, function(x) maskRanger(initialDist=sdm, maskLayers=maskLayer, logicString=x)$refinedDist)
  ### Set projection if available
  proj.FUN <- function(rasterList, proj4){
    raster::crs(rasterList) <- proj4
  }
  if (!is.null(maskProjection)){
    lapply(sensitivityStack, proj.FUN, maskProjection)
  }
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
    tapply(raster::area(r2), r2[], sum)
  }
  # apply area calculation to list of rasters. results are square km
  sensitivityAreas <- lapply(sensitivityOnes, calcAreas)
  
  maskValues <- gsub(".*>","",stringsOfLogic)
  names(sensitivityStack) <- paste0("threshold_of_", rev(maskValues))
  sensitivityStack <- raster::stack(sensitivityStack)
  
  # ## Plotting metrics
  # .sensitivityPlotting <- function(stringsOfLogic, sensitivityStack, sensitivityAreas){
  #   ## Plotting each mask and one plot of area vs threshold
  #   # area vs. threshold
  #   maskValues <- gsub(".*<","",stringsOfLogic)
  #   names(sensitivityStack) <- paste0("threshold_of_", rev(maskValues))
  #   sensitivityStack <- raster::stack(sensitivityStack)
  #  # grDevices::dev.new()
  #   oldpar <- par(no.readonly = TRUE)
  #   on.exit(par(oldpar))
  #   graphics::par(mfrow=c(2,(length(stringsOfLogic)/2)+1))
  #   colPal <- grDevices::rainbow(5)
  #   lapply(names(sensitivityStack), function(x) raster::plot(sensitivityStack[[x]], main = x, xlab = "long", ylab = "lat"))
  #   #### for here allow for all threshold values, but only plot some maps
  # 
  #   graphics::plot(rev(maskValues), sensitivityAreas, ylab = "Area (square km)", xlab = "Mask values", main = "Mask Threshold Area Sensitivity", col = colPal, pch = 19, type= "l")
  #   graphics::points(rev(maskValues), sensitivityAreas, ylab = "Area (square km)", xlab = "Mask values", main = "Mask Threshold Area Sensitivity", col = colPal, pch = 19)
  #   graphics::axis(labels=NA,side=1,tck=-0.015,at=maskValues)
  #   # Get values to add to plot
  #   nums <- gsub(".*= ", "", sensitivityAreas)
  #   nums <- gsub( ").*$", "", nums)
  #   nums <- format(round(unlist(lapply(nums, function(x) as.numeric(as.character(x)))), 3))
  #   # add values
  #   graphics::text(maskValues, sensitivityAreas, labels = nums, cex= 1, pos=3)
  #   # add legend
  # #  graphics::legend(x = "topright", inset=c(-0.2, 0), col = colPal, legend = rev(names(sensitivityStack)), pch = 19, cex=0.7)
  #   #print(cbind(maskValues, sensitivityAreas))
  #   sensitivityReturns <- NULL
  #   return(list(sensitivityStack, cbind(maskValues, sensitivityAreas)))
  # }
  # .sensitivityPlotting(stringsOfLogic, sensitivityStack, sensitivityAreas)
  return(list("Sensitivity_Rasters" = sensitivityStack, "Areas" = as.data.frame(cbind(maskValues, sensitivityAreas))))
}



