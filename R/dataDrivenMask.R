#=========================================================================================
#=========================================================================================
#' @title Creating a minimum and maximum value mask to apply to models
#' @description Function masks a model raster by the minimum and maximum observed values at occurrences. Occurrence localities with dates paired with raster layers from those same dates are used to determine the minimum and maximum observed values through time.
#'   
#' @param datedOccs A .csv file containing 3 columns of "longitude", "latitude", "date". Dates must either be year only, or YMD in a lubridate accepted format
#' @param RSenv A rasterStack of remotely sensed data (i.e., percent Forest Cover) with names as dates (eg. Y2000, Y2001) or date-times YMD (e.g.. Y2000-02-01)
#' @param dateScale a character vector of the scale the RSenv and corresponding datedOccs. Could be "year", "month", "day".
#' @param Model A raster file (generally in continuous output) with extent, resolution, etc. matching RSenv.
#' @param Bounds User selects which bounds observed for the species are limiting: "upper", "lower", "both"
#'         
#' @author Peter Galante <pgalante[at]amnh.org>
#' 
##########################################################################################
#############################  MASTER FUNCTION TO MASK MODEL OUTPUT  #####################
##########################################################################################
RSmask<-function(datedOccs, RSenv, dateScale, Model, Bounds){
  require(dplyr)
  ###############################################################################
  ### Extract occs into sub-tables based on dates (here, using only year)  ######
  ##############  Eventually, find online RS data for tutorial  #################
  ##  First parse out the dates into the appropriate dateScale (year, month, date)
  occ1 <- .reDate(datedOccs = datedOccs, dateScale = 'year')
  ##  Next, for each appropriate dateScale, get unique dates
  uniqueDates <- .uniDates(occ1 = occ1, dateScale = dateScale)
  ##  Last, for each unique date, create different tibbles
  occ3<-.parseDate(dateScale, occ1, uniqueDates)
  ###############################################################################
  ### For each occs sub-table, extract values from corresponding env/RS sub-stack
  ###############################################################################
  ######### Use bounds to set mask limits and create mask  ######################
  ulBounds <-.valExtract(occ3, RSenv)
  .dataMask(RSenv, ulBounds, Model, Bounds)
}
