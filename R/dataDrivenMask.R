#=======================================================================================================
#=======================================================================================================
#' @title Creating a minimum and maximum value mask to apply to models
#' @description Function masks a model raster by the minimum and maximum observed values at occurrences. Occurrence localities with dates paired with raster layers from those same dates are used to determine the minimum and maximum observed values through time.
#'   
#' @param datedOccs A .csv file containing 3 columns of "longitude", "latitude", "date". Dates must either be year only, or YMD in a lubridate accepted format
#' @param RSenv A rasterStack of remotely sensed data (ie. % Forest Cover) with names as dates (eg. Y2000, Y2001) or date-times YMD (eg. Y2000-02-01)
#' @param dateScale a character vector of the scale the RSenv and corresponding datedOccs. Could be "year", "month", "day".
#' @param Model A raster file (generally in continuous output) with extent, resolution, etc. matching RSenv.
#'    
#' @author Peter Galante <pgalante[at]amnh.org>
#' 
#########################################################################################################
#############################  MASTER FUNCTION TO MASK MODEL OUTPUT  ####################################
#########################################################################################################
RSmask<-function(datedOccs, RSenv, dateScale, Model){
  library(tidyverse)
  library(raster)
  library(lubridate)
  ###############################################################################
  ### Extract occs into sub-tables based on dates (here, using only year)  ######
  ##############  Eventually, find online RS data for tutorial  #################
  ##  First parse out the dates into the appropriate dateScale (year, month, date)
  reDate <- function(DatedOccs, dateScale){
    if(dateScale == "year"){
      return(occ %>% dplyr::select(long,lat,date) %>%
               mutate(date = as.Date(paste0(date, "-01-01"))) %>%
               mutate(years = year(date)))
    }
    if(dateScale == "month"){
      return(occ %>% dplyr::select(long,lat,date) %>%
               mutate(date = parse_date_time(date, "ym")) %>%
               mutate(months = month(date)) %>%
               mutate(years = year(date)))
    }
    if(dateScale == "day"){
      return(occ %>% dplyr::select(long,lat,date) %>%
               mutate(date = ymd(date)) %>%
               mutate(years = year(date)) %>% 
               mutate(months = month(date)) %>%
               mutate(days = day(date)))
    }
  }
  occ1 <- reDate(DatedOccs = datesOccs, dateScale = 'year')
  ##  Next, for each appropriate dateScale, get unique dates
  uniDates<-function(occ1, dateScale){
    require(lubridate)
    
    if (dateScale == "year"){
      return(arrange(unique(dplyr::select(occ1, years)), years))
    } 
    if (dateScale == "month"){
      return(arrange(unique(dplyr::select(occ1, years, months)), years))
    } 
    if (dateScale == "day"){
      return(arrange(unique(dplyr::select(occ1, years, months, days)), years))
    }
  }
  uniqueDates <- uniDates(occ1 = occ1, dateScale = dateScale)
  ##  Last, for each unique date, create different tibbles
  parseDate <- function(dateScale, occ1, uniqueDates){
    t1 <- NULL
    if (dateScale == "year"){
      for (i in 1:nrow(uniqueDates)){
        t1[[i]] = filter(occ1, years == as.list(uniqueDates)$years[[i]])
      }}
    #### NEED TO FIGURE OUT YEAR MONTH DAY COMBINATIONS
    if (dateScale == "month"){
      t1 <- NULL
      for (i in 1:nrow(uniqueDates)){
        t1[[i]] = filter(occ1, months == as.list(uniqueDates)$years[[i]])
      }}
    
    if (dateScale == "day"){
      t1 <- NULL
      for (i in 1:nrow(uniqueDates)){
        t1[[i]] = filter(occ1, days == as.list(uniqueDates)$years[[i]])
      }}
    return(t1)
  }
  occ3<-parseDate(dateScale, occ1, uniqueDates)
  ###############################################################################
  ### For each occs sub-table, extract values from corresponding env/RS sub-stack
  valExtract <- function(occ3, RSenv){
    occ4 <- lapply(occ3, function(x) x %>% dplyr::select(long, lat))
    vals <- mapply(raster::extract, unstack(RSenv), occ4)
    lowerBound <- min(unlist(vals))
    upperBound <- max(unlist(vals))
    return(as.data.frame(cbind(lowerBound, upperBound)))
  }
  ###############################################################################
  ######### Use bounds to set mask limits and create mask  ######################
  Bounds <-valExtract(occ3, RSenv)
  dataMask <- function(RSenv, Bounds, Model){
    rasterMask <- RSenv[[nlayers(RSenv)]]
    rasterMask[rasterMask > Bounds$lowerBound] <- NA
    rasterMask[rasterMask < Bounds$upperBound] <- NA
    return(mask(Model, rasterMask))
  }
  dataMask(RSenv, Bounds, Model)
}
