#=========================================================================================
#=======================================================================================
#' @importFrom dplyr %>%
.reDate <- function(datedOccs, dateScale){
  #require(dplyr)
  if(dateScale == "year"){
    return(datedOccs %>% dplyr::select(long,lat,date) %>%
             dplyr::mutate(date = as.Date(paste0(date, "-01-01"))) %>%
             dplyr::mutate(years = year(date)))
  }
  if(dateScale == "month"){
    return(datedOccs %>% dplyr::select(long,lat,date) %>%
             dplyr::mutate(date = parse_date_time(date, "ym")) %>%
             dplyr::mutate(months = month(date)) %>%
             dplyr::mutate(years = year(date)))
  }
  if(dateScale == "day"){
    return(datedOccs %>% dplyr::select(long,lat,date) %>%
             dplyr::mutate(date = ymd(date)) %>%
             dplyr::mutate(years = year(date)) %>% 
             dplyr::mutate(months = month(date)) %>%
             dplyr::mutate(days = day(date)))
  }
}

#=========================================================================================
#=======================================================================================
#' @importFrom dplyr %>%
.uniDates<-function(occ1, dateScale){
  #require(dplyr)
  if (dateScale == "year"){
    return(dplyr::arrange(unique(dplyr::select(occ1, years)), years))
  } 
  if (dateScale == "month"){
    return(dplyr::arrange(unique(dplyr::select(occ1, years, months)), years))
  } 
  if (dateScale == "day"){
    return(dplyr::arrange(unique(dplyr::select(occ1, years, months, days)), years))
  }
}
#=========================================================================================
#=======================================================================================
#' @importFrom dplyr %>%
.parseDate <- function(dateScale, occ1, uniqueDates){
  #require(dplyr)
  t1 <- NULL
  if (dateScale == "year"){
    for (i in 1:nrow(uniqueDates)){
      t1[[i]] = dplyr::filter(occ1, years == as.list(uniqueDates)$years[[i]])
    }}
  #### NEED TO FIGURE OUT YEAR MONTH DAY COMBINATIONS
  if (dateScale == "month"){
    t1 <- NULL
    for (i in 1:nrow(uniqueDates)){
      t1[[i]] = dplyr::filter(occ1, months == as.list(uniqueDates)$years[[i]])
    }}
  
  if (dateScale == "day"){
    t1 <- NULL
    for (i in 1:nrow(uniqueDates)){
      t1[[i]] = dplyr::filter(occ1, days == as.list(uniqueDates)$years[[i]])
    }}
  return(t1)
}
#=========================================================================================
#=======================================================================================
#' @importFrom dplyr %>%
.valExtract <- function(occ3, RSenv){
  #require(dplyr)
  occ4 <- lapply(occ3, function(x) x %>% dplyr::select(.data$long, .data$lat))
  vals <- mapply(raster::extract, utils::unstack(RSenv), occ4)
  lowerBound <- min(unlist(vals))
  upperBound <- max(unlist(vals))
  return(as.data.frame(cbind(lowerBound, upperBound)))
}

#=========================================================================================
#=======================================================================================
.dataMask <- function(RSenv, ulBounds, Model, Bounds){
  if(Bounds == "both"){
    RSenvCrop <- raster::crop(RSenv, raster::extent(Model))
    modelCrop <- raster::crop(Model, raster::extent(RSenvCrop))
    rasterMask <- RSenvCrop[[raster::nlayers(RSenvCrop)]]
    rasterMask[rasterMask < ulBounds$lowerBound] <- NA
    rasterMask[rasterMask > ulBounds$upperBound] <- NA
    return(raster::mask(modelCrop, rasterMask))
  }
  if(Bounds == "upper"){
    RSenvCrop <- raster::crop(RSenv, raster::extent(Model))
    modelCrop <- raster::crop(Model, raster::extent(RSenvCrop))
    rasterMask <- RSenvCrop[[raster::nlayers(RSenvCrop)]]
    rasterMask[rasterMask < ulBounds$upperBound] <- NA
    return(raster::mask(modelCrop, rasterMask))
  }
  if(Bounds == "lower"){
    RSenvCrop <- raster::crop(RSenv, raster::extent(Model))
    modelCrop <- raster::crop(Model, raster::extent(RSenvCrop))
    rasterMask <- RSenvCrop[[raster::nlayers(RSenvCrop)]]
    rasterMask[rasterMask < ulBounds$lowerBound] <- NA
    return(raster::mask(modelCrop, rasterMask))
  }
}