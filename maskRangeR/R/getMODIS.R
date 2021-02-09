#' 
#' #=============================================================================
#' #=============================================================================
#' #' @title download MODIS products (e.g., foret cover) based on your occurrence records
#' #'
#' #' @description Accesses yearly (2000 - 2017) MODIS products based on geographic location and date
#' #' @details
#' #' See Examples.
#' #' @param localSavePath the pathway or directory in which to save MODIS rasters
#' #' @param datedOccs a data.frame showing longitude, latitude, and date. The date should match the date scale and of class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time`. Dates must be within the time range of MODIS data (i.e., 2000-2017).
#' #' @param dateScale string: 'year', 'month', or 'day'. CURRENTLY ONLY TESTED FOR YEARLY DATA
#' #' @param dataset MODIS product of interest. Available are: "Percent_Tree_Cover", "Percent_Nontree_Vegetation", "Percent_Nonvegetated", "Quality", "Percent_Tree_Cover_SD", "Percent_Nonvegetated_SD", "Cloud"
#' 
#' #' @examples
#' #' \donttest{
#' #' simulateData()
#' #' getModis(localSavePath = '', datedOccs, dateScale = "year", dataset = "Percent_Tree_Cover")
#' #' }
#' #' @return Saves rasters to specified path
#' #' @author Peter Galante <pgalante@@amnh.org>,
#' #' 
#' # @seealso
#' # @references
#' #' @export
#' 
#' 
#' .getModis <- function(localSavePath, datedOccs, dateScale, dataset){
#' 
#'   .reDate <- function(datedOccs, dateScale){
#'     long=lat=date=parse_date_time=ymd=year=month=day=NULL # trick for check
#'     if(dateScale == "year"){
#'       out=datedOccs %>% dplyr::select(long,lat,date) %>%
#'         dplyr::mutate(date = as.Date(paste0(date, "-01-01"))) %>%
#'         dplyr::mutate(years = lubridate::year(date))
#'     }
#'     if(dateScale == "month"){
#'       out=datedOccs %>% dplyr::select(long,lat,date) %>%
#'         dplyr::mutate(date = parse_date_time(date, "ym")) %>%
#'         dplyr::mutate(months = lubridate::month(date)) %>%
#'         dplyr::mutate(years = lubridate::year(date))
#'     }
#'     if(dateScale == "day"){
#'       out=datedOccs %>% dplyr::select(long,lat,date) %>%
#'         dplyr::mutate(date = lubridate::ymd(date)) %>%
#'         dplyr::mutate(years = lubridate::year(date)) %>% 
#'         dplyr::mutate(months = lubridate::month(date)) %>%
#'         dplyr::mutate(days = lubridate::day(date))
#'     }
#'     return(out)
#'   }
#'   
#'   .uniDates<-function(occ1, dateScale){
#'     years=days=NULL # trick for check
#'     if (dateScale == "year"){
#'       out=dplyr::arrange(unique(dplyr::select(occ1, years)), years)
#'     } 
#'     if (dateScale == "month"){
#'       out=dplyr::arrange(unique(dplyr::select(occ1, years, months)), years)
#'     } 
#'     if (dateScale == "day"){
#'       out=dplyr::arrange(unique(dplyr::select(occ1, years, months, days)), years)
#'     }
#'     return(out)
#'   }
#'   
#'   .parseDate <- function(dateScale, occ1, uniqueDates){
#'     years=days=NULL # trick for check
#'     t1 <- NULL
#'     if (dateScale == "year"){
#'       for (i in 1:nrow(uniqueDates)){
#'         t1[[i]] = dplyr::filter(occ1, years == as.list(uniqueDates)$years[[i]])
#'       }}
#'     #### NEED TO FIGURE OUT YEAR MONTH DAY COMBINATIONS
#'     if (dateScale == "month"){
#'       t1 <- NULL
#'       for (i in 1:nrow(uniqueDates)){
#'         t1[[i]] = dplyr::filter(occ1, months == as.list(uniqueDates)$years[[i]])
#'       }}
#'     
#'     if (dateScale == "day"){
#'       t1 <- NULL
#'       for (i in 1:nrow(uniqueDates)){
#'         t1[[i]] = dplyr::filter(occ1, days == as.list(uniqueDates)$years[[i]])
#'       }}
#'     return(t1)
#'   }
#'   
#'   
#'   # serapate date by date scale  
#'   occ1 <-.reDate(datedOccs = datedOccs, dateScale = dateScale)
#'   # get list of unique dates
#'   unidate <- .uniDates(occ1 = occ1, dateScale = dateScale)
#'   # create sub tables for occs by each datescale (e.g., by year)
#'   dateParse <-.parseDate(dateScale = dateScale, occ1 = occ1, uniqueDates = unidate)
#'   
#'   MODIS::MODISoptions(localArcPath = localSavePath, quiet = FALSE)
#'   # Set extent
#'   colnames(datedOccs) <- c("x","y")
#'   e <- raster::extent(datedOccs)
#'   # Look up in which tiles occur your occurrences 
#'   tileH <- MODIS::getTile(e)@tileH
#'   tileV <- MODIS::getTile(e)@tileV
#'   # Date Range
#'   years <- unidate$years
#'   # Download as hdf
#'   message('downloading hdf files from MODIS- give it a minute!')
#'   hdf = MODIS::getHdf("MOD44B", collection = "006",
#'                       tileH = tileH, tileV = tileV,
#'                       begin = paste0(years[1],".08.28"), end = paste0(utils::tail(years,1),".08.31"), 
#'                       extent = e)
#'   message('done')
#'   # Extract available datasets and only pull out the first one - percent forest cover. 
#'   message('extracting dataset of interest')
#'   pos.data <- as.data.frame(cbind(c(1:7), c("Percent_Tree_Cover", "Percent_Nontree_Vegetation", "Percent_Nonvegetated", "Quality", "Percent_Tree_Cover_SD", "Percent_Nonvegetated_SD", "Cloud")))
#'   dset <- pos.data %>% dplyr::filter(pos.data$V2 == dataset)
#'   dataset.number <- dset[[1]]
#'   #sds <- sapply(lapply(hdf$MOD44B.006, gdalUtils::get_subdatasets), "[", dataset.number)
#'   tt <- lapply(hdf$MOD44B.006, gdalUtils::get_subdatasets)
#'   ttList <- sapply(tt, grep, pattern = paste0("\\", dataset, "$"))
#'   # grep(pattern = paste0("\\", dataset, "$"), tt[[1]])
#'   sds <- sapply(tt, "[[", ttList[1])
#'   # Sort datasets by years 
#'   sds.byDate <- lapply(years, function(x) unique(grep(paste(x, collapse="|"), sds, value=T)))
#'   # convert to raster
#'   rgdals <- lapply(sds.byDate, sapply, rgdal::readGDAL)
#'   rrs <- lapply(rgdals, sapply, raster::raster)
#'   ## Merge tiles of same year, then stack years
#'   #### make rrs.list a true list of lists
#'   # list them correctly
#'   rrs <- lapply(rrs,FUN=function(x) {names(x) <- NULL 
#'   x})
#'   # Merge by year
#'   message('merging tiles')
#'   merged.rrs <- lapply(rrs, function(x) do.call(raster::merge, x))
#'   stack.merged.rrs <- raster::stack(merged.rrs)
#'   message('finished merging')
#'   # reproject to wgs84
#'   message('reprojecting rasters to wgs84')
#'   rrs.repro <- raster::projectRaster(stack.merged.rrs, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ", method = "bilinear")
#'   message('finished reprojecting')
#'   # Rename rasters
#'   names(rrs.repro) <- paste0("y", years)
#'   # Save rasters
#'   message('saving rasters')
#'   raster::writeRaster(rrs.repro, filename = paste0(localSavePath, "/", names(rrs.repro)), bylayer=T, format = "GTiff", overwrite=T)
#'   
#' # }
