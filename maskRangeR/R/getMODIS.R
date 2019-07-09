
#=============================================================================
#=============================================================================
#' @title download MODIS products (e.g., foret cover) based on your occurrence records
#'
#' @description Accesses yearly (2000 - 2017) MODIS products based on geographic location and date
#' @details
#' See Examples.
#' @param localSavePath the pathway or directory in which to save MODIS rasters
#' @param datedOccs a `SpatialPointsDataFrame` where one column is labeled `date` and has class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time` 
#' @param dateScale string: 'year', 'month', or 'day'
#' @param dataset MODIS product of interest. Available are: “Percent tree cover”, “Percent nontree vegetation”, “Percent nonvegetated”, “Quality”, “Percent tree cover 50”, “Percent nonvegetated sd”

#' @examples
#' \donttest{
#' simulateData()
#' getModis(localSavePath = '', datedOccs, dateScale = "year", dataset = "Percent tree cover")
#' }
# @return
#' @author Peter Galante <pgalante@@amnh.org>,
#' 
# @seealso
# @references
#' @export


getModis <- function(localSavePath, datedOccs, dateScale, dataset){

  # serapate date by date scale  
  occ1 <-.reDate(datedOccs = datedOccs, dateScale = dateScale)
  # get list of unique dates
  unidate <- .uniDates(occ1 = occ1, dateScale = dateScale)
  # create sub tables for occs by each datescale (e.g., by year)
  dateParse <-.parseDate(dateScale = dateScale, occ1 = occ1, uniqueDates = unidate)
  
  MODIS::MODISoptions(localArcPath = localSavePath, quiet = FALSE)
  # Set extent
  colnames(datedOccs) <- c("x","y")
  e <- raster::extent(datedOccs)
  # Look up in which tiles occur your occurrences 
  tileH <- MODIS::getTile(e)@tileH
  tileV <- MODIS::getTile(e)@tileV
  # Date Range
  years <- unidate$years
  # Download as hdf
  message('downloading hdf files from MODIS- give it a minute!')
  hdf = MODIS::getHdf("MOD44B", collection = "006",
                      tileH = tileH, tileV = tileV,
                      begin = paste0(years[1],".08.28"), end = paste0(utils::tail(years,1),".08.31"), 
                      extent = e)
  message('done')
  # Extract available datasets and only pull out the first one - percent forest cover. 
  message('extracting dataset of interest')
  pos.data <- as.data.frame(cbind(c(1:7), c("Percent tree cover", "Percent nontree vegetation", "Percent nonvegetated", "Quality", "Percent tree cover 50", "Percent nonvegetated sd", "Cloud")))
  dset <- pos.data %>% dplyr::filter(pos.data$V2 == dataset)
  dataset.number <-dset[[1]]
  sds <- sapply(lapply(hdf$MOD44B.006, gdalUtils::get_subdatasets), "[", dataset.number)
  # Sort datasets by years 
  sds.byDate <- lapply(years, function(x) unique(grep(paste(x, collapse="|"), sds, value=T)))
  # convert to raster
  rgdals <- lapply(sds.byDate, sapply, rgdal::readGDAL)
  rrs <- lapply(rgdals, sapply, raster::raster)
  ## Merge tiles of same year, then stack years
  ####3make rrs.list a true list of lists
  # list them correctly
  rrs <- lapply(rrs,FUN=function(x) {names(x) <- NULL 
  x})
  # Merge by year
  message('merging tiles')
  merged.rrs <- lapply(rrs, function(x) do.call(raster::merge, x))
  stack.merged.rrs <- raster::stack(merged.rrs)
  message('finished merging')
  # reproject to wgs84
  message('reprojecting rasters to wgs84')
  rrs.repro <- raster::projectRaster(stack.merged.rrs, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  message('finished reprojecting')
  # Rename rasters
  names(rrs.repro) <- paste0("y", years)
  # Save rasters
  raster::writeRaster(rrs.repro, filename = paste0(localSavePath, names(rrs.repro)), bylayer=T, format = "GTiff", overwrite=T)
}
