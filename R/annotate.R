#==================================================================================
#==================================================================================
#' @title Annotate point with rasters based on matching dates assoi
#'
#' @description Annotate point with rasters based on matching dates associated with points to dates associated with raster. Specifically, we're thinking of the points as species observations and the rasters as remotely sensed environmental layers, but they can represent any points and rasters with dates.
#' @details
#' See Examples.
#' @param datedOccs a `SpatialPointsDataFrame` where one column is labeled `date` and has class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time` 
#' @param env a raster stack
#' @param envDates a vector of dates the same length as `env`. The vector should have class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time`
#' @param dateScale string: 'year', 'month', or 'day'
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


# try just matching dates between envDates and the occurrence dates
annotate=function(datedOccs,
                  env,
                  envDates,
                  dateScale){
  
  datedOccs$myDate=unlist(do.call(dateScale,list(datedOccs$date)))
  uniqueDates=unique(datedOccs$myDate)
  myEnvDates=unlist(do.call(dateScale,list(envDates)))
  
  out=unlist(lapply(seq_along(uniqueDates),function(x,datedOccs,myEnvDates,env){
    pts=subset(datedOccs,myDate==uniqueDates[x])
    keep=match(uniqueDates[x],myEnvDates)
    if(is.na(keep)) {
      print(paste0('Environmental layers were missing for date ',uniqueDates[x]) )
      out=rep(NA,nrow(pts))
      return(out)
    }
    if(length(keep)>1) stop('Multiple dates in your environmental layers correspond 
                            to the dates for your occurrences; make sure your 
                            environmental layers have unique dates')
    raster::extract(env[[keep]],pts)
  },datedOccs=datedOccs,myEnvDates=myEnvDates,env=env))
  datedOccs$env=out
  return(datedOccs)
}
