#===========================================================================
#===========================================================================
#' @title Annotate point data with rasters based on matching dates.
#'
#' @description Annotate point data with rasters based on matching dates associated with points to dates associated with rasters. Specifically, we're thinking of the points as species observations and the rasters as remotely sensed environmental layers, but they can represent any points and rasters with dates.
#' @details
#' See Examples.
#' @param datedOccs a `SpatialPointsDataFrame` where one column is labeled `date` and has class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time` 
#' @param env a raster stack
#' @param envDates a vector of dates the same length as `env`. The vector should have class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time`
#' @param dateScale string: 'year', 'month', or 'day'
# @examples
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
  # uses the convention that dates for values indicated by a year are YYYY-01-01
  # uses the convention that dates for values indicated by a month are YYYY-MM-01
  if(dateScale=='year') {
    form="%Y"
  } else if (dateScale=='month'){
    form="%Y-%m"
  } else if (dateScale=='day'){
    form="%Y-%m-%d"
  } else { stop('Choose a supported value for dateScale: year, month or day')}
  
  datedOccs$myDate=format(datedOccs$date,form)
  uniqueDates=stats::na.omit(unique(datedOccs$myDate))
  myEnvDates=format(envDates,form)

  out=lapply(seq_along(uniqueDates),function(x,datedOccs,myEnvDates,env){
    pts=datedOccs[which(datedOccs$myDate==uniqueDates[x]),]
    keep=match(uniqueDates[x],myEnvDates)
    if(is.na(keep)) {
      message(paste0('Environmental layers were missing for date ',uniqueDates[x]) )
      pts$env=rep(NA,nrow(pts))
      return(pts)
    }
    if(length(keep)>1) stop('Multiple dates in your environmental layers correspond 
                            to the dates for your occurrences; make sure your 
                            environmental layers have unique dates')
    pts$env=raster::extract(env[[keep]],pts)
    return(pts)
  },datedOccs=datedOccs,myEnvDates=myEnvDates,env=env)

  tmp=suppressWarnings(do.call('rbind',out))
  lost=nrow(datedOccs)-nrow(tmp)
  if(lost>0) message(paste(lost,'points were omitted because they had no dates'))
  return(tmp)
}
