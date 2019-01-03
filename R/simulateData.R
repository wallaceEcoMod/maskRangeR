#==================================================================================
#==================================================================================
#' @title Data Simulation 
#'
#' @description Simulates data for vignette-like function demonstrations. This function returns a number of variables to the global environment so that examples can be run.
#'  
#' @details
#' See Examples.
#' @param 
# @examples
#' \dontrun{
#' system.time({simulateData()})
#' # Look at some of the example variables needed
#' datedOccs
#' class(env)
#' head(sp1.xy)
#' }
#'
# @return
#' @author Peter Galante <pgalante@@amnh.org>,
#' @note 
# @seealso
# @references
# @aliases
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
# FUNCTION TO CREATE ALL SIMULATED DATA AND EXPORT A LIST USABLE FOR EACH EXAMPLE IN USECASE
simulateData <- function(){
  options(warn=-1)
  require(raster)
  require(dismo)
  require(maskRangeR)
  library(devtools)
  library(lubridate)
  library(rgdal)
  ###  Initiating data for use case # 1
  # Initiate empty raster object with extent and resolution 
  r1 <<- raster(extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
  # Generate random occurrence points
  datedOccs<<-data.frame(randomPoints(r1, 5))
  # Pair dates (as years) with the occurrences
  datedOccs$date <<- 2010:2014
  # convert dates to formal date objects
  datedOccs$date <<- parse_date_time(datedOccs$date, orders = c("Y", "Ym"))
  # Create values showing distance from random points to simulate suitability surface
  sdm <<- distanceFromPoints(r1, datedOccs[,1:2])
  # Create 5 different rasters as env data
  env1 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
  env2 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
  env3 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
  env4 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
  env5 <<- distanceFromPoints(r1, data.frame(randomPoints(sdm, 3)))
  env <<- stack(env1, env2, env3, env4, env5)
  ### Initiating data for use case # 2
  ## Generate random polygon
  coords <- randomPoints(sdm, 3)
  polyg <- Polygon(coords)
  polyg <<- SpatialPolygons(list(Polygons(list(polyg), ID = "a")))
  ### Initiating data for use case # 3
  
  ### Initiating data for use case # 4
  ## Generate some species occurrence records
  r1.svm <<- raster(extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
  values(r1.svm) <<- (1:ncell(r1.svm))^2
  r2.svm <<- raster(extent(c(-72, -64, 41, 50)), res = c(0.008333333, 0.008333333))
  values(r2.svm) <<- (ncell(r2.svm):1)^2
  sp1.xy <<- data.frame(randomPoints(r1.svm, 15, prob = T)); colnames(sp1.xy) <<- c("longitude", "latitude")
  sp2.xy <<- data.frame(randomPoints(r2.svm, 15, prob = T)); colnames(sp2.xy) <<- c("longitude", "latitude")
}
