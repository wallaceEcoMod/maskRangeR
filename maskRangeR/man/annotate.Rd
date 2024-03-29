% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate.R
\name{annotate}
\alias{annotate}
\title{Annotate point data with rasters based on matching dates.}
\usage{
annotate(datedOccs, env, envDates, dateScale)
}
\arguments{
\item{datedOccs}{a `SpatialPointsDataFrame` of occurrence localities (generally longitude and latitude in decimal degrees) paired with dates. One column must be labeled `date` and have class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time`}

\item{env}{a raster stack}

\item{envDates}{a vector of dates the same length as `env`. The vector should have class `POSIXct`, e.g., as obtained from using `lubridate::parse_date_time`}

\item{dateScale}{string: 'year', 'month', or 'day'}
}
\value{
a SpatialPointsDataFrame
}
\description{
Annotate point data with rasters based on matching dates associated with points to dates associated with rasters. Specifically, we're thinking of the points as species observations and the rasters as remotely sensed environmental layers, but they can represent any points and rasters with dates.
}
\details{
See Examples.
}
\examples{

r1 <- raster::raster(nrows=50, ncols=50, xmn=-50, xmx=50)
raster::values(r1)<- runif(n = (50*50))
r2 <-  raster::raster(nrows=50, ncols=50, xmn=-50, xmx=50)
raster::values(r2)<- runif(n = (50*50))
env <-  raster::stack(r1,r2)
names(env) <- c("1995","1996")
datedOccs <- data.frame(cbind(c(0,10), c(-10,15)))
colnames(datedOccs) <- c("long", "lat")
datedOccs$date <- c("1995", "1996")
datedOccs$date <- lubridate::parse_date_time(datedOccs$date, orders = c("Y", "Ym"))
sp::coordinates(datedOccs) <- c("long", "lat")
raster::projection(datedOccs) <-  raster::projection(env)
dateScale = "year"
envDates <- c("1995","1996")
annotate(datedOccs = datedOccs, env = env, envDates = envDates, dateScale = dateScale)

}
\author{
Cory Merow <cory.merow@gmail.com>,
}
