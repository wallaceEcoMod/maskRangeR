# Testing the annoting function for different time scales
# this could be turned into formal tests 
library(maskRanger)
library(raster)
library(lubridate)
dataDir='/Users/ctg/Dropbox/Projects/Wallace/maskRanger_Misc/UseCaseData/dataDriven/AmbyOpac'

datedOccs=read.csv(paste0(dataDir,'/datedOccsAmby.csv'))
potentialDist=raster(paste0(dataDir,'/AmbyOpac.tif'))
month.f=list.files(paste0(dataDir,'/Monthly'),full.names=T)
day.f=list.files(paste0(dataDir,'/Daily'),full.names=T)


monthEnv=stack(month.f)
monthEnvDates=parse_date_time(substring(tools::file_path_sans_ext(basename(month.f)),2),
                              orders = "%y%m%d")
dayEnv=stack(day.f)
dayEnvDates=parse_date_time(substring(tools::file_path_sans_ext(basename(day.f)),2),
                              orders = "%y%m%d")

#format occurrence dates
datedOccs$date=paste(datedOccs$year,datedOccs$month,datedOccs$day,sep='_')
datedOccs$date=parse_date_time(datedOccs$date,orders = "%y%m%d")
datedOccs=datedOccs[,c('x','y','date')]
sp::coordinates(datedOccs)=c('x','y')
projection(datedOccs)=projection(monthEnv)

datedOccsM=annotate(datedOccs,env=monthEnv,envDates=monthEnvDates,dateScale='month')
datedOccsM@data

datedOccsD=annotate(datedOccs,env=dayEnv,envDates=dayEnvDates,dateScale='day')
datedOccsD@data
