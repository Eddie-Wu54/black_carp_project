library(ncdf4)
library(sp)
library(raster)

## Set working directory
setwd("C:/Users/eddie/Desktop/Waterloo/Thesis/Black carp data analysis/black carp temp")


## Make the outline of the url

# https://psl.noaa.gov/thredds/fileServer/Datasets/cpc_global_temp/tmax.1979.nc

start <- "https://psl.noaa.gov/thredds/fileServer/Datasets/cpc_global_temp/"
end <- ".nc"


## The data contains two variables (tmax and tmin)
# for years 1979-2022
years<-seq(1979,2022,1)
var<-c("tmax.","tmin.") # periods added to make url easier


## Download all of the data files (took 2.5-3 hours)
for(i in 1:length(var)){
  for(j in 1:length(years)){
    myurl<-paste0(start,var[i],years[j],end)
    download.file(myurl,
                  destfile = paste0("C:/Users/eddie/Desktop/Waterloo/Thesis/Black carp data analysis/black carp temp/cpc/",
                                    var[i],years[j],end),mode="wb")
  }
}
