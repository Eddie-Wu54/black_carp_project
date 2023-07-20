#' This script is used to extract relevant location-specific maximum and minimum
#' temperatures from the .nc cpc files, and to calculate the growing degree day.


library(raster)
library(ncdf4)
library(sp)


#### Download the 1979 file to get the spatial points ####
tmin.1979 <- brick("cpc/tmin.1979.nc", varname = "tmin")
tmin.1979 <- rotate(tmin.1979)
plot(tmin.1979$X1979.01.01)


#### Import our specific locations ####

## Import location latitudes and longitudes
loc <- read.csv("location_no_temps.csv")
str(loc)

lat<-loc$Latitude
lon<-loc$Longitude

# Turn them to spatial points
coords <- data.frame(longitude=lon, latitude=lat)
locations <- SpatialPoints(coords, proj4string = tmin.1979@crs)

# Create a new dataframe to store all the degree day data from 1979-2022 for all
# these locations (44 years of data)
degree_day <- matrix(NA, length(loc$Loc), 45)
degree_day[,1] <- loc$Loc


#### Extracting points for degree days base 0 ####
years <- seq(1979,2022,1)

for(i in 1:44){
  # Download tmax
  tmax.name <- paste0("cpc/tmax.",years[i],".nc")
  tmax <- brick(tmax.name, varname = "tmax")
  tmax <- rotate(tmax)
  # Extract tmax
  max <- extract(tmax,locations,layer=1)
  # Download tmin
  tmin.name <- paste0("cpc/tmin.",years[i],".nc")
  tmin <- brick(tmin.name, varname = "tmin")
  tmin <- rotate(tmin)
  # Extract tmin
  min <- extract(tmin,locations,layer=1)
  
  # Calculate the degree days
  degree <- (max+min)/2 - 10
  degree[degree < 0] <- 0
  calculated.degrees <- rowSums(degree, na.rm = TRUE)
  degree_day[,i+1] <- as.numeric(calculated.degrees)
  degree_day[]
  removeTmpFiles(h=0.001)
}

# Rename all the columns with years
degree_day <- as.data.frame(degree_day)
for (i in 1:44) {
  colnames(degree_day)[i+1] <- years[i]
}
View(degree_day)

## Export data
write.csv(degree_day, "location_degree_day_raw.csv")
