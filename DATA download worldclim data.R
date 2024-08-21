#' This script is used to:
#' 
#' 1. download the three temperature metrics from worldclim database for each location.


library(sp)
library(raster)


# Import data "location_no_temps.csv"
location <- read.csv("location_no_temps.csv")
str(location)

# Download climate data
climdata <- getData("worldclim", var="bio",res=5)

# We want to use annual temperature, average temp in the warmest and coldest quarter
# this is 1, 10 and 11
climdata <- climdata[[c(1,10,11)]]
names(climdata) <- c("AnnualTemp","WarmTemp","ColdTemp")

# Convert our coordinates to spatial points
lat <- location$Latitude
lon <- location$Longitude
coords <- data.frame("long"=lon, "lat"=lat)
# Convert to spatial points
points <- SpatialPoints(coords, proj4string = climdata@crs)
# Extract temperatures
values <- extract(climdata, points)

location <- cbind(location, values/10)
View(location)

# Export to a new excel sheet
write.csv(location, file = "locations_has_temps.csv")


