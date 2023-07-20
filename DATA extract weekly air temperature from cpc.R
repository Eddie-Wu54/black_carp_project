#' This script:
#' 
#' 1. Get the weekly average temperature data from the cpc dataset
#' 2. Calculate the weekly average water temperature data using Mohseni et al., 1999
#' 3. Obtain the annual average water temperature from 1979-2022
#' 4. Obtain the COLD QUARTER water temperature () from 1979-2022


library(raster)
library(ncdf4)
library(sp)


#### Define the function for water-air temperature relationship ####
# Equation parameters obtained from Mohseni et al. 1999
a <- 26.2
b <- 13.3
y <- 0.18
u <- 0.8

# Define a function for water-air temperature relationship
water_air <- function(x) {
  u + (a-u)/(1+exp(y*(b-x)))
}


#### Download the 1979 file to get the spatial points ####
tmin.1979 <- brick("cpc/tmin.1979.nc", varname = "tmin")
tmin.1979 <- rotate(tmin.1979)


#### Import our specific locations ####

## Import location latitudes and longitudes
loc <- read.csv("location_no_temps.csv")
str(loc)

lat<-loc$Latitude
lon<-loc$Longitude

# Turn them to spatial points
coords <- data.frame(longitude=lon, latitude=lat)
locations <- SpatialPoints(coords, proj4string = tmin.1979@crs)


#### CALCULATION STARTS HERE ####

# Create a data frame to store the final results
results <- data.frame(matrix(NA, nrow = 28, ncol = 45))
results[,1] <- loc$Location

results.cold <- data.frame(matrix(NA, nrow = 28, ncol = 45))
results.cold[,1] <- loc$Location

# We need to run the analysis for 44 years
year <- seq(1979,2022,1)

# Use a for loop to circulate through 1979-2022
for (ya in 1:44) {

  #### Step 1: Calculate the weekly air temperature from cpc ####
  
  # Create a data frame to store the weekly temperature for one year
  week <- matrix(NA, length(loc$Loc), 52)
  rownames(week) <- loc$Loc
  
  # Download tmax
  tmax.name <- paste0("cpc/tmax.",year[ya],".nc")
  tmax <- brick(tmax.name, varname = "tmax")
  tmax <- rotate(tmax)
  # Extract tmax
  max <- extract(tmax,locations,layer=1)
  # Download tmin
  tmin.name <- paste0("cpc/tmin.",year[ya],".nc")
  tmin <- brick(tmin.name, varname = "tmin")
  tmin <- rotate(tmin)
  # Extract tmin
  min <- extract(tmin,locations,layer=1)
  
  day.median <- (max+min)/2
  
  ## Get the weekly average temperature data for one year
  for (l in 1:length(loc$Loc)) {
    # l is the index for each location point
    k <- 1 # use k to track the index of the current week
    for (j in 1:52) {
      # j is the index for each week
      week[l,j] <- mean(day.median[l,k:(k+6)], na.rm = TRUE)
      k <- k + 7
    }
  }
  
  
  #### Step 2: Obtain the water temperature from air temperature ####
  
  # Convert the weekly air temperature matrix to a data frame
  air.weekly <- as.data.frame(week)
  
  # Apply the function to the air temperature
  water.weekly <- sapply(air.weekly, FUN = water_air)
  
  # Get the average water temperature for each location
  water.avg <- rowSums(water.weekly, na.rm = TRUE)/52
  results[,ya+1] <- water.avg
  
  # Get the average water temperature for only the COLD QUARTER (12,1,2)
  #water.avg.cold <- rowSums(water.weekly[ , c(1,2,3,4,5,6,7,8,49,50,51,52)], 
  #                          na.rm=TRUE)/12
  #results.cold[,ya+1] <- water.avg.cold
  
  # Clear temporary files
  removeTmpFiles(h=0.001)
}


write.csv(results, "water.csv")
write.csv(results.cold, "waterCold.csv")
