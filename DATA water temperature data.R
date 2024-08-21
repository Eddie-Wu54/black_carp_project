#' This script:
#' 
#' 1. Extracts the water temperature data from 23 unique locations for the black
#' carp analyses.
#' 
#' 2. The extracted temperature is in a weekly format. We calculated the annual
#' average water temperature (BIOL1) and the water temperature from the coldest
#' quarter (BIOL11) using the "dismo" package.


library(raster)
library(ncdf4)
library(sp)
library(ggplot2)
library(dplyr)
library(dismo)


#### Get spatial points ####
sp <- brick("futureStream/E2O_hist_1979_1985.nc", varname = "waterTemperature")


#### Import our specific locations ####
## Import location latitudes and longitudes
loc <- read.csv("location_no_temps.csv")
str(loc)

lat<-loc$Latitude
lon<-loc$Longitude

# Turn them to spatial points
coords <- data.frame(longitude=lon, latitude=lat)
locations.spatial <- SpatialPoints(coords, proj4string = sp@crs)


#### Extract water temperature data ####
wt1 <- brick("futureStream/E2O_hist_1979_1985.nc", varname = "waterTemperature")
wt2 <- brick("futureStream/E2O_hist_1986_1995.nc", varname = "waterTemperature")
wt3 <- brick("futureStream/E2O_hist_1996_2005.nc", varname = "waterTemperature")

# Include all the temperature data in one data frame (27 years)
weeklyWater <- extract(wt1, locations.spatial)
weeklyWater <- cbind(weeklyWater, extract(wt2, locations.spatial))
weeklyWater <- cbind(weeklyWater, extract(wt3, locations.spatial))

# Change from Kelvin to Celsius
M.weeklyWater <- weeklyWater - 273.15 # should have 1404 columns of data


#### Get the water temperature metrics ####

## Create a dataframe to store the results
results <- matrix(NA, 32, 2)
colnames(results) <- c("annual temp", "cold temp")


## For each location (each iteraction is one location)

for (loc.index in 1:32) { # loc.index refers to the location number

  #' temp.loc - 1040 rows (52 weeks over 27 years)
  #' weekly_avg_data - 52 rows (27-year average for 52 weeks)
  #' monthly data - 12 rows (27-year average monthly Max and Min)
  
  # Load the temperature data for that location
  temp.loc <- as.data.frame(M.weeklyWater[loc.index,])
  temp.loc[,"Week"] <- c(1:52) # add week number
  colnames(temp.loc) <- c("Temp", "Week")
  
  
  ## STEP 1: Calculate the 27-year average temperature for each week
  weekly_avg_data <- temp.loc %>%
    group_by(Week) %>%
    summarize(AvgValue = mean(Temp, na.rm = TRUE))
  
  
  # STEP 2: Get max and min water temp for each month
  # Import the template for 52 weeks
  template <- read.csv("water_week_template.csv")
  weekly_avg_data <- cbind(template, weekly_avg_data[,"AvgValue"])
  
  # Get the monthly maximum and minimum
  monthly_data <- as.data.frame(matrix(NA, 12, 3))
  colnames(monthly_data) <- c("month", "max", "min")
  
  for (i in 1:12) { # i indicates month number
    monthly_data[i,"month"] <- i
    
    # subset the specific months (those towards the start/end of each month were
    # counted in both months)
    sub <- weekly_avg_data[weekly_avg_data$monthstart == i | weekly_avg_data$monthend == i,]
    monthly_data[i,"max"] <- max(sub$AvgValue)
    monthly_data[i,"min"] <- min(sub$AvgValue)
  }
  
  
  ## STEP 3: Get the BIO variables (BIO1 and BIO11)
  prec <- c(0,0,0,0,0,0,0,0,0,0,0,0) # the precipitation values does not matter
  bio <- biovars(prec, tmin = monthly_data$min, tmax = monthly_data$max)
  
  results[loc.index,"annual temp"] <- bio[1]
  results[loc.index,"cold temp"] <- bio[11]
  
}


## Export the results
write.csv(results, "water temperature black carp.csv")





