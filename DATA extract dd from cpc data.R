#' This script is used to:
#' 
#' 1. extract relevant location-specific maximum and minimum temperatures from the .nc cpc files.
#' 2. calculate the growing degree day.
#' 3. calculate the number of days below 5 degrees annually.


library(raster)
library(ncdf4)
library(sp)
library(dplyr)


get_below5 <- function(temp){
  
  p <- numeric()
  below_count <- 0
  above_count <- 0
  start_index <- NULL
  end_index <- NULL
  already_started <- FALSE # to check whether the temp series has started
  has_value <- FALSE # to check whether the resulting list is empty
  
  for (i in 1:length(temp)) {
    t <- temp[i]
    
    if (t < 5) {
      below_count <- below_count + 1
      above_count <- 0
    } else {
      above_count <- above_count + 1
      below_count <- 0
    }
    
    # Check if we have 5 consecutive days below 5 degrees to START
    if (below_count >= 5 & !already_started) {
      already_started = TRUE
      start_index = i-4
    }
    
    # Check if we have 5 consecutive days above 5 degrees to END
    if (already_started & above_count >= 5) {
      end_index = i-5
      len = end_index - start_index + 1
      p <- c(p, len)
      has_value = TRUE
      # reset
      above_count = 0; below_count = 0
      start_index = NULL; end_index = NULL;
      already_started = FALSE
    }
  }
  
  # Check special cases
  if (!already_started & !has_value) { #never started
    return(0)
  } else if (already_started & !has_value) { #never ended
    len = i - start_index + 1
    return(len)
  } else if (already_started & has_value) { #started for multiple times, but never ends
    len = i - start_index + 1
    p <- c(p, len)
  }
  
  # Select the max from the p list
  return(max(p))
}
get_below8_3days <- function(temp){
  
  p <- numeric()
  below_count <- 0
  above_count <- 0
  start_index <- NULL
  end_index <- NULL
  already_started <- FALSE # to check whether the temp series has started
  has_value <- FALSE # to check whether the resulting list is empty
  
  for (i in 1:length(temp)) {
    t <- temp[i]
    
    if (t < 8) {
      below_count <- below_count + 1
      above_count <- 0
    } else {
      above_count <- above_count + 1
      below_count <- 0
    }
    
    # Check if we have 5 consecutive days below 5 degrees to START
    if (below_count >= 3 & !already_started) {
      already_started = TRUE
      start_index = i-2
    }
    
    # Check if we have 5 consecutive days above 5 degrees to END
    if (already_started & above_count >= 3) {
      end_index = i-3
      len = end_index - start_index + 1
      p <- c(p, len)
      has_value = TRUE
      # reset
      above_count = 0; below_count = 0
      start_index = NULL; end_index = NULL;
      already_started = FALSE
    }
  }
  
  # Check special cases
  if (!already_started & !has_value) { #never started
    return(0)
  } else if (already_started & !has_value) { #never ended
    len = i - start_index + 1
    return(len)
  } else if (already_started & has_value) { #started for multiple times, but never ends
    len = i - start_index + 1
    p <- c(p, len)
  }
  
  # Select the max from the p list
  return(max(p))
}


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


## Create a new dataframe to store all the degree day data from 1979-2022 for all
# these locations (44 years of data)
degree_day <- matrix(NA, length(loc$Location), 45)
degree_day[,1] <- loc$Loc


## Create a list to store the median dataframe for each year
median_list <- vector("list", 44)



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
  
  # Calculate median and store in a list
  median <- (max+min)/2
  median_list[[i]] <- median
  
  # Calculate the degree days
  degree <- (max+min)/2
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


## Degree day average for each location
degree_day_sum <- matrix(NA, length(loc$Location), 1)
rownames(degree_day_sum) <- loc$Location

# Use a loop for each location
for (i in 1:32) {
  sum <- sum(sapply(degree_day[i,][,-1], as.numeric))
  count <- rowSums(degree_day != 0)[i] - 1
  
  row_average <- sum/count
  degree_day_sum[i] <- row_average
}


## Export data
write.csv(degree_day, "location_degree_day_raw.csv")
write.csv(degree_day_sum, "location_degree_day_base0.csv")




#### Consecutive days below 5 degree ####

## Create a new dataframe to store the number of days below 5 degree from 1979-2022 for all
below_5 <- matrix(NA, length(loc$Location), 44)
below_5[,1] <- loc$Loc # first column is location name!!!


## Loop
for (locnum in 1:32) { # loop through each location (32 locs in total)
  
  for (y in 1:43) { # loop through the years
    
    year1 <- as.data.frame(median_list[[y]][locnum,])
    year2 <- as.data.frame(median_list[[y+1]][locnum,])
    colnames(year1) <- "airT"
    colnames(year2) <- "airT"
    combined <- rbind(year1, year2)
    # cut from Sep 1(#244) to Apr 30(#485) - 242 data points
    cut <- combined[244:485,] %>% na.omit()
    
    if(length(cut) == 0) {next} # make sure the entire list is not empty
    
    # get below 5 length and store in the results dataframe
    below_5[locnum, y+1] <- get_below5(cut)
  }
}


## Get the results

days.b5 <- data.frame(loc = loc$Loc,
                      med = apply(below_5[,-1], 1, median))

write.csv(days.b5,"below_5.csv")







