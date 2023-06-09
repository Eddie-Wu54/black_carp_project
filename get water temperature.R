library(raster)
library(ncdf4)
library(sp)
library(ggplot2)
library(dplyr)


#### Get spatial points ####
sp <- brick("futureStream/E2O_hist_1979_1985.nc", varname = "waterTemperature")


#### Import our specific locations ####
## Import location latitudes and longitudes
loc <- read.csv("tributary locations.csv")
str(loc)

lat<-loc$latitude
lon<-loc$longitude

# Turn them to spatial points
coords <- data.frame(longitude=lon, latitude=lat)
locations.spatial <- SpatialPoints(coords, proj4string = sp@crs)

#### Extract water temperature data ####
wt1 <- brick("futureStream/E2O_hist_1979_1985.nc", varname = "waterTemperature")
wt2 <- brick("futureStream/E2O_hist_1986_1995.nc", varname = "waterTemperature")
wt3 <- brick("futureStream/E2O_hist_1996_2005.nc", varname = "waterTemperature")

# Include all the temperature data in one data frame
weeklyWater <- extract(wt1, locations.spatial)
weeklyWater <- cbind(weeklyWater, extract(wt2, locations.spatial))
weeklyWater <- cbind(weeklyWater, extract(wt3, locations.spatial))

# Change from Kelvin to Celsius
M.weeklyWater <- weeklyWater - 273.15



#### Get the average week temperature throughout the year ####

## Create two data frames to store the average water temperature data and
## the standard deviation for each week of the year (1979-2005)
week.avg <- matrix(NA, 6, 52)
week.me <- matrix(NA, 6, 52)
confidence_level <- 0.95

# Use two for loops to calculate the average for each week over 27 years
for (i in 1:52) { # i index which week
  
  # sub setting that week for each year
  subset <- data.frame(M.weeklyWater[,i])
  
  for (j in 1:26) { # j index which year
    subset <- cbind(subset, M.weeklyWater[,i+52*j])
  }
  # Calculate the mean
  week.avg[,i] <- rowSums(subset)/27
  # Calculate the margin of error (for confidence interval)
  for (y in 1:6) { # y index each location
    standard_error <- sd(subset[y,]) / sqrt(27)
    week.me[y,i] <- abs(qnorm((1 - confidence_level) / 2)) * standard_error
  }
}



#### Save each location separately ####
# Clean the data so that we have a data frame for each river location

## St Louis River ##
time <- seq(from=1, to=52)
st.louis.mod <- as.data.frame(t(rbind(time, week.avg[1,], week.me[1,])))

# Add the lower and upper confidence intervals
st.louis.mod <- cbind(st.louis.mod, st.louis.mod$V2 - st.louis.mod$V3)
st.louis.mod <- cbind(st.louis.mod, st.louis.mod$V2 + st.louis.mod$V3)

# Rename the data frame
colnames(st.louis.mod) <- c("weeks", "temperature.avg", "temperature.me",
                        "lower.CI", "upper.CI")

write.csv(st.louis.mod, "st_louis_model.csv")


## Saginaw River ##
saginaw.mod <- as.data.frame(t(rbind(time, week.avg[2,], week.me[2,])))

saginaw.mod <- cbind(saginaw.mod, saginaw.mod$V2 - saginaw.mod$V3)
saginaw.mod <- cbind(saginaw.mod, saginaw.mod$V2 + saginaw.mod$V3)
colnames(saginaw.mod) <- c("weeks", "temperature.avg", "temperature.me",
                           "lower.CI", "upper.CI")

write.csv(saginaw.mod, "water temperature clean/saginaw_model.csv")


## Fox River ##
fox.mod <- as.data.frame(t(rbind(time, week.avg[3,], week.me[3,])))

fox.mod <- cbind(fox.mod, fox.mod$V2 - fox.mod$V3)
fox.mod <- cbind(fox.mod, fox.mod$V2 + fox.mod$V3)
colnames(fox.mod) <- c("weeks", "temperature.avg", "temperature.me",
                       "lower.CI", "upper.CI")

write.csv(fox.mod, "water temperature clean/fox_model.csv")


## Portage-Burns Waterways ##
pb.mod <- as.data.frame(t(rbind(time, week.avg[4,], week.me[4,])))

pb.mod <- cbind(pb.mod, pb.mod$V2 - pb.mod$V3)
pb.mod <- cbind(pb.mod, pb.mod$V2 + pb.mod$V3)
colnames(pb.mod) <- c("weeks", "temperature.avg", "temperature.me",
                       "lower.CI", "upper.CI")

write.csv(pb.mod, "water temperature clean/pb_model.csv")


## Vermilion River ##
vermilion.mod <- as.data.frame(t(rbind(time, week.avg[5,], week.me[5,])))

vermilion.mod <- cbind(vermilion.mod, vermilion.mod$V2 - vermilion.mod$V3)
vermilion.mod <- cbind(vermilion.mod, vermilion.mod$V2 + vermilion.mod$V3)
colnames(vermilion.mod) <- c("weeks", "temperature.avg", "temperature.me",
                      "lower.CI", "upper.CI")

write.csv(vermilion.mod, "water temperature clean/vermilion_model.csv")


## Genesee River ##
genesee.mod <- as.data.frame(t(rbind(time, week.avg[6,], week.me[6,])))

genesee.mod <- cbind(genesee.mod, genesee.mod$V2 - genesee.mod$V3)
genesee.mod <- cbind(genesee.mod, genesee.mod$V2 + genesee.mod$V3)
colnames(genesee.mod) <- c("weeks", "temperature.avg", "temperature.me",
                             "lower.CI", "upper.CI")

write.csv(genesee.mod, "water temperature clean/genesee_model.csv")




## Plotting
ggplot(st.louis, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  theme_bw()
  


## Notes for the for loops

subset <- data.frame(M.weeklyWater[,1])
subset <- cbind(subset, M.weeklyWater[,1+52*1])
subset <- cbind(subset, M.weeklyWater[,1+52*2])
subset

confidence_level <- 0.95
standard_error <- sd(subset[1,]) / sqrt(3)
week.me[y,i] <- qnorm((1 - confidence_level) / 2) * standard_error

qnorm((1 - confidence_level) / 2) * standard_error

qnorm(0.05/2)
qnorm((1 - 0.95)/2)

M.weeklyWater[,52] # j = 0
M.weeklyWater[,52 + 52*(1)] # j = 1
M.weeklyWater[,52 + 52*(2)] # j = 2
M.weeklyWater[,52 + 52*(25)] # j = 3










## Calculate annual temperature (optional)
annual <- rowSums(weeklyWater.C, na.rm = TRUE)/364
annual <- data.frame(annual)
write.csv(annual, file = "waterT_test.csv")
