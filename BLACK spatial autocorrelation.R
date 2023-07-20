#' This script is used to examine the spatial auto-correlation between
#' the test locations. Both global moran's I and local moran's I are used.


library(dplyr)
library(spdep)
library(sp)
library(nlme)
library(ape)
library(MuMIn)


#### Import data ####

location <- read.csv("location_no_temps.csv")
location <- unique(location) # remove duplicating locations

# Clean the data
carp <- read.csv("eddie_carp_new.csv")
carp.r <- carp %>% 
  filter(sex != "male") %>% # keep only the female data points
  distinct(location, .keep_all = TRUE) # remove all repeating locations
str(carp.r)


#### Global moran's test ####

## Create a distance matrix
geo <- as.matrix(cbind(carp.r$latitude, carp.r$longtitude))
samples.dist <- as.matrix(spDists(geo, longlat = TRUE)) # dist() and spDists() produces same outcomes
samples.dist.inv <- 1/samples.dist
diag(samples.dist.inv) <- 0

## Examine the spatial autocorrelation on regression residuals

# Get residuals from whole dataset for each variable (use log AAM)
lm.annual <- lm(log(carp.r$AAM)~carp.r$AnnualTemp) # Annual temperature
lm.cold <- lm(log(carp.r$AAM)~carp.r$ColdTemp) # Cold temperature
lm.gdd <- lm(log(carp.r$AAM)~carp.r$average_gdd_0) # Growing degree day
summary(lm.annual)
summary(lm.cold)
summary(lm.gdd)

# Run the Moran.I test on the residuals
Moran.annual <- Moran.I(lm.annual$residuals, samples.dist.inv)
Moran.cold <- Moran.I(lm.cold$residuals, samples.dist.inv)
Moran.gdd <- Moran.I(lm.gdd$residuals, samples.dist.inv)

Moran.annual
Moran.cold
Moran.gdd



#### Local moran's test ####

# Create a data frame of the coordinates and AAM data
data.spatial <- data.frame(data = carp.r$AAM,
                     longitude=location$Longitude, latitude=location$Latitude)

# Convert to spatial object
coordinates(data.spatial) <- c("longitude", "latitude")
proj4string(data.spatial) <- CRS("+proj=longlat +datum=WGS84")


geo <- as.matrix(cbind(carp.r$longtitude, carp.r$latitude))


## Find the nearest neighbor distance for each location
nearest_distances <- apply(samples.dist, 1, function(row) min(row[row > 0]))
nearest_distances

threshold <- nearest_distances[18]
# The threshold value is the minimum distance that gives each point
# at least one neighbor.


## Compute distance-band weights
# (Both methods give the same result)

# METHOD 1: Using the spatial object for calculation
dist.band <- dnearneigh(data.spatial, d1 = 0, d2 = 9000)
summary(dist.band)

# METHOD 2: Using purely the geographical coordinates for calculation
dist.band.geo <- dnearneigh(geo, d1 = 0, d2 = 9000, longlat = TRUE)
summary(dist.band.geo)

nb <- nb2listw(dist.band, style="W")
nb
## {Problem of using what distance as identifying the neareast neightbouring?}


## Conduct the local moran's test

Lmoran.annual <- localmoran(carp.r$AAM, nb, zero.policy = TRUE)

# View the local moran I values and results
Lmoran.annual
summary(Lmoran.annual, geo)

# See the p value for the 20 locations
hist(Lmoran.annual[,5])






columbus <- st_read(system.file("shapes/columbus.shp", package="spData")[1], quiet=TRUE)
col.gal.nb <- read.gal(system.file("weights/columbus.gal", package="spData")[1])
coords <- coordinates(as(columbus, "Spatial"))
cards <- card(col.gal.nb)
col.w <- nb2listw(col.gal.nb)
