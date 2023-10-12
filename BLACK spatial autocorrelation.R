#' This script is used to examine the spatial auto-correlation between
#' the test locations. Both global moran's I and local moran's I (correlog plots)
#' are used.


library(gstat)
library(ggplot2)
library(dplyr)
library(spdep)
library(sp)
library(nlme)
library(ape)
library(MuMIn)
library(raster)
library(ncf) # for the correlog function


#### Import data ####

location <- read.csv("location_no_temps.csv")
location <- unique(location) # remove duplicating locations

# Import and clean the data
carp <- read.csv("eddie_carp_new.csv")
carp$condition <- as.factor(carp$condition)
carp.r <- carp %>% filter(!row_number() == 5) %>% filter(sex != "male")


## Download one file to get the spatial points
# (this defines what projection to use when converting to spatial objects)
tmin.1979 <- brick("cpc/tmin.1979.nc", varname = "tmin")
tmin.1979 <- rotate(tmin.1979)



#### Global moran's test (entire dataset) ####

## Make spatial dataframe
coords <- data.frame("long"=location[,3],"lat"=location[,2])
df <- data.frame(a = 1:nrow(location[3]))
spatial.data <- SpatialPointsDataFrame(coords,df,proj4string = tmin.1979@crs)

# Get a distance matrix from all points
dists <- spDists(spatial.data, longlat = TRUE)


## Examine the spatial autocorrelation on regression residuals

# Get residuals from whole dataset for each variable (use log AAM)
lm.annual <- lm(log(carp.r$AAM)~carp.r$AnnualTemp) # Annual temperature
lm.cold <- lm(log(carp.r$AAM)~carp.r$ColdTemp) # Cold temperature
lm.warm <- lm(log(carp.r$AAM)~carp.r$WarmTemp) # Warm temperature
lm.gdd <- lm(log(carp.r$AAM)~carp.r$average_gdd_0) # Growing degree day

summary(lm.annual)
summary(lm.cold)
summary(lm.warm) # no significant relationship there
summary(lm.gdd)


# Run the Moran.I test on the residuals
Moran.annual <- Moran.I(lm.annual$residuals, dists)
Moran.cold <- Moran.I(lm.cold$residuals, dists)
Moran.gdd <- Moran.I(lm.gdd$residuals, dists)

Moran.annual
Moran.cold
Moran.gdd




#### Local moran's test (entire dataset) ####

## For annual temperature
png("annual residuals corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))

# Use the coorelog function to develop the relationship
test <- correlog(coords$long, coords$lat, lm.annual$residuals,
                 increment=50, resamp=500, latlon=T)

# Plot with the entire distance range
plot(test, main="Annual Average Temperature Regression Residuals")
abline(h=0)
text(17400, min(test$correlation)+1, "A", cex=1.5)

# Reduce the distance range to 2500 km
plot(test, main="", xlim=c(0,2500))
abline(h=0)
text(2500, min(test$correlation)+1, "B", cex=1.5)
dev.off()


## For cold temperature
png("cold residuals corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))

# Use the coorelog function to develop the relationship
test <- correlog(coords$long, coords$lat, lm.cold$residuals,
                 increment=50, resamp=500, latlon=T)

# Plot with the entire distance range
plot(test, main="Cold Quarter Temperature Regression Residuals")
abline(h=0)
text(17400, min(test$correlation)+1, "A", cex=1.5)

# Reduce the distance range to 2500 km
plot(test, main="", xlim=c(0,2500))
abline(h=0)
text(2500, min(test$correlation)+1, "B", cex=1.5)
dev.off()



#### Variogram ####

## Create a data frame to store the necessary information
vario.data <- data.frame(residuals = lm.annual$residuals,
                         x = coords$long, y = coords$lat)

# Turn them to spatial points
geo.spatial<-SpatialPoints(coords, proj4string = tmin.1979@crs)

# OR

# Use another method for turning them into spatial object
spdf <- SpatialPointsDataFrame(coords = coords, data = vario.data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


## Variogram
vario <- variogram(vario.data$residuals~1, data = geo.spatial)
vario
plot(vario)

# Fit a model
lzn.fit <- fit.variogram(vario, model=vgm(1, "Sph", 900, 1)) # fit model
plot(vario, lzn.fit) # plot the sample values, along with the fit model




#### Local moran's test after removing the SU data point ####
carp.r.c <- carp.r %>% filter(!row_number() == 20)

## For annual temperature
png("annual residuals corr SU removal.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
lm.annual <- lm(log(carp.r.c$AAM)~carp.r.c$AnnualTemp)

# Use the coorelog function to develop the relationship
test <- correlog(carp.r.c$longitude, carp.r.c$latitude, lm.annual$residuals,
                 increment=50, resamp=500, latlon=T)

# Plot with the entire distance range
plot(test, main="Annual Average Temperature Regression Residuals Without South Ukraine")
abline(h=0)

# Reduce the distance range to 2500 km
plot(test, main="", xlim=c(0,2500))
abline(h=0)

dev.off()


## For cold temperature
png("cold residuals corr SU removal.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
lm.cold <- lm(log(carp.r.c$AAM)~carp.r.c$ColdTemp)

# Use the coorelog function to develop the relationship
test <- correlog(carp.r.c$longitude, carp.r.c$latitude, lm.cold$residuals,
                 increment=50, resamp=500, latlon=T)

# Plot with the entire distance range
plot(test, main="Cold Quarter Temperature Regression Residuals Without South Ukraine")
abline(h=0)

# Reduce the distance range to 2500 km
plot(test, main="", xlim=c(0,2500))
abline(h=0)

dev.off()



#### Local moran's test for the interaction model ####

# Define the models
lm.annual.int <- lm(log(AAM)~AnnualTemp*condition, data = carp.r.c) # Annual temperature
lm.cold.int <- lm(log(AAM)~ColdTemp*condition, data = carp.r.c) # Cold temperature

summary(lm.annual.int)
summary(lm.cold.int)


## Correlogram for cold temperature
png("cold residuals corr interaction.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))

# Use the coorelog function to develop the relationship
test <- correlog(carp.r.c$longitude, carp.r.c$latitude, lm.cold.int$residuals,
                 increment=50, resamp=500, latlon=T)

# Plot with the entire distance range
plot(test, main="Cold Quarter Temperature Regression Residuals Without South Ukraine")
abline(h=0)

# Reduce the distance range to 2500 km
plot(test, main="", xlim=c(0,2500))
abline(h=0)

dev.off()
