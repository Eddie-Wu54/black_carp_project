#' This script is used to examine the spatial auto-correlation between
#' the test locations, and to use spatial sub-sample to remove such effects.

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

geo <- cbind(carp.r$latitude, carp.r$longtitude)

## Calculate the critical threshold
knn1 <- knearneigh(geo)
k1 <- knn2nb(knn1)
critical.threshold <- max(unlist(nbdists(k1,coords)))
critical.threshold
# The threshold value is the minimum distance that gives each point
# at least one neighbor.

## Compute distance-band weights
dist.band <- dnearneigh(coords, 0, critical.threshold)
summary(dist.band)

nb <- nb2listw(dist.band, style="W")
nb
## Conduct the local moran's test

Lmoran.annual <- localmoran(carp.r$AAM, nb, zero.policy = TRUE)
Lmoran.annual.r <- localmoran.mc(carp.r$AAM, nb)

hist(Lmoran.annual[,5])


