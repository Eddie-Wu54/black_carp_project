#' This script is used to calculate the distance between location points.
#' The output file is a matrix containing the pairwise location.

#' This script also determines which locations are within 250 km of the others,
#' allowing us to manually assign spatial codes to conduct spatial sub-sampling.

library(geosphere)
library(dplyr)


#### Calculate the distance for BLACK CARP ####

# Import Location data: latitudes and longitudes
loc <- read.csv("location_no_temps.csv")
loc <- unique(loc)

# Rearrange the data frame so that longitude is first, latitude is second
# (meets the requirement for fun=distHaversine)
loc <- data.frame(loc$Loc, loc$Longitude, loc$Latitude)
str(loc)
names(loc)<-c("Location","Longitude","Latitude")
View(loc)


## Calculate the distance
# Create a matrix of 20*20 for 20 locations
M <- matrix(NA, nrow(loc), nrow(loc))
colnames(M) <- loc$Location
rownames(M) <- loc$Location

# Calculate the distance
for(i in 1:nrow(loc)) {
  for(j in 1:nrow(loc)) {
    M[i,j] <- distm(loc[i,2:3],loc[j,2:3],fun=distHaversine)/1000
  }
}
View(M)

write.csv(M, file="location_distance.csv")


# Make a matrix to store only the distances we care about
N <- matrix(NA,nrow(loc),nrow(loc))
colnames(N) <- loc$Location
rownames(N) <- loc$Location
View(N)

# Only have the distances less than 250, others are returned as 0
for(i in 1:nrow(loc)) {
  for(j in 1:nrow(loc)) {
    if(M[i,j] <= 250) {
      N[i,j] <- M[i,j]
    } else {
      N[i,j] <- 0
    }
  }
}

# Gives the number of locations that are within 250km of another location
sum(colSums(N)!=0)
#' Only 2 pairs of locations have spatial autocorrelation. Central China & Hongze
#' Lake; Hukou County & Ganjiang.


# We want all the columns where the colSum is >0
Dist <- N[,colSums(N)>0]
Dist <- Dist[rowSums(Dist)>0,]
ncol(Dist) # matches the number of locations
