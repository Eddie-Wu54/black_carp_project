#' This script is used to calculate the coordinates of geo centers.

## Import data (newly-added locations)

new.loc <- read.csv("new location data.csv")

# Convert all lat/longs to radians
new.loc[,2:9] <- (new.loc[,2:9])*(pi/180)
View(new.loc)


#### Calculate X, Y and Z for each location ####

# Make empty data frame for X, Y, and Z
loc.name <- new.loc$Location

Xn <- NA; Xs <- NA; Xe <- NA; Xw <- NA
Xi <- data.frame(loc.name,Xn,Xe,Xs,Xw)
head(Xi)

Yn <- NA; Ys <- NA; Ye <- NA; Yw <- NA
Yi <- data.frame(loc.name,Yn,Ye,Ys,Yw)
head(Yi)

Zn <- NA; Zs <- NA; Ze <- NA; Zw <- NA
Zi <- data.frame(loc.name,Zn,Ze,Zs,Zw)
head(Zi)


# Calculate X
for(i in 1:length(Xi$loc.name)){
  #north
  Xi[i,2] <- cos(new.loc[i,2])*cos(new.loc[i,3])
  #east
  Xi[i,3] <- cos(new.loc[i,4])*cos(new.loc[i,5])
  #south
  Xi[i,4] <- cos(new.loc[i,6])*cos(new.loc[i,7])
  #west
  Xi[i,5] <- cos(new.loc[i,8])*cos(new.loc[i,9])
}
head(Xi)


# Calculate Y
for(i in 1:length(Yi$loc.name)){
  #north
  Yi[i,2] <- cos(new.loc[i,2])*sin(new.loc[i,3])
  #east
  Yi[i,3] <- cos(new.loc[i,4])*sin(new.loc[i,5])
  #south
  Yi[i,4] <- cos(new.loc[i,6])*sin(new.loc[i,7])
  #west
  Yi[i,5] <- cos(new.loc[i,8])*sin(new.loc[i,9])
}
head(Yi)


# Calculate Z
for(i in 1:length(Zi$loc.name)){
  #north
  Zi[i,2] <- sin(new.loc[i,2])
  #east
  Zi[i,3] <- sin(new.loc[i,4])
  #south
  Zi[i,4] <- sin(new.loc[i,6])
  #west
  Zi[i,5] <- sin(new.loc[i,8])
}
head(Zi)


# Take averages for each X,Y,Z
Xavg <- rowMeans(Xi[,2:5])
Yavg <- rowMeans(Yi[,2:5])
Zavg <- rowMeans(Zi[,2:5])

# Convert back to cartesian coordinates
hypo <- sqrt((Xavg*Xavg)+(Yavg*Yavg))
head(hypo)

Lat<-atan2(Zavg,hypo)
Long<-atan2(Yavg,Xavg)

# Convert back to degrees (round to 4 decimal places)
Latitude <- round(Lat*(180/pi), 4)
Longitude <- round(Long*(180/pi), 4)
str(Latitude)

new <- as.data.frame(cbind(loc.name, Latitude, Longitude))
head(new)
write.csv(new, file = "new location coordinates.csv")

