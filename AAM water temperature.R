

# Equation parameters obtained from Mohseni et al. 1999
a <- 26.2
b <- 13.3
y <- 0.18
u <- 0.8

# Define a function for water-air temperature relationship
water_air <- function(x) {
  u + (a-u)/(1+exp(y*(b-x)))
}

## Import the data
air <- read.csv("weekly data2005.csv", stringsAsFactors = TRUE)
air.weekly <- air[,-1]
str(air.weekly)

water <- sapply(air.weekly, FUN = water_air)
