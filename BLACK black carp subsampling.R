

#' If we are to sub-sample at 250km, we would have two sets of auto-correlating points:
#' 
#' 1. Central China, Hongze Lake
#' 2. Ganjiang, Hukou, Pingjiang


library(dplyr)
library(ggplot2)
library(ggfortify)
library(ncf) # for correlog() function


## Import the black carp data
Black <- read.csv("eddie_carp_new.csv")
Black$condition <- as.factor(Black$condition)

# Clean the data
black.clean <- Black %>% filter(!row_number() == 5) %>% filter(sex != "male")
str(black.clean)

## Get the data for chinese mainland
black.china <- black.clean[black.clean$china == "y",]



#### ENTIRE DATASET: no sub-sampling at all ####

lm.annual <- lm(log(AAM)~AnnualTemp, data = black.clean)
summary(lm.annual)

lm.cold <- lm(log(AAM)~ColdTemp, data = black.clean)
summary(lm.cold)



#### ENTIRE DATASET: sub-sampling at 250 km ####
table(black.clean$spatial.code.250)

# Store the r2 values
r2.250.raw <- matrix(NA,10, 2)
colnames(r2.250.raw) <- c("annual","cold")


## Check the local moran's I for annual
png("annual residuals corr sub250.png", width= 2404, height= 4000, units="px", res = 300)
par(mfrow=c(5,2),mar=c(4,4,2,2))

for (i in 1:10){
  sub <- black.clean %>% group_by(spatial.code.250) %>% sample_n(size=1)
  reg.sub.annual <- lm(log(sub$AAM)~sub$AnnualTemp)
  
  test <- correlog(sub$longitude, sub$latitude, reg.sub.annual$residuals,
                   increment=50, resamp=500, latlon=T)
  plot(test, main="", xlim=c(0,2500))
  abline(h=0)
  
  r2.250.raw[i,1] <- summary(reg.sub.annual)$adj.r.squared #get the r2
}

dev.off()


## Check the local moran's I for cold
png("cold residuals corr sub250.png", width= 2404, height= 4000, units="px", res = 300)
par(mfrow=c(5,2),mar=c(4,4,2,2))

for (i in 1:10){
  sub <- black.clean %>% group_by(spatial.code.250) %>% sample_n(size=1)
  reg.sub.cold <- lm(log(sub$AAM)~sub$ColdTemp)
  
  test <- correlog(sub$longitude, sub$latitude, reg.sub.cold$residuals,
                   increment=50, resamp=500, latlon=T)
  plot(test, main="", xlim=c(0,2500))
  abline(h=0)
  
  r2.250.raw[i,2] <- summary(reg.sub.cold)$adj.r.squared #get the r2
}

dev.off()



#### ENTIRE DATASET: sub-sampling at 550 km ####

r2.550.raw <- matrix(NA,10, 2)
colnames(r2.550.raw) <- c("annual","cold")

## Check the local moran's I for annual
png("annual residuals corr sub550.png", width= 2404, height= 4000, units="px", res = 300)
par(mfrow=c(5,2),mar=c(4,4,2,2))

for (i in 1:10){
  sub <- black.clean %>% group_by(spatial.code.550) %>% sample_n(size=1)
  reg.sub.annual <- lm(log(sub$AAM)~sub$AnnualTemp)
  
  test <- correlog(sub$longitude, sub$latitude, reg.sub.annual$residuals,
                   increment=50, resamp=500, latlon=T)
  plot(test, main="", xlim=c(0,2500))
  abline(h=0)
  
  r2.550.raw[i,1] <- summary(reg.sub.annual)$adj.r.squared #get the r2
}

dev.off()

## Check the local moran's I for cold
png("cold residuals corr sub550.png", width= 2404, height= 4000, units="px", res = 300)
par(mfrow=c(5,2),mar=c(4,4,2,2))

for (i in 1:10){
  sub <- black.clean %>% group_by(spatial.code.550) %>% sample_n(size=1)
  reg.sub.cold <- lm(log(sub$AAM)~sub$ColdTemp)
  
  test <- correlog(sub$longitude, sub$latitude, reg.sub.cold$residuals,
                   increment=50, resamp=500, latlon=T)
  plot(test, main="", xlim=c(0,2500))
  abline(h=0)
  
  r2.550.raw[i,2] <- summary(reg.sub.cold)$adj.r.squared #get the r2
}

dev.off()

# Get the r2 for the annual and cold
mean(unique(r2.550.raw[,1]))
mean(unique(r2.550.raw[,2]))


## Compare to the original r2 value before and after sub-sampling
r2 <- data.frame(
  Time = c("Original", "Sub 250","Sub 550"),
  Annual = c(summary(lm.annual)$adj.r.squared, mean(unique(r2.250.raw[,1])),
             mean(unique(r2.550.raw[,1]))),
  Cold = c(summary(lm.cold)$adj.r.squared, mean(unique(r2.250.raw[,2])),
           mean(unique(r2.550.raw[,2])))
)

#### CHINA DATASET: no subsampling at all ####

# Run the models
lm.annual <- lm(log(AAM)~AnnualTemp, data = black.china)
summary(lm.annual)

lm.cold <- lm(log(AAM)~ColdTemp, data = black.china)
summary(lm.cold)


## For annual temperature
png("annual residuals corr china.png", width= 2404, height= 800, units="px", res = 300)
par(mfrow=c(1,1),mar=c(4,4,2,2))

# Use the coorelog function to develop the relationship
test <- correlog(black.china$longitude, black.china$latitude, lm.annual$residuals,
                 increment=50, resamp=500, latlon=T)

# Plot with the entire distance range
plot(test, main="Annual Average Temperature Regression Residuals")
abline(h=0)
text(17400, min(test$correlation)+1, "A", cex=1.5)
dev.off()


## For cold temperature
png("cold residuals corr china.png", width= 2404, height= 800, units="px", res = 300)
par(mfrow=c(1,1),mar=c(4,4,2,2))

# Use the coorelog function to develop the relationship
test <- correlog(black.china$longitude, black.china$latitude, lm.cold$residuals,
                 increment=50, resamp=500, latlon=T)

# Plot with the entire distance range
plot(test, main="Cold Quarter Temperature Regression Residuals")
abline(h=0)
text(17400, min(test$correlation)+1, "A", cex=1.5)
dev.off()



#### CHINA DATASET: sub-sample at 250 km ####

## RUN annual for 5 times
png("annual residuals corr china sub.png", width= 2404, height= 4000, units="px", res = 300)
par(mfrow=c(5,1),mar=c(4,4,2,2))

for (i in 1:5){
  sub <- black.china %>% group_by(spatial.code.250) %>% sample_n(size=1)
  reg.sub.annual <- lm(log(sub$AAM)~sub$AnnualTemp)
  
  test <- correlog(sub$longitude, sub$latitude, reg.sub.annual$residuals,
                   increment=50, resamp=500, latlon=T)
  plot(test, main="")
  abline(h=0)
}

dev.off()


## RUN cold for 5 times
png("cold residuals corr china sub.png", width= 2404, height= 4000, units="px", res = 300)
par(mfrow=c(5,1),mar=c(4,4,2,2))

for (i in 1:5){
  sub <- black.china %>% group_by(spatial.code.250) %>% sample_n(size=1)
  reg.sub.cold <- lm(log(sub$AAM)~sub$ColdTemp)
  
  test <- correlog(sub$longitude, sub$latitude, reg.sub.cold$residuals,
                   increment=50, resamp=500, latlon=T)
  plot(test, main="")
  abline(h=0)
}

dev.off()


table(black.china$spatial.code.250)


