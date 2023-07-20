#' This script is used to conduct individual and grouped species regression for
#' all Asian carp species. We use sub-sampling to determine
#' 
#' 1. if for each species (or combined), the slope is significantly different between
#'    artificial and natural conditions.
#' 
#' 2. if the results for asian carp are similiar to that of black carp.

library(dplyr)
library(ggplot2)
library(ggfortify)

## Import data
asian.carp <- read.csv("asian carp final.csv")
asian.carp$Condition <- as.factor(asian.carp$Condition)
str(asian.carp)

## Separate by species
Grass <- asian.carp[asian.carp$Species=="Grass",]
Bighead <- asian.carp[asian.carp$Species=="Bighead",]
Silver <- asian.carp[asian.carp$Species=="Silver",]
Big.sil <- rbind(Bighead, Silver) # combine the two groups

## Create a data frame to store all the final results
results.final <- matrix(NA,3,6)
colnames(results.final) <- c("slope.artificial", "intercept.artificial", 
                             "slope.natural", "intercept.natural",
                             "p value", "R^2")
rownames(results.final) <- c("asian", "big and silver", "grass")



#### Grouped species regression ####

## Keep only the Natural and Artificial conditions
asian.carp.clean <- asian.carp %>% 
  filter(Condition %in% c("natural", "artificial"))


## Preliminary analyses
# Plot the results (did not account for spatial autocorrelation)
ggplot(asian.carp.clean, aes(x = AnnualTemp, y = log(AAM), color = Condition))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()

# Pure ANCOVA without accounting for spatial autocorrelation
# Interaction model
asian.mod <- lm(log(AAM)~AnnualTemp*Condition, data = asian.carp.clean)
anova(asian.mod)
summary(asian.mod)
autoplot(asian.mod)


# Linear addictive model
asian.linear <- lm(log(AAM)~AnnualTemp+Condition, data = asian.carp.clean)
anova(asian.linear)
summary(asian.linear)
autoplot(asian.linear)

asian <- lm(log(AAM)~AnnualTemp, data = asian.carp.clean)
summary(asian)

AIC(asian.mod)
AIC(asian.linear)
AIC(asian)


## Create a matrix to store the results over the sub-sampling
results.raw <- matrix(NA,100,6)
colnames(results.raw) <- c("slope.artificial", "intercept.artificial", 
                           "slope.natural", "intercept.natural",
                           "p value", "R^2")


## Sub-sampling to avoid spatial autocorrelation
table(asian.carp.clean$Code)

# Sub-sample and run regression for 1000 times
for(i in 1:nrow(results.raw)) {
  # sub-sample by location
  sub <- asian.carp.clean %>% group_by(Code) %>% sample_n(size=1)
  # run regression
  reg <- lm(log(sub$AAM)~sub$AnnualTemp*sub$Condition)
  values <- summary(reg)
  results.raw[i,1]<-values$coef[2,1] # slope for artificial
  results.raw[i,2]<-values$coef[1,1] # intercept for artificial
  results.raw[i,3]<-values$coef[2,1] + values$coef[4,1] # slope for natural
  results.raw[i,4]<-values$coef[1,1] + values$coef[3,1] # intercept for natural
  results.raw[i,5]<-values$coef[2,4] # p value for the AAM ~ temp relationship
  results.raw[i,6]<-values$adj.r.squared # R2
}

# Take the mean of only the unique possibilities for slope, intercept, R2
results.final[1,1] <- mean(unique(results.raw[,"slope.artificial"]))
results.final[1,2] <- mean(unique(results.raw[,"intercept.artificial"]))
results.final[1,3] <- mean(unique(results.raw[,"slope.natural"]))
results.final[1,4] <- mean(unique(results.raw[,"intercept.natural"]))
results.final[1,6] <- mean(unique(results.raw[,"R^2"]))

# Count the number of times when p value is greater than 0.05
results.final[1,5] <- sum(results.raw[,"p value"] > 0.05)

summary(reg)

## Plot the two lines
# Create two functions corresponding to the lines
func.natural <- function(x) { results.line[1,1] * x + results.line[1,2] }
func.artificial <- function(x) { results.line[1,1] * x + results.line[1,2] }

curve(func.natural, from = 0, to = 30, col = 2)
curve(func.artificial, from = 0, to = 30, col = 3, add = TRUE)



#### Individual species regression (Silver and bighead carp) ####

Big.sil <- Big.sil %>% 
  filter(Condition %in% c("natural", "artificial"))


## Preliminary plotting (with no autocorrelation)
ggplot(Big.sil, mapping = aes(x = AnnualTemp, y = log(AAM), color = Condition))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()


## Create a matrix to store the results over the sub-sampling
results.raw <- matrix(NA,100,6)
colnames(results.raw) <- c("slope.artificial", "intercept.artificial", 
                           "slope.natural", "intercept.natural",
                           "p value", "R^2")


## Sub-sampling to avoid spatial autocorrelation
table(Big.sil$Code)

# Sub-sample and run regression for 1000 times
for(i in 1:nrow(results.raw)) {
  # sub-sample by location
  sub <- Big.sil %>% group_by(Code) %>% sample_n(size=1)
  # run regression
  reg <- lm(log(sub$AAM)~sub$AnnualTemp*sub$Condition)
  values <- summary(reg)
  results.raw[i,1]<-values$coef[2,1] # slope for artificial
  results.raw[i,2]<-values$coef[1,1] # intercept for artificial
  results.raw[i,3]<-values$coef[2,1] + values$coef[4,1] # slope for natural
  results.raw[i,4]<-values$coef[1,1] + values$coef[3,1] # intercept for natural
  results.raw[i,5]<-values$coef[2,4] # p value
  results.raw[i,6]<-values$adj.r.squared # R2
}

# Take the mean of only the unique possibilities for slope, intercept, R2
results.final[2,1] <- mean(unique(results.raw[,"slope.artificial"]))
results.final[2,2] <- mean(unique(results.raw[,"intercept.artificial"]))
results.final[2,3] <- mean(unique(results.raw[,"slope.natural"]))
results.final[2,4] <- mean(unique(results.raw[,"intercept.natural"]))
results.final[2,6] <- mean(unique(results.raw[,"R^2"]))

# Count the number of times when p value is greater than 0.05
results.final[2,5] <- sum(results.raw[,"p value"] > 0.05)

## Pure ANCOVA on the non-autocorrelated data
ann.big <- lm(log(AAM)~AnnualTemp*Condition, data = Big.sil)
anova(ann.big)
summary(ann.big)
autoplot(ann.big)



#### Individual species regression (Grass carp) ####

Grass <- Grass %>% 
  filter(Condition %in% c("natural", "artificial"))


## Preliminary plotting (with no autocorrelation)
ggplot(Grass, mapping = aes(x = AnnualTemp, y = log(AAM), color = Condition))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()


## Create a matrix to store the results over the sub-sampling
results.raw <- matrix(NA,100,6)
colnames(results.raw) <- c("slope.artificial", "intercept.artificial", 
                           "slope.natural", "intercept.natural",
                           "p value", "R^2")


## Sub-sampling to avoid spatial autocorrelation
table(Grass$Code)

# Sub-sample and run regression for 1000 times
for(i in 1:nrow(results.raw)) {
  # sub-sample by location
  sub <- Grass %>% group_by(Code) %>% sample_n(size=1)
  # run regression
  reg <- lm(log(sub$AAM)~sub$AnnualTemp*sub$Condition)
  values <- summary(reg)
  results.raw[i,1]<-values$coef[2,1] # slope for artificial
  results.raw[i,2]<-values$coef[1,1] # intercept for artificial
  results.raw[i,3]<-values$coef[2,1] + values$coef[4,1] # slope for natural
  results.raw[i,4]<-values$coef[1,1] + values$coef[3,1] # intercept for natural
  results.raw[i,5]<-values$coef[2,4] # p value
  results.raw[i,6]<-values$adj.r.squared # R2
}

# Take the mean of only the unique possibilities for slope, intercept, R2
results.final[3,1] <- mean(unique(results.raw[,"slope.artificial"]))
results.final[3,2] <- mean(unique(results.raw[,"intercept.artificial"]))
results.final[3,3] <- mean(unique(results.raw[,"slope.natural"]))
results.final[3,4] <- mean(unique(results.raw[,"intercept.natural"]))
results.final[3,6] <- mean(unique(results.raw[,"R^2"]))

# Count the number of times when p value is greater than 0.05
results.final[3,5] <- sum(results.raw[,"p value"] > 0.05)



#### Sub-sampling for black carp ####


