#' This script 
#' 
#' 1. fit a VBGF from the observed Mississippi black carp catch data
#' obtained from Whitledge et al, 2022.
#' 
#' 2. fit a linear curve from the same dataset.
#' 
#' 3. compare both model fits with AIC and visualize the graphs.


{
library(FSA)
library(dplyr)
library(ggplot2)
library(nlstools)
library(optistock)
}


Missi <- read.csv("Mississippi.csv", stringsAsFactors = TRUE)
Missi <- filter(Missi, Annuli != 0) # Remove the age-0 data points
Missi.female <- dplyr::filter(Missi, Sex=="Female")
Missi.male <- dplyr::filter(Missi, Sex=="Male")


#### Obtain the VBGF paramaters #####

# Define the VBGF equation
vb <- vbFuns(param="Typical")

# Find starting values for the nls() function estimation
f.starts <- vbStarts(Tot.Length~Annuli, data=Missi)
f.starts

# Use nls() to estimate the parameters from observed data
vb.fit <- nls(Tot.Length~vb(Annuli,Linf,K,t0), data=Missi,
             start=f.starts)
coef(vb.fit)


## Get the confidence intervals for model prediction

# Bootstrap the model fitting process
boot_fit <- nlsBoot(vb.fit)
summary(vb.fit)
# Predict length at age from the model. t is age. Here, we tell R
# to predict the length at each unique age in our original data, and
# calculate some bootstrapped confidence intervals
vb.boot.preds <- data.frame(
  predict(boot_fit, vb, t = sort(unique(Missi$Annuli))))
names(vb.boot.preds) <- c("age", "vbfit", "lwr", "upr")


#### Fit with a linear regression ####

lm.fit <- lm(Tot.Length~Annuli,data = Missi)
summary(lm.fit)
confint(lm.fit, level = 0.95)

# Predict the length at age with the linear model
age <- data.frame(Annuli = sort(unique(Missi$Annuli)))
lm.preds <- data.frame(predict.lm(lm.fit, age))
names(lm.preds) <- "lmfit"

## Create a master data frame to store the age and predicted values from
# both models
predicted <- cbind(vb.boot.preds, lm.preds)

# Compare the two model fits with AIC
AIC(vb.fit, lm.fit)


#### Plot the curve ####

ggplot() +
  geom_jitter(data=Missi, aes(y=Tot.Length, x=Annuli),
              width = 0.1, alpha = 0.2, size = 2) +
  geom_line(data=predicted, aes(y=vbfit, x=age, color="vbfit")) +
  #geom_ribbon(data=predicted, aes(ymin=lwr, ymax=upr, x=age,
     #                              color = NULL), alpha = 0.3) +
  geom_line(data=predicted, aes(y=lmfit, x=age, color="lmfit")) +
  labs(x = "Age (years)", y = "Total length (mm)", color = "Legend")+
  theme_bw()


#### Get the age values at length = 800 and 1000 ####

# Our obtained VBGF curve
Linf <- as.numeric(coef(vb.fit)[1])
K <- as.numeric(coef(vb.fit)[2])
t0 <- as.numeric(coef(vb.fit)[3])

# For males
inv_vb(len=800, Linf, K, t0)

# For females
inv_vb(len=1000, Linf, K, t0)

