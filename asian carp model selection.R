#' This script is used to compare the difference models for Asian carp AAM under
#' two different conditions. We use:
#' 
#' 1. linear addictive model  2. Interaction model
#' 
#' on:
#' 
#' 1. Annual temperature  2. Cold temperature
#' 
#' We also check the Cook's distance to determine if outliers needs to be removed.


library(ggplot2)
library(ggfortify)
library(dplyr)


## Import data
asian.carp <- read.csv("asian carp final.csv")
asian.carp$Condition <- as.factor(asian.carp$Condition)
str(asian.carp)

Black <- read.csv("eddie_carp_new.csv")
Black$condition <- as.factor(Black$condition)
str(Black)

## Separate by species
Grass <- asian.carp[asian.carp$Species=="Grass",]
Bighead <- asian.carp[asian.carp$Species=="Bighead",]
Silver <- asian.carp[asian.carp$Species=="Silver",]
Big.sil <- rbind(Bighead, Silver) # combine the two groups



#### Asian carp all species ####

asian.carp.clean <- asian.carp %>% 
  filter(Condition %in% c("natural", "artificial"))

## Linear addictive models (same slope, different intercept)

asian.linear <- lm(log(AAM)~ColdTemp+Condition, data = asian.carp.clean)
anova(asian.linear)
summary(asian.linear)
autoplot(asian.linear)

plot(asian.linear, which = 5)


## Interaction models (ANCOVA: different slope, different intercept)
asian.int <- lm(log(AAM)~ColdTemp*Condition, data = asian.carp.clean)
anova(asian.int)
summary(asian.int)
autoplot(asian.int)

plot(asian.int, which = 5)


## Simple linear model (same slope, same intercept)

asian.simple <- lm(log(AAM)~ColdTemp, data = asian.carp.clean)
anova(asian.simple)
summary(asian.simple)
autoplot(asian.simple)

plot(asian.simple, which = 5)

## Compare the AIC scores
AIC(asian.linear)
AIC(asian.int)
AIC(asian.simple)

# Check the Cook's distance that are three times larger than the mean
cooksD <- cooks.distance(asian.simple)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential



#### Black carp ####

Black.clean <- Black %>% filter(!row_number() == 15) %>% filter(sex != "male")

Black.clean <- Black.clean[Black.clean$AAM != 11,]

## Linear addictive models (same slope, different intercept)

black.linear <- lm(log(AAM)~ColdTemp+condition, data = Black.clean)
anova(black.linear)
summary(black.linear)
autoplot(black.linear)

plot(black.linear, which = 5)


## Interaction models (ANCOVA: different slope, different intercept)
black.int <- lm(log(AAM)~ColdTemp*condition, data = Black.clean)
anova(black.int)
summary(black.int)
autoplot(black.int)

plot(black.int, which = 5)


## Simple linear model (same slope, same intercept)

black.simple <- lm(log(AAM)~ColdTemp, data = Black.clean)
anova(black.simple)
summary(black.simple)
autoplot(black.simple) # did not meet??

plot(black.simple, which = 5)

black <- lm(log(AAM)~ColdTemp:condition, data = Black.clean)
summary(black)
## Compare the AIC scores
AIC(black.linear)
AIC(black.int)
AIC(black.simple)
AIC(black)

hist(log(Black.clean$AAM))
mean(log(Black.clean$AAM)) + 3 * sd(log(Black.clean$AAM))
sd(log(Black.clean$AAM))


#### Grass carp ####

Grass.clean <- Grass %>% 
  filter(Condition %in% c("natural", "artificial"))

## Linear addictive models (same slope, different intercept)

grass.linear <- lm(log(AAM)~ColdTemp+Condition, data = Grass.clean)
anova(grass.linear)
summary(grass.linear)
autoplot(grass.linear)

plot(grass.linear, which = 5)


## Interaction models (ANCOVA: different slope, different intercept)
grass.int <- lm(log(AAM)~ColdTemp*Condition, data = Grass.clean)
anova(grass.int)
summary(grass.int)
autoplot(grass.int)

plot(grass.int, which = 5)


## Simple linear model (same slope, same intercept)

grass.simple <- lm(log(AAM)~ColdTemp, data = Grass.clean)
anova(grass.simple)
summary(grass.simple)
autoplot(grass.simple)

plot(grass.simple, which = 5)

## Compare the AIC scores
AIC(grass.linear)
AIC(grass.int)
AIC(grass.simple)



#### Grass carp ####

Big.sil.clean <- Big.sil %>% 
  filter(Condition %in% c("natural", "artificial"))

## Linear addictive models (same slope, different intercept)

bs.linear <- lm(log(AAM)~ColdTemp+Condition, data = Big.sil.clean)
anova(bs.linear)
summary(bs.linear)
autoplot(bs.linear)

plot(bs.linear, which = 5)


## Interaction models (ANCOVA: different slope, different intercept)
bs.int <- lm(log(AAM)~ColdTemp*Condition, data = Big.sil.clean)
anova(bs.int)
summary(bs.int)
autoplot(bs.int)

plot(bs.int, which = 5)


## Simple linear model (same slope, same intercept)

bs.simple <- lm(log(AAM)~ColdTemp, data = Big.sil.clean)
anova(bs.simple)
summary(bs.simple)
autoplot(bs.simple)

plot(bs.simple, which = 5)

## Compare the AIC scores
AIC(bs.linear)
AIC(bs.int)
AIC(bs.simple)


