#' This script is used to compare the four difference models for establishing
#' the relationship between AAM and temperature + condition for black carp and
#' Asian carp.
#' 
#' We use:
#'
#' 1. Linear addictive model  2. Interaction model
#' 3. Simple linear model  4. Grouped-specific model
#' 
#' on:
#' 
#' 1. Annual temperature  2. Cold temperature
#' 
#' We also conducted the same AIC analyses for all Asian carp species. But for
#' Asian carp, the data is subsampled.


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



#### Black carp (does not subsample) ####

Black.clean <- Black %>% filter(!row_number() == 5) %>% filter(sex != "male") %>% 
  filter(!condition == "na")

# OPTIONAL
Black.clean <- Black.clean[Black.clean$AAM != 11,]
Black.clean <- Black.clean[Black.clean$AAM != 4,]
Black.clean <- Black.clean[Black.clean$location != "Heilong Jiang"]


## Simple linear model (same slope, same intercept)
black.simple <- lm(log(AAM)~AnnualTemp, data = Black.clean)
anova(black.simple)
summary(black.simple)
autoplot(black.simple) # did not meet??

plot(black.simple, which = 5)


## Linear addictive models (same slope, different intercept)
black.linear <- lm(log(AAM)~AnnualTemp+condition, data = Black.clean)
anova(black.linear)
summary(black.linear)
autoplot(black.linear)

plot(black.linear, which = 5)


## Interaction model (different slope, same intercept)
black.int <- lm(log(AAM)~AnnualTemp:condition, data = Black.clean)
summary(black)


## Grouped-specific models (ANCOVA: different slope, different intercept)
black.group <- lm(log(AAM)~AnnualTemp*condition, data = Black.clean)
anova(black.int)
summary(black.int)
autoplot(black.int)

plot(black.int, which = 5)


## Compare the AIC scores
AIC(black.simple, black.linear, black.int, black.group)


## Outlier check
hist(log(Black.clean$AAM))
mean(log(Black.clean$AAM)) + 3 * sd(log(Black.clean$AAM))
sd(log(Black.clean$AAM))



#### Asian carp all species (subsample 1000 times) ####

## Look at the spatial codes for the current asian carp data
asian.carp.clean <- asian.carp %>% 
  filter(Condition %in% c("natural", "artificial"))

table(asian.carp.clean$Code)


## Simple linear model
asian.linear.results <- matrix(NA,1000,1)
for(i in 1:1000){
  sub <- asian.carp.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp, data = sub)
  asian.linear.results[i,1]<-as.numeric(AIC(reg))
}


## Linear addictive models (same slope, different intercept)
asian.add.results <- matrix(NA,1000,1)
for(i in 1:1000){
  sub <- asian.carp.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp+Condition, data = sub)
  asian.add.results[i,1]<-as.numeric(AIC(reg))
}


## Interaction models (different slope, same intercept)
asian.int.results <- matrix(NA,1000,1)
for(i in 1:1000){
  sub <- asian.carp.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp:Condition, data = sub)
  asian.int.results[i,1]<-as.numeric(AIC(reg))
}


## Grouped-specific model (different slope, different intercept)
asian.group.results <- matrix(NA,1000,1)
for(i in 1:1000){
  sub <- asian.carp.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp*Condition, data = sub)
  asian.group.results[i,1]<-as.numeric(AIC(reg))
}


## Compare the AIC results
mean(unique(asian.linear.results[,1]))
mean(unique(asian.add.results[,1]))
mean(unique(asian.int.results[,1]))
mean(unique(asian.group.results[,1]))

# Do I compare the mean? Or other values?
# Do I need to do each individual asian carp species?



# Check the Cook's distance that are three times larger than the mean
cooksD <- cooks.distance(asian.simple)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential



#### Grass carp (subsample 100 times) ####

Grass.clean <- Grass %>% 
  filter(Condition %in% c("natural", "artificial"))

table(Grass.clean$Code)


## Simple linear model
grass.linear.results <- matrix(NA,100,1)
for(i in 1:100){
  sub <- Grass.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp, data = sub)
  grass.linear.results[i,1]<-as.numeric(AIC(reg))
}


## Linear addictive models (same slope, different intercept)
grass.add.results <- matrix(NA,100,1)
for(i in 1:100){
  sub <- Grass.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp+Condition, data = sub)
  grass.add.results[i,1]<-as.numeric(AIC(reg))
}


## Interaction models (different slope, same intercept)
grass.int.results <- matrix(NA,100,1)
for(i in 1:100){
  sub <- Grass.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp:Condition, data = sub)
  grass.int.results[i,1]<-as.numeric(AIC(reg))
}


## Grouped-specific model (different slope, different intercept)
grass.group.results <- matrix(NA,100,1)
for(i in 1:100){
  sub <- Grass.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp*Condition, data = sub)
  grass.group.results[i,1]<-as.numeric(AIC(reg))
}

## Compare the AIC scores
mean(unique(grass.linear.results[,1]))
mean(unique(grass.add.results[,1]))
mean(unique(grass.int.results[,1]))
mean(unique(grass.group.results[,1]))



#### Bighead and Silver carp (subsample 100 times) ####

Big.sil.clean <- Big.sil %>% 
  filter(Condition %in% c("natural", "artificial"))

table(Big.sil.clean$Code)


## Simple linear model
bs.linear.results <- matrix(NA,100,1)
for(i in 1:100){
  sub <- Big.sil.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp, data = sub)
  bs.linear.results[i,1]<-as.numeric(AIC(reg))
}


## Linear addictive models (same slope, different intercept)
bs.add.results <- matrix(NA,100,1)
for(i in 1:100){
  sub <- Big.sil.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp+Condition, data = sub)
  bs.add.results[i,1]<-as.numeric(AIC(reg))
}


## Interaction models (different slope, same intercept)
bs.int.results <- matrix(NA,100,1)
for(i in 1:100){
  sub <- Big.sil.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp:Condition, data = sub)
  bs.int.results[i,1]<-as.numeric(AIC(reg))
}


## Grouped-specific model (different slope, different intercept)
bs.group.results <- matrix(NA,100,1)
for(i in 1:100){
  sub <- Big.sil.clean %>% group_by(Code) %>% sample_n(size=1)
  reg <- lm(log(AAM)~AnnualTemp*Condition, data = sub)
  bs.group.results[i,1]<-as.numeric(AIC(reg))
}

## Compare the AIC scores
mean(unique(bs.linear.results[,1]))
mean(unique(bs.add.results[,1]))
mean(unique(bs.int.results[,1]))
mean(unique(bs.group.results[,1]))





