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
library(tidyverse)


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

plot(black.simple, which = 2)


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
table(asian.carp.clean$Code_Str)


## Subsampling for 1000 times (define all four models in one iteration!)
# Create the matrices to store the results
asian.linear.results <- matrix(NA,1000,4)
asian.add.results <- matrix(NA,1000,4)
asian.int.results <- matrix(NA,1000,4)
asian.group.results <- matrix(NA,1000,4)


# For 1000 iterations
for(i in 1:1000){
  sub <- asian.carp.clean %>% group_by(Code_Str) %>% sample_n(size=1)
  
  # annual
  reg.linear.annual <- lm(log(AAM)~AnnualTemp, data = sub)
  reg.add.annual <- lm(log(AAM)~AnnualTemp+Condition, data = sub)
  reg.int.annual <- lm(log(AAM)~AnnualTemp:Condition, data = sub)
  reg.group.annual <- lm(log(AAM)~AnnualTemp*Condition, data = sub)
  
  # AICs for annual
  asian.linear.results[i,1]<-as.numeric(AICc(reg.linear.annual))
  asian.add.results[i,1]<-as.numeric(AICc(reg.add.annual))
  asian.int.results[i,1]<-as.numeric(AICc(reg.int.annual))
  asian.group.results[i,1]<-as.numeric(AICc(reg.group.annual))
  
  # R2 for annual
  asian.linear.results[i,2]<-summary(reg.linear.annual)$adj.r.squared
  asian.add.results[i,2]<-summary(reg.add.annual)$adj.r.squared
  asian.int.results[i,2]<-summary(reg.int.annual)$adj.r.squared
  asian.group.results[i,2]<-summary(reg.group.annual)$adj.r.squared
  
  # cold
  reg.linear.cold <- lm(log(AAM)~ColdTemp, data = sub)
  reg.add.cold <- lm(log(AAM)~ColdTemp+Condition, data = sub)
  reg.int.cold <- lm(log(AAM)~ColdTemp:Condition, data = sub)
  reg.group.cold <- lm(log(AAM)~ColdTemp*Condition, data = sub)
  
  # AICs for cold
  asian.linear.results[i,3]<-as.numeric(AICc(reg.linear.cold))
  asian.add.results[i,3]<-as.numeric(AICc(reg.add.cold))
  asian.int.results[i,3]<-as.numeric(AICc(reg.int.cold))
  asian.group.results[i,3]<-as.numeric(AICc(reg.group.cold))
  
  # R2 for cold
  asian.linear.results[i,4]<-summary(reg.linear.cold)$adj.r.squared
  asian.add.results[i,4]<-summary(reg.add.cold)$adj.r.squared
  asian.int.results[i,4]<-summary(reg.int.cold)$adj.r.squared
  asian.group.results[i,4]<-summary(reg.group.cold)$adj.r.squared
}


## R^2 values for the four models
# annual
r2annual <- data.frame(
  Model = c("Simple linear", "Linear additive", "Interaction", "Grouped"),
  R2 = c(mean(unique(asian.linear.results[,2])),
         mean(unique(asian.add.results[,2])),
         mean(unique(asian.int.results[,2])),
         mean(unique(asian.group.results[,2])))
)

# cold
r2cold <- data.frame(
  Model = c("Simple linear", "Linear additive", "Interaction", "Grouped"),
  R2 = c(mean(unique(asian.linear.results[,4])),
         mean(unique(asian.add.results[,4])),
         mean(unique(asian.int.results[,4])),
         mean(unique(asian.group.results[,4])))
)


## AIC values - below is only for one temperature metric
# Quick average of AICs
mean(unique(asian.linear.results[,1]))
mean(unique(asian.add.results[,1]))
mean(unique(asian.int.results[,1]))
mean(unique(asian.group.results[,1]))

# Calculate the differences of AIC values
aic.asian <- matrix(NA,1000,3) # store the differences in AIC values
aic.asian[,1] <- asian.add.results[,1] - asian.linear.results[,1]
aic.asian[,2] <- asian.int.results[,1] - asian.linear.results[,1]
aic.asian[,3] <- asian.group.results[,1] - asian.linear.results[,1]


# Look at the distribution of differences
# Create a data frame
data <- as.data.frame(aic.asian)
colnames(data) <- c("additive-linear","interation-linear","group-linear")

# Convert to long data format
data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Group", values_to = "Value")

# Define the desired order of groups
desired_order <- c("additive-linear","interation-linear","group-linear")

# Convert "Group" to a factor with desired order
data_long$Group <- factor(data_long$Group, levels = desired_order)

# Violin plot
ggplot(data_long, aes(x = Group, y = Value, fill = Group))+
  geom_violin()+
  labs(title = "Differences of AIC scores among models, using simple linear model as base. Annual Temp")+
  theme_bw()





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


## Look at the distribution of the AIC scores
# Create a data frame
data <- as.data.frame(cbind(grass.linear.results, grass.add.results,
                            grass.int.results, grass.group.results))
colnames(data) <- c("linear", "additive", "interaction", "grouped")

# Convert to long data format
data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Group", values_to = "Value")

# Define the desired order of groups
desired_order <- c("linear", "additive", "interaction", "grouped")

# Convert "Group" to a factor with desired order
data_long$Group <- factor(data_long$Group, levels = desired_order)

# Violin plot
ggplot(data_long, aes(x = Group, y = Value, fill = Group))+
  geom_violin()+
  labs(title = "Grass carp 100 iterations")+
  theme_bw()


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


## Look at the distribution of the AIC scores
# Create a data frame
data <- as.data.frame(cbind(bs.linear.results, bs.add.results,
                            bs.int.results, bs.group.results))
colnames(data) <- c("linear", "additive", "interaction", "grouped")

# Convert to long data format
data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Group", values_to = "Value")

# Define the desired order of groups
desired_order <- c("linear", "additive", "interaction", "grouped")

# Convert "Group" to a factor with desired order
data_long$Group <- factor(data_long$Group, levels = desired_order)

# Violin plot
ggplot(data_long, aes(x = Group, y = Value, fill = Group))+
  geom_violin()+
  labs(title = "Bighead and silver carp 100 iterations")+
  theme_bw()


## Compare the AIC scores
mean(unique(bs.linear.results[,1]))
mean(unique(bs.add.results[,1]))
mean(unique(bs.int.results[,1]))
mean(unique(bs.group.results[,1]))





