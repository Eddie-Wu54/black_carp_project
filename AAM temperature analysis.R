#' This script is used to investigate the relationship between AAM and
#' normal temperature metrics (Annual temp, Cold temp, Warm temp, Water temperature).

#' This script also investigates whether the results will be any different if
#' the entire dataset is categorized by Conditions or sex.

library(ggplot2)
library(ggfortify)
library(dplyr)

# Import the data
carp <- read.csv("eddie_carp_new.csv")
carp$condition <- as.factor(carp$condition)
str(carp)

#### Data manipulation: Three scenarios we are looking at here ####

# 1. Use both artificial and natural conditions
carp.f <- carp %>% filter(!row_number() == 15) %>% filter(sex != "male")
carp.m <- carp %>% filter(!row_number() == 15) %>% filter(sex != "female")

# 2. Only natural conditions (remove all artificial)
carpall.natural <- carp[which(carp$condition=="natural"),]
carp.f <- carpall.natural %>% filter(!row_number() == 9) %>% filter(sex != "male")
carp.m <- carpall.natural %>% filter(!row_number() == 9) %>% filter(sex != "female")

# 3. Only artificial conditions (remove all natural)
carpall.artificial <- carp[which(carp$condition=="artificial"),]
carp.f <- carpall.artificial %>% filter(sex != "male")
carp.m <- carpall.artificial %>% filter(sex != "female")


#### Annual mean temperature ####

## For both sexes
aam.temp <- lm(log(carp$AAM) ~ carp$AnnualTemp)
summary(aam.temp)
plot(aam.temp)

ggplot(carp, mapping = aes(AnnualTemp, AAM))+
  geom_point(aes(colour = sex))+
  geom_smooth(method = "lm")+
  labs(x = "Annual T", y = "Age at Maturity")+
  theme_bw()

## For females only
aam.temp.f <- lm(carp.f$AAM ~ carp.f$AnnualTemp)
summary(aam.temp.f)
plot(aam.temp.f)

ggplot(carp.f, mapping = aes(AnnualTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Annual Temperature", y = "Age at Maturity", title = "Females only")+
  theme_classic(base_size = 16)

## For males only
aam.temp.m <- lm(carp.m$AAM ~ carp.m$AnnualTemp)
summary(aam.temp.m)
plot(aam.temp.m)

ggplot(carp.m, mapping = aes(AnnualTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Annual T", y = "Age at Maturity", title = "Males")+
  theme_classic(base_size = 16)


#### Warm temperature ####
aam.warm <- lm(log(carp$AAM) ~ carp$WarmTemp)
summary(aam.warm)
plot(aam.warm)

ggplot(carp, mapping = aes(WarmTemp, AAM))+
  geom_point(aes(colour = sex))+
  geom_smooth(method = "lm")+
  labs(x = "Annual T", y = "Age at Maturity")+
  theme_bw()
# No significant relationship when using warm temperature at all!
# Therefore, did not run analysis by sex.
# Separate by male or female does not have any significant relationship as well.


#### Cold temperature ####

# For both sexes
aam.cold <- lm(log(carp$AAM) ~ carp$ColdTemp)
summary(aam.cold)
plot(aam.cold)

ggplot(carp, mapping = aes(ColdTemp, AAM))+
  geom_point(aes(colour = sex))+
  geom_smooth(method = "lm")+
  labs(x = "Cold T", y = "Age at Maturity")+
  theme_bw()

# For females only
aam.cold.f <- lm(log(carp.f$AAM) ~ carp.f$ColdTemp)
summary(aam.cold.f)
plot(aam.cold.f)

ggplot(carp.f, mapping = aes(ColdTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Cold T", y = "Age at Maturity", title = "Females")+
  theme_classic(base_size = 16)

# For males only
aam.cold.m <- lm(log(carp.m$AAM) ~ carp.m$ColdTemp)
summary(aam.cold.m)
plot(aam.cold.m)

ggplot(carp.m, mapping = aes(ColdTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Cold T", y = "Age at Maturity", title = "Males")+
  theme_classic(base_size = 16)


#### Water temperature ####

# For females only
aam.water.f <- lm(log(carp.f$AAM) ~ carp.f$WaterTemp)
summary(aam.water.f)
plot(aam.water.f)

ggplot(carp.f, mapping = aes(WaterTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Water T", y = "Age at Maturity", title = "Females")+
  theme_classic(base_size = 16)

# For males only
aam.water.m <- lm(log(carp.m$AAM) ~ carp.m$WaterTemp)
summary(aam.water.m)
plot(aam.water.m)

ggplot(carp.m, mapping = aes(WaterTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Water T", y = "Age at Maturity", title = "Males")+
  theme_classic(base_size = 16)


#### Separate the analysis by habitat type (use only females) ####

temp.habitat <- lm(log(AAM)~AnnualTemp*condition, data = carp.f)
autoplot(temp.habitat)
plot(temp.habitat)

summary(temp.habitat)
anova(temp.habitat)
# No significant interaction effects. (No additional, significant variation in
# age at maturity by allowing different slopes for each condition.)


## Plot the entire female dataset with two different conditions
ggplot(carp.f, aes(x = AnnualTemp, y = log(AAM), color = condition))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()



#### Separate the analysis by SEX ####

temp.sex <- lm(log(AAM)~AnnualTemp*sex, data = carp)
autoplot(temp.sex)

summary(temp.sex)
anova(temp.sex)


## Plot the entire dataset with different sex categories
ggplot(carp, aes(x = AnnualTemp, y = log(AAM), color = sex))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()




