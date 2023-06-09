#' This script is used to calculate the relationship between black carp age at
#' maturity and growing degree day

library(ggplot2)
library(dplyr)

#### Import data ####

# AAM data for black carp
carp <- read.csv("eddie_carp_new.csv")

# Seperate by sex
carp.f <- carp %>% filter(!row_number() == 15) %>% filter(sex != "male")
carp.m <- carp %>% filter(!row_number() == 15) %>% filter(sex != "female")

#### Relationship between growing degree day base 0 and AAM ####

## Both sexes:
# Regression:
age.gdd <- lm(log(carp$AAM)~carp$average_gdd_0)
summary(age.gdd)
plot(age.gdd)

# Plot the AMM verse gdd_0:
ggplot(carp, mapping = aes(average_gdd_0, AAM))+
  geom_point(aes(colour = sex))+
  geom_smooth(method = "lm")+
  labs(x = "Growing degree day", y = "Age at maturity")+
  theme_bw()


## For females only:
age.gdd.f <- lm(log(carp.f$AAM)~carp.f$average_gdd_0)
summary(age.gdd.f)
plot(age.gdd)

ggplot(carp.f, mapping = aes(average_gdd_0, log(AAM)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Growing degree day", y = "Age at maturity",
       title = "Female, Base 0")+
  theme_classic(base_size = 16)

## For males only:
age.gdd.m <- lm(log(carp.m$AAM)~carp.m$average_gdd_0)
summary(age.gdd.m)
plot(age.gdd)

ggplot(carp.m, mapping = aes(average_gdd_0, log(AAM)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Growing degree day", y = "Age at maturity")+
  theme_classic(base_size = 16)


#### Relationship between growing degree day base 5 and AAM ####

## For females only:
age.gdd.5.f <- lm(log(carp.f$AAM)~carp.f$average_gdd_5)
summary(age.gdd.5.f)
plot(age.gdd.5.f)

ggplot(carp.f, mapping = aes(average_gdd_5, log(AAM)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Growing degree day base 5", y = "Age at maturity",
       title = "Female, Base 5")+
  theme_classic(base_size = 16)

## For males only:
age.gdd.5.m <- lm(log(carp.m$AAM)~carp.m$average_gdd_5)
summary(age.gdd.5.m)
plot(age.gdd.5.m)

ggplot(carp.m, mapping = aes(average_gdd_5, log(AAM)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Growing degree day base 5", y = "Age at maturity",
       title = "Male")+
  theme_classic(base_size = 16)


#### Relationship between growing degree day base 10 and AAM ####

## For females only:
age.gdd.10.f <- lm(log(carp.f$AAM)~carp.f$average_gdd_10)
summary(age.gdd.10.f)
plot(age.gdd.10.f)

ggplot(carp.f, mapping = aes(average_gdd_10, log(AAM)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Growing degree day base 10", y = "Age at maturity",
       title = "Female, Base 10")+
  theme_classic(base_size = 16)

## For males only:
age.gdd.10.m <- lm(log(carp.m$AAM)~carp.m$average_gdd_10)
summary(age.gdd.10.m)
plot(age.gdd.10.m)

ggplot(carp.m, mapping = aes(average_gdd_10, log(AAM)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Growing degree day base 10", y = "Age at maturity",
       title = "Male")+
  theme_classic(base_size = 16)
