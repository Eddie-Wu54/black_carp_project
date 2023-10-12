#' This script is used to calculate the relationship between black carp age at
#' maturity and growing degree day (different bases). We want to determine if
#' the relationship would be different using different bases.

library(ggplot2)
library(dplyr)


#### Import data ####

# AAM data for black carp
carp <- read.csv("eddie_carp_new.csv")

# Seperate by sex
carp.f <- carp %>% filter(!row_number() == 5) %>% 
  filter(sex != "male") # remove all males

carp.m <- carp %>% filter(!row_number() == 5) %>% 
  filter(sex != "female")
# here we still keep the SU data point


#### Relationship between growing degree day base 0 and AAM ####

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
  labs(x = "Growing degree day", y = "Age at maturity",
       title = "Male, Base 0")+
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
       title = "Male, Base 5")+
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
       title = "Male, Base 10")+
  theme_classic(base_size = 16)
