#' This script investigated:
#' 
#' 1. The relationship between black carp AAM and latitude.
#' 2. The relationship between AAM of other Asian carp species and latitude.



library(ggplot2)
library(dplyr)
library(ggpubr)

#### Import the AMM data ####
# new file containing additional data points from China and US
carp.new <- read.csv("eddie_carp_new.csv", stringsAsFactors = TRUE)
str(carp.new)


#### For all the sex groups ####
# AAM verse latitude:
aam.latitude <- lm(carp.new$AAM ~ carp.new$latitude)
summary(aam.latitude)
plot(aam.latitude)

# Plot the AAM verse latitude (both sexes):
ggplot(carp.new, mapping = aes(latitude, AAM))+
  geom_point(aes(colour = data.source))+
  geom_smooth(method = "lm")+
  labs(x = "Latitude", y = "Age at Maturity")+
  theme_bw()


#### For one sex only ####
## Only for females:

# Remove the duplicate on Yangtze River and also male data:
carp.f <- carp.new %>% filter(!row_number() == 15) %>% filter(sex != "male")

# Female AAM verse latitude:
aam.latitude.f <- lm(carp.f$AAM ~ carp.f$latitude)
summary(aam.latitude.f)
plot(aam.latitude.f)

# Plot the female AAM verse latitude:
ggplot(carp.f, mapping = aes(latitude, AAM))+
  geom_point(aes(shape = data.source))+
  geom_smooth(method = "lm")+
  labs(x = "Latitude", y = "Age at Maturity")+
  theme_classic(base_size = 16)+
  theme(legend.position = c(0.15,0.9),
        legend.text=element_text(size=rel(1)), #change legend text size
        legend.title=element_blank()) #remove legend title

# Save the graph:
ggsave("images/aam latitude black.jpeg",
       width = 5.6, height = 3.8, dpi = 300)


## Only for males:
# Male AAM verse latitude:
carp.m <- carp.new %>% filter(sex != "female")

aam.latitude.m <- lm(carp.m$AAM ~ carp.m$latitude)
summary(aam.latitude.m)
plot(aam.latitude.m)

# Plot the AMM verse latitude:
ggplot(aamloc.m, mapping = aes(latitude, AMM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Latitude", y = "Age at Maturity",
       title = "Only for males")+
  theme_bw()


#### AMM verse latitude relationship for other carp species ####

# Import and filter out different carp species
carp.all <- read.csv("black carp clean data.csv")

grass <- carp.all %>% 
  filter(Species == "Grass")
bighead <- carp.all %>% 
  filter(Species == "Bighead")
silver <- carp.all %>% 
  filter(Species == "Silver")


# Run analysis - grass carp
aam.latitude.grass <- lm(grass$AAM ~ grass$Latitude)
summary(aam.latitude.grass)
plot(aam.latitude.grass)

grassplot <- ggplot(grass, mapping = aes(Latitude, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Latitude", y = "Age at Maturity", title = "(a) Grass carp")+
  theme_classic(base_size = 16)

# Run analysis - bighead carp
aam.latitude.bighead <- lm(bighead$AAM ~ bighead$Latitude)
summary(aam.latitude.bighead)
plot(aam.latitude.bighead)

bigheadplot <- ggplot(bighead, mapping = aes(Latitude, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Latitude", y = "Age at Maturity", title = "(b) Bighead carp")+
  theme_classic(base_size = 16)

# Run analysis - silver carp
aam.latitude.silver <- lm(silver$AAM ~ silver$Latitude)
summary(aam.latitude.silver)
plot(aam.latitude.silver)

silverplot <- ggplot(silver, mapping = aes(Latitude, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Latitude", y = "Age at Maturity", title = "(c) Silver carp")+
  theme_classic(base_size = 16)

# Arrange three carp plots onto one panel
ggarrange(grassplot, bigheadplot, silverplot, ncol=3)

# Save the graph:
ggsave("images/aam latitude asian.jpeg",
       width = 12, height = 3.8, dpi = 300)



median(c(rep(4, 68), rep(6, 100)))
median(c(rep(4, 47), rep(6, 95)))