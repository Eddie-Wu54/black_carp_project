#' This script
#' 
#' 1. cleans the water temperature data from different river tributaries.
#' 2. Superimpose them on the plot obtained from the global water temperature model.
#' 3. Calculate the Root Mean Square Error (RMSE) between the predicted value and
#' the observed value.

library(ggplot2)
library(dplyr)


WT <- read.csv("water temperature clean/water_temperature_final_clean.csv",
               stringsAsFactors = TRUE)


#### St Louis River (2011-2014) ####

# observed values
st.louis <- WT %>% filter(river == "stlouis")
str(st.louis)

# predicted values
st.louis.mod <- read.csv("water temperature clean/st_louis_model.csv")


## Plotting
ggplot(st.louis.mod, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(st.louis, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()



#### Saginaw River ####


#### Fox River ####

# observed values
fox <- WT %>% filter(river == "fox")
str(fox)

# predicted values
fox.mod <- read.csv("water temperature clean/fox_model.csv")


## Plotting
ggplot(fox.mod, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(fox, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()



#### Portage-Burns Waterways ####

# observed values
pb <- WT %>% filter(river == "portage burns")
str(pb)

# predicted values
pb.mod <- read.csv("water temperature clean/pb_model.csv")


## Plotting
ggplot(pb.mod, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(pb, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()

#### Vermilion River ####




#### Genesee River ####

# observed values
genesee <- WT %>% filter(river == "genesee")
str(genesee)

# predicted values
genesee.mod <- read.csv("water temperature clean/genesee_model.csv")


## Plotting
ggplot(genesee.mod, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(genesee, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()










