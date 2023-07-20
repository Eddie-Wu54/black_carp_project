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
st.louis.field <- WT %>% filter(river == "stlouis")
str(st.louis.field)

# predicted values
st.louis.pred <- read.csv("water temperature clean/st_louis_model.csv")

## Plotting
ggplot(st.louis.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(st.louis.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((st.louis.field$temp - st.louis.pred$temperature.avg)^2))



#### Saginaw River ####


#### Fox River ####

# observed values
fox.field <- WT %>% filter(river == "fox")
str(fox.field)

# predicted values
fox.pred <- read.csv("water temperature clean/fox_model.csv")

## Plotting
ggplot(fox.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(fox.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()

## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((fox.field$temp - fox.pred$temperature.avg)^2))



#### Portage-Burns Waterways ####

# observed values
pb.field <- WT %>% filter(river == "portage burns")
str(pb.field)

# predicted values
pb.pred <- read.csv("water temperature clean/pb_model.csv")

## Plotting
ggplot(pb.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(pb.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()

## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((pb.field$temp - pb.pred$temperature.avg)^2))



#### Vermilion River (2012 - 2014) ####

# observed values
vermilion.field <- WT %>% filter(river == "vermilion")
str(vermilion.field)

# predicted values
vermilion.pred <- read.csv("water temperature clean/vermilion_model.csv")

## Plotting
ggplot(vermilion.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(vermilion.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()

## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((vermilion.field$temp - vermilion.pred$temperature.avg)^2))



#### Genesee River ####

# observed values
genesee.field <- WT %>% filter(river == "genesee")
str(genesee.field)

# predicted values
genesee.pred <- read.csv("water temperature clean/genesee_model.csv")

## Plotting
ggplot(genesee.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(genesee.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()

## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((genesee.field$temp - genesee.pred$temperature.avg)^2))



#### Big Creek River ####

# observed values
big.creek.field <- WT %>% filter(river == "bigcreek")
str(big.creek.field)

# predicted values
big.creek.pred <- read.csv("water temperature clean/bigcreek_model.csv")


## Plotting
ggplot(big.creek.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(big.creek.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((big.creek.field$temp - big.creek.pred$temperature.avg)^2))



#### Big Otter River ####

# observed values
big.otter.field <- WT %>% filter(river == "bigotter")
str(big.otter.field)

# predicted values
big.otter.pred <- read.csv("water temperature clean/bigotter_model.csv")


## Plotting
ggplot(big.otter.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(big.otter.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((big.otter.field$temp - big.otter.pred$temperature.avg)^2))



#### Still River ####

# observed values
still.field <- WT %>% filter(river == "still")
str(still.field)

# predicted values
still.pred <- read.csv("water temperature clean/still_model.csv")


## Plotting
ggplot(still.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(still.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((still.field$temp - still.pred$temperature.avg)^2))



#### Mississagi River ####

# observed values
mississagi.field <- WT %>% filter(river == "mississagi")
str(mississagi.field)

# predicted values
mississagi.pred <- read.csv("water temperature clean/mississagi_model.csv")


## Plotting
ggplot(mississagi.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(mississagi.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((mississagi.field$temp - mississagi.pred$temperature.avg)^2))



#### Nipigon River ####

# observed values
nipigon.field <- WT %>% filter(river == "nipigon")
str(nipigon.field)

# predicted values
nipigon.pred <- read.csv("water temperature clean/nipigon_model.csv")


## Plotting
ggplot(nipigon.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(nipigon.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((nipigon.field$temp - nipigon.pred$temperature.avg)^2))



#### Humber River ####

# observed values
humber.field <- WT %>% filter(river == "humber")
str(humber.field)

# predicted values
humber.pred <- read.csv("water temperature clean/humber_model.csv")


## Plotting
ggplot(humber.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1)+
  geom_line(aes(x = weeks, y = lower.CI), color = "blue", alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), color = "blue", alpha = 0.5)+
  geom_point(humber.field, mapping = aes(x = weeks, y = temp), color = "red")+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
sqrt(mean((humber.field$temp - humber.pred$temperature.avg)^2))



tri.location <- read.csv("tributary locations.csv")
tri.location <- tri.location[tri.location$tributary.name != "Saginaw River",]

