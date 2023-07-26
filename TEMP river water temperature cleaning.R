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

WT$year <- as.factor(WT$year)
str(WT)



#### St Louis River (2011-2014) ####

# observed values
st.louis.field <- WT %>% filter(river == "stlouis")
str(st.louis.field)

# predicted values
st.louis.pred <- read.csv("water temperature clean/st_louis_model.csv")

## Plotting
ggplot(st.louis.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1.5)+
  geom_line(aes(x = weeks, y = lower.CI), size = 1, alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), size = 1, alpha = 0.5)+
  geom_line(st.louis.field, mapping = aes(x = weeks, y = temp, color = year))+
  scale_colour_hue()+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
# Remove the observations from non-complete years
st.louis.field.c <- st.louis.field[st.louis.field$complete == 1,]

# Get the unique levels of the "year" variable
unique_year <- unique(st.louis.field.c$year)
unique_year

# Create a vector list to store the results
rmse_list <- vector("numeric", 0)

# Loop through each level and calculate the RMSE
for (year.number in unique_year) {
  sub <- subset(st.louis.field.c, year == year.number) # create subsets
  value <- sqrt(mean((sub$temp - st.louis.pred$temperature.avg)^2))
  rmse_list[year.number] <- value
}

rmse_list



#### Saginaw River ####


#### Fox River ####

# observed values
fox.field <- WT %>% filter(river == "fox")
str(fox.field)

# predicted values
fox.pred <- read.csv("water temperature clean/fox_model.csv")

## Plotting
ggplot(fox.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1.5)+
  geom_line(aes(x = weeks, y = lower.CI), size = 1, alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), size = 1, alpha = 0.5)+
  geom_line(fox.field, mapping = aes(x = weeks, y = temp, color = year))+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
# Remove the observations from non-complete years
fox.field.c <- fox.field[fox.field$complete == 1,]

# Get the unique levels of the "year" variable
unique_year <- unique(fox.field.c$year)
unique_year

# Create a vector list to store the results
rmse_list <- vector("numeric", 0)

# Loop through each level and calculate the RMSE
for (year.number in unique_year) {
  sub <- subset(fox.field.c, year == year.number) # create subsets
  value <- sqrt(mean((sub$temp - fox.pred$temperature.avg)^2))
  rmse_list[year.number] <- value
}

rmse_list



#### Portage-Burns Waterways ####

# observed values
pb.field <- WT %>% filter(river == "portage")
str(pb.field)

# predicted values
pb.pred <- read.csv("water temperature clean/pb_model.csv")

## Plotting
ggplot(pb.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1.5)+
  geom_line(aes(x = weeks, y = lower.CI), size = 1, alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), size = 1, alpha = 0.5)+
  geom_line(pb.field, mapping = aes(x = weeks, y = temp, color = year))+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
#' All the years are complete for pb

# Get the unique levels of the "year" variable
unique_year <- unique(pb.field$year)
unique_year

# Create a vector list to store the results
rmse_list <- vector("numeric", 0)

# Loop through each level and calculate the RMSE
for (year.number in unique_year) {
  sub <- subset(pb.field, year == year.number) # create subsets
  value <- sqrt(mean((sub$temp - pb.pred$temperature.avg)^2))
  rmse_list[year.number] <- value
}

rmse_list



#### Vermilion River (2012 - 2014) ####

# observed values
vermilion.field <- WT %>% filter(river == "vermilion")
str(vermilion.field)

# predicted values
vermilion.pred <- read.csv("water temperature clean/vermilion_model.csv")

## Plotting
ggplot(vermilion.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1.5)+
  geom_line(aes(x = weeks, y = lower.CI), size = 1, alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), size = 1, alpha = 0.5)+
  geom_line(vermilion.field, mapping = aes(x = weeks, y = temp, color = year))+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
#' All years complete for vermilion

# Get the unique levels of the "year" variable
unique_year <- unique(vermilion.field$year)
unique_year

# Create a vector list to store the results
rmse_list <- vector("numeric", 0)

# Loop through each level and calculate the RMSE
for (year.number in unique_year) {
  sub <- subset(vermilion.field, year == year.number) # create subsets
  value <- sqrt(mean((sub$temp - vermilion.pred$temperature.avg)^2))
  rmse_list[year.number] <- value
}

rmse_list



#### Genesee River ####

# observed values
genesee.field <- WT %>% filter(river == "genesee")
str(genesee.field)

# predicted values
genesee.pred <- read.csv("water temperature clean/genesee_model.csv")

## Plotting
ggplot(genesee.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1.5)+
  geom_line(aes(x = weeks, y = lower.CI), size = 1, alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), size = 1, alpha = 0.5)+
  geom_line(genesee.field, mapping = aes(x = weeks, y = temp, color = year))+
  theme_bw()


## Calculate the Root Mean Square Error (RMSE)
#' All years complete for genesee

# Get the unique levels of the "year" variable
unique_year <- unique(genesee.field$year)
unique_year

# Create a vector list to store the results
rmse_list <- vector("numeric", 0)

# Loop through each level and calculate the RMSE
for (year.number in unique_year) {
  sub <- subset(genesee.field, year == year.number) # create subsets
  value <- sqrt(mean((sub$temp - genesee.pred$temperature.avg)^2))
  rmse_list[year.number] <- value
}

rmse_list



#### Big Creek River ####

# observed values
big.creek.field <- WT %>% filter(river == "big creek")
str(big.creek.field)

# predicted values
big.creek.pred <- read.csv("water temperature clean/bigcreek_model.csv")


## Plotting
ggplot(big.creek.pred, aes(x = weeks, y = temperature.avg))+
  geom_line(size = 1.5)+
  geom_line(aes(x = weeks, y = lower.CI), size = 1, alpha = 0.5)+
  geom_line(aes(x = weeks, y = upper.CI), size = 1, alpha = 0.5)+
  geom_line(big.creek.field, mapping = aes(x = weeks, y = temp, color = year))+
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

