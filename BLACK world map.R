#' This script is used to plot the distribution of our data points on a global
#' and Chinese map. We also plot the distribution of the latitudes on a histogram.



library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)


#### Global distribution map ####

## Import locations and corresponding coordinates
location <- read.csv("location_no_temps.csv")
location <- unique(location) # remove duplicating locations


## Get the world data
world <- ne_countries(scale = "small", returnclass = "sf")
# Our projection: "+proj=webmerc +datum=WGS84"


## Plots (global plot)
world %>% 
  filter(admin != "Antarctica") %>%
  # remove antarctica
  ggplot()+
  geom_sf()+
  geom_point(aes(x=Longitude, y=Latitude), data = location,
             size = 1, color = "darkred")+
  theme_bw()+
  xlab("Longitude") + ylab("Latitude")

ggsave("map_web.png", width = 8, height = 5.5, dpi = 300)



#### Chinese distribution map ####

## Get data points for China (17 data points)
carp <- read.csv("eddie_carp_new.csv")
carp.r <- carp %>% 
  filter(sex != "male") %>% # keep the non-male data points
  distinct(location, .keep_all = TRUE) # remove all repeating locations
black.china <- carp.r[carp.r$china == "y",]


## Plots (china plot)
world %>% 
  filter(admin == "China" | admin =="Taiwan") %>%
  # select only China
  ggplot()+
  geom_sf()+
  geom_point(aes(x=longitude, y=latitude), data = black.china,
             size = 2, color = "darkred")+
  theme_bw()+
  xlab("Longitude") + ylab("Latitude")



#### Histogram of latitude distribution ####

ggplot(data=location, aes(x=Latitude))+
  geom_histogram(binwidth=1, color = "black")+
  theme_bw()



#### Separate the points based on latitude ####

# Latitude points
latitude_points <- location$Latitude

# Create 1-degree bins
bins <- cut(latitude_points,
            breaks = seq(floor(min(latitude_points)), ceiling(max(latitude_points)), by = 1), include.lowest = TRUE)
bins

# Print bin labels and their values
for (bin_label in unique(bins)) {
  bin_values <- latitude_points[bins == bin_label] # get the latitude value
  bin_name <- location[location$Latitude == bin_values,1] # get the corresponding location name
  print(paste("Bin:", as.character(bin_label)))
  print(bin_values)
  print(bin_name)
  cat("\n")
}
