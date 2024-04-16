#' This script is for making different graphs in the black carp anaylses.
#' 
#' 1. We first calculate the prediction lines and CIs from the model and data.
#' 2. We then create a three-panel graph with: annual temperature; cold temperature,
#' annual growing degree days.


{
  library(tidyverse)
  library(ggplot2)
  library(gridExtra)
  library(ggtext)
  library(ggpubr)
  library(grid)
  library(rnaturalearth)
  library(sf)
  library(raster)
  library(maptools) #to get the country outlines
  library(scales) #ggplot2 scales and legends
}



#### Data import and cleaning ####

## Black carp
Black <- read.csv("eddie_carp_new.csv")
Black$condition <- as.factor(Black$condition)

Black <- Black %>% filter(!row_number() == 5) %>% filter(sex != "male")
black.clean <- Black %>% filter(!row_number() == 20) # Remove SU
black.clean$data.source <- as.factor(black.clean$data.source)


## Asian carp
Asian <- read.csv("asian carp final.csv")
Asian$Condition <- as.factor(Asian$Condition)

asian.clean <- Asian %>% 
  filter(Condition %in% c("natural", "artificial"))




#### The four figure captions ####
figure1.cap <- "Figure 1. Distribution of locations that reported Black Carp age at sexual maturity and mean air temperature of the coldest quarter from 1970-2000 (obtained from the WORLDCLIM dataset; Fick & Hijmans, 2017). A data point in the Mississippi River is not shown."
figure2.cap <- "Figure 2. Winter duration as the best predictor of Black Carp age at sexual maturity F<sub>20,1</sub> = 16.31, p = 6.4e-4, adj.R<sup>2</sup> = 0.42. Dashed lines represent 95% confidence intervals."
figure3.cap <- paste("Figure 3. Linear regression of Black Carp and other Asian Carp species using average air temperature from the coldest",
                     "quarter (A), and base 0 annual average degree days (B). Black solid lines represent Black Carp and green dashed lines",
                     "represent other Asian carps. Shaded areas are 95% confidence intervals. Slope, intercept, and confidence intervals for",
                     "other Asian Carp species are average values from 1000 iterations.", sep = "\n")
figureA1.cap <- paste("Figure A1. Linear regression of Black Carp using annual average temperature (A), and annual average temperature from",
                      "the coldest quarter (B). Solid circles and lines represent air temperature; while empty triangles and dashed lines represent",
                      "water temperature.", sep = "\n")



# use expression():
# figure2.cap <- expression(paste("Figure 2. Winter duration as the best predictor of Black Carp age at sexual maturity (",
#                                F[20,1] == 16.31, p == 6.4*10^-4, adj.R^2 == 0.42,
#                                "). Dashed lines represent 95% confidence intervals."))




#### Prediction lines and confidence intervals ####

## Black carp
# Annual
black.annual <- lm(log(AAM)~AnnualTemp, data = black.clean)
new_data <- data.frame(AnnualTemp = seq(min(black.clean$AnnualTemp),
                               max(black.clean$AnnualTemp), by = 0.1))
predictions <- predict(black.annual, newdata = new_data, interval = "confidence", level = 0.95)
pred.annual <- cbind(new_data, predictions)

# Cold
black.cold <- lm(log(AAM)~ColdTemp, data = black.clean)
new_data <- data.frame(ColdTemp = seq(min(black.clean$ColdTemp),
                                        max(black.clean$ColdTemp), by = 0.1))
predictions <- predict(black.cold, newdata = new_data, interval = "confidence", level = 0.95)
pred.cold <- cbind(new_data, predictions)

# GDD0
black.gdd <- lm(log(AAM)~average_gdd_0, data = black.clean)
new_data <- data.frame(average_gdd_0 = seq(min(black.clean$average_gdd_0),
                                      max(black.clean$average_gdd_0), by = 1))
predictions <- predict(black.gdd, newdata = new_data, interval = "confidence", level = 0.95)
pred.gdd <- cbind(new_data, predictions)

# Annual water
black.water <- lm(log(AAM)~WaterTemp, data = black.clean)
new_data <- data.frame(WaterTemp = seq(min(black.clean$WaterTemp),
                                       max(black.clean$WaterTemp), by = 0.1))
predictions <- predict(black.water, newdata = new_data, interval = "confidence", level = 0.95)
pred.water <- cbind(new_data, predictions)

# Cold water
black.waterC <- lm(log(AAM)~WaterCold, data = black.clean)
new_data <- data.frame(WaterCold = seq(min(black.clean$WaterCold),
                                       max(black.clean$WaterCold), by = 0.1))
predictions <- predict(black.waterC, newdata = new_data, interval = "confidence", level = 0.95)
pred.waterC <- cbind(new_data, predictions)

# Winter duration
black.below5 <- lm(log(AAM)~below5, data = black.clean)
new_data <- data.frame(below5 = seq(min(black.clean$below5),
                                    max(black.clean$below5), by = 0.1))
predictions <- predict(black.below5, newdata = new_data, interval = "confidence", level = 0.95)
pred.below5 <- cbind(new_data, predictions)



## Asian carp
asian.results <- matrix(NA,1000,6)
colnames(asian.results) <- c("slope.a","intercept.a",
                             "slope.c","intercept.c",
                             "slope.g","intercept.g")

# Create dataframes to store the ASIAN CARP results
# (shrink to the data range of black carp)
ci.annual <- data.frame(AnnualTemp = seq(min(asian.clean$AnnualTemp),
                                         max(asian.clean$AnnualTemp), by = 0.1),
                        SumCI.lwr = 0, SumCI.upr = 0)

ci.cold <- data.frame(ColdTemp = seq(min(asian.clean$ColdTemp),
                                     max(asian.clean$ColdTemp), by = 0.1),
                      SumCI.lwr = 0, SumCI.upr = 0)

ci.gdd <- data.frame(average_gdd_0 = seq(min(asian.clean$average_gdd_0),
                                         max(asian.clean$average_gdd_0), by = 1),
                     SumCI.lwr = 0, SumCI.upr = 0)


## 1000 iterations
for(i in 1:1000){
  sub <- asian.clean %>% group_by(Code_Str) %>% sample_n(size=1)
  
  ## annual
  reg.linear.annual <- lm(log(AAM)~AnnualTemp, data = sub)
  asian.results[i,1]<-summary(reg.linear.annual)$coef[2,1] #slope
  asian.results[i,2]<-summary(reg.linear.annual)$coef[1,1] #intercept
  
  value <- predict(reg.linear.annual, newdata = ci.annual, interval = "confidence", level = 0.95)
  ci.annual[,"SumCI.lwr"] <- ci.annual[,"SumCI.lwr"] + value[,"lwr"]
  ci.annual[,"SumCI.upr"] <- ci.annual[,"SumCI.upr"] + value[,"upr"]
  
  
  ## cold
  reg.linear.cold <- lm(log(AAM)~ColdTemp, data = sub)
  asian.results[i,3]<-summary(reg.linear.cold)$coef[2,1] #slope
  asian.results[i,4]<-summary(reg.linear.cold)$coef[1,1] #intercept
  
  value <- predict(reg.linear.cold, newdata = ci.cold, interval = "confidence", level = 0.95)
  ci.cold[,"SumCI.lwr"] <- ci.cold[,"SumCI.lwr"] + value[,"lwr"]
  ci.cold[,"SumCI.upr"] <- ci.cold[,"SumCI.upr"] + value[,"upr"]
  
  
  ## gdd0
  reg.linear.gdd <- lm(log(AAM)~average_gdd_0, data = sub)
  asian.results[i,5]<-summary(reg.linear.gdd)$coef[2,1] #slope
  asian.results[i,6]<-summary(reg.linear.gdd)$coef[1,1] #intercept
  
  value <- predict(reg.linear.gdd, newdata = ci.gdd, interval = "confidence", level = 0.95)
  ci.gdd[,"SumCI.lwr"] <- ci.gdd[,"SumCI.lwr"] + value[,"lwr"]
  ci.gdd[,"SumCI.upr"] <- ci.gdd[,"SumCI.upr"] + value[,"upr"]
}

# Calculate the average
avg.slope.a <- mean(unique(asian.results[,"slope.a"]))
avg.slope.c <- mean(unique(asian.results[,"slope.c"]))
avg.slope.g <- mean(unique(asian.results[,"slope.g"]))

avg.intercept.a <- mean(unique(asian.results[,"intercept.a"]))
avg.intercept.c <- mean(unique(asian.results[,"intercept.c"]))
avg.intercept.g <- mean(unique(asian.results[,"intercept.g"]))




#### Figure 1 - Raster distribution map ####

## Get the worldclim data
climate <- getData("worldclim", var="bio", res=5)
climate.crop <- crop(climate, extent(0,150,10,60)) # keep only the region we want
raster::plot(climate.crop$bio11)


## Create a dataframe for the raster temperature file
# Use only the cold quarter temperature (bio11)
climate.cold <- climate.crop$bio11
rasdf <- as.data.frame(climate.cold,xy=TRUE) %>% 
  drop_na() %>% 
  mutate(bio11=bio11/10) # change the temperature to correct Celcius matrics
head(rasdf)


## Get the country boarders (Africa, Europe, Asia)
region <- ne_countries(continent = 'asia', returnclass = "sf")
region <- rbind(region, ne_countries(continent = 'europe', returnclass = "sf"))
region <- rbind(region, ne_countries(continent = 'africa', returnclass = "sf"))

ggplot()+
  geom_sf(data=region)

# Remove the Mississippi point for plotting
black.nomissi <- black.clean %>% filter(!row_number() == 14)


## Plotting
png("figure 1_black carp distribution.png", width= 3200, height= 1500, units="px", res = 300)

ggplot()+
  geom_raster(aes(x=x,y=y,fill=bio11), data=rasdf)+
  geom_sf(fill='transparent',data=region)+
  geom_point(aes(x=longitude, y=latitude), data = black.nomissi,
             size = 3, color = "black")+
  labs(x='Longitude',y='Latitude', caption = figure1.cap)+
  scale_fill_gradient2(name = "Temperature",
                       low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-40, 30))+
  coord_sf(xlim = c(0, 150), ylim = c(10, 60), expand = FALSE)+ # crop to show only the region we want
  theme_bw()+
  theme(plot.caption = element_textbox_simple(size = 12, lineheight = 1.5,
                                              margin = margin(t = 15)), # move caption away from plot
        plot.caption.position = "plot")


dev.off()




#### Figure 2 - Best predictor of Black carp - Winter Duration ####
#' Relationship between winter duration and log(AAM)
#' show the old and new data points
png("figure 2_black carp winter duration.png", width= 1900, height= 1650, units="px", res = 300)

# legends
symbol_types <- c("old"=2, "new"=16)


ggplot(black.clean)+
  # points
  geom_point(size=4, aes(x=below5, y=log(AAM), shape=data.source))+
  # prediction line
  geom_line(data=pred.below5, aes(below5, fit), size=1)+
  # confidence interval
  geom_line(data=pred.below5, aes(below5, upr), linetype="dashed", size=1)+
  geom_line(data=pred.below5, aes(below5, lwr), linetype="dashed", size=1)+
  # labels and annotations
  labs(x = "Winter duration", y = "In Age at maturity", caption = figure2.cap)+
  theme_classic()+
  scale_shape_manual(name="", values=symbol_types,
                     labels=c("newly added data",
                              "data obtained from Nico et al. (2005), 
                              \nBrook et al. (2023)"))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.48,0.1),
        legend.text = element_text(size = 11, lineheight = 0.5),
        legend.key.height = unit(0.75, "cm"),
        legend.background = element_blank(), #make background transparent
        plot.caption = element_textbox_simple(size = 12, lineheight = 1.5,
                                    margin = margin(t = 12)), # move caption away from plot
        plot.caption.position = "plot")

dev.off()




#### Figure 3 - Comparing Asian carp and Black carp on two temp scales ####
#' Two panel graph
#' Cold quarter temperature, GDD0
#' Asian carp, Black carp, with confidence interval
png("figure 3_black asian compare.png", width= 2700, height= 1480, units="px", res = 300)

# legends
color_types <- c("Black"="black", "Asian"="#5BA300")
line_types <- c("Black"=1,"Asian"=5)


# Cold temperature
cold_temperature <- ggplot()+
  # black carp - cold temp
  geom_smooth(data=black.clean, aes(x=ColdTemp, y=log(AAM), linetype="Black"),
              method="lm", color="Black")+
  # asian carp - cold temp
  geom_segment(data=ci.cold, size=1,
               aes(x=min(ColdTemp),
                   y=avg.slope.c * min(ColdTemp) + avg.intercept.c,
                   xend=max(ColdTemp),
                   yend=avg.slope.c * max(ColdTemp) + avg.intercept.c,
                   color="Asian", #color argument need to be put inside aes() to use color type defined
                   linetype="Asian"))+
  geom_ribbon(data=ci.cold, aes(x=ColdTemp,
                                ymin=SumCI.lwr/1000, ymax=SumCI.upr/1000),
              fill="#5BA300", alpha=0.3)+
  labs(x = "Coldest quarter average air temperature (°C)", y = "In Age at maturity")+
  annotate("text", x = 22, y = 2.5, color = "black", size = 7,
           label = "A")+ #figure number
  theme_classic()+
  xlim(-25.4, 26.8)+
  ylim(0.6,2.5)+
  scale_color_manual(name="", values=color_types,
                     labels=c("Black Carp","other Asian carps"))+
  scale_linetype_manual(name="", values=line_types,
                        labels=c("Black Carp","other Asian carps"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.05,0.1),
        legend.text = element_text(size = 15),
        legend.background = element_blank(),  #make background transparent
        plot.margin = margin(b=12,l=10,t=10)) 

cold_temperature


# gdd
gdd <- ggplot(black.clean)+
  # black carp - gdd
  geom_smooth(data=black.clean, aes(x=average_gdd_0, y=log(AAM), linetype="Black"),
              color="Black", method="lm")+
  # asian carp - gdd
  geom_segment(data=ci.gdd, size=1,
               aes(x=min(average_gdd_0),
                   y=avg.slope.g * min(average_gdd_0) + avg.intercept.g,
                   xend=max(average_gdd_0),
                   yend=avg.slope.g * max(average_gdd_0) + avg.intercept.g,
                   color="Asian",
                   linetype="Asian"))+
  geom_ribbon(data=ci.gdd, aes(x=average_gdd_0,
                                ymin=SumCI.lwr/1000, ymax=SumCI.upr/1000),
              fill="#5BA300", alpha=0.3)+
  labs(x = "base 0 annual average degree days", y = "")+
  annotate("text", x = 9500, y = 2.5, color = "black", size = 7,
           label = "B")+ # figure number
  theme_classic()+
  scale_x_continuous(limits = c(2300, 10184))+
  scale_y_continuous(limits = c(0, 2.5))+
  scale_color_manual(name="", values=color_types,
                     labels=c("Black Carp","other Asian carps"))+
  scale_linetype_manual(name="", values=line_types,
                        labels=c("Black Carp","other Asian carps"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.05,0.1),
        legend.text = element_text(size = 15),
        legend.background = element_blank(),
        plot.margin = margin(b=12,r=10,t=10)) # make background transparent

gdd

# Caption
cap <- textGrob(figure3.cap, just = "left", x=0,
                gp = gpar(fontsize = 12))
combined <- grid.arrange(cold_temperature, gdd, nrow=1, ncol=2,
                         bottom = cap)

dev.off()




#### Figure A1 - Comparing black carp AIR and WATER temperature ####
#' This plot is to compare using air or water temperature on two temp scales
png("figure A1_air water compare.png", width= 2750, height= 1480, units="px", res = 300)

# Legends
symbol_types <- c("AirTemp"=16, "WaterTemp"=2)
line_types <- c("AirTemp"=1,"WaterTemp"=2)

# Two annual temperatures
annual_p <- ggplot(black.clean)+
  # annual air temp
  geom_point(size=4, aes(x=AnnualTemp, y=log(AAM), shape="AirTemp"))+
  geom_line(data=pred.annual, aes(AnnualTemp, fit, linetype="AirTemp"), size=1)+ # prediction line
  # annual water temp
  geom_point(size=4, aes(x=WaterTemp, y=log(AAM), shape="WaterTemp"))+
  geom_line(data=pred.water, aes(WaterTemp, fit, linetype="WaterTemp"), size=1)+ # prediction line
  labs(x = "Annual average temperature (°C)", y = "In Age at maturity")+
  annotate("text", x = 20, y = 2.2, color = "black", size = 7,
           label = "A")+ # figure number
  theme_classic()+
  scale_linetype_manual(name="", values=line_types,
                        labels=c("Air temperature","Water temperature"))+
  scale_shape_manual(name="", values=symbol_types,
                     labels=c("Air temperature","Water temperature"))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.05,0.1),
        legend.text = element_text(size = 15),
        legend.background = element_blank(),
        plot.margin = margin(b=12,t=10,l=10)) # make background transparent

annual_p


# Two cold temperatures
cold_p <- ggplot(black.clean)+
  # annual air temp
  geom_point(size=4, aes(x=ColdTemp, y=log(AAM), shape="AirTemp"))+
  geom_line(data=pred.cold, aes(ColdTemp, fit, linetype="AirTemp"), size=1)+ # prediction line
  # annual water temp
  geom_point(size=4, aes(x=WaterCold, y=log(AAM), shape="WaterTemp"))+
  geom_line(data=pred.waterC, aes(WaterCold, fit, linetype="WaterTemp"), size=1)+ # prediction line
  labs(x = "Coldest quarter average temperature (°C)", y = "")+
  annotate("text", x = 12, y = 2.2, color = "black", size = 7,
           label = "B")+ # figure number
  theme_classic()+
  scale_linetype_manual(name="", values=line_types,
                        labels=c("Air temperature","Water temperature"))+
  scale_shape_manual(name="", values=symbol_types,
                     labels=c("Air temperature","Water temperature"))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.05,0.1),
        legend.text = element_text(size = 15),
        legend.background = element_blank(),
        plot.margin = margin(b=12,t=10,r=10)) # make background transparent

cold_p

# Caption
cap <- textGrob(figureA1.cap, just = "left", x=0,
                gp = gpar(fontsize = 12))
combined <- grid.arrange(annual_p, cold_p, nrow=1, ncol=2,
                         bottom = cap)

dev.off()




#### Other plots - Best predictor of Black carp from BEFORE EFF REVISION ####
#' Relationship between cold quarter temperature and log(AAM)
#' show the old and new data points
png("graph 1_black carp cold temp.png", width= 1800, height= 1280, units="px", res = 300)

# legends
symbol_types <- c("old"=2, "new"=16)


cold_temperature <- ggplot(black.clean)+
  # points
  geom_point(size=4, aes(x=ColdTemp, y=log(AAM), shape=data.source))+
  # prediction line
  geom_line(data=pred.cold, aes(ColdTemp, fit), size=1)+
  # confidence interval
  geom_line(data=pred.cold, aes(ColdTemp, upr), linetype="dashed", size=1)+
  geom_line(data=pred.cold, aes(ColdTemp, lwr), linetype="dashed", size=1)+
  # labels and annotations
  labs(x = "Coldest Quarter Average Air Temperature (°C)", y = "In Age at Maturity")+
  theme_classic()+
  scale_shape_manual(name="", values=symbol_types,
                     labels=c("newly added data",
                              "obtained from Nico et al. (2005), 
                              \nBrook et al. (2023)"))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.05,0.1),
        legend.text = element_text(size = 11, lineheight = 0.4),
        legend.key.height = unit(0.75, "cm"),
        legend.background = element_blank()) # make background transparent

cold_temperature

dev.off()

