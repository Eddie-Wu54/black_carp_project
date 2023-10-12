#' This script is for making different graphs in the black carp anaylsis.


library(ggplot2)
library(gridExtra)

#### Temperature predictions of black carp age at maturity ####

## Data import and cleaning
Black <- read.csv("eddie_carp_new.csv")
Black$condition <- as.factor(Black$condition)

Black <- Black %>% filter(!row_number() == 5) %>% filter(sex != "male")
black.clean <- Black %>% filter(!row_number() == 20) # Remove SU
black.clean$data.source <- as.factor(black.clean$data.source)


## Prediction lines and Confidence intervals

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
                                      max(black.clean$average_gdd_0), by = 0.1))
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




## Set plotting devices
png("temperature predictions black carp.png", width= 2800, height= 2560, units="px", res = 300)
png("temperature predictions black carpdemo.png", width= 4200, height= 2560, units="px", res = 300)

# Annual temperature
annual <- ggplot(black.clean, aes(x = AnnualTemp, y = log(AAM)))+
  geom_point(size = 5,
             aes(fill = data.source, shape = data.source))+
  geom_line(data = pred.annual, aes(AnnualTemp, fit), color = "black", size = 1)+ # prediction line
  geom_line(data = pred.annual, aes(AnnualTemp, lwr), linetype = "dashed")+  # lower CI
  geom_line(data = pred.annual, aes(AnnualTemp, upr), linetype = "dashed")+  # upper CI
  labs(x = "Annual Average Air Temperature (°C)", y = "In Age at Maturity")+
  annotate("text", x = 20, y = 2.2, color = "black", size = 7,
           label = "A")+ # figure number
  scale_shape_manual(values = c(21, 24))+  # Define shapes (21 is filled, 24 is empty)
  scale_fill_manual(values = c("black", "white"))+  # Define fill colors
  theme_classic()+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12), legend.position = "none")

# Cold temperature
cold <- ggplot(black.clean, aes(x = ColdTemp, y = log(AAM)))+
  geom_point(size = 5,
             aes(fill = data.source, shape = data.source))+
  geom_line(data = pred.cold, aes(ColdTemp, fit), color = "black", size = 1)+ # prediction line
  geom_line(data = pred.cold, aes(ColdTemp, lwr), linetype = "dashed")+  # lower CI
  geom_line(data = pred.cold, aes(ColdTemp, upr), linetype = "dashed")+  # upper CI
  labs(x = "Cold Quarter Air Temperature (°C)", y = "In Age at Maturity")+
  annotate("text", x = 10, y = 2.2, color = "black", size = 7,
           label = "B")+ # figure number
  scale_shape_manual(values = c(21, 24))+  # Define shapes (21 is filled, 24 is empty)
  scale_fill_manual(values = c("black", "white"))+  # Define fill colors
  theme_classic()+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12), legend.position = "none")

# Base 0 annual growing degree day
gdd <- ggplot(black.clean, aes(x = average_gdd_0, y = log(AAM)))+
  geom_point(size = 5,
             aes(fill = data.source, shape = data.source))+
  geom_line(data = pred.gdd, aes(average_gdd_0, fit), color = "black", size = 1)+ # prediction line
  geom_line(data = pred.gdd, aes(average_gdd_0, lwr), linetype = "dashed")+  # lower CI
  geom_line(data = pred.gdd, aes(average_gdd_0, upr), linetype = "dashed")+  # upper CI
  labs(x = "Base 0 Annual Average Degree Days", y = "In Age at Maturity")+
  annotate("text", x = 7500, y = 2.2, color = "black", size = 7,
           label = "C")+ # figure number
  scale_shape_manual(values = c(21, 24))+  # Define shapes (21 is filled, 24 is empty)
  scale_fill_manual(values = c("black", "white"))+  # Define fill colors
  theme_classic()+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12), legend.position = "none")

# Annual water temperature
water <- ggplot(black.clean, aes(x = WaterTemp, y = log(AAM)))+
  geom_point(size = 5,
             aes(fill = data.source, shape = data.source))+
  geom_line(data = pred.water, aes(WaterTemp, fit), color = "black", size = 1)+ # prediction line
  geom_line(data = pred.water, aes(WaterTemp, lwr), linetype = "dashed")+  # lower CI
  geom_line(data = pred.water, aes(WaterTemp, upr), linetype = "dashed")+  # upper CI
  labs(x = "Annual Average Water Temperature (°C)", y = "In Age at Maturity")+
  annotate("text", x = 22, y = 2.2, color = "black", size = 7,
           label = "D")+ # figure number
  scale_shape_manual(values = c(21, 24))+  # Define shapes (21 is filled, 24 is empty)
  scale_fill_manual(values = c("black", "white"))+  # Define fill colors
  theme_classic()+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12), legend.position = "none")

# Cold water temperature
coldwater <- ggplot(black.clean, aes(x = WaterCold, y = log(AAM)))+
  geom_point(size = 5,
             aes(fill = data.source, shape = data.source))+
  geom_line(data = pred.waterC, aes(WaterCold, fit), color = "black", size = 1)+ # prediction line
  geom_line(data = pred.waterC, aes(WaterCold, lwr), linetype = "dashed")+  # lower CI
  geom_line(data = pred.waterC, aes(WaterCold, upr), linetype = "dashed")+  # upper CI
  labs(x = "Cold Quarter water temperature (°C)", y = "In Age at Maturity")+
  annotate("text", x = 13.5, y = 2.2, color = "black", size = 7,
           label = "E")+ # figure number
  scale_shape_manual(values = c(21, 24))+  # Define shapes (21 is filled, 24 is empty)
  scale_fill_manual(values = c("black", "white"))+  # Define fill colors
  theme_classic()+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12), legend.position = "none")



grid.arrange(annual, cold, gdd, water, coldwater, nrow=2, ncol=3)

dev.off()
