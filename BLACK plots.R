#' This script is for making different graphs in the black carp anaylsis.


library(ggplot2)
library(gridExtra)

#### Temperature predictions of black carp age at maturity ####

## Data import and cleaning
Black <- read.csv("eddie_carp_new.csv")
Black$condition <- as.factor(Black$condition)

Black <- Black %>% filter(!row_number() == 5) %>% filter(sex != "male")
black.clean <- Black %>% filter(!row_number() == 20) # Remove SU


## Set plotting devices
png("temperature predictions black carp.png", width= 2800, height= 1280, units="px", res = 300)

# Annual temperature
annual <- ggplot(black.clean, aes(x = AnnualTemp, y = log(AAM)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  labs(x = "Annual Average Temperature", y = "In Age at Maturity")+
  annotate("text", x = 5, y = 1.65, color = "black", size = 5,
           label = "y = -0.017x + 1.98 \n F = 10.34 on 20 df \n adj.R2 = 0.31")+
  annotate("text", x = 20, y = 2.2, color = "black", size = 7,
           label = "A")+ # figure number
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

# Cold temperature
cold <- ggplot(black.clean, aes(x = ColdTemp, y = log(AAM)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  labs(x = "Cold Quarter Temperature", y = "In Age at Maturity")+
  annotate("text", x = -10, y = 1.65, color = "black", size = 5,
           label = "y = -0.01x + 1.77 \n F = 13.72 on 20 df \n adj.R2 = 0.38")+
  annotate("text", x = 10, y = 2.2, color = "black", size = 7,
           label = "B")+ # figure number
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))


grid.arrange(annual, cold, nrow=1)

dev.off()
