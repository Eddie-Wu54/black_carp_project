#' This script plots black carp AAM against annual mean temperature with different
#' conditions (natural, artificial, combined).


library(ggplot2)
library(ggpubr)
library(dplyr)


# Import the data
carp <- read.csv("eddie_carp_new.csv")
carp$condition <- as.factor(carp$condition)
str(carp)

#### Data manipulation: Three senarios we are looking at here ####

# 1. Use both artificial and natural conditions
carp.f <- carp %>% filter(!row_number() == 15) %>% filter(sex != "male")
carp.m <- carp %>% filter(!row_number() == 15) %>% filter(sex != "female")

# 2. Only natural conditions (remove all artificial)
carpall.natural <- carp[which(carp$condition=="natural"),]
carp.f.n <- carpall.natural %>% filter(!row_number() == 9) %>% filter(sex != "male")
carp.m.n <- carpall.natural %>% filter(!row_number() == 9) %>% filter(sex != "female")

# 3. Only artificial conditions (remove all natural)
carpall.artificial <- carp[which(carp$condition=="artificial"),]
carp.f.a <- carpall.artificial %>% filter(sex != "male")
carp.m.a <- carpall.artificial %>% filter(sex != "female")


#### Create the graph ####

combined <- 
  ggplot(carp.f, mapping = aes(AnnualTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Annual Mean Temperature", y = "Age at Maturity",
       title = "(a) Conditions combined")+
  theme_classic(base_size = 16)


natural <-
  ggplot(carp.f.n, mapping = aes(AnnualTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Annual Mean Temperature", y = "Age at Maturity",
       title = "(b) Natural conditions for females")+
  theme_classic(base_size = 16)


artificial <-
  ggplot(carp.f.a, mapping = aes(AnnualTemp, AAM))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Annual Mean Temperature", y = "Age at Maturity",
       title = "(c) Artificial conditions for females")+
  theme_classic(base_size = 16)


art.nat <- 
  ggplot(carp.f, mapping = aes(AnnualTemp, AAM, color = condition))+
  geom_point()+
  geom_smooth(carp.f.n, mapping = aes(AnnualTemp, AAM), method = "lm", se = T)+
  geom_smooth(carp.f.a, mapping = aes(AnnualTemp, AAM), method = "lm", se = T)+
  labs(x = "Annual Mean Temperature", y = "Age at Maturity",
       title = "(b) Conditions separated")+
  theme_classic(base_size = 16)+
  theme(legend.position = c(0.85,0.9), legend.title=element_blank())+
  scale_color_manual(values = c("artificial" = "red", "natural" = "blue"))


# Arrange onto one panel
ggarrange(combined, natural, artificial, ncol=3)
ggarrange(combined, art.nat, ncol=2)


# Save the graph:
ggsave("images/aam latitude asian.jpeg",
       width = 12, height = 3.8, dpi = 300)
ggsave("images/aam temperatures conditions.jpeg",
       width = 8.5, height = 3.8, dpi = 300)

