library(dplyr)
library(ggplot2)

#import data: final complete data.csv
aamloc<-read.csv(file.choose())
str(aamloc)

#separate 
Black<-aamloc[aamloc$Species=="Black",]


#black carp####
#has no spatially autocorrelated locations
Black


ann.black<-lm(log(Black$AAM)~Black$AnnualTemp)
summary(ann.black)

warm.black<-lm(log(Black$AAM)~Black$WarmTemp)
summary(warm.black)

cold.black<-lm(log(Black$AAM)~Black$ColdTemp)
summary(cold.black)

dd.black<-lm(log(Black$AAM)~Black$AnnualDD)
summary(dd.black)

#bighead####
#has 4 autocorrelated locations (locations within 250km)
table(Bighead$Code)
#so we subsample and run a regression many times to get all possibilities
#and take the mean
#annual
ann.big<-matrix(NA,1000,4)

colnames(ann.big)<-c("slope",
                     "intercept",
                     "p value",
                     "R^2")
for(i in 1:nrow(ann.big)){
  #subsample by location
  sub<-Bighead %>% group_by(Code) %>% sample_n(size=1)
  #run regression
  reg<-lm(log(sub$AAM)~sub$AnnualTemp)
  values<-summary(reg)
  ann.big[i,1]<-values$coef[2,1] #slope
  ann.big[i,2]<-values$coef[1,1] #int
  ann.big[i,3]<-values$coef[2,4]#p value
  ann.big[i,4]<-values$adj.r.squared#R2
}

#take the mean of only the unique possibilities
mean(unique(ann.big[,"slope"]))
mean(unique(ann.big[,"intercept"]))
mean(unique(ann.big[,"p value"]))
mean(unique(ann.big[,"R^2"]))

#warm
warm.big<-matrix(NA,1000,4)

colnames(warm.big)<-c("slope",
                     "intercept",
                     "p value",
                     "R^2")
for(i in 1:nrow(warm.big)){
  sub<-Bighead %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$WarmTemp)
  values<-summary(reg)
  warm.big[i,1]<-values$coef[2,1] #slope
  warm.big[i,2]<-values$coef[1,1] #int
  warm.big[i,3]<-values$coef[2,4]#p value
  warm.big[i,4]<-values$adj.r.squared#R2
}

mean(unique(warm.big[,"slope"]))
mean(unique(warm.big[,"intercept"]))
mean(unique(warm.big[,"p value"]))
mean(unique(warm.big[,"R^2"]))

#cold
cold.big<-matrix(NA,1000,4)

colnames(cold.big)<-c("slope",
                      "intercept",
                      "p value",
                      "R^2")
for(i in 1:nrow(cold.big)){
  sub<-Bighead %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$ColdTemp)
  values<-summary(reg)
  cold.big[i,1]<-values$coef[2,1] #slope
  cold.big[i,2]<-values$coef[1,1] #int
  cold.big[i,3]<-values$coef[2,4]#p value
  cold.big[i,4]<-values$adj.r.squared#R2
}

mean(unique(cold.big[,"slope"]))
mean(unique(cold.big[,"intercept"]))
mean(unique(cold.big[,"p value"]))
mean(unique(cold.big[,"R^2"]))

#degree days
dd.big<-matrix(NA,1000,4)

colnames(dd.big)<-c("slope",
                      "intercept",
                      "p value",
                      "R^2")
for(i in 1:nrow(dd.big)){
  sub<-Bighead %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$AnnualDD)
  values<-summary(reg)
  dd.big[i,1]<-values$coef[2,1] #slope
  dd.big[i,2]<-values$coef[1,1] #int
  dd.big[i,3]<-values$coef[2,4]#p value
  dd.big[i,4]<-values$adj.r.squared#R2
}

mean(unique(dd.big[,"slope"]))
mean(unique(dd.big[,"intercept"]))
mean(unique(dd.big[,"p value"]))
mean(unique(dd.big[,"R^2"]))

#grass####
#has 10 autocorrelated locations
table(Grass$Code)
#annual
ann.grass<-matrix(NA,5000,4)

colnames(ann.grass)<-c("slope",
                     "intercept",
                     "p value",
                     "R^2")
for(i in 1:nrow(ann.grass)){
  sub<-Grass %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$AnnualTemp)
  values<-summary(reg)
  ann.grass[i,1]<-values$coef[2,1] #slope
  ann.grass[i,2]<-values$coef[1,1] #int
  ann.grass[i,3]<-values$coef[2,4]#p value
  ann.grass[i,4]<-values$adj.r.squared#R2
}

mean(unique(ann.grass[,"slope"]))
mean(unique(ann.grass[,"intercept"]))
mean(unique(ann.grass[,"p value"]))
mean(unique(ann.grass[,"R^2"]))

#warm
warm.grass<-matrix(NA,5000,4)

colnames(warm.grass)<-c("slope",
                       "intercept",
                       "p value",
                       "R^2")
for(i in 1:nrow(warm.grass)){
  sub<-Grass %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$WarmTemp)
  values<-summary(reg)
  warm.grass[i,1]<-values$coef[2,1] #slope
  warm.grass[i,2]<-values$coef[1,1] #int
  warm.grass[i,3]<-values$coef[2,4]#p value
  warm.grass[i,4]<-values$adj.r.squared#R2
}

mean(unique(warm.grass[,"slope"]))
mean(unique(warm.grass[,"intercept"]))
mean(unique(warm.grass[,"p value"]))
mean(unique(warm.grass[,"R^2"]))

#cold
cold.grass<-matrix(NA,5000,4)

colnames(cold.grass)<-c("slope",
                        "intercept",
                        "p value",
                        "R^2")
for(i in 1:nrow(cold.grass)){
  sub<-Grass %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$ColdTemp)
  values<-summary(reg)
  cold.grass[i,1]<-values$coef[2,1] #slope
  cold.grass[i,2]<-values$coef[1,1] #int
  cold.grass[i,3]<-values$coef[2,4]#p value
  cold.grass[i,4]<-values$adj.r.squared#R2
}

mean(unique(cold.grass[,"slope"]))
mean(unique(cold.grass[,"intercept"]))
mean(unique(cold.grass[,"p value"]))
mean(unique(cold.grass[,"R^2"]))

#degree day
dd.grass<-matrix(NA,5000,4)

colnames(dd.grass)<-c("slope",
                        "intercept",
                        "p value",
                        "R^2")
for(i in 1:nrow(dd.grass)){
  sub<-Grass %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$AnnualDD)
  values<-summary(reg)
  dd.grass[i,1]<-values$coef[2,1] #slope
  dd.grass[i,2]<-values$coef[1,1] #int
  dd.grass[i,3]<-values$coef[2,4]#p value
  dd.grass[i,4]<-values$adj.r.squared#R2
}

mean(unique(dd.grass[,"slope"]))
mean(unique(dd.grass[,"intercept"]))
mean(unique(dd.grass[,"p value"]))
mean(unique(dd.grass[,"R^2"]))

#silver####
table(Silver$Code)
#Silver has 5 autocorrelated locations
#annual
ann.silver<-matrix(NA,1000,4)

colnames(ann.silver)<-c("slope",
                       "intercept",
                       "p value",
                       "R^2")
for(i in 1:nrow(ann.silver)){
  sub<-Silver %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$AnnualTemp)
  values<-summary(reg)
  ann.silver[i,1]<-values$coef[2,1] #slope
  ann.silver[i,2]<-values$coef[1,1] #int
  ann.silver[i,3]<-values$coef[2,4]#p value
  ann.silver[i,4]<-values$adj.r.squared#R2
}

mean(unique(ann.silver[,"slope"]))
mean(unique(ann.silver[,"intercept"]))
mean(unique(ann.silver[,"p value"]))
mean(unique(ann.silver[,"R^2"]))

#warm
warm.silver<-matrix(NA,1000,4)

colnames(warm.silver)<-c("slope",
                        "intercept",
                        "p value",
                        "R^2")
for(i in 1:nrow(warm.silver)){
  sub<-Silver %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$WarmTemp)
  values<-summary(reg)
  warm.silver[i,1]<-values$coef[2,1] #slope
  warm.silver[i,2]<-values$coef[1,1] #int
  warm.silver[i,3]<-values$coef[2,4]#p value
  warm.silver[i,4]<-values$adj.r.squared#R2
}

mean(unique(warm.silver[,"slope"]))
mean(unique(warm.silver[,"intercept"]))
mean(unique(warm.silver[,"p value"]))
mean(unique(warm.silver[,"R^2"]))

#cold
cold.silver<-matrix(NA,1000,4)

colnames(cold.silver)<-c("slope",
                         "intercept",
                         "p value",
                         "R^2")
for(i in 1:nrow(cold.silver)){
  sub<-Silver %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$ColdTemp)
  values<-summary(reg)
  cold.silver[i,1]<-values$coef[2,1] #slope
  cold.silver[i,2]<-values$coef[1,1] #int
  cold.silver[i,3]<-values$coef[2,4]#p value
  cold.silver[i,4]<-values$adj.r.squared#R2
}

mean(unique(cold.silver[,"slope"]))
mean(unique(cold.silver[,"intercept"]))
mean(unique(cold.silver[,"p value"]))
mean(unique(cold.silver[,"R^2"]))

#degree day
dd.silver<-matrix(NA,1000,4)

colnames(dd.silver)<-c("slope",
                         "intercept",
                         "p value",
                         "R^2")
for(i in 1:nrow(dd.silver)){
  sub<-Silver %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(log(sub$AAM)~sub$AnnualDD)
  values<-summary(reg)
  dd.silver[i,1]<-values$coef[2,1] #slope
  dd.silver[i,2]<-values$coef[1,1] #int
  dd.silver[i,3]<-values$coef[2,4]#p value
  dd.silver[i,4]<-values$adj.r.squared#R2
}

mean(unique(dd.silver[,"slope"]))
mean(unique(dd.silver[,"intercept"]))
mean(unique(dd.silver[,"p value"]))
mean(unique(dd.silver[,"R^2"]))

