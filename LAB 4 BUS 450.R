#Lab 4 Bus 450
#library(tidyverse)
#library(ggthemes)
#library(ISLR)
#library(leaps)
#1
collegeclean <- read.csv("/Users/acaciamoran/Downloads/colleges_clean.csv")
collegecleannew <- collegeclean[c(-1,-2,-3,-4)]
head(collegecleannew)
plot(collegecleannew$SAT_AVG,collegecleannew$ADM_RATE, main= "PREDICTION OF ADMISSION RATE", xlab="SAT AVERAGE", ylab= "ADMISSION RATE")
polyn <- lm(ADM_RATE~poly(SAT_AVG, 4, raw=TRUE), data= collegecleannew)
dataf <- cbind(collegecleannew$SAT_AVG, predict(polyn))
datafn <- dataf[order(dataf[,1]),]
lines(datafn[,1], datafn[,2], col="red", lwd=3)

#fitting spline
lines(smooth.spline(collegecleannew$SAT_AVG, collegecleannew$ADM_RATE),col="green", lwd=3)



#2
#fitting a spline

#install.packages("randomForest")
#install.packages("FNN")
library(randomForest)
library(FNN)
#install.packages("caTools")
library(caTools)
sample2 = sample.split(collegecleannew$ADM_RATE, SplitRatio = .75)
sample2
train = subset(collegecleannew, sample2 == TRUE)
test  = subset(collegecleannew, sample2 == FALSE)
dim(train)
dim(test)
head(train)
rf <- randomForest(
  ADM_RATE ~ .,
  data=train)
print(rf)
print(importance(fit,type = 2))
pred = predict(rf, newdata=test)

#knn
sample23 = sample.split(collegecleannew$ADM_RATE, SplitRatio = .75)
sample23
traink = subset(collegecleannew, sample23 == TRUE)
testk  = subset(collegecleannew, sample23 == FALSE)
train.adm <- collegecleannew$ADM_RATE[sample23]
knear <- knn(traink, testk,train.adm, k=1)
knear




