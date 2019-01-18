setwd("D:\\Study\\R\\R-Studies\\DataTrainingAndTesting")
dat <- read.csv("Data.csv", header = TRUE)
head(dat)
ggplot()+
  geom_point(data=dat, aes(x=x,y=y), size=0.2, color="black")
#Loading the content of an array of Data.
indices<-c(3,9,20)
dat[indices,]

indices<-sample(nrow(dat),nrow(dat)*0.75) # 75% training Set and 25% Test Set
trainSet<-dat[indices,]
testSet<-dat[-indices,]
ggplot()+
  geom_point(data=trainSet, aes(x=x,y=y), size=0.2, color="black")+
  geom_point(data=testSet, aes(x=x,y=y), size=0.2, color="blue")
nrow(testSet)

library(splines)
library(ggplot2)

#Spline Modeling
fit <- smooth.spline(trainSet$x, trainSet$y, df=6) #decrease freedom higher sharp curve
#Change the df =50 higher or lower to get the different results
f<-function(X){
  return(predict(fit,X))
}
predicted_test_data<-data.frame(x=trainSet$x,y=f(trainSet$x))
#Drawing Test Data and the Predicted Curve using Train Data.
ggplot()+
  geom_point(data=testSet, aes(x=x,y=y), size=0.2, color="blue")+
  geom_line(data=predicted_test_data, aes(x=y.x,y=y.y), color="red") #+

#Using Leniar Regression
##################################
######## Lenear Model  ###########
##################################
leniarModel <- lm(formula = y~x,data=trainSet)
x<-c(2,8)
y<-predict(leniarModel,data.frame(x))
leniarPredictedPoints <-data.frame(x,y)
lenearMean <- mean((trainSet$y - predict(leniarModel,testSet))^2)

##################################
######## Quadractice Model  ######
##################################
m<-lm(y~poly(x,2,raw=TRUE),data=trainSet)
quadracticModel <- lm(formula = y~x+I(x^2),data=trainSet)
#Here m & quadracticModel both creates the same model
f=function(x){
  return(predict(quadracticModel, data.frame(x)))
}
x<-trainSet$x
y<-predict(quadracticModel,testSet)
quadPredictedPoints <-data.frame(x=trainSet$x,y=f(trainSet$x))
quadMean <- mean((trainSet$y - f(trainSet$x))^2)

##################################
######## Spline Model  ######
##################################

library(splines)
splineFit <- smooth.spline(trainSet$x, trainSet$y, df=5) #decrease freedom higher sharp curve
#Change the df =50 higher or lower to get the different results
splineFun<-function(X){
  return(predict(splineFit,X))
}
spline_curve<-data.frame(x=testSet$x,y=splineFun(testSet$x))

ggplot()+
  geom_point(data=testSet, aes(x=x,y=y), size=0.2, color="blue")+
  geom_line(data=leniarPredictedPoints, aes(x=x,y=y), color="red") +
  geom_line(data=quadPredictedPoints, aes(x=x,y=y), color="green") +
  geom_line(data=spline_curve, aes(x=y.x,y=y.y))
  stat_function(data=data.frame(x=c(1,10),y=c(0,200)), aes(x=x),fun=f, color="green") #+

splineMean<-mean((testSet$y - splineFun(testSet$x)$y)^2)
