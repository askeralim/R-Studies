setwd("D:\\Study\\R\\R-Studies\\DataTrainingAndTesting")
dat <- read.csv("Data.csv", header = TRUE)
indices<-sample(nrow(dat),nrow(dat)*0.75) # 75% training Set and 25% Test Set
trainSet<-dat[indices,]
testSet<-dat[-indices,]
mse<-numeric()
for(i in 1:10){
  model<-lm(y~poly(x,i,raw=TRUE), data = trainSet)
  mse[i]<- mean((testSet$y-predict(model,testSet))^2)
  print(mse)
}
# 124.2762
# 100.4715  For polynomial 2, It is getting Best, So no need of doing higher polynomials, Check the following graph to evaluate it.
# 100.4873
# 101.3886
# 101.375
# 102.5053
# 102.3114
# 102.7458
# 102.4152
# 102.2979

x<-c(1:10)
library(ggplot2)
mse_data<-data.frame(x,y=mse)
ggplot()+
  geom_point(data=mse_data, aes(x=x,y=y), size=0.2, color="blue")+
  geom_line(data=mse_data, aes(x=x,y=y), color="red") 

#######################################
######## Plotting Multiple  ###########
#######################################
mse_calc<-function(train,test){
  mse<-numeric()
  for(i in 1:10){
    model<-lm(y~poly(x,i,raw=TRUE), data = trainSet)
    mse[i]<- mean((testSet$y-predict(model,testSet))^2)
  }
  return(mse)
}
mse_data<-numeric()
x<-1:10
plot<-ggplot()
for(i in 1:10){
  indices<-sample(nrow(dat),nrow(dat)*0.75) # 75% training Set and 25% Test Set
  trainSet<-dat[indices,]
  testSet<-dat[-indices,]
  mse<-mse_calc(trainSet,testSet)
  mse_data<-data.frame(x,y=mse)
  plot<-plot+geom_point(data=mse_data, aes(x=x,y=y), size=0.2, color="blue")
  plot<-plot+geom_line(data=mse_data, aes(x=x,y=y), color="red")
}
plot