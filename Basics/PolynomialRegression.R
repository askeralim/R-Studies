X<-c(-1000:1000)
f<-function(X){
  return(X^3-3*X^2+5*X-12)
}
Y<-f(X)
graphData = data.frame(X,Y)
library(ggplot2)
ggplot()+
  geom_path(data=graphData, aes(x=X,y=Y), size=0.2, color="black")

# The same above can be achieved as below
X<-c(-1000, 1000)
axis<-data.frame(X)
ggplot()+
  stat_function(data=axis, aes(X), fun=f) #f is the function above

#Short Hand for above
ggplot()+
  stat_function(data=data.frame(X=c(-100,100)), aes(X), fun=f) 

setwd("D:\\Study\\R\\R-Studies\\Basics")
dat <- read.csv("Data.csv", header = TRUE)
f<-function(X){
  return(34.04*X-65.27)
}
#####################################################
### LENIAR Modeling with Polinomial of power of 1 ###
#####################################################
#lm have the equation of m =  (NΣxy − Σx Σy)/N(Σx^2) − (Σx)^2
lm(y~x,dat)
line<-data.frame(x=c(-2,20),y=f(c(-2,20)))
#Drawing Residuals
dat$group<-c(1:100)
newLine<-data.frame(x=dat$x,y=f(dat$x),group)
combineData<-rbind(dat,newLine)

ggplot()+
  geom_point(data=dat, aes(x=x,y=y), size=0.2, color="black")+
  geom_line(data=line, aes(x=line_x,y=line_y), color="blue")+
  geom_point(data=newLine, aes(x=x,y=y), size=0.2, color="red")+
  geom_line(data=combineData, aes(x=x,y=y,group=group), color="green")
#Residual Value is : 158423.5
sum((dat$y - newLine$y)^2)

#####################################################
### LENIAR Modeling with Polinomial of power of 2 ###
#####################################################
#lm have the equation of m =  (NΣxy − Σx Σy)/N(Σx^2) − (Σx)^2
lm(y~x+I(x^2),data=dat)
f2<-function(X){
  return(2.9522*(X^2)+0.9719*X-0.5685)
}
line2<-data.frame(x=dat$x,y=f2(dat$x))
ggplot()+
  geom_point(data=dat, aes(x=x,y=y), size=0.2, color="black")+
  #geom_line(data=line, aes(x=line_x,y=line_y), color="blue")+
  geom_line(data=line2, aes(x=x,y=y), color="orange") #+
  #geom_point(data=newLine, aes(x=x,y=y), size=0.2, color="red")+
  #geom_line(data=combineData, aes(x=x,y=y,group=group), color="green")

sum((dat$y - line2$y)^2)

#####################################################
### LENIAR Modeling with Polinomial of power of 3 ###
#####################################################
#lm have the equation of m =  (NΣxy − Σx Σy)/N(Σx^2) − (Σx)^2
lm(y~x+I(x^2)+ I(x^3),data=dat)
f3<-function(X){
  return( 0.02024*(X^3)+2.60142*(X^2)+2.49528*X-1.67268)
}
line3<-data.frame(x=dat$x,y=f3(dat$x))
ggplot()+
  geom_point(data=dat, aes(x=x,y=y), size=0.2, color="black")+
  #geom_line(data=line, aes(x=line_x,y=line_y), color="blue")+
  geom_line(data=line3, aes(x=x,y=y), color="orange") #+
#geom_point(data=newLine, aes(x=x,y=y), size=0.2, color="red")+
#geom_line(data=combineData, aes(x=x,y=y,group=group), color="green")

sum((dat$y - line3$y)^2)
# [1] 34483.81 - very close to Polinomila of 2.

#####################################################
###   LENIAR Modeling with Smoothing Spline    ######
#####################################################
library(splines)
fit <- smooth.spline(dat$x, dat$y, df=30) #decrease freedom higher sharp curve
#Change the df =50 higher or lower to get the different results
f4<-function(X){
  return(predict(fit,X))
}
spline_curve<-data.frame(x=dat$x,y=f4(dat$x))
ggplot()+
  geom_point(data=dat, aes(x=x,y=y), size=0.2, color="black")+
  #geom_line(data=line, aes(x=line_x,y=line_y), color="blue")+
  geom_line(data=line3, aes(x=x,y=y), color="blue") +
  geom_line(data=spline_curve, aes(x=y.x,y=y.y), color="red") #+
#geom_point(data=newLine, aes(x=x,y=y), size=0.2, color="red")+
#geom_line(data=combineData, aes(x=x,y=y,group=group), color="green")

sum((dat$y - spline_curve$y.y)^2)
# [1] 30879.84 - very close to Polinomial of 3.