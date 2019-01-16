library(UsingR)
head(father.son)
library(ggplot2)

#iInding a Line which is teh mean of the datas.

line_x<-c(57,78)
line_y<-c(58,79)
line<-data.frame(line_x,line_y)
ggplot()+
  geom_point(data=father.son, aes(x=fheight,y=sheight), size=0.2)+
  geom_line(data=line, aes(x=line_x,y=line_y), color="red")

#line equation for above line
#x-x1/x2-x1 =y-y1/y2-y1
#x-60/75-60 = y-61/76-61
#x-60 = y-61 => y=x+1
X<-father.son$fheight
Y<-father.son$sheight
group<-c(1:1078)
newData<-data.frame(X,Y,group)
Y<-X+1
mean<-data.frame(X,Y,group)
#Bind the both data together
combineData<-rbind(newData,mean)

# Drawing distance between the actual value and the mean value positions.
ggplot()+
  geom_point(data=newData, aes(x=X,y=Y), size=0.2, color="black")+
  geom_point(data=mean, aes(x=X,y=Y), size=0.2, color="red")+
  geom_line(data=line, aes(x=line_x,y=line_y), color="blue")+
  geom_line(data=combineData, aes(x=X,y=Y,group=group), color="green")

#Finding Good Extimate by square of difference in Y
head(newData$Y)
head(mean$Y)
head(mean$Y - newData$Y)
square<-(mean$Y - newData$Y)^2
sum(square) #8303.877 Called Residual
#Find the best fitting line which minimize the Residual
#R Leniar Model Method. Example - https://www.mathsisfun.com/data/least-squares-regression.html
lm(Y~X,data=newData)
#Coefficients:
#   (Intercept)     X  
#   33.8866       0.5141 
c<-33.8866
m<-0.5141
#y=m*x+c  Equation of a line
line_y<-m*line_x+c
newLine<-data.frame(line_x, line_y)

# Drawing distance between the actual value and the mean value positions.
ggplot()+
  geom_point(data=newData, aes(x=X,y=Y), size=0.2, color="black")+
  geom_point(data=mean, aes(x=X,y=Y), size=0.2, color="red")+
  geom_line(data=line, aes(x=line_x,y=line_y), color="blue")+
  geom_line(data=newLine, aes(x=line_x,y=line_y), color="orange")+
  geom_line(data=combineData, aes(x=X,y=Y,group=group), color="green")

##################################
#####  DRAW FITTING LINE #########
##################################
# Drawing distance between the actual value and the mean value positions.
X<-mean$X
Y<-m*X+c
newMean<-data.frame(X,Y, group)
newCombineData<-rbind(newData,newMean)
ggplot()+
  geom_point(data=newData, aes(x=X,y=Y), size=0.2, color="black")+
  geom_point(data=mean, aes(x=X,y=Y), size=0.2, color="red")+
  geom_line(data=line, aes(x=line_x,y=line_y), color="blue")+
  geom_line(data=newLine, aes(x=line_x,y=line_y), color="red")+
  geom_line(data=newCombineData, aes(x=X,y=Y,group=group), color="green")

##################################
#########  PREDICTION ############
##################################
ggplot()+
  geom_line(data=newLine, aes(x=line_x,y=line_y), color="red")
#We Know the Prediction Line Slope and intercept earlier as 
#  c 33.8866
#  m 0.5141
#Prediction of a sons height while father have 70 will be as follows.
y<- 0.5141*70+33.8866

#Answer will be:
# [1] 69.8736