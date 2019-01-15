#Sample of data (Population) between 1-10, where numbers is repeated,
#and the total is 100 numbers
sample(1:10, 100, replace=TRUE)

#Random Normal Population where Mean =50 and Standard Deviation is 10, 
#Then the numbers will be close to 50 and some where near betweeen 50+10 and 50-10.
rnorm(100,50,10)

#Draw a GGPlot to see how the data is spread out
x<-rep(1, 100)
y<-rnorm(100,50,10)
dat <- data.frame(x,y)

mean_x<-1
mean_y<-50
mean<-data.frame(mean_x, mean_y)
library(ggplot2)
ggplot() +
  geom_point(data=dat, aes(x = x,y = y), size=1, color= "blue") +
  geom_point(data=mean, aes(x = mean_x,y = mean_y), size=5, color= "red")

#Draw Multiple Data with different SD,

x<- c(rep(1,100), rep(9,100), rep(15,100))
y<- c(rnorm(100, 50, 10), rnorm(100, 30, 10), rnorm(100, 78, 10))
dat <- data.frame(x,y)
X<-c(1,9,15)
Y<-c(50,30,78)
mean<-data.frame(X, Y)
ggplot() +
  geom_point(data=dat, aes(x = x,y = y), size=1, color= "blue") +
  geom_point(data=mean, aes(x = X,y = Y), size=5, color= "red")

#Draw scattered data around a leniar line.
X<- rnorm(100,10,5)
Y<-3*X+14
line_x<-c(-10,30)#Minimum of means X -5 and Max 25
line_y<-c(-16,104)#Minimum of means Y 0 and Max 80
line<-data.frame(p1,p2)

means<-data.frame(X,Y)
line<-data.frame(line_p1, line_p2)

SD_X <- means$X
SD_Y<- sapply(means$Y, function(y) rnorm(1,y,10))
SD<-data.frame(SD_X,SD_Y)
ggplot() +
  geom_line(data=line, aes(x=line_x, y=line_y), color='blue')+
  geom_point(data=means, aes(x = X,y = Y), size=1, color= "red")+
  geom_point(data=SD, aes(x = SD_X,y = SD_Y), size=1)+
  scale_x_continuous(limits=c(-10,30))+
  scale_y_continuous(limits=c(-20,100))


