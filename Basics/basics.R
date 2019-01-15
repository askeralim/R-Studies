library(gapminder)
summary(gapminder)
mean(gapminder$lifeExp)
a=25


x<-c(1,2,3)
y<-c(7,7,8)
DATA<- data.frame(x,y)
DATA$x
DATA$y

x <- c(1,2,3)
x <- c(1,2,3)
y <- c(1,4,9)
DATA <-data.frame(x,y)
library(ggplot2)

#Survival Graph Based on Class.
ggplot() +
  geom_point(data=DATA, aes(x = x,y = y), size=1, color= factor(y)) +
  scale_x_continuous(limits = c(0,10))+
  scale_y_continuous(limits = c(0,10))+
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill="Survived") 

# Draw lines between ponts.
ggplot() +
  geom_line(data=DATA, aes(x = x,y = y), size=1, color= factor(y)) +
  scale_x_continuous(limits = c(0,10))+
  scale_y_continuous(limits = c(0,10))+
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill="Survived") 