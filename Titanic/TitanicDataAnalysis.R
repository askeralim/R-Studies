setwd("D:\\Study\\R\\R-Studies\\Titanic")
test <- read.csv("test.csv", header = TRUE)
train <- read.csv("train.csv", header = TRUE)
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
data.combined <- rbind(train, test.survived)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

library(ggplot2)
train$Pclass <- as.factor(train$Pclass)

#Survival Graph Based on Class.
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill="Survived")

head(as.character(train$Name))
length(unique(as.character(data.combined$Name)))
str(train)
library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]
mrses  <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

males  <- data.combined[which(train$Sex == "male"),]
males[1:5,]

data.combined <- data.frame(Survived = rep("None", nrow(data.combined)), data.combined[,])

extractTitle <- function(name){
    if(length(grep("Miss.", name))>0){
        return ("Miss.")
    }else if(length(grep("Master.", name))>0){
      return ("Master.")
    }else if(length(grep("Mrs.", name))>0){
      return ("Mrs.")
    }else if(length(grep("Mr.", name))>0){
      return ("Mr.")
    }else{
      return("Other")
    }
}
titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)

#Survival Graph Based on Gender.
ggplot(data.combined[1:891,], aes(x = title, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass" )+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill="~Survived")