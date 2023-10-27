#Getting the path of directory
print(getwd())

#Setting the path of directory
setwd("E:/Study/LPU/B.TECH/4th Year/7th Semester/INT234 - PREDICTIVE ANALYTICS/PREDICTIVE-ANALYTICS/R codes/Datasets")


summary(df)

# Boxplot
library(ggplot2)
ggplot(data = df, aes(x=factor(df$Survived),
                      y=df$Age))+geom_boxplot()

library(dplyr)
library(rpart)
library(rpart.plot)
set.seed(679)
Titanic<-read.csv("TITANIC 1.csv")
View(Titanic)
head(Titanic)
summary(Titanic)
class(Titanic)
str(Titanic)
is.na(Titanic)

Randomize<- sample(1:nrow(Titanic))
Titanic<- Titanic(Randomize, )
