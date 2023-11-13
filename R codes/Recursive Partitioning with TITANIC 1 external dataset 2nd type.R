getwd()
library(dplyr)
library(rpart)
library(rpart.plot)
setwd("E:/Study/LPU/B.TECH/4th Year/7th Semester/INT234 - PREDICTIVE ANALYTICS/PREDICTIVE-ANALYTICS/R codes/Datasets")
data <- read.csv("TITANIC 1.csv")
View(data)
str(data)
summary(data)
data <- subset(data, select = c("PassengerId","Survived","Sex","Age","Fare","Pclass"))
View(data)
str(data)
summary(data)
data$Age[is.na(data$Age)]<-mean(data$Age,na.rm = TRUE)
data$Fare[is.na(data$Fare)]<- mean(data$Fare,na.rm = TRUE)

#data$Survived <- as.factor(data$Survived)
data$Survived <- cut(data$Survived,2,levels=c(1,0), labels = c('Yes','No'))  
data$Pclass <- cut(data$Pclass,3,levels=c(1,2,3),labels = c('Low','Medium','High'))
#mutate(data$Pclass = factor(data$Pclass,levels = c(1,2,3),labels = c('Upper','Middle','Lower')))

set.seed(678)
ran <- sample(1:nrow(data), 0.90 * nrow(data))
nor <- function(x) { (x - min(x)) / (max(x) - min(x)) }
cols<- c('PassengerId','Age','Fare')
data_norm <- data %>% mutate_at(cols,nor)
summary(data_norm)
train_data <-data_norm[ran,]
test_data <- data_norm[-ran,]
decision_tree_model = rpart(Survived~., data = train_data, method = "class")
plot(decision_tree_model)
rpart.plot(decision_tree_model)
data_predict = predict(decision_tree_model,test_data,type = "class")
data_prediction_table = table(test_data[,2],data_predict)
data_prediction_table
(data_performance = sum(diag(data_prediction_table))/sum(data_prediction_table))*100
