##APPLY MACHINE LEARNING MODEL ON DIAMOND DATASET
??diamond
library(ggplot2)

##load data
View(diamonds)
df<-data(diamonds)
str(diamonds)
summary(diamonds)
##See the structure
head(diamonds)

#Generate a random numbers sample that is 90% of the total no. of rows of dataset.
ran<-sample(1:nrow(diamonds), 0.9*nrow(diamonds))
str(diamonds)
range(diamonds$carat)
range(diamonds$price)
##the normalization function is created.
nor<-function(x){ (x-min(x))/(max(x)-min(x)) }
##Run normalization on first 7 columns of dataset because they are the predicators.
##As 2,3 and 4th are categorical columns.We don't take them.

dia_norm<-as.data.frame(lapply(diamonds[,c(1,5,6,7,8,9,10)], nor))
str(diamonds)
summary(diamonds)
summary(dia_norm)

##extracting training set
dia_train <- dia_norm[ran,]
##extracting testing set
dia_test <- dia_norm[-ran,]
##extract 2nd column of train dataset because it will be used as 'cl' argument in
##function.
##also convert ordered factor to normal factor
dia_target<-as.factor(diamonds[ran,2])
##extract 5th column if test dataset to measure the accuracy
dia_test<-as.factor(diamonds[-ran,2])
##also convert ordered factor to normal factor
test_target<-as.factor(diamonds[-ran,2])
##load the package class
install.packages("class")
library(class)

?knn

##run knn function
pr<-knn(dia_train,dia_test,cl=dia_target,k=218)
##K is calculated by square root of no. of rows.

##Create confusion matrix
tab <- table(pr,dia_test)

##this function divides the correct predictions by total number of predictions
##that tells us how accurate the model is.
tab
accuracy<-function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)