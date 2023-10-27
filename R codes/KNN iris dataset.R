##APPLY MACHINE LEARNING MODEL ON IRIS DATASET

##load data

df<-data("iris")
View(iris)
##See the structure
head(iris)
##Generate a random number that is 90% of the total number of rows in dataset.
ran<-sample(1:nrow(iris), 0.9*nrow(iris))
str(iris)
range(iris$Sepal.Length)
range(iris$Petal.Width)
View(iris)
##the normalization function is created.
nor<-function(x){ (x-min(x))/(max(x)-min(x)) }
nor
##Run normalization on first 4 columns of dataset because they are the predicators.

iris_norm<-as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
iris_norm
summary(iris)
summary(iris_norm)

##extracting training set
iris_train <- iris_norm[ran,]
##extracting testing set
iris_test <- iris_norm[-ran,]
##extract 5th column of train dataset because it iwll be used as 'cl' argument in
##function.
iris_target_category<-iris[ran,5]
##extract 5th column if test dataset to measure the accuracy
iris_test_category<-iris[-ran,5]
##load the package class
install.packages("class")
library(class)

?knn

##run knn function
pr<-knn(iris_train,iris_test,cl=iris_target_category,k=10)

##Create confusion matrix
tab <- table(pr,iris_test_category)

##this function divides the correct predictions by total number of predictions
##that tells us how accurate the model is.
tab
accuracy<-function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
