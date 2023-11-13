# Loading data
data(mtcars)
View(mtcars)
# Structure 
str(mtcars)
# Installing Packages
#install.packages("e1071")
#install.packages("caTools")
#install.packages("class")
# Loading package
library(e1071)
library(caTools)
library(class)
# Loading data
data(mtcars)
head(mtcars)
summary(mtcars)
# Splitting data into train
# and test data
split <- sample.split(mtcars, SplitRatio = 0.9)
train_cl <- subset(mtcars, split == "TRUE")
test_cl <- subset(mtcars, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])
train_scale

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$carb,
                      k = 11)
classifier_knn

# Confusion Matrix
cm <- table(test_cl$carb, classifier_knn)
cm
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(cm)

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$carb)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$carb,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$carb)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$carb,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$carb)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$carb,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$carb)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$carb,
                      k = 15)
misClassError <- mean(classifier_knn != test_cl$carb)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$carb,
                      k = 19)
misClassError <- mean(classifier_knn != test_cl$carb)
print(paste('Accuracy =', 1-misClassError))

