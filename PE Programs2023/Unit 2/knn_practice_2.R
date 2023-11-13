str(iris)
table(iris$Species)
round(prop.table(table(iris$Species)) * 100, digits = 1)
summary(iris)
normalize <- function(x) { 
  num <- x - min(x) 
  denom <- max(x) - min(x) 
  return (num/denom) 
}
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_norm)

set.seed(1234) 
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
table(ind)
prop.table(table(ind))
round(prop.table(table(ind)) * 100, digits = 1)
iris.training <- iris[ind==1, 1:4] 
iris.training
iris.test <- iris[ind==2, 1:4]
iris.test
iris.trainLabels <- iris[ind==1, 5] 
iris.trainLabels
iris.testLabels <- iris[ind==2, 5]
iris.testLabels
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
