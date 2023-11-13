set.seed(100)
x <- matrix(rnorm(40),20,2)
#it will create a matrix of 40 normalized random numbers arranged in
#20 rows and 2 columns 
x
y <- rep(c(-1,1),c(10,10))
#it will repeat -1 and 1 for 10-10 times
y
x[y==1,]
x[y == 1,] = x[y == 1,] + 1
x[y==1,]
plot(x, col = y + 3, pch = 19)
library(e1071)
data = data.frame(x, y = as.factor(y))
data
?svm
data.svm = svm(y ~ ., data = data, kernel = "linear", cost = 10, 
               scale = FALSE)
print(data.svm)
plot(data.svm, data)
