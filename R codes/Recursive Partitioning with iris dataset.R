install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

View(iris)
set.seed(678)
s=sample(nrow(iris),100)
iris_train=iris[s, ]
iris_test=iris[-s,]
?rpart
iris_decision_tree_model = rpart(Species~., data=iris_train, method = "class")
iris_decision_tree_model
plot(iris_decision_tree_model)
text(iris_decision_tree_model)

?rpart.plot
rpart.plot(iris_decision_tree_model)
rpart.plot(iris_decision_tree_model, type=4, extra=103)

iris_predict = predict(iris_decision_tree_model, iris_test, type="class")

iris_predict_table=table(iris_test[,5], iris_predict)
iris_predict_table

(iris_performance=sum(diag(iris_predict_table))/sum(iris_predict_table))*100
