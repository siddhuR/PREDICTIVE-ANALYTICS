library(ISLR) #carseats dataset is present in ISLR
#install.packages("tree")
library(tree)
attach(Carseats)
str(Carseats)

#preprocessing
range(Sales)
New_Sales <- ifelse(Sales>=8, "Yes","No")
#we want to predict sales that's why converting continuous values to categor
#ical data
Carseats = data.frame(Carseats, New_Sales)
str(Carseats)
Carseats=Carseats[,-1]
str(Carseats)

#creating training and testing data
set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
trainingdata = Carseats[train,]
testingdata = Carseats[test,]
testingNew_Sales = New_Sales[test]

#Creating tree
tree <- tree(New_Sales~., trainingdata)
plot(tree)
text(tree)

#testing
tree_pred = predict(tree, testingdata, type = "class")
mean(tree_pred != testingNew_Sales)

#pruning
set.seed(2)
cv_tree = cv.tree(tree, FUN = prune.misclass)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev)
pruned_tree = prune.misclass(tree, best = 9)
plot(pruned_tree)
text(pruned_tree)

#testing the new tree
tree_pred=predict(pruned_tree,testingdata, type="class")
mean(tree_pred != testingNew_Sales)