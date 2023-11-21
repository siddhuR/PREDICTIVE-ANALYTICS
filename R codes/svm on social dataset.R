dataset<-read.csv("Datasets/social.csv")
View(dataset)
str(dataset)
dataset= dataset[3:5]
View(dataset)
#taking colums from 3 to 5


dataset$Purchased <- factor(dataset$Purchased, levels=c(0,1))
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set= subset(dataset,split==TRUE)
test_set <- subset(dataset,split==FALSE)
#feature scaling

#normalization
training_set[-3] <- scale(training_set[-3])
View(training_set)
test_set[-3] = scale(test_set[-3])

library(e1071)
classifier = svm(formula = Purchased~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

y_pred = predict(classifier,test_set)
View(y_pred)

cm = table(test_set[,3],y_pred)
cm
