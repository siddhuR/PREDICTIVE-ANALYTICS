#install.packages("datarium")
library(datarium)
data("marketing")
str(marketing)
summary(marketing)
plot(marketing, col = "red")
#install.packages("ggplot2")
library(ggplot2)
#ggpairs(marketing)
splitratio = 0.75
set.seed(101)
library(caTools)
sample=sample.split(marketing$youtube, SplitRatio = splitratio)
train = subset(marketing,sample==TRUE)
test=subset(marketing,sample==FALSE)
train_size = dim(train)
test_size = dim(test)
model <- lm(sales ~ youtube+facebook+newspaper, data = marketing)
summary(model)
pred <- predict(model, test)
data_size = dim(marketing)
data_size
numx <- data_size[1]*(1 - splitratio)
numx
x_axis <- seq(numx)
x_axis
df <- data.frame(x_axis, pred,test$sales)
df
g <- ggplot(df, aes(x=x_axis))
g
g <- g + geom_line(aes(y=pred, colour="Predicted"))
g
g <- g + geom_point(aes(x=x_axis, y=pred, colour="Predicted"))
g
g <- g + geom_line(aes(y=test$sales, colour="Actual"))
g
g <- g + geom_point(aes(x=x_axis, y=test$sales, colour="Actual"))
g
g <- g + scale_colour_manual("", values = c(Predicted="red", Actual="blue"))
g