library(neuralnet)
library(MASS)

Boston
View(Boston)
help("Boston")
DataFrame = Boston
str(DataFrame)
hist(DataFrame$medv)
dim(DataFrame)
head(DataFrame)
#returns first or last parts of an object

maxValue = apply(DataFrame, 2,max)
#Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix. 1 indicates
#rows and 2 indicates column
maxValue
minValue = apply(DataFrame, 2,min)
minValue
DataFrame = as.data.frame( scale(DataFrame,center = minValue, scale = maxValue-minValue))
#Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix.
ind = sample(1:nrow(DataFrame),400)                           
trainDF = DataFrame[ind,]
testDF = DataFrame[-ind,]
allVars = colnames(DataFrame)
predictorVars = allVars[!allVars%in% "medv"]
predictorVars
predictorVars = paste(predictorVars, collapse =  "+")
predictorVars
form = as.formula(paste("medv~", predictorVars, collapse = "+"))
form
neuralMOdel = neuralnet(formula = form, hidden = c(4,2),linear.output = T,data = trainDF)
plot(neuralMOdel)
