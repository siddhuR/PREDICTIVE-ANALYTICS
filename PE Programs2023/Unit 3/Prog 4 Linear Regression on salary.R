# Simple Linear Regression 
# Importing the dataset 
getwd()
dataset = read.csv('salary.csv') 
View(dataset)

# Splitting the dataset into the 
# Training set and Test set 
#install.packages('caTools') 
#used for running window statistic functions, R/W for GIF and 
#ENVI binary file
library(caTools) 
split = sample.split(dataset$Salary, SplitRatio = 0.7) 
View(split)
#0.7 means 70% training and 30% testing
#Used to split the data used during classification into train and test 
#subsets.
#if (0<=SplitRatio<1) then SplitRatio fraction of points from dataset 
#will be set toTRUE
trainingset = subset(dataset, split == TRUE) 
View(trainingset)
testset = subset(dataset, split == FALSE) 
View(testset)
# Fitting Simple Linear Regression to the Training set 
lm.r= lm(formula = Salary ~ YearsExperience, 
         data = trainingset) 
View(lm.r)
#lm is sued to fit linear models. formula is an object of class formula. provides symbolic
#description of the model to be fitted
coef(lm.r) 
#used to extract cofficients from objects
# Predicting the Test set results 
ypred = predict(lm.r, newdata = testset) 

#install.packages("ggplot2") 
library(ggplot2) 

# Visualising the Training set results 
ggplot() + geom_point(aes(x = trainingset$YearsExperience,  
                          y = trainingset$Salary), colour = 'red') +
  geom_line(aes(x = trainingset$YearsExperience, 
                y = predict(lm.r, newdata = trainingset)), colour = 'blue') +
  
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary') 

# Visualising the Test set results 
#ggplot() used to declare the input data frame for a graphic. geom_point()
#used to create
#scatterplots. aes() is used for aesthetics such as color, fill, shape
#geom_line() connects the observations in order of the variable 
#on the x axis.
ggplot() +
  geom_point(aes(x = testset$YearsExperience, y = testset$Salary), 
             colour = 'red') +
  geom_line(aes(x = trainingset$YearsExperience, 
                y = predict(lm.r, newdata = trainingset)),  
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary') 

