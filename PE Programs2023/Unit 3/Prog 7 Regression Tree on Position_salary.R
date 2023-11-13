# Decision Tree Regression

# Importing the dataset
getwd()
dataset = read.csv('Position_Salaries.csv')
View(dataset)
dataset = dataset[2:3]
dataset
# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 0.65)
training_set = subset(dataset, split == TRUE)
training_set
test_set = subset(dataset, split == FALSE)
test_set

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))

# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor, test_set)
y_pred
library(rpart.plot)
tree<-rpart.plot(regressor)

# Visualising the Decision Tree Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
x_grid
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame
                                        (Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')