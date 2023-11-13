#Polynomial Regression
# Select position_salaries dataset
dataset <- read.csv(file.choose(),sep = ',',header = T)
dataset = dataset[2:3]
dataset

#Fitting Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ .,
             data = dataset)
summary(lin_reg)

#Fitting Polynomial regression to the dataset

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
dataset
poly_reg = lm(formula = Salary ~ .,
              data = dataset)
summary(poly_reg)
dataset$Level
dataset$Level2
dataset$Level3
dataset$Level4

# Visualizing the Linear Reqression in the dataset
install.packages('ggplot2')
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red')+
  geom_line(aes(x=dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue')+
  ggtitle('Truth or Bluff(Polynomial Regression)')+
  xlab('level')+
  ylab('Salary')

#Visualising the regression Model results(for higher resolution and smoother )
#install.packages("ggplot2")
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() + 
  geom_point(aes(x = dataset$Level, y= dataset$Salary),
             colour = 'red')+
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = 
                                          data.frame
                                        (level = x_grid,
                                          level2 = x_grid^2,
                                          Level3 = x_grid^3,
                                          Level4 = x_grid^4))),
            colour = 'blue')+
  ggtitle('Truth or bluff(Polynomial Regression')+
  xlab('Level')+
  ylab('Salary')

#Predicting a new result with linear Regression
predict(lin_reg, data.frame(Level = 6.5))
#Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(level = 6.5,
                             level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))

