getwd()
install.packages("ggplot2")
library(ggplot2)
#Setting the path of directory
setwd("E:/Study/LPU/B.TECH/4th Year/7th Semester/INT234 - PREDICTIVE ANALYTICS/R codes")

#Preprocessing the sales data
df <- read.csv("Datasets/SalesDatafor preprocessing.csv")
View(df)
str(df)
summary(df)

# Delete all rows with missing data and name the dataset as df1
df1 <- na.omit(df)
View(df1)
summary(df1)

# Replace the missing values with the mean value of each variable
df$Sales[is.na(df$Sales)] <- mean(df$Sales, na.rm = TRUE)
df$Profit[is.na(df$Profit)] <- mean(df$Profit, na.rm = TRUE)
df$Unit.Price[is.na(df$Unit.Price)] <- mean(df$Unit.Price, na.rm = TRUE)
summary(df)

df$Order.Priority[is.na(df$Order.Priority)] <- sample(levels(df$Order.Priority),
                                                          size = sum(is.na(df$Order.Priority)),
                                                          replace = TRUE)


# Missing values for variables order.priority, ship mode and customer.Name cannot be
              # replaced by the mean value, because these variables are categorical.

# Replace the missing values with random value between min and max of each variable
df$Sales[is.na(df$Sales)] <- runif(sum(is.na(df$Sales)), min(df$Sales, na.rm = TRUE), max(df$Sales, na.rm = TRUE))
df$Profit[is.na(df$Profit)] <- runif(sum(is.na(df$Profit)), min(df$Profit, na.rm = TRUE), max(df$Profit, na.rm = TRUE))
df$Unit.Price[is.na(df$Unit.Price)] <- runif(sum(is.na(df$Unit.Price)), min(df$Unit.Price, na.rm = TRUE), max(df$Unit.Price, na.rm = TRUE))
summary(df)
View(df)
# Since missing values in df have been replaced wuth the mean value of each variable.

sd(df$Order.Quantity)
sd(df$Sales)
sd(df$Profit)
sd(df$Unit.Price)
summary(df)
#it will divide graph as 1 row and 2 column so by column wise or vertically
#2 graph can be printer but by row wise only 1
par(mfrow =c(1,2))
boxplot(df$Order.Quantity,main ='Order quanitiy')
boxplot(df$Sales,main= "Sales")
boxplot(df$Shipping.Cost, main='Shipping cost')
boxplot(df$Unit.Price,main= 'Unit price')
#if will divide the graph into 1 row and 1 column section
par(mfrow = c(1,1))
summary(df)
barplot(summary(df$Order.Priority))

#change column name of loww to low
# Change the "Loww" level to "Low"
print(levels(df$Order.Priority))
levels(df$Order.Priority)[levels(df$Order.Priority) == "Loww"] <- "Low"

# Verify the updated levels
print(levels(df$Order.Priority))


getwd()
#setwd("C:/Users/shitl/OneDrive/Documents/r studio/dataset")
getwd()
data <- read.csv('Datasets/SalesDatafor preprocessing.csv', stringsAsFactors = TRUE)

summary(data)

data$Order.Priority[is.na(data$Order.Priority)] <- sample(levels(data$Order.Priority),
                                                          size = sum(is.na(data$Order.Priority)),
                                                          replace = TRUE)
summary(data)
data$Customer.Name[is.na(data$Customer.Name)] <- sample(levels(data$Customer.Name),
                                                        size = sum(is.na(data$Customer.Name)),
                                                        replace = TRUE)
data$Ship.Mode[is.na(data$Ship.Mode)] <- sample(levels(data$Ship.Mode),
                                                size = sum(is.na(data$Ship.Mode)),
                                                replace = TRUE)
summary(data)

#standerd deviation
sd(data$Order.Quantity)
sd(data$Sales)
sd(data$Profit)
sd(data$Unit.Price)

getwd()
data <- read.csv('Datasets/SalesDatafor preprocessing.csv')
View(data)
summary(data)

#data$Sales[is.na(data$Sales)] will check if their is na and if their is then
#first remove na using na.rm then calculate mean and store it 
data$Sales[is.na(data$Sales)] <- mean(data$Sales,na.rm = TRUE)
data$Profit[is.na(data$Profit)] <- mean(data$Profit,na.rm = TRUE)
data$Unit.Price[is.na(data$Unit.Price)] <- mean(data$Unit.Price,na.rm = TRUE)
summary(data)
str(data)

