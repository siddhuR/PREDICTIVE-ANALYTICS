getwd()

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

# Missing values for variables order.priority, ship mode and customer.Name cannot be
              # replaced by the mean value, because these variables are categorical.

# Replace the missing values with random value between min and max of each variable
df$Sales[is.na(df$Sales)] <- runif(sum(is.na(df$Sales)), min(df$Sales, na.rm = TRUE), max(df$Sales, na.rm = TRUE))
df$Profit[is.na(df$Profit)] <- runif(sum(is.na(df$Profit)), min(df$Profit, na.rm = TRUE), max(df$Profit, na.rm = TRUE))
df$Unit.Price[is.na(df$Unit.Price)] <- runif(sum(is.na(df$Unit.Price)), min(df$Unit.Price, na.rm = TRUE), max(df$Unit.Price, na.rm = TRUE))
summary(df)
View(df)
# Since missing values in df have been replaced wuth the mean value of each variable.