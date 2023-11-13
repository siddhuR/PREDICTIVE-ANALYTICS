getwd()
data <- read.csv('Datasets/SalesDatafor preprocessing.csv', stringsAsFactors = TRUE)
summary(data)
library(ggplot2)
#standerd deviation
str(data)
attach(data)
data$Sales[is.na(data$Sales)] <- mean(data$Sales,na.rm = TRUE)
data$Profit[is.na(data$Profit)] <- mean(data$Profit,na.rm = TRUE)
data$Unit.Price[is.na(data$Unit.Price)] <- mean(data$Unit.Price,na.rm = TRUE)
data$Order.Priority[is.na(data$Order.Priority)] <- sample(levels(data$Order.Priority),
                                                          size = sum(is.na(data$Order.Priority)),
                                                          replace = TRUE)

sd(data$Order.Quantity)
sd(data$Sales)
sd(data$Profit)
sd(data$Unit.Price)
summary(data)
#it will divide graph as 1 row and 2 column so by column wise or vertically
#2 graph can be printer but by row wise only 1
par(mfrow =c(1,2))
boxplot(data$Order.Quantity,main ='Order quanitiy')
boxplot(data$Sales,main= "Sales")
boxplot(data$Shipping.Cost, main='Shipping cost')
boxplot(data$Unit.Price,main= 'Unit price')
#if will divide the graph into 1 row and 1 column section
par(mfrow = c(1,1))
summary(data)
barplot(summary(data$Order.Priority))

#change column name of loww to low
# Change the "Loww" level to "Low"
print(levels(data$Order.Priority))
levels(data$Order.Priority)[levels(data$Order.Priority) == "Loww"] <- "Low"

# Verify the updated levels
print(levels(data$Order.Priority))

#removing outliers using boxplot.stat 
ind <- which(data$Order.Quantity %in% boxplot.stats(data$Order.Quantity)$out | 
               data$Sales %in% boxplot.stats(data$Sales)$out |
               data$Profit %in% boxplot.stats(data$Profit)$out|
               data$Unit.Price %in% boxplot.stats(data$Unit.Price)$out|
               data$Shipping.Cost %in% boxplot.stats(data$Shipping.Cost)$out)
data2 <- data[-ind,]
data2


#another way to change the loww to low

data2$Order.Priority[data2$Order.Priority=="Loww"]<-"Low"
data2$Order.Priority<- droplevels(data2$Order.Priority)
barplot(summary(data$Order.Priority))
# Finally, we can draw the new bloxplot
par(mfrow = c(1,-2))
boxplot(data$Order.Quantity,  main = "Order.Quantity")
boxplot(data$Profit, main = "Profit")
boxplot(data$Sales,  main= "Sales")
boxplot(data$Shipping.Cost, main = "Shipping Cost")
