getwd()

#Setting the path of directory
setwd("E:/Study/LPU/B.TECH/4th Year/7th Semester/INT234 - PREDICTIVE ANALYTICS/PREDICTIVE-ANALYTICS/R codes")

df<- read.csv("Datasets/sales dataset.csv")
View(df)
summary(df)

# Boxplot
library(ggplot2)
ggplot(data = df, aes(x=factor(df$Item.Category),
                       y=df$Sales_Cost))+geom_boxplot()
# in the above boxplot we can able to see the outliers in cellular row only.

ggplot(data = df, aes(x=factor(df$Item.Category),
                      y=df$Sales_Qty))+geom_boxplot()
