getwd()

install.packages("ggplot2")
library(ggplot2)
#Setting the path of directory
setwd("E:/Study/LPU/B.TECH/4th Year/7th Semester/INT234 - PREDICTIVE ANALYTICS/PREDICTIVE-ANALYTICS/R codes")


df<- read.csv("Datasets/house.csv")
View(df)
str(df)

library(dplyr)
df1 <- df[,-1]
df1<- df %>% select(c(-1))
View(df1)

str(df)
str(df1) 

#Boxplot
ggplot(data = df1, aes(x=factor(df1$rooms),y=df1$price, fill=factor(rooms)))+
  geom_boxplot()
#histogram
ggplot(data = df1, aes(x=price))+ geom_histogram()

