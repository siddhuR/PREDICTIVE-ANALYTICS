#Getting the path of directory
print(getwd())

#Setting the path of directory
setwd("E:/Study/LPU/B.TECH/4th Year/7th Semester/INT234 - PREDICTIVE ANALYTICS/PREDICTIVE-ANALYTICS/R codes/Datasets")

# Reading the dataset
df<-read.csv("sales dataset.csv")
View(df)
print(is.data.frame(df))
print(ncol(df))
print(nrow(df))
str(df)
summary(df)

#changing the type from char to factor
df$Item.code<-as.factor(df$Item.code)
str(df)

print(df[which.max(df$Sales_Amt),TRUE])

retval<- subset(df, Sales_Amt== max(Sales_Amt))
View(retval)

info<- subset(df, df$Sales_Amt> 1000 &
                df$Department == "Finance")
View(info)

newdata <- subset(df,df$Sales_Amt>1000 & df$Department=="Finance"
                  ,select=c('Employee.Name','Employee.Country'))
View(newdata)

retval <- subset(df,as.Date(df$Sales.Date,"%d-%m-%Y")>as.Date("01-01-2014","%d-%m-%Y"))
View(retval)
View(df)

