getwd()

df1 <- read.csv("boston.csv")
View(df1)
str(df1)
summary(df1)
sum(is.na(df1))
head(df1)



df1
df <- df1[,-9]
View(df)
View(df1)
set.seed(123)

kModel <- kmeans(df1$RAD,centers = 3,nstart = 25)
kModel
kModel$cluster

library(ClusterR)
library(cluster)

plot(df[1:13],
     col = kModel$cluster,
     main = "Variation in clusters")

clusplot(df[1:13], 
         kModel$cluster,xlab = "Age",ylab = "RAD",
         main = "KMeans Clusters",
         shade = TRUE,
         color = TRUE,
         labels = 3)

