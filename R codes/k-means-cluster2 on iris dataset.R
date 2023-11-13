View(iris)
sum(is.na(iris))
library("ggplot2")
ggplot(iris, aes(x=Petal.Length,y=Petal.Width,color=Species)) +geom_point()
?kmeans
result<-kmeans(iris[,3:4],centers = 3,nstart = 25)
print(result)
result$cluster
head(result$cluster, 4)
result$size
result$centers

data("iris")
str(iris)
iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new)
head(iris.class)
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)
result<- kmeans(iris.new,3)
result$size
table(result$cluster,iris.class)

#install.packages("factoextra")
library("factoextra")
#install.packages("NbClust")
#library("NbClust")
fviz_nbclust(iris[,3:4],kmeans,method = "wss") +
  geom_vline(xintercept = 3,linetype=5) + labs(subtitle="Elbow method")

#visulization
library("ggplot2")
library(factoextra)

ggplot(data = newdata, aes(x=result$cluster)) + geom_bar(fill = "steelblue")

#Cluster
fviz_cluster(result, data = newdata)

