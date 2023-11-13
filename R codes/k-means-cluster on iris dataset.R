#iris data set with normalization

data("iris")
View(airquality)

head(iris)
summary(iris)

#NAs
colSums(is.na(iris))


library(dplyr)
df = iris %>% select(1:4) %>% scale() %>% data.frame()
#df = cbind(df, iris$Species)

#species = factor(iris$Species)
#levels(species)[1] = 1 #setosa
#levels(species)[2] = 2 #versicolor
#levels(species)[3] = 3 #virginica
#levels(species)
#df = cbind(df, species)

head(df)

result = kmeans(df, 3, nstart = 25)
print(result)
aggregate(df, by = list(cluster = result$cluster),mean)
#df = cbind(df, iris$Species)
newdata = cbind(df, cluster=result$cluster)

head(newdata)
result$cluster
head(result$cluster,4)
result$size
result$centers

#visulization
library("ggplot2")
library(factoextra)

ggplot(data = newdata, aes(x=result$cluster)) + geom_bar(fill = "steelblue")

#Cluster
fviz_cluster(result, data = newdata)
