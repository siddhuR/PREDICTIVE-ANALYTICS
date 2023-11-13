iris
iris1=iris
iris1
iris1$Species=NULL
iris1
d = dist(iris1, method = "euclidean")
?hclust
hfit = hclust(d, method = "average")
plot(hfit)
?cutree
grps = cutree(hfit,k=4)
grps
rect.hclust(hfit,k=4, border = "red")

