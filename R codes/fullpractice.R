#Linear regression

d<- read.csv('Datasets/LUNGCAP.csv')
#View(d)
str(d)
summary(d)
sum(is.na(d))

lapply(d, str)
lapply(d, summary)

#null values remove
d$LungCap[is.na(d$LungCap)] <- mean(d$LungCap, na.rm = T)
d$Age[is.na(d$Age)] <- mean(d$Age, na.rm = T)
d$Height[is.na(d$Height)] <- mean(d$Height, na.rm = T)

Ln<- scale(d$LungCap)
An<- scale(d$Age)
Hn<- scale(d$Height)
View(d)
attach(d)
plot(Age, LungCap)

model<- lm(LungCap~+Age)
abline(model,col="red",lwd=3)

pre <- predict(model,d)
cm <- table(pre,d$LungCap)

#===============================================================================

# polynomial regresion


# Select position_salaries dataset
dataset <- read.csv(file.choose(),sep = ',',header = T)
View(dataset)
dataset = dataset[2:3]
dataset

#Fitting Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ .,
             data = dataset)
plot(dataset$Level, dataset$Salary, main = "plot")
abline(lin_reg)
summary(lin_reg)

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
dataset
poly_reg = lm(formula = Salary ~ .,
              data = dataset)
summary(poly_reg)
dataset$Level
dataset$Level2
dataset$Level3
dataset$Level4
library(ggplot2)
g<-ggplot(dataset,)+
  geom_point(aes(x=Level,y=Salary),colour='blue')+
  geom_line(aes(x=Level,y=Salary),colour='red')
g
#Predicting a new result with linear Regression
predict(lin_reg, data.frame(Level = 6.5))
#Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4=6.5^4))


#===============================================================================

# Apriori

library(arules)

data<- read.transactions("groceries.csv",sep = ",")

inspect(data)
apriori(data)


groceryrules <- apriori(groceries, parameter = 
                          list(support =
                                 0.006, confidence = 0.25, 
                               minlen = 2))



df <- apriori(data,parameter = list(support=0.006,confidence=0.2,minlen=3))
df 

write(df,file = "dfrules2.csv",sep=",",quote=TRUE,row.names=FALSE)



#===============================================================================



#KNN

df <- read.csv("Datasets/wisc_bc_data.csv")
sum(is.na(df))
View(df)
table(df$diagnosis)
summary(df)
str(df)
df<-df[-1]

table(df$diagnosis)
df$diagnosis <- factor(df$diagnosis)
#df$diagnosis<- factor(df$diagnosis, 
#                        levels = 
 #                         c("B", "M"),
  #                      labels = c("Benign", "Malignant"))

summary(df)
str(df)

norm<- scale(df[-1])
#n <- function(x) {return(x-min(x))/(max(x)-min(x))}
#norm<- as.data.frame(lapply(df[-1],n))

str(df)
View(norm)

train1 <- norm[1:469,]
test1 <- norm[470:569,]
trainlabel <- df[1:469,1]

testlable <-df[470:569,1]

library(class)
clf <- knn(train=train1,test=test1,cl=trainlabel,k=25)


a1<-table(testlable,clf)
a1
a <- (sum(diag(a1)))/(sum(a1))
a


#===============================================================================

#naivebayes

df <- iris
View(df)
class(df)

library(caTools)
split <- sample.split(df$Species,SplitRatio = 0.7)
trainiris <- subset(df,split==TRUE)
testiris <- subset(df,split==FALSE)


library(e1071)

clf1 <- naiveBayes(Species~., trainiris)


pred <- predict(clf1,testiris)

table(pred,testiris[,5])

library(caret)
a<-confusionMatrix(pred,testiris[,5])
a


#================================================================================


# Decisiom Tree

library(party)
ds <- readingSkills
View(ds)

str(ds)
sum(is.na(ds))
summary(ds)
ds$nativeSpeaker=factor(ds$nativeSpeaker)
set.seed(2000)
library(caTools)
split <- sample.split(ds$nativeSpeaker,SplitRatio = 0.75)


train1<- subset(ds,split==T)
test1 <- subset(ds,split==F)

library(rpart)
library(rpart.plot)
dstree <- ctree(nativeSpeaker~., data = train1)
a<-predict(dstree, test)
clf <- rpart(nativeSpeaker~., train1,method = "class")
rpart.plot(clf)
b<-predict(clf,test1)
table(test$nativeSpeaker,a)

library(caret)
a1 <- confusionMatrix(test$nativeSpeaker,a)
a1



#===============================================================================

#Randomforest

??ggplot
getwd()

data <- read.csv("Datasets/whitewines.csv")
View(data)
head(data)
summary(data)
data$quality = as.factor(data$quality)
data_size =floor(nrow(data)*.80)
index <- sample(1:nrow(data),size = data_size)
training <- data[index,]
testing <- data[-index,]
#install.packages('randomForest')
library(randomForest)
rf <- randomForest(quality~.,data=training,mtry = 4, ntree=200,importance=TRUE)
rf
plot(rf)


#===============================================================================


# ANN
getwd()

df = read.csv("Datasets/Concrete_Data.csv")
#View(df)

sum(is.na(df))

df1 <- scale(df)


set.seed(1234)
split <- sample.split(df$strength,SplitRatio = 0.75)

train <- subset(df1,split==T)
test <- subset(df1,split==F)

library(neuralnet)
#attach(df)
model <- neuralnet(strength~., data = train,hidden=5 )

plot(model)

model

a<- predict(model,test)
a

m<- compute(a,test[1:8])







cf <- table(a,train$strength)

library(caret)
confusionMatrix(a,test$strength)
class(test)
cor(a,test$strength)
sd(a)

#===============================================================================

#Kmeans

data <- iris
head(iris)
library(ClusterR)
library(cluster)
str(data)
data <- data[,-5]
sum(is.na(data))
set.seed(123)
??kmeans
kModel <- kmeans(data,centers = 3,nstart = 20)
kModel
kModel$cluster
cm<- table(iris$Species,kModel$cluster)
cm
sum(diag(cm))/(sum(cm))
accuracy(cm)

plot(data[1:2],
     col = kModel$cluster,
     main = "K-means with 3 clusters")

clusplot(data[1:2], 
         kModel$cluster,xlab = "Sepal.Length",ylab = "Sepal.Width",
         main = "KMeans",
         shade = TRUE,
         color = TRUE,
         labels = 3)



#===============================================================================

#Heirarichal clustering

iris
iris1 = iris
iris1$Species = NULL
iris1
d = dist(iris1,method = "euclidean")
?hclust
hfit = hclust(d,method = "average")
plot(hfit)
grps = cutree(hfit, k=4)
grps
rect.hclust(hfit, k=4,border='red')
