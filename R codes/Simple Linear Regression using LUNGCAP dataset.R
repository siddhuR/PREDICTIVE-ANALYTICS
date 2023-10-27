
# Choose LUNGCAP.csv file
data<-read.csv(file.choose(), header = T, stringsAsFactors = T)
?attach
attach(data)
View(data)
str(data)
summary(data)
data<- data[,-1]
?lm
?abline

# Handling the NA's
data$LungCap[is.na(data$LungCap)] <- mean(data$LungCap, na.rm = T)
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = T)
data$Height[is.na(data$Height)] <- mean(data$Height, na.rm = T)

# NOrmalization
nor <- function(x) { (x = min(x)) / (max(x)-min(x))}
norm <- as.data.frame(lapply(data[, c(1, 2, 3)], nor))
summary(norm)

#install.packages("lm")

plot(Age, LungCap, main = "scaatterplot")
model <- lm(LungCap~Age)
summary(model)
attributes(model)
abline(model,col = 2, lwd = 3)
checklungs = data.frame(Age = 10)
result = predict(model, checklungs)
result
