# preparing the dataset
getwd()
data <- read.csv("binary.csv" )
str(data)
# Draw a histogram for gre data
hist(data$gre)
# Min-Max Normalization
data$gre <- (data$gre - min(data$gre)) / (max(data$gre) - min(data$gre))
hist(data$gre)
# Min-Max Normalization
data$gpa <- (data$gpa - min(data$gpa)) / (max(data$gpa) - min(data$gpa))
hist(data$gpa)
data$rank <- (data$rank - min(data$rank)) / (max(data$rank) - min(data$rank))
hist(data$rank)
set.seed(222)
inp <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
inp
training_data <- data[inp==1, ]
test_data <- data[inp==2, ]
library(neuralnet)
set.seed(333)
n <- neuralnet(admit~gre + gpa + rank,
               data = training_data,
               hidden = 5)
# plot our neural network
plot(n)
# Prediction
output <- compute(n, training_data[, -1])
head(output$net.result)
# confusion Matrix $Misclassification error -Training data
output <- compute(n, rep = 1, training_data[, -1])
p1 <- output$net.result
head(training_data[1, ])
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(pred1, training_data$admit)
tab1
1 - sum(diag(tab1)) / sum(tab1)