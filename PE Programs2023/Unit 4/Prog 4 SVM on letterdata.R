getwd()
letters <- read.csv("letterdata.csv")
View(letters)
str(letters)
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]
#install.packages("kernlab") 
#to implement ksvm
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")
#it will create a model letter classifier for training data. the kernel
#name is vanilladot
letter_classifier
letter_predictions <- predict(letter_classifier, letters_test)
#it will make predictions for the unseen data using our model
head(letter_predictions)
#Just to check first 6 rows of the letter_predictions
table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
#where predictions are equal to expected values
table(agreement)
prop.table(table(agreement))
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,
                              kernel = "rbfdot")
#created another model using rbfdot kernal
letter_predictions_rbf <- predict(letter_classifier_rbf,
                                  letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))