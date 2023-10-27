# Load necessary libraries
library(dplyr)  # for data manipulation
library(tidyr)  # for data tidying
library(caret)  # for classification
library(caretEnsemble)  # for regression and classification
library(cluster)  # for clustering

# Load your dataset (replace 'your_dataset.csv' with the actual file path or data frame)
your_data <- read.csv("your_dataset.csv")

# Explore the dataset
head(your_data)  # View the first few rows
str(your_data)   # Check the structure of the dataset
summary(your_data)  # Summary statistics

# Check for missing values (NA)
na_summary <- sapply(your_data, function(x) sum(is.na(x)))
print(na_summary)

# Replace missing values with a specific value (e.g., 0) in a specific column (replace 'column_name')
your_data$column_name[is.na(your_data$column_name)] <- 0

# Alternatively, you can use the 'na.omit()' function to remove rows with NAs
your_data <- na.omit(your_data)

# Regression Example
# Fit a linear regression model (replace 'target_column' with the column you want to predict)
lm_model <- lm(target_column ~ ., data = your_data)

# View regression model summary
summary(lm_model)

# Classification Example
# Assuming you have a classification target variable named 'class_target'
# Split data into training and testing sets (70-30 split)
set.seed(123)  # for reproducibility
train_index <- createDataPartition(your_data$class_target, p = 0.7, list = FALSE)
train_data <- your_data[train_index, ]
test_data <- your_data[-train_index, ]

# Train a classification model (e.g., Random Forest)
rf_model <- train(class_target ~ ., data = train_data, method = "rf")

# Make predictions
predictions <- predict(rf_model, test_data)

# Evaluate the classification model (e.g., confusion matrix and accuracy)
confusionMatrix(predictions, test_data$class_target)

# Clustering Example
# Assuming you want to perform k-means clustering on your dataset
# Select the columns for clustering (replace 'columns_for_clustering')
cluster_data <- your_data[, c('columns_for_clustering')]

# Perform k-means clustering with a specified number of clusters (e.g., 3)
kmeans_model <- kmeans(cluster_data, centers = 3)

# View cluster assignments
cluster_assignments <- kmeans_model$cluster

# Save the cluster assignments back to your data
your_data$cluster_group <- cluster_assignments

# Save the preprocessed dataset (optional)
# write.csv(your_data, file = "preprocessed_data.csv")



