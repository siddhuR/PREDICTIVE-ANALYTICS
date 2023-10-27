# Select insurance dataset
dataset <- read.csv(file.choose(),sep = ',',header = T)
dataset = dataset[2:3]
dataset