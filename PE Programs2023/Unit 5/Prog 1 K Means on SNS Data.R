getwd()
teens <- read.csv("snsdata.csv")
str(teens)
View(teens)
table(teens$gender)
table(teens$gender, useNA = "ifany")
summary(teens$age) #for numeric data
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)
summary(teens$age) #more missing data

#fill the missing data for gender
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
#if gender is female assign 1 otherwise 0
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
#if gender is NA, it will assign 1 to no_gender variable
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
#compare original gender column with newly created female 
#column to check the replacement.no missing data now
table(teens$no_gender, useNA = "ifany")
#display 0 for gender and 1 for NA

#fill the missing data for age
mean(teens$age)
#mean for missing data is undefined 
mean(teens$age, na.rm = TRUE) 
#remove missing data once to find mean for average age for graduation year
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE) 
#used to find average age for every year
#after removing all the NA values
ave_age <- ave(teens$age, teens$gradyear, FUN =
                 function(x) mean(x, na.rm = TRUE))
#use ave function to calculate the mean of all the subgroups available
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age) 
#use ave on the NA values
summary(teens$age) #no missing values now

#training data
#install.packages("stats") #implementation of Kmeans
library(stats)
interests <- teens[5:40]
#consider numeric values
interests_z <- as.data.frame(lapply(interests, scale))
#scale to rescale features to get mean of 0 and std deviation of 1
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)#take five clusters

#model performance
teen_clusters$size 
#show the cluster size of 5 clusters. so many teenagers 
#have same interest so to verify find the centroid
teen_clusters$centers 
#to find the centroid.some values are more than mean and 
#some are below mean

#improve performance
teens$cluster <- teen_clusters$cluster
teens[1:5, c("cluster", "gender", "age", "friends")]
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)