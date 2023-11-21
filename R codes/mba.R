library(arules)
getwd()
groceries <- read.transactions("groceries.csv",
                               sep = ",")
groceries
inspect(groceries[1:5])
summary(groceries) 
itemFrequencyPlot(groceries, support = 0.1)
#plot the bar chart using atleast 10% of support
itemFrequencyPlot(groceries, topN = 20)
#plot with 20 items
apriori(groceries) 
#by default support is 0.1 and confidence is 0.8
groceryrules <- apriori(groceries, parameter = 
                          list(support =
                                 0.006, confidence = 0.25, 
                               minlen = 2))
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
#for support, assumimg one item is getting purchased 
#2 times a day, 
#means 60 times a month. it means 
#60/9835 equals 0.006. for confidence consider 
#a rule moving the
#smoke detectors closer to the
#batteries increase sale
groceryrules
#463 rules created
summary(groceryrules)
#if lift more than 1, it means that the two items are found 
#together more often than one would expect
#by chance.
inspect(groceryrules[1:3])
inspect(sort(groceryrules, by = "lift")[1:5])
#sort the best 5 rules from all the rules
berryrules <- subset(groceryrules, items %in% "whole milk")
#filter out all the rules having berries
inspect(berryrules)

#write rules to the csv file
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
