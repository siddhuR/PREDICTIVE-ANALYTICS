#start
install.packages("arulesViz")
install.packages("arules")
library(datasets)
library(arules)
library(arulesViz)
#explore the data
data(Groceries)
class(Groceries)
Groceries@itemInfo[1:20,]
View(Groceries)

apply(Groceries@data[,10:20],2,function(r) paste(Groceries@itemInfo[r,"labels"]
                                                 , collapse=", "))
itemsets<-apriori(Groceries,parameter=list(minlen=1,maxlen=1,support=0.02,
                                           target= "frequent itemsets"))
summary(itemsets)
inspect(head(sort(itemsets,by="support"),10))
itemsets<-apriori(Groceries,parameter=list(minlen=2,maxlen=2,support=0.02,
                                           target="frequent itemsets"))
summary(itemsets)
inspect(head(sort(itemsets,by="support"),10))
itemsets<-apriori(Groceries,parameter=list(minlen=3,maxlen=3,support=0.02,
                                           target="frequent itemsets"))
summary(itemsets)
inspect(head(sort(itemsets,by="support"),10))
itemsets<-apriori(Groceries,parameter=list(minlen=4,maxlen=4,support=0.02,
                                           target="frequent itemsets"))
summary(itemsets)
inspect(head(sort(itemsets,by="support"),10))
itemFrequencyPlot(Groceries,topN=20,type="absolute")
?apriori
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
summary(rules)
options(digits=2)
inspect(rules[1:10])
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:10])
plot(rules)

slope<-sort(round(rules@quality$lift/rules@quality$confidence,2))
unlist(lapply(split(slope,f=slope),length))
inspect(head(sort(rules,by="lift"),10))
confidentRules<-rules[quality(rules)$confidence>0.9]
confidentRules
plot(confidentRules,method="matrix",measure=c("lift","confidence"),
     control=list(reorder="none"))
highLiftRules<-head(sort(rules,by="lift"),5)
plot(highLiftRules,method="graph",control=list(type="items"))