library(arules)
library(arulesViz)
book <- read.csv(file.choose())

View(book)

rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))
rules
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
