zoo <- read.csv(file.choose())
library(class)
library(knn.covertree)
str(zoo)
summary(zoo)
names(zoo)
types <- table(zoo$type)
types
zoo_target <- zoo[,18]
zoo_key <- zoo[,1]
zoo$animal.name <- NULL
k <- sqrt(17)+1
m1 <- knn.cv(zoo, zoo_target, k, prob = TRUE)
prediction <- m1
t1 <- table(zoo_target,prediction)
t1
acc <- sum(diag(t1)/length(zoo_target)*100)
acc
data.frame(types)
