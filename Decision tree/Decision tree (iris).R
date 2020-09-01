data("iris")
str(iris)
summary(iris)

#Data partition

set.seed(123)
ind <- sample(2,
              nrow(iris),
              replace = TRUE,
              prob= c(0.8, 0.2))
train <- iris[ind==1,]
test <- iris[ind==2,]

#Decision tree model

library("party")
tree<- ctree(Species~.,train,
             controls = ctree_control(mincriterion =.999,minsplit = 6))
print(tree)

#visulaization of decision tree
plot(tree)
plot(tree,type="simple")

#predicition
predict(tree,train, type= "prob")


#misclassification of error-train data

p1 <- predict(tree,train)
tab1 <- table(Predicted=p1, Actual=train$Species)
tab1
1-sum(diag(tab1)/sum(tab1))


#misclassification of error -test data
p2=predict(tree,test)
tab2 <- table(Predicted=p2,Actual=test$Species)
1-sum(diag(tab2)/sum(tab2))