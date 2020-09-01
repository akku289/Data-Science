set.seed(123)
FraudCheck <- read.csv(file.choose())
hist(FraudCheck$Taxable.Income)
hist(FraudCheck$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]
str(FC)
table(FC$Risky_Good) 
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
install.packages("randomForest")
library(randomForest)
rf <- randomForest(Risky_Good~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting
pred1 <- predict(rf, train)
head(pred1)
head(train$Risky_Good)
confusionMatrix(pred1, train$Risky_Good)
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good)
# Error Rate in Random Forest Model :
plot(rf)
