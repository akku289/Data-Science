library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
FraudCheck <- read.csv(file.choose())
hist(FraudCheck$Taxable.Income)
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)

FC_train <- FC[1:300,]

FC_test <- FC[301:600,]

png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good)summary(op_tree)
CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)