library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)
train_sal <- read.csv(file.choose())
str(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)
test_sal <- read.csv(file.choose())
str(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
ggplot(data = train_sal,aes(train_sal$Salary,train_sal$age, fill=train_sal$Salary))+geom_boxplot()+ggtitle("Box Plot")
plot (train_sal$workclass,train_sal$Salary)
plot(x= train_sal$education,y=train_sal$Salary)
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalgain, fill = train_sal$Salary)) +
geom_boxplot() +
ggtitle("Box Plot")
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalloss, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(train_sal$native,train_sal$Salary)
ggplot(data=train_sal,aes(x = train_sal$education, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("education Density Plot")
model1<-ksvm(train_sal$Salary~., 
             data= train_sal,kernel = "vanilladot")
model1
Salary_prediction <- predict(model1, test_sal)

table(Salary_prediction,test_sal$Salary)
model_rfdot<-ksvm(train_sal$Salary~., 
                  data= train_sal,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_sal)
mean(pred_rfdot==test_sal$Salary) 
model_vanilla<-ksvm(train_sal$Salary~., 
                    data= train_sal,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test_sal)
mean(pred_vanilla==test_sal$Salary)
