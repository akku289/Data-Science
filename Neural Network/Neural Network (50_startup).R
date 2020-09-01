library(nnet)
library(neuralnet)
library(plyr)
library(NeuralNetTools)
startup <- read.csv(file.choose())
View(startup)
class(startup)
startup$State <- revalue(startup$State,
                         c("New York"="0", "California"="1", "Florida"="2"))
str(startup)
startup$State <- as.numeric(startup$State)
attach(startup)
startup <- as.data.frame(startup)
str(startup)
plot(State, Profit)
plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)
pairs(startup)
cor(startup)
summary(startup)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
startup_norm<-as.data.frame(lapply(startup,FUN=normalize))
summary(startup_norm)
view(startup_norm)
set.seed(123)
ind <- sample(2,nrow(startup_norm),replace = TRUE,prob = c(0.75,0.25))
train <- startup_norm[ind==1,]
test <- startup_norm[ind==2,]
startup_model <- neuralnet(Profit~.,train)
str(startup_model)
plot(startup_model)
plotnet(startup_model)
model_perfection <- compute(startup_model,test[1:4])
model_perfection
predicted_profit <- model_perfection$net.result
cor(predicted_profit,test$Profit)
#de-normalizing to get the actual prediction on profit

un_max <- max(startup$Profit)
un_min <- min(startup$Profit)
unnormalize <- function(x,min,max){return((max-min)*x+min)}
actual_profit_prediction <- unnormalize(predicted_profit,un_max,un_min)
head(actual_profit_prediction)

#to increase accuracy of model
startup_model2 <-neuralnet(Profit~.,train,hidden = 2)
plot(startup_model2)
model_perfection2 <- compute(startup_model2,train[1:4])
model_perfection2
