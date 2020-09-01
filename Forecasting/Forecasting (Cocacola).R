library(forecast)
library(fpp)
library(smooth)
library(readxl)
Cocacola <- read_excel(file.choose())
View(Cocacola)  
windows()
plot(Cocacola$Sales,type="o")
Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')
CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)
CocacolaData["t"]<- 1:42
View(CocacolaData)
CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
CocacolaData["t_square"]<-CocacolaData["t"]*CocacolaData["t"]
attach(CocacolaData)
train<-CocacolaData[1:36,]

test<-CocacolaData[37:40,]


linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad"),c(rmse_linear,rmse_expo,rmse_Quad))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))
new_model_fin <- new_model$fitted.values

View(new_model_fin)

# pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Quarter <- as.data.frame(CocacolaData$Quarter)

Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")
View(Final)
