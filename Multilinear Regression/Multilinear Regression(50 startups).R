Startups <- read.csv(file.choose())
View(Startups)
class(Startups)
library(plyr)
Startups$State <- revalue(Startups$State,
                          c("New York"="0", "California"="1", "Florida"="2")) 
Startups$State <- as.numeric(Startups$State)
attach(Startups)
Startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)


Startups <- as.data.frame(Startups)

attach(Startups)
summary(Startups)
plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
pairs(Startups)
cor(Startups)
Model.Startups <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(Model.Startups)
Model.Startups1 <- lm(Profit~RD_Spend+log(Administration))
summary(Model.Startups1)
library(corpcor)
cor2pcor(cor(Startups))
library(mvinfluence)
library(car)
influence.measures(Model.Startups)
influenceIndexPlot(Model.Startups, id.n=3)

# Regression after deleting the 49th and 50th observation, which is influential observation

# Logarthimic Transformation 
Model.Startups_Log<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=Startups[-c(49,50),])  

summary(Model.Startups_Log)
confint(Model.Startups_Log,level=0.95)
predict(Model.Startups_Log,interval="predict")
Model.Startups_Fin1<-lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=Startups[-c(49,50),])
summary(Model.Startups_Fin1)
# Exponential Transformation :
Model.Startups_exp<-lm(log(Profit)~RD_Spend+Administration+Marketing_Spend+State,data=Startups[-c(49,50),])
summary(Model.Startups_exp)
# Quad Model
Model.Startups_Quad <- lm(Profit~RD_Spend+I(RD_Spend^2)+Administration+I(Administration^2)
                          +Marketing_Spend+I(Marketing_Spend^2)+State+I(State^2),data=Startups[-c(49,50),])
summary(Model.Startups_Quad)
confint(Model.Startups_Quad,level=0.95)
predict(Model.Startups_Quad,interval="predict")
vif(Model.Startups_Log)
FinalModel<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+
                 log(State),data=Startups[-c(49,50),])
summary(FinalModel)
Profit_Predict <- predict(FinalModel,interval="predict")
Final <- cbind(Startups$RD_Spend,Startups$Administration,Startups$Marketing_Spend,
               Startups$State,Startups$Profit,Profit_Predict)
View(Final)
plot(FinalModel)
