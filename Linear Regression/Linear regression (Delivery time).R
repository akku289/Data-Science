dt.st <- read.csv(file.choose()) 
View(dt.st)
plot(dt.st$Sorting.Time,dt.st$Delivery.Time)
boxplot(dt.st)
hist(dt.st$Sorting.Time)
hist(dt.st$Delivery.Time)
summary(dt.st)
dt<- dt.st$Delivery.Time
st <- dt.st$Sorting.Time
cor(st,dt)
reg<-lm(dt~st)
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval="predict")
# Logarthmic transformation
reg_log<-lm(dt~log(st))  
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# Exponential model 
reg_exp<-lm(log(dt)~st)
summary(reg_exp)
confint(reg_exp,level=0.95)
exp(predict(reg_exp,interval="predict"))
# Quadratic model
dt.st[,"st_sq"] = st*st

quad_mod <- lm(dt~st+I(st^2),data=dt.st)
summary(quad_mod)
confint(quad_mod,level=0.95)
predict(quad_mod,interval="predict")
# Cubic model
poly_mod <- lm(dt~st+I(st^2)+I(st^3),data=dt.st)
summary(poly_mod)
confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")
predicted_Value <- exp(predict(reg_exp))
predicted_Value
Final <- cbind(Sorting_Time=dt.st$Sorting.Time ,Delivery_Time = dt.st$Delivery.Time,Predicted_Delivery_time=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-dt)^2))
rmse
