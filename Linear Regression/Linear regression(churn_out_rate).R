sh_cr <- read.csv(file.choose())
view(sh_cr)
plot(sh_cr$Salary_hike,sh_cr$Churn_out_rate)
boxplot(sh_cr)
hist(sh_cr$Salary_hike)
hist(sh_cr$Churn_out_rate)
summary(sh_cr)
cr<- sh_cr$Churn_out_rate
sh <- sh_cr$Salary_hike
cor(cr,sh)
# This has a strong negative Correlation 
reg<-lm(cr~sh)
summary(reg)
Confint(reg,level = 0.95)
predict(reg,interval="predict")
reg_log <- lm(cr~log(sh))
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# Exponential model 
reg_exp <- lm(log(cr)~sh)
summary(reg_exp)
Confint(reg_exp,level = 0.95)
exp(predict(reg_exp,interval="predict"))
# Quadratic model
sh_cr[,"sh_sq"] = sh*sh
quad_mod <- lm(cr~sh+I(sh^2),data=sh_cr)
summary(quad_mod)
confint(quad_mod,level=0.95)
predict(quad_mod,interval="predict")
# Cubic model
poly_mod <- lm(cr~sh+I(sh^2)+I(sh^3),data=sh_cr)
summary(poly_mod)
confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")
# Cubic  model gives the best Adjusted R-Squared value
predicted_Value <- predict(poly_mod)
predicted_Value
Final <- cbind(Salary_Hike=sh_cr$Salary_hike,Churn_Rate = sh_cr$Churn_out_rate,Pred_Chr_rate=predicted_Value)
View(Final)

rmse<-sqrt(mean((predicted_Value-cr)^2))
rmse
plot(poly_mod)
