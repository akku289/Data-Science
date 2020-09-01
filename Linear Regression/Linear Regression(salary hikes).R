ye.sh <- read.csv(file.choose())
View(ye.sh)
plot(ye.sh$YearsExperience,ye.sh$Salary)
boxplot(ye.sh)
hist(ye.sh$YearsExperience)
hist(ye.sh$Salary)
summary(ye.sh)
ye<- ye.sh$YearsExperience
sh <- ye.sh$Salary
cor(ye,sh)
reg<-lm(sh~ye)
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval="predict")
# Regression using logarthmic transformation
reg_log<-lm(sh~log(ye))  
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# Cubic model
poly_mod <- lm(sh~ye+I(ye^2)+I(ye^3),data=ye.sh)
summary(poly_mod) 
confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")
# Cubic  model gives the best Adjusted R-Squared value
predicted_Value <- predict(poly_mod)
predicted_Value
Final <- cbind(YearsofExp=ye.sh$YearsExperience,Sal_Hike = ye.sh$Salary,Pred_sal_hike=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-sh)^2))
rmse
plot(poly_mod)
