#Histogram

cars <- read.csv(file.choose())
View(cars)
hist(cars$HP,col="green",main= "Histogram of HP",xlab = "HP")
hist(cars$MPG, col="pink",main= "Histogram of MPG",xlab = "MPG")
hist(cars$VOL, col="blue",main= "Histogram of VOL",xlab = "VOL")
hist(cars$SP, col="red",main= "Histogram of SP",xlab = "SP")
hist(cars$WT, col="darkgrey",main = "Histogram of WT",xlab = "WT")

#Boxplot

boxplot(cars$HP,main= "Boxplot for HP")
boxplot(cars$VOL,main= "Boxplot for Vol")
boxplot(cars$MPG,main= "Boxplot for MPG")
boxplot(cars$SP,main= "Boxplot for SP")
boxplot(cars$WT,main= "Boxplot for WT")
