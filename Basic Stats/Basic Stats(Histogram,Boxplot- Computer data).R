#Histogram

computer <- read.csv(file.choose())
View(computer)
hist(computer$price,col="green",main= "Histogram of Price",xlab = "Price")
hist(computer$speed, col="pink",breaks = 3,main= "Histogram of Speed",xlab = "Speed")
hist(computer$hd, col="blue",main= "Histogram of HD",xlab = "HD")
hist(computer$ram,breaks = 3, col="red",main= "Histogram of Ram",xlab = "Ram")
hist(computer$screen, col="darkgrey",breaks = 3, main = "Histogram of screen",xlab = "Screen")
hist(computer$ads, col="yellow",breaks = 3, main = "Histogram of ADS",xlab = "ADS")
hist(computer$trend, col="black",breaks = 3, main = "Histogram of Trend",xlab = "Trend")

#Boxplot

boxplot(computer$price,main= "Boxplot for Price")
boxplot(computer$speed,main= "Boxplot for Speed")
boxplot(computer$hd,main= "Boxplot for HD")
boxplot(computer$ram,main= "Boxplot for Ram")
boxplot(computer$screen,main= "Boxplot for Screen")
boxplot(computer$ads,main= "Boxplot for ADS")
boxplot(computer$trend,main= "Boxplot for trend")