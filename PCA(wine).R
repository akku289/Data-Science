library(cluster)
library(factoextra)
mydata<-read.csv(file.choose())
View(mydata)
View(mydata[-1]) 
# mydata[-1] -> Considering only numerical values for applying PCA
data <- mydata[,-1]
attach(data)
cor(data)

pca<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
str(pca)
summary(pca)
loadings(pca)
plot(pca)
biplot(pca)
pca$scores[,1:3]
mydata<-cbind(mydata,pca$scores[,1:3])
View(mydata)

clus_data<-mydata[,8:10]

# Normalizing the data 
norm_clus<-scale(clus_data) 
dist1<-dist(norm_clus,method = "euclidean") 

fit1<-hclust(dist1,method="complete") 
plot(fit1) 

rect.hclust(fit1, k=7, border="red")
groups<-cutree(fit1,7) 

membership_1<-as.matrix(groups)

View(membership_1)

final1<-cbind(membership_1,mydata) 
View(final1)
