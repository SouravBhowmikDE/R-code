library(som)
library(ggplot2)
library(lattice)
library(caret)
library(mlbench)
library(e1071)
library(plotly)
library(FNN)

myData= read.csv("TestDataOrzota.csv")
myData
y1=myData[ , "Target"]
y1

#normalize
myData <- as.data.frame(lapply(myData[ , 2:10], normalize))

#applying PCA
pca =prcomp(myData)
plot(pca)
summary(pca)
sigma1<-(myData[2:10])*(t(myData[2:10]))
sigma=svd(myData[2:10])

#normalize
myData <- as.data.frame(lapply(myData[ , 2:10], normalize))
summary(myData$N1)
myData