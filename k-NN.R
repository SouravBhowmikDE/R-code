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
summary(myData$N1)
myData

#dividing into training and test sets
data_train <- myData[1:180,]
data_train
data_test <- myData[181:185,]
data_test

#applying kNN regression
data_test_prediction <- knn.reg(train = data_train, test = data_test, y1[1:180], k=18)
data_test_prediction
data_test_prediction["pred"]