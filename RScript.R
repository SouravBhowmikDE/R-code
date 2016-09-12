library(som)
library(ggplot2)
library(lattice)
library(caret)
library(mlbench)
library(e1071)
library(plotly)
library(gridExtra)
myData= read.csv("TestDataOrzota.csv")
myData
N9=myData[ , "N9"]
#normalize
myData <- as.data.frame(lapply(myData[2:10], normalize))
summary(myData$N1)
myData
#data1=c(myData[2])
#data1
#data1 is a list
data1<-as.vector(myData[2])
plot(data1)
#N5 is a vector
N5=myData[ , "N5"]
N4=myData[ , "N4"]
plot(N5)

#plotting 2 vectors
plot(N5, type="o", col="blue", pch=12)
lines(N4, type="o", pch=22, lty=2, col="red")

#qplot
#
qplot(x=N1)+ scale_x_discrete(breaks=1:31)+ geom_histogram(aes(x = N1), binwidth = 1)
summary(N1)
summary(log10(N1))
summary(log10(N1+1))
summary(sqrt(N1))

#legend
legend(14, 70, c("N5", "N4"), col=c("blue", "red"), pch = c(12, 22))

#labelling
box()
title(xlab="N5", col.lab=rgb(0,0.5,0))
title(ylab="N4", col.lab=rgb(0,0.5,0))
  
#histogram
  barplot(N5)
  barplot(as.matrix(myData[2:3]), main="data", ylab= "Total",beside=TRUE, col=rainbow(185))
  
#piechart(requires a positive vector)
  w=c(1,2,3,4,5)
  pie(w)
  
#dotchart
  dotchart(N5)
  
#skewness
  library("e1071")
  skewness(N5)
  
#collinear  
correlationMatrix <- cor(myData[,2:9])

#highlyCorrelated stores column no. that are correlated
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

#Finding important features
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(target~., data=myData , method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)






