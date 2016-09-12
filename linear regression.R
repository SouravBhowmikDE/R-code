library(som)
library(ggplot2)
library(lattice)
library(caret)
library(mlbench)
library(e1071)
library(plotly)
library(FNN)

#normalize
myData <- as.data.frame(lapply(myData[ , 2:10], normalize))
summary(myData$N1)
myData

Target=myData[ , "Target"]
N1=myData[ , "N1"]
N2=myData[ , "N2"]
N3=myData[ , "N3"]
N4=myData[ , "N4"]
N5=myData[ , "N5"]
N6=myData[ , "N6"]
N7=myData[ , "N7"]
N8=myData[ , "N8"]
N9=myData[ , "N9"]

lmfit = lm( Target ~ N1+N2+N3+N4+N5+N6+N7+N8+N9 )
summary(lmfit)
