# Importing training data ( note the na.string argument in read.csv )
TrainingData<-read.csv("C:/Users/soura/Desktop/Data Science/Analytics Vidhya Hackathons/Loan Prediction/trainingSet.csv", sep = ",", header = TRUE, na.strings=c("NA","NaN", ""))

# Importing training data
TrainingData<-read.csv("C:/Users/soura/Desktop/Data Science/Analytics Vidhya Hackathons/Loan Prediction/trainingSet.csv", sep = ",", header = TRUE)
#apply(TrainingData, 3, function(x) gsub("^$|^ $", NA, x))
# Viewing the data
View(TrainingData)
#-------------------------------------------------------
#This part is very important
#We have 2 factors( Self_Employed and Dependents ) with empty "" as one of the levels. These are actually meant to be NAs. So we explicitly assign them the value NA 
TrainingData$Self_Employed[TrainingData$Self_Employed=='']=NA
TrainingData$Dependents[TrainingData$Dependents=='']=NA

# Now we need to change the no. of levels. For that we use droplevels function to remove unused levels
TrainingData$Dependents<-droplevels(TrainingData$Dependents)
TrainingData$Self_Employed<-droplevels(TrainingData$Self_Employed)

#--------------------------------------------------------

summary(TrainingData$ApplicantIncome)
count(TrainingData)

library(ggplot2)

sum(complete.cases(TrainingData))

sum(!complete.cases(TrainingData))

which(!complete.cases(TrainingData))

library(Tmisc)

#Calculates the no. of missing values and their proportion
propmiss(TrainingData)

#---------------------------------------------------------------------------------------------------------------------
#This section analyses and manipulates missing values or NAs 
 # Gives missing values per column
  sapply(TrD,function(x) sum(is.na(x)))

# Gives unique values per column
sapply(TrD, function(x) length(unique(x)))

# For a visual representation of missing values
library(Amelia)
missmap(TrD, main = "Missing values vs observed")

is.factor(TrD$Married)
is.factor(TrD$Dependents)
summary(TrD$Dependents)

#---------------------------------------------------------------------------------------------------------------------
  
#Removing all the rows that contain atleast one missing value
TrD <- na.omit(TrainingData)

TrD$Loan_ID<- NULL
TrD$Self_Employed<-NULL
TrD$Gender <- NULL
TrD$Married <- NULL
TrD$Property_Area<-NULL

View(TrD)
#----------------------------------------------------------------------------------------------------------------------
# This section is for logistic regression
#mylogit <- glm(Loan_Status ~  + Dependents + Education + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History , data = TrD, family = "binomial")
mylogit <- glm(Loan_Status ~  + Dependents + Education + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History , data = TrD, family = "binomial")
summary(mylogit)








TrD$Amt_per_time<-TrD$LoanAmount/TrD$Loan_Amount_Term

#----------------------------------------------------------------------------------------------------------------------
xtabs(~ Loan_Status + Dependents, data = TrD)

ggplot(aes( x = Loan_Status, y = LoanAmount), data = TrD) + geom_point()

library(VIF)

vif(TrD$Loan_Status, TrD )