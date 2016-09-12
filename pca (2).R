mydata <- read.csv("TestDataOrzota.csv")  # read csv file 
m1<-mydata
mydata$N1[1:20][mydata$N1[1:20]==0] <- NA
mydata$N3[21:40][mydata$N3[21:40]==0] <- NA

head(mydata)
any(is.na(mydata))

#View a summary() of dataset

summary(mydata)

#-----------------------------------------------------------------------------

library(DmWR)

impdata <- knnImputation(mydata, k = 5)

head(impdata)

#------------------------------------------------------------------------------

library(clusterSim)

dataNormZ <- data.Normalization(impdata[2:10],type="n1",normalization="column")

head(dataNormZ)

#-------------------------------------------------------------------------------

# apply PCA 

n.pca <- prcomp(dataNormZ, center = TRUE, scale. = TRUE)

print(n.pca)
summary(n.pca)

#plot(n.pca, type = "l")

#compute standard deviation of each principal component
std_dev <- n.pca$sdev

#compute variance
pr_var <- std_dev^2

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
              ylab = "Cumulative Proportion of Variance",
              type = "b")