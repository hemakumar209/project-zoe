rm(list=ls(all=T))
getwd()
setwd("C:/Users/sauragarwal/Desktop/dsr")
# Read the data
rawData <- read.csv("Data.csv", header=T)

#Check data stats
length(unique(rawData$CustomerID))
length(unique(rawData$ProductID))
length(unique(rawData$TransactionID))
sum(rawData$Cost)

# Let's categorize CustomerID and ProductID
rawData$CustomerID <- as.factor(rawData$CustomerID)
rawData$ProductID <- as.factor(rawData$ProductID)
rawData$RetailStore <- as.factor(rawData$RetailStore)
rawData$TransactionID <- as.factor(rawData$TransactionID)

#Make sure Cost is numeric
rawData$Cost <- as.numeric(rawData$Cost)

# Convert the date fields to uniform format
rawData$Transaction.Time <- gsub("/", "-", rawData$Transaction.Time)


rawData$Transaction.Time <- strptime(rawData$Transaction.Time,
                                     format='%d-%m-%Y %H:%M')

rawData$Transaction.Time <- as.Date(rawData$Transaction.Time)
#raw data in the right formats

# Remove NAs. There is no 29th in Feb of 2011. Those are the only NAs. So, let's remove them.
rawData <- na.omit(rawData)


# Let's create lookup data just in case we need it later
Customer <- unique(rawData$CustomerID)
Products <- unique(rawData$ProductID)
Transactions <- unique(rawData$TransactionID)

Months <- months(rawData$Transaction.Time)
Quarters <- quarters(rawData$Transaction.Time)
WeekDays <- weekdays(rawData$Transaction.Time)

#Aggregating data by unique Customer ID , on unique number of 

aggCustId <-aggregate(rawData, by=list(rawData$CustomerID), function(x) length(unique(x)))
        
aggCustIdnew <- aggregate(rawData$Cost ~ CustomerID, rawData, sum)

aggnew <- merge(aggCustId, aggCustIdnew, by.x = c("Group.1"), by.y = "CustomerID")

#Removing columns which aren't required 

aggnew <- aggnew[ -c(3,5,7) ]

#Renaming aggregated columns for further use

library(plyr)
aggnew <- rename(aggnew, c("Group.1"="CustomerID", "RetailStore"="StoreFrequency","TransactionID"="TransactionFrequency","ProductID"="ProductFrequency","rawData$Cost"="RevenueValue"))


#library(infotheo)
#aggnew$RevenueValue <- discretize(aggnew$RevenueValue, disc="equalfreq",nbins=5)
#aggnew$RevenueValue <- data.matrix(aggnew$RevenueValue)
#aggnew <- aggnew[ -c(5) ]

#aggnew <- rename(aggnew, c("RevenueValueBinned"="RevenueValue"))

#aggnew <- na.omit(aggnew,na.action=TRUE)

#Assigning CustomerID as rownames and removing it from dataset for easy clustering

rownames(aggnew) <- aggnew$CustomerID

aggnew <- aggnew[ -c(1) ]

# Changing type of Revenue column

class(aggnew$RevenueValue) <- "integer"

#aggnew <- data.matrix(aggnew)

#is.matrix(aggnew)

#Checking all types are good to go

sapply(aggnew, class)

#Checking for outliers

boxplot(aggnew)


#Analyze for outliers

boxplot(aggnew[,c(3)])
plot(aggnew[,c(3)])

## from the above plots we observe that there are so many entries whose values are in the outlier category. Hence, it would not be a good idea to remove this outlier.

##Since the data attributes are of different varieties their scales are also different. In order to maintain uniform scalability we scale the columns.

aggnewcl <- scale(aggnew)

# Calculating variance and storing at the first index in wss

wss <- (nrow(aggnewcl)-1)*sum(apply(aggnewcl,2,var))

## iterate through wss array 15 times and sum up all the variance in every iteration and store it in wss array

for (i in 2:15) wss[i] <- sum(fit=kmeans(aggnewcl,centers=i,15)$withinss)

## plot each iteration to display the elbow graph 

plot(1:15, wss, type="b",main="15 clusters", xlab="Number of Clusters",ylab="Within cluster sum of squares")

##As we can see from the above output the slope of the graph changes majorly in 4 iteration, hence we consider the optimized number of cluster as 3 in which we can get the optimum result

fit <- kmeans(aggnew,4)

## Let's check the summary of the kmeans objects

fit$iter

## checking withinss i.e. the intra cluster bond strength factor for each cluster

fit$withinss

## checking betweenss i.e. the inter cluster distance between cluster

fit$betweenss

#Interpreting mined patterns

plot(aggnewcl,col=fit$cluster,pch=15)
points(fit$centers,col=1:8,pch=6)

library(cluster)
library(fpc)
plotcluster(aggnewcl,fit$cluster)
points(fit$centers,col=1:8,pch=16)

clusplot(aggnewcl, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#Writing the Clusters to a file

is.data.frame(myDF)
myDF <- cbind(Row.Names = rownames(fit$cluster), fit$cluster)
myDF <- cbind(Row.Names = rownames(myDF), myDF)
FDF <- as.data.frame(myDF)
sapply(FDF, class)
class(FDF$Row.Names) <- "numeric"
class(FDF$V2) <- "numeric"

write.csv(file="file1.csv", x=FDF)
write.csv(file="file2.csv",x=aggnew)

#Hierarchical clustering

# Ward Hierarchical Clustering
d <- dist(aggnewcl, method = "euclidean") # distance matrix
fit1 <- hclust(d, method="ward.D") 
plot(fit1) # display dendogram

groups <- cutree(fit1, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters 
rect.hclust(fit1, k=4, border="red")


# Ward Hierarchical Clustering with Bootstrapped p values
aggnewcl <- t(aggnewcl)
library(pvclust)
fit2 <- pvclust(aggnewcl, method.hclust="ward.D",method.dist="euclidean")
plot(fit2) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit2, alpha=.95)


# Model Based Clustering
library(mclust)
fit3 <- Mclust(aggnewcl)
plot(fit3) # plot results 
summary(fit3) # display the best model


# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit$cluster, fit1$cluster)


