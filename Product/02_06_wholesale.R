# Data Appliation Lab - copyright

install.packages("NbClust")
library(NbClust)

data <- read.csv('2.Wholesale_customers_data.csv', header = T,sep=',')

str(data)
summary(data)

# Scale data
testdata <- scale(testdata)
summary(testdata)

# Determine number of clusters. Option 1: visual rule
wss <- (nrow(testdata)-1)*sum(apply(testdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(testdata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Determine number of clusters. Option 2: more frequent optimal number
res <- NbClust(data, diss=NULL, distance = "euclidean", min.nc=2, max.nc=12, 
               method = "kmeans", index = "all")

# More information
res$All.index
res$Best.nc
res$All.CriticalValues
res$Best.partition

# K-Means Cluster Analysis (based on the proposed number by NbCluster)
fit <- kmeans(testdata, 3)

# Calculate average for each cluster
aggregate(data,by=list(fit$cluster),FUN=mean)

# Add segmentation to dataset
data <- data.frame(data, fit$cluster)
head(data)
