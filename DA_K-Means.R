library(caret)
library(class)
km = read.csv(file.choose())
View(km)
str(km)
tripcluster = kmeans(km,3)
tripcluster
summary(tripcluster)


#Using the Elbow method 
kmax = 10
wss = rep(NA,kmax)
nclust = list()

for (i in 1:kmax) {
  driveClasses = kmeans(km,i)
  wss[i] = driveClasses$tot.withinss
  nclust[[i]] = driveClasses$size 
}

plot(1:kmax,wss,type = "b", xlab = "No. of Clusters", ylab = "Total within clusters sum of squares value  ")


#Testing 
wss[1]
length(nclust)
nclust[10]
nclust[1]
nclust[2]
