library(caret)
library(e1071)
library(class)
install.packages("e1071")

kn = read.csvread.csv("/Users/virajdattkohir/Downloads/serviceTrainData.csv",row.names = 1)
knt = read.csv("/Users/virajdattkohir/Downloads/serviceTestData.csv",row.names = 1)
summary(kn)
head(kn)
str(kn)


predictknn = knn(train = kn[,-6], test = knt[,-6],cl = kn$Service, k =3)
predictknn
con_mat = table(predictknn,knt[,6])
con_mat
confusionMatrix(predictknn,knt$Service)
mean(predictknn!= knt$Service)
