library(caret)
library(class)
kn = read.csv(file.choose())
kn = kn[, !apply(is.na(kn ),2,all)]
str(kn)
kn$Promoffer = as.factor(kn$Promoffer)
str(kn)
knb = kn

plot(kn$Income,kn$Spending, las = 1,xlab = "Annual Income", ylab = "Hosehold Area", pch=c(21,19)[as.numeric(kn$Promoffer)])

kn = kn[,c(-5:-9)]
str(kn)
#Normalization
kn[,c(1,2,4)] = scale(kn[,c(1,2,4)], center = T, scale = T) 
head(kn)

partidx = sample(1:nrow(kn), 0.6*nrow(kn), replace = F)
length(partidx)
kntrain = kn[partidx,]
kntest = kn[-partidx,]


knn_model = knn(train = kntrain[,c(1,2,4)],test =  kntrain[,c(1,2,4)], cl = kntrain$Promoffer, k = 300)

knn_model
kntrain$Promoffer
mean(knn_model != kntrain$Promoffer)
table("Actual Value"=kntrain$Promoffer, "Predicted"=knn_model)

length(kntrain$Promoffer[which(kntrain$Promoffer==0)])
#Chossing the value of K 
modtrain = NULL
modtest = NULL
errtest = NULL
errtrain = NULL

for(i in 1:30) {
  modtrain = knn(train =kntrain[,1:2], test = kntrain[,1:2], cl = kntrain$Promoffer, k = i )
  modtest = mod = knn(train =kntrain[,1:2], test = kntest[,1:2], cl = kntrain$Promoffer, k = i )
  errtrain[i] = 100*mean(mod != kntrain$Promoffer)
  errtest[i] = 100*mean(mod != kntest$Promoffer)
}
length(errtest)
length(errtrain)
dfp11 = data.frame("Value of K"=1:30,"ErrorTraining" = errtrain,"ErrorValidation" = errtest)

round(dfp11, digits =2)
plot(dfp11$Value.of.K, dfp11$ErrorValidation, las = 1, type = "l", xlab = "value of k", ylab = "Validation Error", ylim = c(0,80))
lines(dfp11$Value.of.K, dfp11$ErrorTraining)

summary(knn_model)
