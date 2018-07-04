cr = read.csv("/Users/virajdattkohir/Downloads/crashTest_1.csv",row.names = 1)
crT = read.csv("/Users/virajdattkohir/Downloads/crashTest_1_TEST.csv",row.names = 1)
library(caret)
View(cr)

str(cr)
summary(cr)
logfit = glm(formula = cr$CarType~., family = "binomial", data = cr)
logfit
summary(logfit)
logtrain = predict(logfit, type = 'response')
plot(logtrain)
tapply(logtrain,cr$CarType,mean)
logpredicttest = predict(logfit, newdata = crT, type = 'response')
plot(logpredicttest)

crT[logpredicttest <= 0.5, "Predicted"] = "Hatchback"
crT[logpredicttest > 0.5, "Predicted"] = "SUV"
View(crT)

mt = read.csv("/Users/virajdattkohir/Downloads/mtcars_road_test.csv",row.names = 1)
class(mt)
View(mt)
str(mt)
typeof(mt$am)
typeof(mt$mpg)
as.factor(mt$am)
fitmt = glm(formula =  mt$am~ mt$mpg, family = "binomial", data = mt)
summary(fitmt)

ny= read.csv("/Users/virajdattkohir/Downloads/nyc.csv")
View(ny)
fitmt = glm(formula = ny$East~., family = "binomial", data = ny)
summary(fitmt)
plot(fitmt)
segments(7,0,7,18.8)