ml = read.csv(file.choose())
View(ml)
plot(ml, main="Pairwise Scatter plot")
round(cor(ml),3)
#Food and Service are correlated
nycmlmod = lm(Price~Food+Decor+Service+East,data = ml)
summary(nycmlmod)
nycmlmod2 = lm(Price~Food+Decor+East,data = ml)
summary(nycmlmod2)
nycmlmod3 = lm(Price~Decor+Service+East,data = ml)
summary(nycmlmod3)

#Residual Analysis
plot(nycmlmod2$fitted.values,rstandard(nycmlmod2),main = "Residual plot", xlab = "Predicted bid price", ylab="Stanadrdized Residuals")
abline(h=2,lty=2)#lty = line type(dot)
abline(h=-2,lty=2)
identify(nycmlmod2$fitted.values,rstandard(nycmlmod2))
#56

ml2 = ml[-56,]
nycmlmod2new = lm(Price~Food+Decor+East,data = ml2)
plot(nycmlmod2new$fitted.values,rstandard(nycmlmod2new),main = "Residual plot", xlab = "Predicted bid price", ylab="Stanadrdized Residuals")
l = summary(nycmlmod2new)
abline(h=2,lty=2)#lty = line type(dot)
abline(h=-2,lty=2)
identify(nycmlmod2new$fitted.values,rstandard(nycmlmod2new))
#56,108
ml3 = ml[-140,]
nycmlmod2new1 = lm(Price~Food+Decor+East,data = ml3)
plot(nycmlmod2new1$fitted.values,rstandard(nycmlmod2new1),main = "Residual plot", xlab = "Predicted bid price", ylab="Stanadrdized Residuals")
l2 = summary(nycmlmod2new1)
abline(h=2,lty=2)#lty = line type(dot)
abline(h=-2,lty=2)
identify(nycmlmod2new1$fitted.values,rstandard(nycmlmod2new1))
