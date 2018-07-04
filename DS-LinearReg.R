bonds = read.delim("/Users/virajdattkohir/Downloads/bonds.txt",row.names = 1)
str(bonds)
summary(bonds)
plot(bonds$CouponRate,bonds$BidPrice)

bondsmod = lm(bonds$BidPrice~bonds$CouponRate)
abline(75,3)
summary(bondsmod)

#Test on beta1 is a 2 sided test at alpha = 0.05

alpha = 0.05
n=35
p=1
qt(p = 1-(alpha/2), df = n-p-1)

#b1 = 3.0661 and sd is 0.3068

#Confidence Interval for b1

3.0661-(2.034515 * 0.3068)
3.0661+(2.034515 * 0.3068)

#Computing F-stat

SSE = sum((bonds$BidPrice-bondsmod$fitted.values)^2)

SSR = sum((bondsmod$fitted.values-mean(bonds$BidPrice))^2)

F_stat = (SSR/SSE)*(n-2)


#Outlier Detection

#Residual Analysis

plot(bondsmod$fitted.values,rstandard(bondsmod),main = "Residual plot", xlab = "Predicted bid price", ylab="Stanadrdized Residuals")

abline(h=2,lty=2)#lty = line type(dot)
abline(h=-2,lty=2)
identify(bondsmod$fitted.values,rstandard(bondsmod))

bondsnew = bonds[-13,]
bondsmod1 = lm(bondsnew$BidPrice~bondsnew$CouponRate)
plot(bondsmod1$fitted.values,rstandard(bondsmod1),main = "Residual plot", xlab = "Predicted bid price", ylab="Stanadrdized Residuals")

 abline(h=2,lty=2)#lty = line type(dot)
abline(h=-2,lty=2)
summary(bondsmod1)

bondsnew1 = bondsnew[-35,]
bondsmod2 = lm(bondsnew1$BidPrice~bondsnew1$CouponRate)
plot(bondsmod2$fitted.values,rstandard(bondsmod2),main = "Residual plot", xlab = "Predicted bid price", ylab="Stanadrdized Residuals")

abline(h=2,lty=2)#lty = line type(dot)
abline(h=-2,lty=2)
summary(bondsmod2)
identify(bondsmod2$fitted.values,rstandard(bondsmod2))

bondsnew2 = bondsnew1[-c(4,34)]
bondsmod3 = lm(bondsnew2$BidPrice~bondsnew2$CouponRate)
plot(bondsmod3$fitted.values,rstandard(bondsmod3),main = "Residual plot", xlab = "Predicted bid price", ylab="Stanadrdized Residuals")

abline(h=2,lty=2)#lty = line type(dot)
abline(h=-2,lty=2)
summary(bondsmod3)
identify(bondsmod3$fitted.values,rstandard(bondsmod3))








    