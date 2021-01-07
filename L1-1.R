detach(Auto)
detach(Auto)
library(MASS)
library(ISLR)
library(caret)
library(corrplot)

# Pirma uzduotis
data(Auto)
summary(Auto)
attach(Auto)

# Antra uzduotis
# Pasirenku mpg ir weight
plot(Auto$mpg, weight)
abline(1613,5140, lwd=3, col="red")
grid()
#Trecia uzduotis
plot(Auto)

#Ketvirta uzduotis
mydata <- Auto[, c(1,2,3,4,5,6,7,8)]
cormat<-signif(cor(mydata),2)
corrplot(cormat, tl.col = "black", order = "hclust",
         hclust.method = "average", addrect = 4, tl.cex = 0.7)

# Penkta uzduotis
set.seed(1)
idx.train = createDataPartition(y = mydata$mpg, p=0.2, list = FALSE)
train=mydata[idx.train,]
test=mydata[-idx.train,]

lm.fit1 = lm(mpg~., data=train)
summary(lm.fit1)
pred1 = predict(lm.fit1, test)
plot(pred1,test$mpg, xlab="Predicted mpg",
     ylab="True mpg", pch=16)
abline(0,1, lwd=3, col="red")
grid()
summary(pred1)
Rsquared1=cor(pred1, test$mpg)^2
Rsquared1
# Sesta uzduotis

lm.fit2 = lm(mpg~.+weight:acceleration, data=train)
summary(lm.fit2)
pred2 = predict(lm.fit2, test)
plot(pred2,test$mpg, xlab="Predicted mpg",
     ylab="True mpg", pch=16)
abline(0,1, lwd=3, col="red")
grid()
summary(pred2)
Rsquared2=cor(pred2, test$mpg)^2
Rsquared2
# Septinata uzduotis

lm.fit3 = lm(mpg~.+weight+I(weight^2), data=train)
summary(lm.fit3)
pred3 = predict(lm.fit3, test)
plot(pred3,test$mpg, xlab="Predicted mpg",
     ylab="True mpg", pch=16)
abline(0,1, lwd=3, col="red")
grid()
summary(pred3)
Rsquared3=cor(pred3, test$mpg)^2
Rsquared3

