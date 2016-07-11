

## Predict eruptions of Geysers
library(caret);data(faithful); set.seed(333)

## Get the training and testing data, 50% 50%
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=F)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

## Plot the values
plot(trainFaith$waiting,trainFaith$eruptions,col="blue",xlab="Waiting",ylab="Duration")


## Fit a linear model
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)

## Plot the line regression
plot(trainFaith$waiting,trainFaith$eruptions,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted)

## Estimate the duration if the waiting time is 80 minutes
coef(lm1)[1] + coef(lm1)[2]*80


## Can skip the coef step by plugging in the fitted model
## Create a data.frame with the 80 minute waiting time
newdata <- data.frame(waiting=80)
predict(lm1, newdata)


## Test the previous lm model against the test dataset
par(mfrow=c(2,1))

plot(trainFaith$waiting, trainFaith$eruptions,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)

plot(testFaith$waiting, testFaith$eruptions,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)


## Calculate the error rate using the RMSE (Root Squares Mean Error)
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
## Calculate on the test dataset
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))


## Calculate the prediction intervals
pred1 <- predict(lm1,newdata=testFaith, interval="prediction")
ord <-   order(testFaith$waiting)

## Plot the PI
plot(testFaith$waiting,testFaith$eruptions,col="blue")
matlines(testFaith$waiting,pred1,type="l",,col=c(2,3,3), lwd=1)