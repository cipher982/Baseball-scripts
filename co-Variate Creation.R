library(ISLR);library(caret);data(Wage);

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

## Turn text/qualitative features into quantitative numbers, such as 0 or 1
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

## Remove near zero or useless features/variables/predictors using nearZeroVar
nsv <- nearZeroVar(training, saveMetrics=T)
nsv

## Use polynomials for non-straight-line regression
library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis

## Plot the curved regression
lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age, training$wage, pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training), col="red",pch=19,cex=0.5)

predict(bsBasis,age=testing$age)

## Correlated Predictors
library(kernlab);data(spam)

training <- spam[inTrain,]
testing  <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

## More correlation options
data(mdrr)
data.frame(table(mdrrDescr$nR11))

## Find near zero variance features
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)

## Get Correlations
descrCor <-  cor(filteredDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)

descrCor <- cor(filteredDescr)
summary(descrCor[upper.tri(descrCor)])

## Cut off the highly correlated
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])


## Easy and simple way to model?
modelFit <- train(training$type ~.,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))
