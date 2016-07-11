library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=T,returnTrain=T)

sapply(folds,length)