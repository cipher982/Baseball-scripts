library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow = 20, horizon = 10)
names(folds)

folds$train[[500]]