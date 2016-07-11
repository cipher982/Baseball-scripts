library(ISLR);data(Wage);library(ggplot2);library(caret);library(gbm)


## Grab the data and split into train/test
  wage <- subset(Wage, select=-c(logwage))
  inTrain  <- createDataPartition(y=wage$wage, p=0.7, list=F)
  training <- wage[inTrain,]; testing <- wage[-inTrain,]
  
## Apply the GBM training method (boosting with trees)
  modFit <- train(wage ~ ., method="gbm", data = training, verbose = F)
  print(modFit)
  
## Plot the results
  qplot(predict(modFit, testing), wage, data=testing)