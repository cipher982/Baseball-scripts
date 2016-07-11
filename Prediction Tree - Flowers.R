data(iris); library(ggplot2)
names(iris)

## Create the TRAINING and TEST sets
  inTrain   <- createDataPartition(y=iris$Species, p=0.7,list=F)
  training  <- iris[inTrain,]
  testing   <- iris[-inTrain,]
  dim(training);dim(testing)
  
## Plot the Petal and Sepal widths for exploratory analysis
  qplot(Sepal.Width,Petal.Length,color=Species,data=training)

## Perform an Initial Training
  library(caret)
  modFit <- train(Species ~ .,method="rpart", data=training)
  print(modFit$finalModel)
  
## Plot the tree basically
  plot(modFit$finalModel, uniform = T, main="Classification Tree")
  text(modFit$finalModel, use.n=T, all=T, cex=.8)
    
  
## Plot the prettier decision tree
  library(rattle); library(rpart.plot)
  fancyRpartPlot(modFit$finalModel)