data(iris); library(ggplot2)

## Partition the data, 70% train
  inTrain   <- createDataPartition(y=iris$Species, p=0.7, list=F)
  training  <- iris[inTrain,]
  testing   <- iris[-inTrain,]
  
## Begin training the data from Caret
  library(caret)
  
  modFit <- train(Species ~ ., data=training, method='rf', prox=T)
  modFit
  
  getTree(modFit$finalModel, k=2)
  


## Graph the class 'centers'
  irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
  irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
  
  p <- qplot(Petal.Width, Petal.Length, col=Species, data = training)
  p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species), size=5,shape=4,data=irisP)
  
## Predict the new values
  pred   <- predict(modFit, testing); testing$predRight <- pred=testing$Species
  table(pred, testing$Species)
  
  ## Check the missed values predicted
  qplot(Petal.Width, Petal.Length,color=predRight, data=testing, main="newdata Predictions")