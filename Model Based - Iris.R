  data(iris);library(ggplot2)
  

## Grab and partition the data
  inTrain  <- createDataPartition(y=iris$Species, p=0.7, list=F)
  training <- iris[inTrain,]
  testing  <- iris[-inTrain,]
  
## Build the predictions
  modlda = train(Species ~ ., data=training, method="lda")
  modnb  = train(Species ~ ., data=training, method="nb")
  
  plda <- predict(modlda, testing); pnb <- predict(modnb, testing)
  
## Comparison of results
  
  table(plda,pnb)
  
  equalPredictions = (plda==pnb)
  qplot(Petal.Width,Sepal.Width,color=equalPredictions,data=testing)