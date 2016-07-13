library(caret);library(ggplot2); library(AppliedPredictiveModeling)


## Load in the raw datasets
  trainingRaw <- read.csv("pml-training.csv")
  finalTest  <- read.csv("pml-testing.csv")
  
          ## Creates a data frame with three columns: index, ColumnName and
          ## FractionMissing.
          ## index is the column index in df corresponding to ColumnName
          ## ColumnName is as the name implies: the name the column in df
          ## FractionMissing is the fraction of values that are missing or NA.
          ## The closer this value is to 1, the less data the column contains
          getFractionMissing <- function(df = trainingData) {
            colCount <- ncol(df)
            returnDf <- data.frame(index=1:ncol(df),
                                   columnName=rep("undefined", colCount),
                                   FractionMissing=rep(-1, colCount),
                                   stringsAsFactors=FALSE)
            for(i in 1:colCount) {
              colVector <- df[,i]
              missingCount <- length(which(colVector == "") * 1)
              missingCount <- missingCount + sum(is.na(colVector) * 1)
              returnDf$columnName[i] <- as.character(names(df)[i])
              returnDf$FractionMissing[i] <- missingCount / length(colVector)
            }
            
            return(returnDf)
          }  
  
          
          
## Split the data up into a training and testing set
  inTrain      <- createDataPartition(y=trainingRaw$classe, p=0.80, list=F)
  trainingRaw  <- trainingRaw[inTrain,]
  testingRaw   <- trainingRaw[-inTrain,]
  
## Take out the superfluous columns and keep just the data to predict with
  #trainingData <- trainingRaw[,8:159]
  #testingData  <- testingRaw[,8:159]
  trainingData  <- trainingRaw[,c(8,9,10,11,37,38,39,40,41,42,43,60,61,62,63,64,65,66,67,68,84,85,86,102,116,117,118,119,120,121,122,123,124,140,154,155,156,157,158,159,160)]
  
## Extract the classe column 
  trainingClasse <- trainingRaw[,160]
  testingClasse  <- testingRaw[,160]
  
  ## Combine these real quick for the model later
  newTraining <- trainingRaw[,8:160]
  
## Attempt some pre-processing on the training data
  preTraining <- preProcess(trainingData, method=c("center","scale","pca"), thresh = 0.90)
  preTraining
  
## Perform the predictions on the training data with preprocessed data
  trainingClean <- predict(preTraining,trainingData)
  testingClean  <- predict(preTraining, testingData)
  
## Compute the model with the pre-processed data predictors
  myModel <- train(classe ~ ., data = trainingData, method = 'rpart')

## And now apply the new model on the testing set
  myModelResults <- confusionMatrix(testingClasse, predict(myModel, testingClean))
  myModelResults
  
## Plot the decision tree
  library(rattle); library(rpart.plot)
  fancyRpartPlot(myModel$finalModel)
  
## Plot the tree basically
  plot(myModel$finalModel, uniform = T, main="Classification Tree")
  text(myModel$finalModel, use.n=T, all=T, cex=.8)