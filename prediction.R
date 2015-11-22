prediction <- function()
{
  library(ggplot2)
  library(caret)
  library(dplyr)
  library(parallel)
  library(doParallel)
  
  set.seed(42)
  originalTrainingSet = read.csv("pml-training.csv", header = TRUE)
  
  naTolerance <- (length(originalTrainingSet$X)*90)/100
  
  na_count <-sapply(originalTrainingSet, function(y) sum(length(which(is.na(y) || y == ""))))
  na_count <- data.frame(na_count)
  
  naCols <-sapply(na_count, function(y) which(y > 0))
  naCols <- data.frame(naCols)
  
  trainingSet <- select(originalTrainingSet
                        , - naCols[,1]
                        , -X
                        , -user_name
                        , -raw_timestamp_part_1
                        , -raw_timestamp_part_2
                        , -num_window
                        , -new_window
                        , -cvtd_timestamp)
  trainingSet$classe <- as.factor(trainingSet$classe)
  
  trainIndex <- createDataPartition(trainingSet$classe, p=0.6,list=FALSE);
  training <- trainingSet[trainIndex,];
  testing <- trainingSet[-trainIndex,];
  
  registerDoParallel(makeCluster(2))
  na_count2 <-sapply(training, function(y) sum(length(which(is.na(y) || y == ""))))
  as.data.frame(na_count2)
  model_rf <- train(classe ~ .
                    ,  method="rf"
                    , data = training
                    , allowParallel=T
                    , trControl = trainControl(method = "cv", number = 3))
  acc <- predict(model_rf , testing)
confusionMatrix(acc, testing$classe)

  TestSet = read.csv("pml-testing.csv")
  predictionTest <- predict(model_rf, TestSet)
  pml_write_files(as.vector(predictionTest))
  
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}