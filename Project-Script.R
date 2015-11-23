library(caret)
library(rpart)
library(e1071)
library(randomForest)
TrainData <- read.csv("pml-training.csv")
TestData <- read.csv("pml-testing.csv")

set.seed(112233)
TrainPart <- createDataPartition(y = TrainData$classe, p = 0.6, list = FALSE)
TrainDataP1 <- TrainData[TrainPart, ]
TrainDataP2 <- TrainData[-TrainPart, ]

TrainDataP1 <- TrainDataP1[, -(1:5)]
TrainDataP2 <- TrainDataP2[, -(1:5)]
TrainData <- TrainData[, -(1:5)]
TestData <- TestData[, -(1:5)]

ZeroVar <- nearZeroVar(TrainDataP1)
TrainDataP1 <- TrainDataP1[, -ZeroVar]
TrainDataP2 <- TrainDataP2[, -ZeroVar]
TrainData <- TrainData[, -ZeroVar]
TestData <- TestData[, -ZeroVar]

SignificantlyNA <- sapply(TrainDataP1, function(x) mean(is.na(x))) > 0.9
TrainDataP1 <- TrainDataP1[, !SignificantlyNA]
TrainDataP2 <- TrainDataP2[, !SignificantlyNA]

SignificantlyNA1 <- sapply(TrainData, function(x) mean(is.na(x))) > 0.9
TrainData <- TrainData[, !SignificantlyNA1]
TestData <- TestData [, !SignificantlyNA1]

FirstModel <- train(classe ~ ., method = 'rpart', data = TrainDataP1)
prediction <- predict(FirstModel, newdata=TrainDataP2)
confusionMatrix(TrainDataP2$classe, prediction)

CrossVal <- trainControl(method = "cv", number = 2)
RFModel <- train(classe ~ ., method = "rf", data = TrainDataP1, trControl = CrossVal)

RFprediction <- predict(RFModel, newdata=TrainDataP2)
confusionMatrix(TrainDataP2$classe, RFprediction)

CrossVal <- trainControl(method = "cv", number = 2)
RFModelFinal <- train(classe ~ ., method = "rf", data = TrainData, trControl = CrossVal)

RFpredictionFinal <- predict(RFModelFinal, newdata=TestData)
RFpredictionFinal
TestData$classe <- RFpredictionFinal
