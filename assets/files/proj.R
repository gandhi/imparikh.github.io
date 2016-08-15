# Loading libraries
library(caret)
library(randomForest)

# Importing data and setting the seed. Also, removing all places where data is empty
# becaused it caused errors when the random forest model was being created
# otherwise. Also, remove the first few columns because when the test set is evaluated
# without doing that, everything evaluates to "A".
trainingData <- read.csv("~/Downloads/pml-training.csv", na.strings = c("NA", ""))
testingData <- read.csv("~/Downloads/pml-testing.csv", na.strings = c("NA", ""))
trainingData <- trainingData[, colSums(is.na(trainingData)) == 0]
testingData <- testingData[, colSums(is.na(testingData)) == 0]
trainingData <- trainingData[, -c(1:7)]
testingData <- testingData[, -c(1:7)]
set.seed(513)

# Partition data into training and cross validation sets
inTrain <- createDataPartition(trainingData$classe, p = 0.7, list = FALSE)
train <- trainingData[inTrain,]
validation <- trainingData[-inTrain,]

# Modify the resampling method to 10-fold cross-validation and 
# build model.
resampling <- trainControl(method="cv")
rfmodel <- train(classe ~ ., data = train, method ="rf", trControl=resampling)

# Predict on our validation set and look at how it did.
prediction <- predict(rfmodel, validation)
analysis <- confusionMatrix(validation$classe, prediction)
print(analysis, digits=4)

# It did pretty well, try it on the results.
results <- predict(rfmodel, testingData)
print(results)
