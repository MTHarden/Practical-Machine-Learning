# Load Packages, Read, Tidy, and Reduce the Data
library(caret)
library(plyr)
library(dplyr)
library(psych)
library(doParallel)
library(e1071)
library(C50)

training <- read.csv("pml-training.csv", header = TRUE)
testing <- read.csv("pml-testing.csv", header = TRUE)

# NZV
nz <- nearZeroVar(training, saveMetrics = TRUE)
nz$variable <- 1:160
nz <- filter(nz, nzv == FALSE)
training <- select(training, 3:5, 7:11, 18:19, 21:22, 24:25, 27:50, 60:68, 76:77, 80, 83:86, 93:94, 96:97, 99:100, 102:124, 132, 135, 138, 140:141, 151:160)

# Remove NA and NULL 
a <- describe(training)
a <- filter(a, n == 19622)
training <- select(training, 1:8, 25:37, 39:47, 52:54, 61, 72:83, 87, 89:98)

# Split the training data
inTrain <- createDataPartition(y = training$classe, p = 0.60, list = FALSE)
training <- training[inTrain,]
valid <- training[-inTrain,]

# Make and pre-test the Model
set.seed(1234)
registerDoParallel(cores = 4)
modFit <- train(classe ~., data = training, method = "C5.0", prox = TRUE, trControl = trainControl("cv", number = 4, verboseIter = TRUE, allowParallel = TRUE))
predictions <- predict(modFit, newdata = valid)
confusionMatrix(predictions, valid$classe)

# Apply model to Test Data and Generate Answers

answers <- as.character(predict(modFit, newdata=testing))

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files(answers)



