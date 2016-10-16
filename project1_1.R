## download file from online- Loading and preprocessing the data

setwd("/Volumes/KOFI/data_class")

if(!file.exists("./course_8")){dir.create("./course_8")}
TrainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
TestUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(TrainUrl,destfile = "./course_8/pml-training.csv", method = "curl")
download.file(TestUrl,destfile = "./course_8/pml-testing.csv", method = "curl")



## read data file 
Training_data<-read.csv(TrainUrl, header = TRUE, na.strings=c("NA","#DIV/0!",""))
Testing_data <-read.csv(TestUrl, header = TRUE, na.strings=c("NA","#DIV/0!",""))


library(lubridate)
library(AppliedPredictiveModeling)
library(forecast)
library(caret)
library(plyr)
library(gbm)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rattle)
##your goal will be to use data from accelerometers on the belt, 
##forearm, arm, and dumbell of 6 participants.



##cleaining the data for NA and Zeros
# Remove columns full of NAs.
new_testing <- names(Testing_data[,colSums(is.na(Testing_data)) == 0])[8:59]

# Only use  new testing  used in testing data cases.

Training_data1<- Training_data[,c(new_testing,"classe")]
Testing_data1 <- Testing_data[,c(new_testing,"problem_id")]

##str(Training_data)
##summary(Training_data)
set.seed(1234)
## create test/ train data sets 


##withhold 25% of the dataset for testing after the final model is constructed.

InTrain = createDataPartition(Training_data1$classe, p=.75, list = FALSE)
training =Training_data1[ InTrain,]
testing = Training_data1[-InTrain,]


dim(training)
dim(testing)

plot(training$classe, col="red", main="levels of the variable classe training", xlab="classe levels", ylab="Frequency")


## fit classification tree as a model

fit1 <- rpart(classe ~ ., data = training, method="class")


##plot the classification tree
fancyRpartPlot(fit1)
#predict on test values

predtree <- predict(fit1,testing,type = "class")

confusionMatrix(predtree, testing$classe)


## Random Forest Model
# Train the model on the Split Training Data

rfmodel <- randomForest(classe ~. , data = training)

# Use Model to predict on the Allocated Test Set

predrf <- predict(rfmodel, testing, type = "class")

# Create Confusion Matrix to Check Accuracy of Model

confusionMatrix(predrf, testing$classe)

## Final Predict Test

predict(fit1, Testing_data, type = "class")


predict(rfmodel, Testing_data, type = "class")
