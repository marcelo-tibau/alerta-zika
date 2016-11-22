# Codes to download and read the datset:
download.file("https://info.dengue.mat.br/dumps/Mosquito.Borne.Disease.csv", dest="Mosquito.Borne.Disease.csv", mode="wb")

data_zika <- read.csv("Mosquito.Borne.Disease.csv")

head(data_zika)

# Codes to load the libraries to be used:
library("caret")
library("gbm")
library("rpart")
library("rpart.plot")
library("RColorBrewer")
library("rattle")
library("randomForest")

#Code to subset to Zika only data

data_zika_part <- data_zika[data_zika$NM_DISEASE == "Zika", ]

# Codes to partioning the training dataset into two: 70% for training and 30% for testing.

set.seed(14000)
inTrain <- createDataPartition(y=data_zika_part$NM_DISEASE, p=0.7, list=FALSE)
training <- data_zika_part[inTrain, ]
testing <- data_zika_part[-inTrain, ]

dim(training)
dim(testing)

# Code to check possibles Near Zero Variance Variables.
NZV_check <- nearZeroVar(training, saveMetrics = TRUE)
head(NZV_check)

# Code to reset the training dataset without NZV
training <- training[, NZV_check$nzv==FALSE]
dim(training)

# Codes to do the previous to the testing dataset
NZV_check_2 <- nearZeroVar(testing, saveMetrics = TRUE)
testing <- testing[, NZV_check_2$nzv==FALSE]
dim(testing)

# check if the first column will interfer with ML Algorithms
testtraining <- training[c(-1)]

# Codes to clean the variables with more than 60% NAs at the training set
temp_train <- training
for (i in 1:length(training)) {
  if(sum(is.na(training[,i]))/nrow(training)>=0.6) {
    for (j in 1:length(temp_train)) {
      if(length(grep(names(training[i]), names(temp_train)[j]))==1){
        temp_train <- temp_train[,-j]
      }
      
    }
  }
  
}

dim(temp_train)

training <- temp_train
rm(temp_train)

# Codes to perform the same cleaning at the testing dataset:
temp_test <- testing
for (i in 1:length(testing)) {
  if(sum(is.na(testing[,i]))/nrow(testing)>=0.6) {
    for (j in 1:length(temp_test)) {
      if(length(grep(names(testing[i]), names(temp_test)[j]))==1) {
        temp_test <- temp_test[,-j]
      }
      
    }
  }
  
}

dim(temp_test)

testing <- temp_test
rm(temp_test)

# I will test 3 machine learning algorithms to build predictive models. One model, using a random forest ("rf"), the other, using decision trees and a third, using boosted trees ("gbm").
# Then, I will perform cross-validations to predict the outcomes and check the accuracy of each model at the testing set.

# Random Forest Algorithm -> so far it didn't work. I could not get an accuracy response.
# Modelling with Random Forests for Survival, Regression and Classification (RF-SRC) package
library('randomForestSRC')

# Set seed for reproducibility purposes:
set.seed(14000)

# Omiting NA
training_na <- na.omit(training)
testing_na <- na.omit(testing)

# Rip off the first column to prevent it interfers with ML Algorithms. 
testtraining <- training_na[c(-1)]
testtesting <- testing_na[c(-1)]

# Fitting the model with the variables ID_BAIRRO and NU_CEP. I tried to fit with all variables but it didn't work.

fit1 <- rfsrc(ID_BAIRRO~NU_CEP, data = testtraining, statistics=TRUE)

# Cross validating the model:
predictFit1 <- predict.rfsrc(fit1, testtesting, type = "class", statistics=TRUE)
  
accuracy_fit1 <- stat.split.rfsrc(predictFit1)

# To check the accuracy:
accuracy_fit1 <- confusionMatrix(predictFit1, testtesting)


# Modelling with randomForest package

set.seed(14000)
modelFitRF <- randomForest(ID_BAIRRO~NU_CEP, data = testtraining)

# Cross validating the model:
predictFitRF <- predict(modelFitRF, testtesting, type = "class")

# To check the accuracy:

accuracy_FitRF <- confusionMatrix(pred, testtesting$ID_BAIRRO)
accuracy_FitRF

pred <- format(round(predict(modelFitRF,testtesting)))
plot(pred)

acc <- table(factor(pred, levels=min(testtesting):max(testtesting)),factor(testtesting, levels=min(testtesting):max(testtesting)))
acc <- table(factor(pred), levels(testtesting), factor(testtesting))
sort.list(pred)

plot(predictFitRF)
# In createDataPartition(y = data_zika_part$NM_DISEASE, p = 0.7, list = FALSE) :
# Some classes have no records ( Chikungunya, Dengue ) and these will be ignored

