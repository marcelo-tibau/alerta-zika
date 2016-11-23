
download.file("http://datadryad.org/bitstream/handle/10255/dryad.88853/aegypti.csv", dest="aegypti.csv", mode="wb")

download.file("http://datadryad.org/bitstream/handle/10255/dryad.88853/aegypti.csv", dest="aegypti.csv", mode="wb")


data_zika2 <- read.csv("aegypti.csv")


data_zika <- read.csv("Mosquito.Borne.Disease.csv")


library("caret")
library("gbm")
library("rpart")
library("rpart.plot")
library("RColorBrewer")
library("rattle")
library("randomForest")

data_zika_clean <- data_zika[c(-1, -3, -8, -10, -12, -14, -15, -16, -17, -20, -23, -24,
                               -25, -26, -27, -28, -29, -30, -33, -34, -35, -36, -37 -38
                               -39, -40, -41, -42, -43, -44, -45, -46, -48, -49, -50, -52, -53)] 

# data_zika_part <- data_zika[data_zika$NM_DISEASE == "Zika", ]


set.seed(14000)
inTrain <- createDataPartition(y=data_zika_clean$NM_DISEASE, p=0.7, list=FALSE)
training <- data_zika_clean[inTrain, ]
testing <- data_zika_clean[-inTrain, ]


NZV_check <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, NZV_check$nzv==FALSE]

NZV_check_2 <- nearZeroVar(testing, saveMetrics = TRUE)
testing <- testing[, NZV_check_2$nzv==FALSE]

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

training <- temp_train
rm(temp_train)


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

testing <- temp_test
rm(temp_test)

training <- training[c(-4, -5, -9, -10)]

training_na <- na.omit(training)
testing_na <- na.omit(testing)

training_na2 <- na.roughfix(training)
testing_na2 <- na.roughfix(testing)

modelFitRF <- randomForest(NM_DISEASE~., data = training_na2)

predictFitRF <- predict(modelFitRF, training_na2, type = "class")

# conditional inference trees 
tc <- trainControl(method="cv")
mod <- train(NM_DISEASE~., method="cforest", data=training_na2, prox=TRUE, trControl=tc)



accuracy_FitRF <- confusionMatrix(predictFitRF, testing_na2$NM_DISEASE)
accuracy_FitRF

plot(modelFitRF, main = "Random Forest Algorithm")

