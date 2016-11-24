dazika <- read.csv2("Disease.csv")

library("caret")
library("gbm")
library("rpart")
library("rpart.plot")
library("RColorBrewer")
library("NbClust")


# K-mean clustering


dataclustplot <- function(dazika, nc=15, seed=1527){
  dataclust <- (nrow(dazika)-1)*sum(apply(dazika,2,var))
  for (i in 2:nc){
    set.seed(seed)
    dataclust[i] <- sum(kmeans(dazika, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

dataclustplot(dazika)

set.seed(1527)
nc <- NbClust(dazika, min.nc = 2, max.nc = 15, method = "kmeans")




inTrain <- createDataPartition(y=dazika$NM_DISEASE, p=0.7, list=FALSE)
training <- dazika[inTrain, ]
testing <- dazika[-inTrain, ]

# Decision Tree Algorithm
set.seed(14000)
modelzikaone <- rpart(NM_DISEASE~., data = training, method = "class", control = rpart.control(minsplit=30, cp=0.001))





modelFitDT <- rpart(classe ~., data = training, method = "class")
fancyRpartPlot(modelFitDT)

# Cross validating the model:
predictFitDT <- predict(modelFitDT, testing, type = "class")

# To check the accuracy:
accuracy_FitDT <- confusionMatrix(predictFitDT, testing$classe)  
accuracy_FitDT



NZV_check <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, NZV_check$nzv==FALSE]


modelFitRF <- randomForest(NM_DISEASE~., data = training_na2)

predictFitRF <- predict(modelFitRF, training_na2, type = "class")