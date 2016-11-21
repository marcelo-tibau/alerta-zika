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



# In createDataPartition(y = data_zika_part$NM_DISEASE, p = 0.7, list = FALSE) :
# Some classes have no records ( Chikungunya, Dengue ) and these will be ignored

