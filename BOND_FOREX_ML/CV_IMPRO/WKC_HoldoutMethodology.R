library("RPQuantUtils")
library("RPToolsDB")
require(ggplot2)
require("ppcor")
require(graphics)
require("TTR")
require(plyr)
# require(reshape)
require(reshape2)
require(RColorBrewer)
require(stats)
require(Rsolnp)
require(zoo)
require(xts)
require(vars)
# require(Quandl)
require(rpart)
require(randomForest)
# require(rpart.plot)
# require(rattle)
# install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
# require(caret)
require(xgboost)
require(ggplot2)
require(reshape2)

source("./RCode/CV_IMPRO/RP_KLUtility.R")
source("./RCode/CV_IMPRO/RP_KLReturns.R")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")
# outputDataPath <- "C:/My_Kaggle_Challenges_Data/Winton/"

#### Getting to know Kaggle data inspection

### Hold out methodology to make sure that we fit properly our data
library(caret)


#### Getting to know Kaggle data inspection
# Train data
print("Reading training data")

MyTrainData <- read.csv(file=train_path, header=TRUE, sep=",")

trainIndex <- createDataPartition(MyTrainData$Id, p = 0.25, list=FALSE, times=1)
# trainIndex <- createDataPartition(MyTrainData$Id, p = 0.4, list=FALSE, times=1)

MyTrainData <- MyTrainData[trainIndex,]
MyTestData  <- MyTrainData[-trainIndex,]


######### Getting to know the features predictors

# Feature_1 to Feature_25 : predicting features
# Ret_MinusTwo : day-1 
# Ret_MinusOne : mid day d day
# Ret_2 to Ret_120 : first 120 minutes past miday
# Ret_121 to Ret_180 : last 60 minutes till the close of d day
# Ret_PlusOne : day+1   
# Ret_PlusTwo : day+2
# Weight_Intraday ??????
# Weight_Daily ??????
### Getting a sense of the first minute evolution


# WARNING WARNING WARNING WARNING Feature 1 is discrete with Na
#hist(MyTrainData$Feature_1)
head(MyTrainData$Feature_1,30)
# we make it a factor :
MyTrainData$Feature_1 <- as.character(MyTrainData$Feature_1)
# Feature 2 is continuous
#hist(MyTrainData$Feature_2)
head(MyTrainData$Feature_2,30)
# Feature 3 is continuous
#hist(MyTrainData$Feature_3)
head(MyTrainData$Feature_3,30)
# Feature 4 is continuous
#hist(MyTrainData$Feature_4)
head(MyTrainData$Feature_4,30)

# WARNING WARNING WARNING WARNING Feature 5 is discrete
#hist(MyTrainData$Feature_5)
head(MyTrainData$Feature_5,30)
# we make it a factor :
MyTrainData$Feature_5 <- as.character(MyTrainData$Feature_5)

# Feature 6 is continuouS
#hist(MyTrainData$Feature_6)
head(MyTrainData$Feature_6,30)
# Feature 7 is coninuous like Volume
#hist(MyTrainData$Feature_7)
head(MyTrainData$Feature_7,30)
# Feature 8 is coninuous
#hist(MyTrainData$Feature_8)
head(MyTrainData$Feature_8,30)

# WARNING WARNING WARNING WARNING Feature 9 is discrete
#hist(MyTrainData$Feature_9)
head(MyTrainData$Feature_9,30)
# we make it a factor :
MyTrainData$Feature_9 <- as.character(MyTrainData$Feature_9)

# WARNING WARNING WARNING WARNING  Feature 10 is discrete
#hist(MyTrainData$Feature_10)
head(MyTrainData$Feature_10,30)
# we make it a factor :
MyTrainData$Feature_10 <- as.character(MyTrainData$Feature_10)

# Feature 11 is continuous
#hist(MyTrainData$Feature_11)
head(MyTrainData$Feature_11,30)
# Feature 12 is continuous
#hist(MyTrainData$Feature_12)
head(MyTrainData$Feature_12,30)

# WARNING WARNING WARNING WARNING  Feature 13 is discrete
#hist(MyTrainData$Feature_13)
head(MyTrainData$Feature_13,30)
# we make it a factor :
MyTrainData$Feature_13 <- as.character(MyTrainData$Feature_13)

# Feature 14 is conitnuous
#hist(MyTrainData$Feature_14)
head(MyTrainData$Feature_14,30)
# Feature 15 is conitnuous
#hist(MyTrainData$Feature_15)
head(MyTrainData$Feature_15,30)

# WARNING WARNING WARNING WARNING  Feature 16 is discrete
#hist(MyTrainData$Feature_16)
head(MyTrainData$Feature_16,150)
# we make it a factor :
MyTrainData$Feature_16 <- as.character(MyTrainData$Feature_16)
# aggregate(MyTrainData, by=list(MyTrainData$Feature_16),table )
# Feature 17 is continuous
#hist(MyTrainData$Feature_17)
head(MyTrainData$Feature_17,30)
# Feature 18 is continuous
#hist(MyTrainData$Feature_18)
head(MyTrainData$Feature_18,30)
# Feature 19 is continuous
#hist(MyTrainData$Feature_19)
head(MyTrainData$Feature_19,30)

# WARNING WARNING WARNING WARNING  Feature 20 is discrete
#hist(MyTrainData$Feature_20)
head(MyTrainData$Feature_20,30)
# we make it a factor :
MyTrainData$Feature_20 <- as.character(MyTrainData$Feature_20)

# Feature 21 is continuous
#hist(MyTrainData$Feature_21)
head(MyTrainData$Feature_21,30)
# Feature 22 is continuous
#hist(MyTrainData$Feature_22)
head(MyTrainData$Feature_22,30)
# Feature 23 is continuous
#hist(MyTrainData$Feature_23)
head(MyTrainData$Feature_23,30)
# Feature 24 is continuous
#hist(MyTrainData$Feature_24)
head(MyTrainData$Feature_24,30)
# Feature 25 is continuous
#hist(MyTrainData$Feature_25)
head(MyTrainData$Feature_25,30)


# WARNING WARNING WARNING WARNING Feature 1 is discrete with Na
#hist(MyTestData$Feature_1)
head(MyTestData$Feature_1,30)
# we make it a factor :
MyTestData$Feature_1 <- as.character(MyTestData$Feature_1)
# Feature 2 is continuous
#hist(MyTestData$Feature_2)
head(MyTestData$Feature_2,30)
# Feature 3 is continuous
#hist(MyTestData$Feature_3)
head(MyTestData$Feature_3,30)
# Feature 4 is continuous
#hist(MyTestData$Feature_4)
head(MyTestData$Feature_4,30)

# WARNING WARNING WARNING WARNING Feature 5 is discrete
#hist(MyTestData$Feature_5)
head(MyTestData$Feature_5,30)
# we make it a factor :
MyTestData$Feature_5 <- as.character(MyTestData$Feature_5)

# Feature 6 is continuouS
#hist(MyTestData$Feature_6)
head(MyTestData$Feature_6,30)
# Feature 7 is coninuous like Volume
#hist(MyTestData$Feature_7)
head(MyTestData$Feature_7,30)
# Feature 8 is coninuous
#hist(MyTestData$Feature_8)
head(MyTestData$Feature_8,30)

# WARNING WARNING WARNING WARNING Feature 9 is discrete
#hist(MyTestData$Feature_9)
head(MyTestData$Feature_9,30)
# we make it a factor :
MyTestData$Feature_9 <- as.character(MyTestData$Feature_9)

# WARNING WARNING WARNING WARNING  Feature 10 is discrete
#hist(MyTestData$Feature_10)
head(MyTestData$Feature_10,30)
# we make it a factor :
MyTestData$Feature_10 <- as.character(MyTestData$Feature_10)

# Feature 11 is continuous
#hist(MyTestData$Feature_11)
head(MyTestData$Feature_11,30)
# Feature 12 is continuous
#hist(MyTestData$Feature_12)
head(MyTestData$Feature_12,30)

# WARNING WARNING WARNING WARNING  Feature 13 is discrete
#hist(MyTestData$Feature_13)
head(MyTestData$Feature_13,30)
# we make it a factor :
MyTestData$Feature_13 <- as.character(MyTestData$Feature_13)

# Feature 14 is conitnuous
#hist(MyTestData$Feature_14)
head(MyTestData$Feature_14,30)
# Feature 15 is conitnuous
#hist(MyTestData$Feature_15)
head(MyTestData$Feature_15,30)

# WARNING WARNING WARNING WARNING  Feature 16 is discrete
#hist(MyTestData$Feature_16)
head(MyTestData$Feature_16,150)
# we make it a factor :
MyTestData$Feature_16 <- as.character(MyTestData$Feature_16)
# aggregate(MyTrainData, by=list(MyTrainData$Feature_16),table )
# Feature 17 is continuous
#hist(MyTestData$Feature_17)
head(MyTestData$Feature_17,30)
# Feature 18 is continuous
#hist(MyTestData$Feature_18)
head(MyTestData$Feature_18,30)
# Feature 19 is continuous
#hist(MyTestData$Feature_19)
head(MyTestData$Feature_19,30)

# WARNING WARNING WARNING WARNING  Feature 20 is discrete
#hist(MyTestData$Feature_20)
head(MyTestData$Feature_20,30)
# we make it a factor :
MyTestData$Feature_20 <- as.character(MyTestData$Feature_20)

# Feature 21 is continuous
#hist(MyTestData$Feature_21)
head(MyTestData$Feature_21,30)
# Feature 22 is continuous
#hist(MyTestData$Feature_22)
head(MyTestData$Feature_22,30)
# Feature 23 is continuous
#hist(MyTestData$Feature_23)
head(MyTestData$Feature_23,30)
# Feature 24 is continuous
#hist(MyTestData$Feature_24)
head(MyTestData$Feature_24,30)
# Feature 25 is continuous
#hist(MyTestData$Feature_25)
head(MyTestData$Feature_25,30)
## We have to remove the NaN for the sparse model generation
for(i in 1:ncol(MyTrainData)){
  if(is.numeric(MyTrainData[,i])){
    MyTrainData[is.na(MyTrainData[,i]),i] = -999.0
  } else {
    MyTrainData[,i] = as.character(MyTrainData[,i])
    MyTrainData[is.na(MyTrainData[,i]),i] = "999"
    MyTrainData[,i] = as.factor(MyTrainData[,i])
  }
}

for(i in 1:ncol(MyTestData)){
  if(is.numeric(MyTestData[,i])){
    MyTestData[is.na(MyTestData[,i]),i] = -999.0
  } else {
    MyTestData[,i] = as.character(MyTestData[,i])
    MyTestData[is.na(MyTestData[,i]),i] = "999"
    MyTestData[,i] = as.factor(MyTestData[,i])
  }
}


# Averaging the first sccond-half qurter d day minutes returns into one predictor 
first_quarter_minutes <- paste("Ret_",seq(2,120),sep="")
second_quarter_minutes <-  paste("Ret_",seq(121,180),sep="")
## Cumulative returns for quarter aggregation
MyTrainData$Ret_PlusOneQuarter <- rowSums(MyTrainData[,first_quarter_minutes],na.rm=TRUE)
MyTestData$Ret_PlusOneQuarter <- rowSums(MyTestData[,first_quarter_minutes],na.rm=TRUE)

MyTrainData$Ret_PlusTwoQuarter <- rowSums(MyTrainData[,second_quarter_minutes],na.rm=TRUE)
MyTrainData$Ret_SecondMidday <- MyTrainData$Ret_PlusOneQuarter + MyTrainData$Ret_PlusTwoQuarter

MyTestData$Ret_PlusTwoQuarter <- rowSums(MyTestData[,second_quarter_minutes],na.rm=TRUE)
MyTestData$Ret_SecondMidday <- MyTestData$Ret_PlusOneQuarter + MyTestData$Ret_PlusTwoQuarter

# # Weights z scoring
# MyTrainData$Weight_Intraday <- (MyTrainData$Weight_Intraday-mean(MyTrainData$Weight_Intraday) )/sd(MyTrainData$Weight_Intraday)
# MyTrainData$Weight_Daily <- (MyTrainData$Weight_Daily-mean(MyTrainData$Weight_Daily) )/sd(MyTrainData$Weight_Daily)
# 
# MyTrainData$Weight_Intraday <- MyTrainData$Weight_Intraday -min(MyTrainData$Weight_Intraday)
# MyTrainData$Weight_Daily <- MyTrainData$Weight_Daily -min(MyTrainData$Weight_Daily)

# Weights simple renormalization
MyTrainData$Weight_Daily <- MyTrainData$Weight_Daily/sum(MyTrainData$Weight_Daily)
MyTrainData$Weight_Intraday <- MyTrainData$Weight_Intraday/sum(MyTrainData$Weight_Intraday)

# Ret_MinusTwo : day-1 
# Ret_MinusOne : mid day d day
# Ret_2 to Ret_120 : first 120 minutes past miday
# Ret_121 to Ret_180 : last 60 minutes till the close of d day
# Ret_PlusOne : day+1   
# Ret_PlusTwo : day+2

print("Day -1 returns standard deviation")
print(sd(MyTrainData$Ret_MinusTwo))
print("Order of magnitude day")
print("2*10^(-2)")
print("mid day d day  returns standard deviation")
print(sd(MyTrainData$Ret_MinusOne))
print("Order of magnitude half day")
print("1*10^(-2)")

print("second mid day d day first 120 minutes returns average standard deviation")
print(mean(apply(MyTrainData[,first_quarter_minutes],2,sd,na.rm=TRUE)))
print("Order of magnitude minute")
print("1*10^(-3)")
print("second mid day d day last 60 minutes to close  returns average standard deviation")
print(mean(apply(MyTrainData[,second_quarter_minutes],2,sd,na.rm=TRUE)))
print("Order of magnitude minute")
print("1*10^(-3)")
print("second mid day d day first 120 minutes CUMSUM returns standard deviation")
print(sd(MyTrainData$Ret_PlusOneQuarter))
print("Order of magnitude 2/3 half day")
print("1*10^(-2)")
print("second mid day d day last 60 minutes to close CUMSUM returns average standard deviation")
print(sd(MyTrainData$Ret_PlusTwoQuarter))
print("Order of magnitude 1/3 half day")
print("8*10^(-3)")
print("second mid day d day all minutes CUMSUM returns average standard deviation")
print(sd(MyTrainData$Ret_SecondMidday))
print("Order of magnitude half day")
print("1*10^(-2)")

print("Day +_1 returns standard deviation")
print(sd(MyTrainData$Ret_PlusOne))
print("Order of magnitude day")
print("2*10^(-2)")
print("Day +2 returns standard deviation")
print(sd(MyTrainData$Ret_PlusTwo))
print("Order of magnitude day")
print("2*10^(-2)")

# algorithm_used <- "xgboost"
# algorithm_used <- "rpart_cv"
# algorithm_used <- "xgboost_cv"
# algorithm_used <- "rpart"
# algorithm_used <- "xgboost_cv"
# algorithm_used <- "rpart_caret_cv"
algorithm_used <- "xgboost_inner_cv"



### First model M1
print("Training model M1")
my_predicting_columns <- paste0("Feature_",seq(1,25))
my_stump_predicting_columns <- c(my_predicting_columns,"Ret_MinusTwo","Ret_MinusOne","Ret_PlusOneQuarter")
my_predicting_columns <- my_stump_predicting_columns
output_one_column <- "Ret_PlusTwoQuarter"
### Computing yourself the resultsHoldOut
# results <- RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, MyTrainData, MyTestData, MyTrainData$Weight_Intraday)
results <- RP_SimplePredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, MyTrainData, MyTestData, MyTrainData$Weight_Intraday)
## saving the first model prediction
yPred <- results$prediction
y <- MyTestData[,output_one_column]
rmse(y,yPred)
SaveDataFrame(results$prediction,outputDataPath,paste(algorithm_used,"resultsHoldoutM1",sep=""))
## saving the first model xgb forest
SaveDataFrame(results$model,outputDataPath,paste(algorithm_used,"modelHoldoutM1",sep=""))
M1_Ret_PlusTwoQuarter <- results$prediction
M1_Ret_SecondMidday <- MyTestData$Ret_PlusOneQuarter+ M1_Ret_PlusTwoQuarter
# ### Reading back the results
# predictionM1 <- readRDS(paste(outputDataPath,paste(algorithm_used,"resultsM1.rds",sep=""), sep = ""))
# M1_Ret_PlusTwoQuarter <- predictionM1
# M1_Ret_SecondMidday <- MyTestData$Ret_PlusOneQuarter+ M1_Ret_PlusTwoQuarter


#### Second model M2
print("Training model M2")
my_predicting_columns <- my_stump_predicting_columns
output_one_column <- "Ret_PlusOne"
# ### Computing yourself the results
# results <- RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, MyTrainData, MyTestData,MyTrainData$Weight_Daily)
# ## saving the first model prediction
# SaveDataFrame(results$prediction,outputDataPath,paste(algorithm_used,"resultsM2",sep=""))
# ## saving the first model xgb forest
# SaveDataFrame(results$model,outputDataPath,paste(algorithm_used,"modelM2",sep=""))
# M2_Ret_PlusOne <- results$prediction
### Reading back the results
predictionM2 <- readRDS(paste(outputDataPath,paste(algorithm_used,"resultsHoldoutM2.rds",sep=""), sep = ""))
M2_Ret_PlusOne <- predictionM2

#### Second model M21 : we had the prediction from M1 for Ret_PlusTwoQuarter
print("Training model M21")

MyTestData$Ret_PlusTwoQuarter <- M1_Ret_PlusTwoQuarter
MyTestData$Ret_SecondMidday <- MyTestData$Ret_PlusOneQuarter+M1_Ret_PlusTwoQuarter

my_predicting_columns <- c(my_stump_predicting_columns,"Ret_PlusTwoQuarter","Ret_SecondMidday")
output_one_column <- "Ret_PlusOne"
# ### Computing yourself the model
# results <- RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, MyTrainData, MyTestData,MyTrainData$Weight_Daily)
# ## saving the first model prediction
# SaveDataFrame(results$prediction,outputDataPath,paste(algorithm_used,"resultsM21",sep=""))
# ## saving the first model xgb forest
# SaveDataFrame(results$model,outputDataPath,paste(algorithm_used,"modelM21",sep=""))
# M21_Ret_PlusOne <- results$prediction
### Reading back the results
predictionM21 <- readRDS(paste(outputDataPath,paste(algorithm_used,"resultsHoldoutM21.rds",sep=""), sep = ""))
M21_Ret_PlusOne <- predictionM21

#### Third model M3
print("Training model M3")
my_predicting_columns <- my_stump_predicting_columns
output_one_column <- "Ret_PlusTwo"
# ##### Computing yourself the results
# results <- RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, MyTrainData, MyTestData, MyTrainData$Weight_Daily)
# ## saving the first model prediction
# SaveDataFrame(results$prediction,outputDataPath,paste(algorithm_used,"resultsM3",sep=""))
# ## saving the first model xgb forest
# SaveDataFrame(results$model,outputDataPath,paste(algorithm_used,"modelM3",sep=""))
# M3_Ret_PlusTwo <- results$prediction
### Reading back the results
predictionM3 <- readRDS(paste(outputDataPath,paste(algorithm_used,"resultsHoldoutM3.rds",sep=""), sep = ""))
M3_Ret_PlusTwo <- predictionM3

#### Third model M31
print("Training model M31")
MyTestData$Ret_PlusTwoQuarter <- M1_Ret_PlusTwoQuarter
MyTestData$Ret_SecondMidday <- MyTestData$Ret_PlusOneQuarter+M1_Ret_PlusTwoQuarter
my_predicting_columns <- c(my_stump_predicting_columns,"Ret_PlusTwoQuarter","Ret_SecondMidday")
output_one_column <- "Ret_PlusTwo"
# #### Computing yourself the model
# results <- RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, MyTrainData, MyTestData, MyTrainData$Weight_Daily)
# ## saving the first model prediction
# SaveDataFrame(results$prediction,outputDataPath,paste(algorithm_used,"resultsM31",sep=""))
# ## saving the first model xgb forest
# SaveDataFrame(results$model,outputDataPath,paste(algorithm_used,"modelM31",sep=""))
# M31_Ret_PlusTwo <- results$prediction
### Reading back the results
predictionM31 <- readRDS(paste(outputDataPath,paste(algorithm_used,"resultsHoldoutM31.rds",sep=""), sep = ""))
M31_Ret_PlusTwo <- predictionM31

#### Third model M32
print("Training model M32")
MyTestData$Ret_PlusTwoQuarter <- M1_Ret_PlusTwoQuarter
MyTestData$Ret_SecondMidday <- MyTestData$Ret_PlusOneQuarter+M1_Ret_PlusTwoQuarter
MyTestData$Ret_PlusOne <- M2_Ret_PlusOne
my_predicting_columns <- c(my_stump_predicting_columns,"Ret_PlusTwoQuarter","Ret_SecondMidday","Ret_PlusOne")
output_one_column <- "Ret_PlusTwo"
# ### Computing yourself the results
# results <- RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, MyTrainData, MyTestData, MyTrainData$Weight_Daily)
# ## saving the first model prediction
# SaveDataFrame(results$prediction,outputDataPath,paste(algorithm_used,"resultsM32",sep=""))
# ## saving the first model xgb forest
# SaveDataFrame(results$model,outputDataPath,paste(algorithm_used,"modelM32",sep=""))
# M32_Ret_PlusTwo <- results$prediction
## Reading back the results
predictionM32 <- readRDS(paste(outputDataPath,paste(algorithm_used,"resultsHoldoutM32.rds",sep=""), sep = ""))
M32_Ret_PlusTwo <- predictionM32

#### Third model M321
print("Training model M321")
MyTestData$Ret_PlusTwoQuarter <- M1_Ret_PlusTwoQuarter
MyTestData$Ret_SecondMidday <- MyTestData$Ret_PlusOneQuarter+M1_Ret_PlusTwoQuarter
MyTestData$Ret_PlusOne <- M21_Ret_PlusOne
my_predicting_columns <- c(my_stump_predicting_columns,"Ret_PlusTwoQuarter","Ret_SecondMidday","Ret_PlusOne")
output_one_column <- "Ret_PlusTwo"
#### Computing the results yourself
results <- RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, MyTrainData, MyTestData, MyTrainData$Weight_Daily)
## saving the first model prediction
SaveDataFrame(results$prediction,outputDataPath,paste(algorithm_used,"resultsHoldoutM321",sep=""))
## saving the first model xgb forest
SaveDataFrame(results$model,outputDataPath,paste(algorithm_used,"modelHoldoutM321",sep=""))
M321_Ret_PlusTwo <- results$prediction
# ### Reading back the results
# predictionM321 <- readRDS(paste(outputDataPath,paste(algorithm_used,"resultsM321.rds",sep=""), sep = ""))
# M321_Ret_PlusTwo <- predictionM321


## all predicted variables
# M1_Ret_PlusTwoQuarter
# M1_Ret_SecondMidday
# M2_Ret_PlusOne
# M21_Ret_PlusOne
# M3_Ret_PlusTwo
# M31_Ret_PlusTwo
# M32_Ret_PlusTwo

# first mixing methodology
Ret_PlusTwoQuarter <- M1_Ret_PlusTwoQuarter
print(length(Ret_PlusTwoQuarter))
Ret_PlusTwo <- (M3_Ret_PlusTwo+M31_Ret_PlusTwo+M32_Ret_PlusTwo)/3
Ret_PlusOne <- (M2_Ret_PlusOne+M21_Ret_PlusOne)/3
print(length(Ret_PlusOne))

### we evenly split our minutes prediction from Ret_PlusTwoQuarter
print("Aggregating the prediction of all models")
Ret_Minute_PlusTwoQuarter <-  Ret_PlusTwoQuarter/60
Ret_Minute_PlusTwoQuarter_Matrix <- matrix(Ret_Minute_PlusTwoQuarter,nrow=length(Ret_Minute_PlusTwoQuarter),ncol=60,byrow=FALSE)
data_matrix <- cbind(Ret_Minute_PlusTwoQuarter_Matrix,Ret_PlusOne)
data_matrix <- cbind(data_matrix,Ret_PlusTwo)
colnames(data_matrix) <- c(paste0("Ret_",seq(121,180)),"Ret_PlusOne","Ret_PlusTwo")
rownames(data_matrix) <- seq(1,60000)


SaveDataFrame(data_matrix,outputDataPath,paste0("pre_submission1",algorithm_used))


# second mixing methodology
Ret_PlusTwoQuarter <- M1_Ret_PlusTwoQuarter
print(length(Ret_PlusTwoQuarter))
Ret_PlusTwo <- M3_Ret_PlusTwo
Ret_PlusOne <- M2_Ret_PlusOne


### we evenly split our minutes prediction from Ret_PlusTwoQuarter
Ret_Minute_PlusTwoQuarter <-  Ret_PlusTwoQuarter/60
Ret_Minute_PlusTwoQuarter_Matrix <- matrix(Ret_Minute_PlusTwoQuarter,nrow=length(Ret_Minute_PlusTwoQuarter),ncol=60,byrow=FALSE)
data_matrix <- cbind(Ret_Minute_PlusTwoQuarter_Matrix,Ret_PlusOne)
data_matrix <- cbind(data_matrix,Ret_PlusTwo)
colnames(data_matrix) <- c(paste0("Ret_",seq(121,180)),"Ret_PlusOne","Ret_PlusTwo")
rownames(data_matrix) <- seq(1,60000)


SaveDataFrame(data_matrix,outputDataPath,paste0("pre_submission2",algorithm_used))

# Training the stacked model
## Russian dolls model


