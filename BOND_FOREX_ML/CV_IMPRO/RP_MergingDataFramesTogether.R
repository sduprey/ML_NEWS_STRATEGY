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
# Test data
test_path <- "C:/My_Kaggle_Challenges_Data/Winton/test_2.csv"
print("Reading testing data")
MyTestData <- read.csv(file=test_path, header=TRUE, sep=",")
# Train data
train_path <- "C:/My_Kaggle_Challenges_Data/Winton/train.csv"
print("Reading training data")

MyTrainData <- read.csv(file=train_path, header=TRUE, sep=",")
# Sample data
sample_path <- "C:/My_Kaggle_Challenges_Data/Winton/sample_submission.csv"
MySampleSubmissionData <- read.csv(file=sample_path, header=TRUE, sep=",")

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

# MyTrainData$Weight_Intraday <- (MyTrainData$Weight_Intraday-mean(MyTrainData$Weight_Intraday) )/sd(MyTrainData$Weight_Intraday)
# MyTrainData$Weight_Daily <- (MyTrainData$Weight_Daily-mean(MyTrainData$Weight_Daily) )/sd(MyTrainData$Weight_Daily)
# 
# MyTrainData$Weight_Intraday <- MyTrainData$Weight_Intraday -min(MyTrainData$Weight_Intraday)
# MyTrainData$Weight_Daily <- MyTrainData$Weight_Daily -min(MyTrainData$Weight_Daily)
# 


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


#### Reading the previously computed polynomial reduction
# SaveDataFrame(MyTraining180Minutes4Coef,outputDataPath,"training_180_minutes_coefs4_df")
# SaveDataFrame(MyTraining120Minutes4Coef,outputDataPath,"training_120_minutes_coefs4_df")
# 
# SaveDataFrame(MyTraining180Minutes1Coef,outputDataPath,"training_180_minutes_coefs1_df")
# SaveDataFrame(MyTraining120Minutes1Coef,outputDataPath,"training_120_minutes_coefs1_df")
# 
# 
# SaveDataFrame(MyTest120Minutes4Coef,outputDataPath,"test_120_minutes_coefs4_df")
# SaveDataFrame(MyTest120Minutes1Coef,outputDataPath,"test_120_minutes_coefs1_df")


training_180_minutes_coefs4_df <- readRDS(paste(outputDataPath,paste("training_180_minutes_coefs4_df",".rds",sep=""), sep = ""))
training_120_minutes_coefs4_df <- readRDS(paste(outputDataPath,paste("training_120_minutes_coefs4_df",".rds",sep=""), sep = ""))

training_180_minutes_coefs1_df <- readRDS(paste(outputDataPath,paste("training_180_minutes_coefs1_df",".rds",sep=""), sep = ""))
training_120_minutes_coefs1_df <- readRDS(paste(outputDataPath,paste("training_120_minutes_coefs1_df",".rds",sep=""), sep = ""))

test_120_minutes_coefs4_df <- readRDS(paste(outputDataPath,paste("test_120_minutes_coefs4_df",".rds",sep=""), sep = ""))
test_120_minutes_coefs1_df <- readRDS(paste(outputDataPath,paste("test_120_minutes_coefs1_df",".rds",sep=""), sep = ""))

training_last60_minutes_coefs4_df <- readRDS(paste(outputDataPath,paste("training_last60_minutes_coefs4_df",".rds",sep=""), sep = ""))
training_last60_minutes_coefs1_df <- readRDS(paste(outputDataPath,paste("training_last60_minutes_coefs1_df",".rds",sep=""), sep = ""))



# M2_Ret_PlusOne <- predictionM2

MyExpandedTrainData <- cbind(MyTrainData,training_120_minutes_coefs4_df)
MyExpandedTrainData <- cbind(MyExpandedTrainData,training_180_minutes_coefs1_df)
MyExpandedTrainData <- cbind(MyExpandedTrainData,training_180_minutes_coefs4_df)
MyExpandedTrainData <- cbind(MyExpandedTrainData,training_last60_minutes_coefs1_df)


MyExpandedTestData <- cbind(MyTestData,test_120_minutes_coefs4_df)

