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

source("./RCode/RP_KLUtility.R")
source("./RCode/RP_KLReturns.R")

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
test_path <- paste0(outputDataPath, "test.csv")
print("Reading testing data")
MyTestData <- read.csv(file=test_path, header=TRUE, sep=",")
# Train data
train_path <- paste0(outputDataPath, "train.csv")
print("Reading training data")
MyTrainData <- read.csv(file=train_path, header=TRUE, sep=",")

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
    MyTrainData[is.na(MyTrainData[,i]),i] = NaN
  } else {
    MyTrainData[,i] = as.character(MyTrainData[,i])
    MyTrainData[is.na(MyTrainData[,i]),i] = "999"
    MyTrainData[,i] = as.factor(MyTrainData[,i])
  }
}

for(i in 1:ncol(MyTestData)){
  if(is.numeric(MyTestData[,i])){
    MyTestData[is.na(MyTestData[,i]),i] = NaN
  } else {
    MyTestData[,i] = as.character(MyTestData[,i])
    MyTestData[is.na(MyTestData[,i]),i] = "999"
    MyTestData[,i] = as.factor(MyTestData[,i])
  }
}

first_quarter_minutes <- paste("Ret_",seq(2,120),sep="")
FirstQuarterDf <- MyTrainData[,first_quarter_minutes]
# Fitting 4th order model to each row for the minutes 2 to 150
MyTraining120MinutesRSI <- FitRSIModelEachRow(FirstQuarterDf)
MyTraining120MinutesDEMA <- FitDEMAModelEachRow(FirstQuarterDf)
MyTraining120MinutesTREND <- FitTRENDModelEachRow(FirstQuarterDf)
MyTraining120MinutesBollinger <- FitBollingerModelEachRow(FirstQuarterDf)



SaveDataFrame(MyTraining120MinutesRSI,outputDataPath,"training_120_minutes_RSI_df")
SaveDataFrame(MyTraining120MinutesDEMA,outputDataPath,"training_120_minutes_DEMA_df")
SaveDataFrame(MyTraining120MinutesTREND,outputDataPath,"training_120_minutes_TREND_df")
SaveDataFrame(MyTraining120MinutesBollinger,outputDataPath,"training_120_minutes_Bollinger_df")


# 
# 
# MyTraining120Minutes1Coef <- t(Fit1ModelEachRow(FirstQuarterDf))
# # colnames(MyTraining120Minutes4Coef) <- c("120Coef0","120Coef1","120Coef2","120Coef3","120Coef4")
# # colnames(MyTraining120Minutes1Coef) <- c("120Coef10","120Coef11")
# colnames(MyTraining120Minutes4Coef) <- c("120Coef1","120Coef2","120Coef3","120Coef4")
# colnames(MyTraining120Minutes1Coef) <- c("120Coef11")
# 
# 
# all_minutes <- paste("Ret_",seq(2,180),sep="")
# MinutesDf <- MyTrainData[,all_minutes]
# # Fitting 4th order model to each row for the minutes 2 to 150
# MyTraining180Minutes4Coef <- Fit4ModelEachRow(MinutesDf)
# MyTraining180Minutes1Coef <- t(Fit1ModelEachRow(MinutesDf))
# # colnames(MyTraining180Minutes4Coef) <- c("180Coef40","180Coef41","180Coef42","180Coef43","180Coef44")
# # colnames(MyTraining180Minutes1Coef) <- c("180Coef10","180Coef11")
# colnames(MyTraining180Minutes4Coef) <- c("180Coef41","180Coef42","180Coef43","180Coef44")
# colnames(MyTraining180Minutes1Coef) <- c("180Coef11")
# 
# last60_minutes <- paste("Ret_",seq(121,180),sep="")
# last60Df <- MyTrainData[,last60_minutes]
# # Fitting 4th order model to each row for the minutes 2 to 150
# MyTraininglast60Minutes4Coef <- Fit4ModelEachRow(last60Df)
# MyTraininglast60Minutes1Coef <- t(Fit1ModelEachRow(last60Df))
# # colnames(MyTraininglast60Minutes4Coef) <- c("last60Coef40","last60Coef41","last60Coef42","last60Coef43","last60Coef44")
# # colnames(MyTraininglast60Minutes1Coef) <- c("last60Coef10","last60Coef11")
# colnames(MyTraininglast60Minutes4Coef) <- c("last60Coef41","last60Coef42","last60Coef43","last60Coef44")
# colnames(MyTraininglast60Minutes1Coef) <- c("last60Coef11")
# 
# #### Saving 4th coefficient data frame Merging the data frames together
# # no merging here, merging later
# # MyTrainData <- cbind(MyTrainData , MyTraining120Minutes4Coef)
# SaveDataFrame(MyTraining180Minutes4Coef,outputDataPath,"training_180_minutes_coefs4_df")
# SaveDataFrame(MyTraining120Minutes4Coef,outputDataPath,"training_120_minutes_coefs4_df")
# 
# SaveDataFrame(MyTraining180Minutes1Coef,outputDataPath,"training_180_minutes_coefs1_df")
# SaveDataFrame(MyTraining120Minutes1Coef,outputDataPath,"training_120_minutes_coefs1_df")
# 
# SaveDataFrame(MyTraininglast60Minutes1Coef,outputDataPath,"training_last60_minutes_coefs1_df")
# SaveDataFrame(MyTraininglast60Minutes4Coef,outputDataPath,"training_last60_minutes_coefs4_df")
# 
# ##### Some visualization
# ## Checking our fitting 4 coef for row number 10000
# i <- 10000
# y <- CumFromRetToPricesStart(t(FirstQuarterDf[i,]))
# x <- 1:length(y)
# # fit4 <- lm(y~poly(x,4,raw=TRUE))
# #generate range of 50 numbers starting from 30 and ending at 160
# # coefs4 <- coef(fit4)
# # Formula with intercept
# # my_4_prediction <- MyTraining120Minutes4Coef[i,1]*x^0+MyTraining120Minutes4Coef[i,2]*x^1+MyTraining120Minutes4Coef[i,3]*x^2+MyTraining120Minutes4Coef[i,4]*x^3+MyTraining120Minutes4Coef[i,5]*x^4
# # Formula without intercept
# my_4_prediction <- MyTraining120Minutes4Coef[i,1]*x^1+MyTraining120Minutes4Coef[i,2]*x^2+MyTraining120Minutes4Coef[i,3]*x^3+MyTraining120Minutes4Coef[i,4]*x^4
# 
# # my_4_prediction <-predict(fit4, data.frame(x=x))
# toplotDf <- data.frame(x=x , y=y, 
#                        ypred4 =  my_4_prediction
# )
# toplotDf <- melt(toplotDf, id.vars="x")
# g <- ggplot(toplotDf,aes(
#   x = x,y = value, group=variable, color=variable
# )) + geom_line() 
# print(g)
# 
# ExportPlot(g,outputDataPath,paste0("4coefFitting120",i))
# ## Checking our fitting 1 coef for row number 10000
# i <- 10000
# y <- CumFromRetToPricesStart(t(FirstQuarterDf[i,]))
# x <- 1:length(y)
# # fit4 <- lm(y~poly(x,4,raw=TRUE))
# #generate range of 50 numbers starting from 30 and ending at 160
# # coefs4 <- coef(fit4)
# # Formula with intercept
# # my_4_prediction <- MyTraining120Minutes4Coef[i,1]*x^0+MyTraining120Minutes4Coef[i,2]*x^1+MyTraining120Minutes4Coef[i,3]*x^2+MyTraining120Minutes4Coef[i,4]*x^3+MyTraining120Minutes4Coef[i,5]*x^4
# # Formula without intercept
# my_1_prediction <- MyTraining120Minutes1Coef[i,1]*x^1
# # my_4_prediction <-predict(fit4, data.frame(x=x))
# toplotDf <- data.frame(x=x , y=y, 
#                        ypred =  my_1_prediction
# )
# toplotDf <- melt(toplotDf, id.vars="x")
# g <- ggplot(toplotDf,aes(
#   x = x,y = value, group=variable, color=variable
# )) + geom_line() 
# print(g)
# 
# ExportPlot(g,outputDataPath,paste0("1coefFitting120",i))
# for (i in seq(1,40000,10000)){
#   #   plot(t(FirstQuarterDf[1,]))
#   #   max(FirstQuarterDf[1,])
#   #   min(FirstQuarterDf[1,])
#   x <- 2:120
#   
#   y <- CumFromRetToPricesStart(t(FirstQuarterDf[i,]))
#   plot(x,y)
#   fit  <- lm(y~x)
#   #second degree
#   fit2 <- lm(y~poly(x,2,raw=TRUE)-1)
#   #third degree
#   fit3 <- lm(y~poly(x,3,raw=TRUE)-1)
#   #fourth degree
#   fit4 <- lm(y~poly(x,4,raw=TRUE)-1)
#   #generate range of 50 numbers starting from 30 and ending at 160
#   coefs4 <- coef(fit4)
#   my_4_prediction <- coefs4[4]*x^4+coefs4[3]*x^3+coefs4[2]*x^2+coefs4[1]*x^1
#   
#   toplotDf <- data.frame(x=x , y=y, 
#                          ypred1 =  predict(fit, data.frame(x=x)),
#                          ypred2 =  predict(fit2, data.frame(x=x)),
#                          ypred3 =  predict(fit3, data.frame(x=x)),
#                          # ypred4 =  predict(fit4, data.frame(x=x))
#                          ypred4 =  my_4_prediction
#   )
#   
#   
#   toplotDf <- melt(toplotDf, id.vars="x")
#   
#   g <- ggplot(toplotDf,aes(
#     x = x,y = value, group=variable, color=variable
#   )) + geom_line() 
#   print(g)
#   
#   ExportPlot(g,outputDataPath,paste0("4coefFitting",i))
#   
#   summary(fit)
#   summary(fit2)
#   summary(fit3)
#   summary(fit4)
#   #y=ax^4 + bx^3 + cx^2 + dx + e
# }
# # i <- 10000
# # y <- CumFromRetToPricesStart(t(MinutesDf[i,]))
# # x <- 1:length(y)
# # # fit4 <- lm(y~poly(x,4,raw=TRUE))
# # #generate range of 50 numbers starting from 30 and ending at 160
# # # coefs4 <- coef(fit4)
# # my_4_prediction <- MyTraining180Minutes4Coef[i,1]*x^0+MyTraining180Minutes4Coef[i,2]*x^1+MyTraining180Minutes4Coef[i,3]*x^2+MyTraining180Minutes4Coef[i,4]*x^3+MyTraining180Minutes4Coef[i,5]*x^4
# # # my_4_prediction <-predict(fit4, data.frame(x=x))
# # toplotDf <- data.frame(x=x , y=y, 
# #                        ypred4 =  my_4_prediction
# # )
# # toplotDf <- melt(toplotDf, id.vars="x")
# # g <- ggplot(toplotDf,aes(
# #   x = x,y = value, group=variable, color=variable
# # )) + geom_line() 
# # print(g)
# # 
# # ExportPlot(g,outputDataPath,paste0("Fitting180",i))
# ### Now building price momentum indicators from TTR package
# 
# ### Computing now the test 120 minutes polynomial coefficients
# first_quarter_minutes <- paste("Ret_",seq(2,120),sep="")
# TestFirstQuarterDf <- MyTestData[,first_quarter_minutes]
# # Fitting 4th order model to each row for the minutes 2 to 150
# MyTest120Minutes4Coef <- Fit4ModelEachRow(TestFirstQuarterDf)
# MyTest120Minutes1Coef <- t(Fit1ModelEachRow(TestFirstQuarterDf))
# # colnames(MyTest120Minutes4Coef) <- c("120Coef0","120Coef1","120Coef2","120Coef3","120Coef4")
# # colnames(MyTest120Minutes1Coef) <- c("120Coef10","120Coef11")
# colnames(MyTest120Minutes4Coef) <- c("120Coef1","120Coef2","120Coef3","120Coef4")
# colnames(MyTest120Minutes1Coef) <- c("120Coef11")
# 
# SaveDataFrame(MyTest120Minutes4Coef,outputDataPath,"test_120_minutes_coefs4_df")
# SaveDataFrame(MyTest120Minutes1Coef,outputDataPath,"test_120_minutes_coefs1_df")
# 
# # #### Computing day level returns
# # # Averaging the first sccond-half qurter d day minutes returns into one predictor 
# # first_quarter_minutes <- paste("Ret_",seq(2,120),sep="")
# # second_quarter_minutes <-  paste("Ret_",seq(121,180),sep="")
# # ## Cumulative returns for quarter aggregation
# # MyTrainData$Ret_PlusOneQuarter <- rowSums(MyTrainData[,first_quarter_minutes],na.rm=TRUE)
# # MyTestData$Ret_PlusOneQuarter <- rowSums(MyTestData[,first_quarter_minutes],na.rm=TRUE)
# # 
# # MyTrainData$Ret_PlusTwoQuarter <- rowSums(MyTrainData[,second_quarter_minutes],na.rm=TRUE)
# # 
# # MyTrainData$Ret_SecondMidday <- MyTrainData$Ret_PlusOneQuarter + MyTrainData$Ret_PlusTwoQuarter
# # 
# # MyTrainData$Ret_zero <- MyTrainData$Ret_MinusOne + MyTrainData$Ret_SecondMidday
# # 
# # #### Rescaling the weights
# # MyTrainData$Weight_Intraday <- (MyTrainData$Weight_Intraday-mean(MyTrainData$Weight_Intraday) )/sd(MyTrainData$Weight_Intraday)
# # MyTrainData$Weight_Daily <- (MyTrainData$Weight_Daily-mean(MyTrainData$Weight_Daily) )/sd(MyTrainData$Weight_Daily)
# # 
# # MyTrainData$Weight_Intraday <- MyTrainData$Weight_Intraday -min(MyTrainData$Weight_Intraday)
# # MyTrainData$Weight_Daily <- MyTrainData$Weight_Daily -min(MyTrainData$Weight_Daily)
# # 
# # 
# # ### Computing factor models features : size, momentum 1,2 days, Intraday Vol from minutes, last dayly vol
# # ###### Deducing size from price
# # # Ret_MinusTwo 
# # # Ret_MinusOne
# # # Ret_2 => Ret_160 first two third of the half day
# # # Ret_161 => Ret_180 last third of the half day
# # # Ret_PlusOneQuarter+Ret_PlusTwoQuarter=Ret_SecondMidday
# # # Ret_MinusOne + Ret_SecondMidday = Ret_zero
# # # Ret_PlusOne
# # # Ret_PlusTwo
# # 
# # ### First model predictor
# # ## M1 : available Ret_MinusTwo Ret_MinusOne, Ret_2 => Ret_160
# # first_quarter_minutes <- paste("Ret_",seq(2,120),sep="")
# # FirstQuarterDf <- MyTrainData[,first_quarter_minutes]
# # RSIValues <- c(3,5,seq(10,100,10))
# # RSIFirstQuarterDf
# # DEMAValues <- c(3,5,seq(10,100,10))
# # DEMAFirstQuarterDf
# # TrendValues <- c(3,5,seq(10,100,10))
# # TrendFirstQuarterDf
# # 
# # 
# # ### Second model predictor
# # ## M2 : available Ret_MinusTwo Ret_MinusOne, Ret_2 => Ret_160
# # # or
# # ## M21 available Ret_MinusTwo Ret_MinusOne, Ret_2 => Ret_180
# # 
# # 
# # ### Third model predictor
# # ## M31 available Ret_MinusTwo Ret_MinusOne, Ret_2 => Ret_160
# # ## M32 M321 available Ret_MinusTwo Ret_MinusOne, Ret_2 => Ret_180, Ret_PlusOne
# # 
# # # from Ret_2 => Ret_160 predictors
# # 
# # # Ret_MinusTwo : day-1 
# # # Ret_MinusOne : mid day d day
# # # Ret_2 to Ret_120 : first 120 minutes past miday
# # # Ret_121 to Ret_180 : last 60 minutes till the close of d day
# # # Ret_PlusOne : day+1   
# # # Ret_PlusTwo : day+2
# # 
# # print("Day -1 returns standard deviation")
# # print(sd(MyTrainData$Ret_MinusTwo))
# # print("Order of magnitude day")
# # print("2*10^(-2)")
# # print("mid day d day  returns standard deviation")
# # print(sd(MyTrainData$Ret_MinusOne))
# # print("Order of magnitude half day")
# # print("1*10^(-2)")
# # 
# # print("second mid day d day first 120 minutes returns average standard deviation")
# # print(mean(apply(MyTrainData[,first_quarter_minutes],2,sd,na.rm=TRUE)))
# # print("Order of magnitude minute")
# # print("1*10^(-3)")
# # print("second mid day d day last 60 minutes to close  returns average standard deviation")
# # print(mean(apply(MyTrainData[,second_quarter_minutes],2,sd,na.rm=TRUE)))
# # print("Order of magnitude minute")
# # print("1*10^(-3)")
# # print("second mid day d day first 120 minutes CUMSUM returns standard deviation")
# # print(sd(MyTrainData$Ret_PlusOneQuarter))
# # print("Order of magnitude 2/3 half day")
# # print("1*10^(-2)")
# # print("second mid day d day last 60 minutes to close CUMSUM returns average standard deviation")
# # print(sd(MyTrainData$Ret_PlusTwoQuarter))
# # print("Order of magnitude 1/3 half day")
# # print("8*10^(-3)")
# # print("second mid day d day all minutes CUMSUM returns average standard deviation")
# # print(sd(MyTrainData$Ret_SecondMidday))
# # print("Order of magnitude half day")
# # print("1*10^(-2)")
# # 
# # print("Day +_1 returns standard deviation")
# # print(sd(MyTrainData$Ret_PlusOne))
# # print("Order of magnitude day")
# # print("2*10^(-2)")
# # print("Day +2 returns standard deviation")
# # print(sd(MyTrainData$Ret_PlusTwo))
# # print("Order of magnitude day")
# # print("2*10^(-2)")