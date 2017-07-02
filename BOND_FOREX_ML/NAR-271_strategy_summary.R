### Strategy summary
### Launching all pair spread computations
### Trying to forecast the spread between ?/? bonds futures
# library("SIT")
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
library("RPBackTesting", lib.loc="C:/Program Files/R/R-3.2.2/library")

# source("./RCode/RP_Plotting_Utils.R")
# source("./RCode/RP_Macro_Monthly_Utils.R")
# source("./RCode/RP_Spread_Utils.R")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")
outputDataPathWeek <- paste(outputDataPath,"Week_2007/",sep="")

# SaveDataFrame(results,outputDataPathWeek,"week_to_summarize")
results <- readRDS(paste(outputDataPathWeek,"week_to_summarize.rds",sep=""))
#### FACT CHECKING

IR_spreadbidir_today <- computeIR(results$STRATEGY_RETURN, biggest_today_IR$horizon)
IR_spreadbidir_tomorrow <- computeIRfromPrice(results$STRATEGY_TOMORROW, biggest_today_IR$horizon)
IR_spreadbidir_yesterday <- computeIRfromPrice(results$STRATEGY_YESTERDAY, biggest_today_IR$horizon)
# fact checking
STRATEGY_RETURN <- CumfromPricesToRet(results$STRATEGY_TODAY)
sum(STRATEGY_RETURN-results$STRATEGY_RETURN[2:length(results$STRATEGY_RETURN)])

FIRST_BOND_RETURN <- CumfromPricesToRet(results$FIRST_BOND)
sum(FIRST_BOND_RETURN - results$FIRST_BOND_RETURN[2:length(results$STRATEGY_RETURN)])

SECOND_BOND_RETURN <- CumfromPricesToRet(results$SECOND_BOND)
sum(SECOND_BOND_RETURN - results$SECOND_BOND_RETURN[2:length(results$STRATEGY_RETURN)])

FIRST_BOND <- CumFromRetToPricesStart(results$FIRST_BOND_RETURN)
sum(FIRST_BOND - results$FIRST_BOND)

SECOND_BOND <- CumFromRetToPricesStart(results$SECOND_BOND_RETURN)
sum(SECOND_BOND - results$SECOND_BOND)

FIRST_WEIGHT <- results$FIRST_WEIGHT
SECOND_WEIGHT <- results$SECOND_WEIGHT

FIRST_BOND_NEXT_RETURN<-results$FIRST_BOND_NEXT_RETURN
SECOND_BOND_NEXT_OPEN_RETURN<-results$SECOND_BOND_NEXT_OPEN_RETURN


STRATEGY_RETURN <- FIRST_WEIGHT*FIRST_BOND_NEXT_RETURN+SECOND_WEIGHT*SECOND_BOND_NEXT_OPEN_RETURN
sum(STRATEGY_RETURN - results$STRATEGY_RETURN)

# Decomposing our strategy over two legs
sum(results$FIRST_LEG - results$FIRST_WEIGHT*results$FIRST_BOND_NEXT_RETURN)
sum(results$SECOND_LEG - results$SECOND_WEIGHT*results$SECOND_BOND_NEXT_OPEN_RETURN)

#### Computing the turn over metrics
# print(sum(technical_signal[-1]==technical_signal[1:(length(technical_signal)-1)])/length(technical_signal[-1]))
# foo = cbind(as.Date(my_merged_df$DATE[backtesting_index]),traded_return,rep(0,length(traded_return)))
# t = RP_GetTurnOver(data.frame(foo),-1)
# mean(t$TURNOVER)
print("Outputing statistical results for our strategy")
WeightMatrix <- results[,c("DATES","FIRST_WEIGHT","SECOND_WEIGHT")]
colnames(WeightMatrix) <- c("DATE","FIRST_WEIGHT","SECOND_WEIGHT")
# ReturnSerie <- results[,c("DATES","STRATEGY_RETURN")]
# colnames(ReturnSerie) <- c("Date","STRATEGY_RETURN")
ReturnSerie <- results[,c("STRATEGY_RETURN")]
turnover <- RP_GetTurnOver(WeightMatrix,-1)
RP_ReturnStats(ReturnSerie,5,TRUE,WeightMatrix)


