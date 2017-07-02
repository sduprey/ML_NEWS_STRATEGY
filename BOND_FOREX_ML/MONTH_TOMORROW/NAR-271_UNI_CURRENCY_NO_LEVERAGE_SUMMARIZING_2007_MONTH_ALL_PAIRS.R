### Launching all pair spread computations
### Trying to forecast the spread between ?/? bonds futures
# library("SIT")
library("RPQuantUtils")
library("RPToolsDB")
library("RPBackTesting")
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
library(caret)
library(ROCR)

source("./RCode/RP_Plotting_Utils.R")
source("./RCode/RP_Macro_Monthly_Utils.R")
source("./RCode/RP_Spread_Utils.R")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")
# outputDataPathMonth <- paste(outputDataPath,"Month_2007/01_11_2015/",sep="")
# outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007/01_11_2015/",sep="")
outputDataPathMonth <- paste(outputDataPath,"Month_2007_Old/01_11_2015/",sep="")
outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007_Old/01_11_2015/",sep="")


results <- readRDS(paste(outputDataPathStrategyMonth,"all_pairs_results_month_2007.rds", sep = ""))

## Global equally weighted strategy
results$US_FIRST_PREDICTION <- (results$JP_FIRST_PREDICTION + results$GB_FIRST_PREDICTION + results$DE_FIRST_PREDICTION)/3

my_normalizing_weight <- abs(results$US_FIRST_PREDICTION) + abs(results$DE_SECOND_PREDICTION) + abs(results$GB_SECOND_PREDICTION) + abs(results$JP_SECOND_PREDICTION)

results$US_WEIGHT <- results$US_FIRST_PREDICTION/my_normalizing_weight
results$DE_WEIGHT <- results$DE_SECOND_PREDICTION/my_normalizing_weight
results$GB_WEIGHT <- results$GB_SECOND_PREDICTION/my_normalizing_weight
results$JP_WEIGHT <- results$JP_SECOND_PREDICTION/my_normalizing_weight

results$STRATEGY_RETURN <- (results$US_WEIGHT*results$DE_FIRST_BOND_NEXT_RETURN+results$DE_WEIGHT*results$DE_SECOND_BOND_NEXT_OPEN_RETURN+results$GB_WEIGHT*results$GB_SECOND_BOND_NEXT_OPEN_RETURN+results$JP_WEIGHT*results$JP_SECOND_BOND_NEXT_OPEN_RETURN)
results$STRATEGY_TODAY <- CumFromRetToPricesStart(results$STRATEGY_RETURN)

print(abs(results$US_WEIGHT) + abs(results$DE_WEIGHT) + abs(results$GB_WEIGHT) +abs(results$JP_WEIGHT))


##### 5 quantiles methodology
### US uni currency strategy
Pred <- results$US_FIRST_PREDICTION
NextRet <- results$DE_FIRST_BOND_NEXT_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/5)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 0
df$my_prediction_quantiles[df$my_prediction_quantiles == 4] <- 1
df$my_prediction_quantiles[df$my_prediction_quantiles == 5] <- 1.5

StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
USSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


### JP uni currency strategy
Pred <- results$JP_SECOND_PREDICTION
NextRet <- results$JP_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/5)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 0
df$my_prediction_quantiles[df$my_prediction_quantiles == 4] <- 1
df$my_prediction_quantiles[df$my_prediction_quantiles == 5] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
JPSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


### GB uni currency strategy
Pred <- results$GB_SECOND_PREDICTION
NextRet <- results$GB_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/5)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 0
df$my_prediction_quantiles[df$my_prediction_quantiles == 4] <- 1
df$my_prediction_quantiles[df$my_prediction_quantiles == 5] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
GBSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)

### DE uni currency strategy
Pred <- results$DE_SECOND_PREDICTION
NextRet <- results$DE_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/5)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 0
df$my_prediction_quantiles[df$my_prediction_quantiles == 4] <- 1
df$my_prediction_quantiles[df$my_prediction_quantiles == 5] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
DESTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


plot(DESTRATEGY_TODAY)
plot(GBSTRATEGY_TODAY)
plot(JPSTRATEGY_TODAY)
plot(USSTRATEGY_TODAY)


uni_df <- data.frame(DATES= results$DATES,DE=DESTRATEGY_TODAY,DE_BOND=results$DE_SECOND_BOND,GB=GBSTRATEGY_TODAY,GB_BOND=results$GB_SECOND_BOND,JP=JPSTRATEGY_TODAY,JP_BOND=results$JP_SECOND_BOND,US=USSTRATEGY_TODAY,US_BOND=results$DE_FIRST_BOND )


toplot_df <-  melt(uni_df[,c("DATES", "DE","DE_BOND","GB","GB_BOND","JP","JP_BOND","US","US_BOND")],"DATES")
my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathMonth,"5_quantiles")

##### 4 quantiles methodology
### US uni currency strategy
Pred <- results$US_FIRST_PREDICTION
NextRet <- results$DE_FIRST_BOND_NEXT_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/4)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 1
df$my_prediction_quantiles[df$my_prediction_quantiles == 4] <- 1.5

StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
USSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


### JP uni currency strategy
Pred <- results$JP_SECOND_PREDICTION
NextRet <- results$JP_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/4)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 1
df$my_prediction_quantiles[df$my_prediction_quantiles == 4] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
JPSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


### GB uni currency strategy
Pred <- results$GB_SECOND_PREDICTION
NextRet <- results$GB_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/4)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 1
df$my_prediction_quantiles[df$my_prediction_quantiles == 4] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
GBSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)

### DE uni currency strategy
Pred <- results$DE_SECOND_PREDICTION
NextRet <- results$DE_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/4)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 1
df$my_prediction_quantiles[df$my_prediction_quantiles == 4] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
DESTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


plot(DESTRATEGY_TODAY)
plot(GBSTRATEGY_TODAY)
plot(JPSTRATEGY_TODAY)
plot(USSTRATEGY_TODAY)


uni_df <- data.frame(DATES= results$DATES,DE=DESTRATEGY_TODAY,DE_BOND=results$DE_SECOND_BOND,GB=GBSTRATEGY_TODAY,GB_BOND=results$GB_SECOND_BOND,JP=JPSTRATEGY_TODAY,JP_BOND=results$JP_SECOND_BOND,US=USSTRATEGY_TODAY,US_BOND=results$DE_FIRST_BOND )


toplot_df <-  melt(uni_df[,c("DATES", "DE","DE_BOND","GB","GB_BOND","JP","JP_BOND","US","US_BOND")],"DATES")
my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathMonth,"4_quantiles")


##### 3 quantiles methodology
### US uni currency strategy
Pred <- results$US_FIRST_PREDICTION
NextRet <- results$DE_FIRST_BOND_NEXT_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/3)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- 0
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 1.5

StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
USSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


### JP uni currency strategy
Pred <- results$JP_SECOND_PREDICTION
NextRet <- results$JP_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/3)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- 0
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
JPSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


### GB uni currency strategy
Pred <- results$GB_SECOND_PREDICTION
NextRet <- results$GB_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/3)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- 0
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
GBSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)

### DE uni currency strategy
Pred <- results$DE_SECOND_PREDICTION
NextRet <- results$DE_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/3)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1.5
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- 0
df$my_prediction_quantiles[df$my_prediction_quantiles == 3] <- 1.5
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
DESTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


plot(DESTRATEGY_TODAY)
plot(GBSTRATEGY_TODAY)
plot(JPSTRATEGY_TODAY)
plot(USSTRATEGY_TODAY)


uni_df <- data.frame(DATES= results$DATES,DE=DESTRATEGY_TODAY,DE_BOND=results$DE_SECOND_BOND,GB=GBSTRATEGY_TODAY,GB_BOND=results$GB_SECOND_BOND,JP=JPSTRATEGY_TODAY,JP_BOND=results$JP_SECOND_BOND,US=USSTRATEGY_TODAY,US_BOND=results$DE_FIRST_BOND )


toplot_df <-  melt(uni_df[,c("DATES", "DE","DE_BOND","GB","GB_BOND","JP","JP_BOND","US","US_BOND")],"DATES")
my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)
ExportPlot(g,outputDataPathMonth,"3_quantiles")

### US uni currency strategy
Pred <- results$US_FIRST_PREDICTION
NextRet <- results$DE_FIRST_BOND_NEXT_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/2)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- 1
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
USSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


### JP uni currency strategy
Pred <- results$JP_SECOND_PREDICTION
NextRet <- results$JP_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/2)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- 1
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
JPSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


### GB uni currency strategy
Pred <- results$GB_SECOND_PREDICTION
NextRet <- results$GB_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/2)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- 1
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
GBSTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)

### DE uni currency strategy
Pred <- results$DE_SECOND_PREDICTION
NextRet <- results$DE_SECOND_BOND_NEXT_OPEN_RETURN
df <- data.frame(Predictions = Pred , NextReturns = NextRet)
df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=1/2)), include.lowest=TRUE))
df$my_prediction_quantiles <- as.numeric(df$my_prediction_quantiles)
df$my_prediction_quantiles[df$my_prediction_quantiles == 1] <- -1
df$my_prediction_quantiles[df$my_prediction_quantiles == 2] <- 1
StratNextReturn <- df$my_prediction_quantiles*df$NextReturns
DESTRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)


plot(DESTRATEGY_TODAY)
plot(GBSTRATEGY_TODAY)
plot(JPSTRATEGY_TODAY)
plot(USSTRATEGY_TODAY)


uni_df <- data.frame(DATES= results$DATES,DE=DESTRATEGY_TODAY,DE_BOND=results$DE_SECOND_BOND,GB=GBSTRATEGY_TODAY,GB_BOND=results$GB_SECOND_BOND,JP=JPSTRATEGY_TODAY,JP_BOND=results$JP_SECOND_BOND,US=USSTRATEGY_TODAY,US_BOND=results$DE_FIRST_BOND )


toplot_df <-  melt(uni_df[,c("DATES", "DE","DE_BOND","GB","GB_BOND","JP","JP_BOND","US","US_BOND")],"DATES")
my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)
ExportPlot(g,outputDataPathMonth,"2_quantiles")

##### More sophisticated strategy




### US uni currency strategy
for (my_quantile_number in c(0.1,1/4,1/3,1/2)){
  Pred <- results$US_FIRST_PREDICTION
  NextRet <- results$DE_FIRST_BOND_NEXT_RETURN
  df <- data.frame(Predictions = Pred , NextReturns = NextRet)
  df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=my_quantile_number)), include.lowest=TRUE))
  df$signal <- NA
  df$signal[(df$my_prediction_quantiles == min(levels(df$my_prediction_quantiles)))] <- -1
  df$signal[(df$my_prediction_quantiles == max(levels(df$my_prediction_quantiles)))] <- 1
  
  x <- xts(df$signal, Sys.Date()+1:length(df$signal))
  no_na_x <- na.locf(x)
  df$signal_quant <- as.numeric(no_na_x)
  na_index <- !is.na.data.frame(df$signal_quant)
  df <- df[na_index,]
  dates <- results$DATES[na_index]
  ## change here
  bond_return <- results$DE_FIRST_BOND_RETURN[na_index]
  StratNextReturn <- df$signal_quant*df$NextReturns
  STRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)
  BOND <- CumFromRetToPricesStart(bond_return)
  
  uni_df <- data.frame(DATES= results$DATES[na_index],STRAT=STRATEGY_TODAY,BOND= BOND)
  
  toplot_df <-  melt(uni_df[,c("DATES", "STRAT","BOND")],"DATES")
  my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",sep="")
  g<-ggplot(
    toplot_df,aes(
      x = DATES,y = value,group = variable,color = variable
    )
  ) +
    geom_line() +
    scale_x_date() +
    ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
    theme(title = element_text(size = 12, face = 'bold')) +
    theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
    theme(legend.background = element_rect(fill = "gray90")) +
    theme(legend.key.size = unit(0.7, "cm"))
  print(g)
  print("Next quantile")
}
### we do not enter trade until we encounter an extreme quantile




### US uni currency strategy
for (my_quantile_number in c(0.1,1/4,1/3,1/2)){
  Pred <- results$JP_SECOND_PREDICTION
  NextRet <- results$JP_SECOND_BOND_NEXT_OPEN_RETURN

  df <- data.frame(Predictions = Pred , NextReturns = NextRet)
  df$my_prediction_quantiles <- with(df, cut(Pred, breaks=quantile(Pred, probs=seq(0,1, by=my_quantile_number)), include.lowest=TRUE))
  df$signal <- NA
  df$signal[(df$my_prediction_quantiles == min(levels(df$my_prediction_quantiles)))] <- -1
  df$signal[(df$my_prediction_quantiles == max(levels(df$my_prediction_quantiles)))] <- 1
  
  x <- xts(df$signal, Sys.Date()+1:length(df$signal))
  no_na_x <- na.locf(x)
  df$signal_quant <- as.numeric(no_na_x)
  na_index <- !is.na.data.frame(df$signal_quant)
  df <- df[na_index,]
  dates <- results$DATES[na_index]
  ## change here
  bond_return <- results$JP_SECOND_BOND_OPEN_RETURN[na_index]
  StratNextReturn <- df$signal_quant*df$NextReturns
  STRATEGY_TODAY <- CumFromRetToPricesStart(StratNextReturn)
  BOND <- CumFromRetToPricesStart(bond_return)
  
  uni_df <- data.frame(DATES= results$DATES[na_index],STRAT=STRATEGY_TODAY,BOND= BOND)
  
  toplot_df <-  melt(uni_df[,c("DATES", "STRAT","BOND")],"DATES")
  my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",sep="")
  g<-ggplot(
    toplot_df,aes(
      x = DATES,y = value,group = variable,color = variable
    )
  ) +
    geom_line() +
    scale_x_date() +
    ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
    theme(title = element_text(size = 12, face = 'bold')) +
    theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
    theme(legend.background = element_rect(fill = "gray90")) +
    theme(legend.key.size = unit(0.7, "cm"))
  print(g)
  print("Next quantile")
}


