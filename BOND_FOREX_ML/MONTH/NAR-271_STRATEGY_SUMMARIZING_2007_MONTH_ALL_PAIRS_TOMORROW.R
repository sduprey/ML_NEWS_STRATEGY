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
outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")
outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007/",sep="")


results <- readRDS(paste(outputDataPathStrategyMonth,"all_pairs_results_month_2007.rds", sep = ""))

# results$STRATEGY_RETURN <- (results$JP_STRATEGY_RETURN+results$GB_STRATEGY_RETURN+results$DE_STRATEGY_RETURN)/3
# results$STRATEGY_TODAY <- CumFromRetToPricesStart(results$STRATEGY_RETURN)
# 
# results$US_WEIGHT <- (results$DE_FIRST_WEIGHT + results$GB_FIRST_WEIGHT + results$JP_FIRST_WEIGHT)/3
# results$DE_WEIGHT <- results$DE_SECOND_WEIGHT/3
# results$JP_WEIGHT <- results$JP_SECOND_WEIGHT/3
# results$GB_WEIGHT <- results$GB_SECOND_WEIGHT/3

### Deutschland visualization
my_result_spread_name <- "US_DE"
my_pair <- c("US","DE")
my_pair[1]<-"US"
my_pair[2]<-"DE"

toplot_df <-results[,c("DATES", "DE_STRATEGY_TOMORROW","DE_FIRST_BOND","DE_SECOND_BOND")]
colnames(toplot_df) <-  c("DATES", "DE_STRATEGY","DE_FIRST_BOND","DE_SECOND_BOND")
toplot_df <-  melt(toplot_df,"DATES")

my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
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

ExportPlot(g,outputDataPathStrategyMonth,my_result_spread_name)

### Deutschland visualization
my_result_spread_name <- "US_GB"
my_pair <- c("US","GB")
my_pair[1]<-"US"
my_pair[2]<-"GB"
toplot_df <- results[,c("DATES", "GB_STRATEGY_TOMORROW","GB_FIRST_BOND","GB_SECOND_BOND")]
colnames(toplot_df) <-  c("DATES", "GB_STRATEGY","GB_FIRST_BOND","GB_SECOND_BOND")
toplot_df <-  melt(toplot_df,"DATES")

my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
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

ExportPlot(g,outputDataPathStrategyMonth,my_result_spread_name)


my_result_spread_name <- "US_JP"
my_pair <- c("US","JP")
my_pair[1]<-"US"
my_pair[2]<-"JP"
toplot_df <- results[,c("DATES", "JP_STRATEGY_TOMORROW","JP_FIRST_BOND","JP_SECOND_BOND")]
colnames(toplot_df) <-  c("DATES", "JP_STRATEGY","JP_FIRST_BOND","JP_SECOND_BOND")
toplot_df <-  melt(toplot_df,"DATES")

my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
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

ExportPlot(g,outputDataPathStrategyMonth,my_result_spread_name)
