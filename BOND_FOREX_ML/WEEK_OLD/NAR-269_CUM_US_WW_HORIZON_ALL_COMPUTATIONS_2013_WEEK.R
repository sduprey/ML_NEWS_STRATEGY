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

source("./RCode/RP_Plotting_Utils.R")
source("./RCode/RP_Macro_Monthly_Utils.R")
source("./RCode/RP_Spread_Utils.R")
source("./RCode/RP_Dates_Utils.R")
source("./RCode/RP_Df_Utils.R")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

outputDataPathWeek <- paste(outputDataPath,"Week_2013/",sep="")
backtesting_starting_date <- "2013-01-01"
backtesting_ending_date <- "2015-10-01"

# my_pairs = list (c("US","FR"),c("US","GB"),c("US","JP"))
my_pairs = list(c("US","DE"),c("US","FR"),c("US","GB"),c("US","JP"))

# my_pairs = list (c("US","DE"))

options(warn=-1)

for (my_pair in my_pairs){
  # investment_horizon <- c(5,21,1)
  investment_horizon <- c(5)
  
  # my_depths <- c(3,4)
  my_depths <- c(3,4)
  # my_algorithms <- c("rpart","rpart_unpruned")
  # my_algorithms <- c("xgboost","rpart_unpruned","randomForest","rpart")
  my_algorithms <- c("xgboost","rpart_unpruned","svm","rpart")
  
  # my_algorithms <- c("rpart_unpruned")
  europe_handling <- c(TRUE,FALSE)
  #europe_handling <- c(FALSE)
  to_zscore <- c(TRUE,FALSE)
  # to_zscore <- c(TRUE)
  # spread_amplification_factors <- c(0.05,0.1,0.5,1,5) 
  spread_amplification_factors <- c(0.5,1,0.05,0.1,5) 
  
  for (depth in my_depths){
    for (algorithm in my_algorithms){
      for (europe_as_third_country in europe_handling){
        for (zscore in to_zscore){
          for (my_horizon in investment_horizon) {
            for (spread_amplification_factor in spread_amplification_factors){
              # capture.output(
              all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, spread_amplification_factor,algorithm, europe_as_third_country, zscore, depth, my_horizon)
              # )
              
              SaveDataFrame(all_results$results,outputDataPathWeek,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"spread_results_week_2013",sep=""))
              SaveDataFrame(all_results$sentiments,outputDataPathWeek,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"spread_sentiments_week_2013",sep=""))
              
              results <- all_results$results
              
              #######toplot_df <-  melt(results[,c("DATES","SPREAD_STRATEGY_TODAY", "SPREAD_STRATEGY_YESTERDAY", "SPREAD_STRATEGY_TOMORROW", "STRATEGY_TODAY", "STRATEGY_YESTERDAY","STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
              toplot_df <-  melt(results[,c("DATES","STRATEGY_TODAY", "STRATEGY_YESTERDAY","STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
              
              my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
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
              
              
              IR <- computeIR(results$STRATEGY_RETURN, my_horizon)
              print("Information ratio for Ravenpack news trading strategy today s night return")
              print(IR)
              
            }
          }
        }
      }
    }
  }
}
