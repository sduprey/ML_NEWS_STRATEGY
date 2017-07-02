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

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

backtesting_starting_date <- "2013-01-01"
backtesting_ending_date <- "2015-10-01"


# my_pairs = list (c("US","DE"),c("US","EU"),c("US","FR"),c("US","GB"),c("US","JP"))
my_pairs = list(c("US","DE"),c("US","FR"),c("US","GB"),c("US","JP"))

# my_pairs = list (c("US","DE"))

options(warn=-1)
my_pair_IR_results_df_name <- "all_pairs_month_2013"
my_pair_IR_results_df <- data.frame(second_leg=character(),
                                    horizon=double(),
                                    spread_amplification_factor=double(),
                                    depth=double(),
                                    algorithm=character(),
                                    europe_as_third_country=logical(),
                                    zscore=logical(),
                                    #######IR_spread_today=double(),
                                    #######IR_spread_tomorrow=double(),
                                    #######IR_spread_yesterday=double(),
                                    IR_spreadbidir_today=double(),
                                    IR_spreadbidir_tomorrow=double(),
                                    IR_spreadbidir_yesterday=double()
)
for (my_pair in my_pairs){
  # investment_horizon <- c(5,21,1)
  investment_horizon <- c(21)
  
  # my_depths <- c(3,4)
  my_depths <- c(3,4)
  my_algorithms <- c("rpart","rpart_unpruned")
  # my_algorithms <- c("rpart_unpruned")
  europe_handling <- c(TRUE,FALSE)
  #europe_handling <- c(FALSE)
  to_zscore <- c(TRUE,FALSE)
  to_zscore <- c(TRUE)
  # spread_amplification_factors <- c(0.05,0.1,0.5,1,5) 
  spread_amplification_factors <- c(0.5,1,0.05,0.1,5) 

 
  
  for (depth in my_depths){
    for (algorithm in my_algorithms){
      for (europe_as_third_country in europe_handling){
        for (zscore in to_zscore){
          for (my_horizon in investment_horizon) {
            for (spread_amplification_factor in spread_amplification_factors){
              #               # capture.output(
              #               all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, spread_amplification_factor,algorithm, europe_as_third_country, zscore, depth, my_horizon)
              #               # )
              #               
              #               SaveDataFrame(all_results$results,outputDataPath,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"spread_results_2013",sep=""))
              #               SaveDataFrame(all_results$sentiments,outputDataPath,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"spread_sentiments_2013",sep=""))
              
#               print("depth")
#               print(depth)
#               print("algorithm")
#               print(algorithm)
#               print("europe_as_third_country")
#               print(europe_as_third_country)
#               print("zscore")
#               print(zscore)
#               print("horizon")
#               print(my_horizon)
#               print("spread_amplification_factor")
#               print(spread_amplification_factor)
#               
              my_result_spread_name <- paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon, "spread_results_2013",sep="")
              my_result_spread_filename <- paste(my_result_spread_name, ".rds",sep="")
              
              results <- readRDS(paste(outputDataPath,my_result_spread_filename, sep = ""))
              
              
              
              IR_spreadbidir_today <- computeIR(results$STRATEGY_RETURN, my_horizon)
              IR_spreadbidir_tomorrow <- computeIRfromPrice(results$STRATEGY_TOMORROW, my_horizon)
              IR_spreadbidir_yesterday <- computeIRfromPrice(results$STRATEGY_YESTERDAY, my_horizon)
              #########IR_spread_today <- computeIR(results$SPREAD_STRATEGY_TODAY, my_horizon)
              #########IR_spread_tomorrow <-computeIRfromPrice(results$SPREAD_STRATEGY_TOMORROW, my_horizon)
              #########IR_spread_yesterday <-computeIRfromPrice(results$SPREAD_STRATEGY_YESTERDAY, my_horizon)
              
#               print("Information ratio for Ravenpack news trading strategy today s night return")
#               print(IR_spreadbidir_today)
              
              new_row_df <- data.frame(second_leg=my_pair[2],
                                       horizon=my_horizon,
                                       spread_amplification_factor=spread_amplification_factor,
                                       depth=depth,
                                       algorithm=algorithm,
                                       europe_as_third_country=europe_as_third_country,
                                       zscore=zscore,
                                       #########IR_spread_today=IR_spread_today,
                                       #########IR_spread_tomorrow=IR_spread_tomorrow,
                                       #########IR_spread_yesterday=IR_spread_yesterday,
                                       IR_spreadbidir_today=IR_spreadbidir_today,
                                       IR_spreadbidir_tomorrow=IR_spreadbidir_tomorrow,
                                       IR_spreadbidir_yesterday=IR_spreadbidir_yesterday
              )
              
              my_pair_IR_results_df <- rbind(my_pair_IR_results_df,new_row_df)
              #########colnames(my_pair_IR_results_df) <- c("horizon","spread_amplification_factor","depth", "algorithm", "europe_as_third_country", "zscore", "IR_spread_today", "IR_spread_tomorrow", "IR_spread_yesterday", "IR_spreadbidir_today", "IR_spreadbidir_tomorrow", "IR_spreadbidir_yesterday" )
              colnames(my_pair_IR_results_df) <- c("second_leg","horizon","spread_amplification_factor","depth", "algorithm", "europe_as_third_country", "zscore","IR_spreadbidir_today", "IR_spreadbidir_tomorrow", "IR_spreadbidir_yesterday" )
              
              
              
              
#               if ((( !is.na(IR_spreadbidir_today) && abs(IR_spreadbidir_today)> 0.7) && (!is.na(IR_spreadbidir_tomorrow) && abs(IR_spreadbidir_tomorrow) >0.66))|| (( !is.na(IR_spreadbidir_today) && abs(IR_spreadbidir_today)> 1) ) ){
#                 # fact checking
#                 STRATEGY_RETURN <- CumfromPricesToRet(results$STRATEGY_TODAY)
#                 sum(STRATEGY_RETURN - results$STRATEGY_RETURN)
#                 
#                 FIRST_BOND_RETURN <- CumfromPricesToRet(results$FIRST_BOND)
#                 sum(FIRST_BOND_RETURN - results$FIRST_BOND_RETURN)
#                 
#                 SECOND_BOND_RETURN <- CumfromPricesToRet(results$SECOND_BOND)
#                 sum(SECOND_BOND_RETURN - results$SECOND_BOND_RETURN)
#                 
#                 FIRST_BOND <- CumFromRetToPricesStart(FIRST_BOND_RETURN)
#                 sum(FIRST_BOND - results$FIRST_BOND)
#                 
#                 SECOND_BOND <- CumFromRetToPricesStart(SECOND_BOND_RETURN)
#                 sum(SECOND_BOND - results$SECOND_BOND)
#                 
#                 FIRST_WEIGHT <- results$FIRST_WEIGHT
#                 SECOND_WEIGHT <- results$SECOND_WEIGHT
#                 
#                 FIRST_BOND_NEXT_RETURN<-results$FIRST_BOND_NEXT_RETURN
#                 SECOND_BOND_NEXT_OPEN_RETURN<-results$SECOND_BOND_NEXT_OPEN_RETURN
#                 
#                 head(results$DATES,8)
#                 head(results$FIRST_BOND_NEXT_RETURN,8)
#                 head(FIRST_BOND_RETURN,8)
#                 
#                 head(results$DATES,5)
#                 head(results$SECOND_BOND_NEXT_OPEN_RETURN,5)
#                 head(results$SECOND_BOND_OPEN_RETURN,5)
#                 
#                 
#                 STRATEGY_RETURN <- FIRST_WEIGHT*FIRST_BOND_NEXT_RETURN+SECOND_WEIGHT*SECOND_BOND_NEXT_OPEN_RETURN
#                 sum(STRATEGY_RETURN - results$STRATEGY_RETURN)
#                 
#                 
#                 
#                 results$FIRST_LEG <- results$FIRST_WEIGHT*results$FIRST_BOND_NEXT_RETURN
#                 results$SECOND_LEG <- results$SECOND_WEIGHT*results$FIRST_BOND_NEXT_RETURN
#                 
#                 
#                 # some plotting to assess performance after fact checking
#                 
#                 #######toplot_df <-  melt(results[,c("DATES", "STRATEGY_TODAY","SPREAD_STRATEGY_YESTERDAY", "SPREAD_STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
#                 toplot_df <-  melt(results[,c("DATES", "STRATEGY_TODAY","FIRST_BOND","SECOND_BOND")],"DATES")
#                 
#                 my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
#                 g<-ggplot(
#                   toplot_df,aes(
#                     x = DATES,y = value,group = variable,color = variable
#                   )
#                 ) +
#                   geom_line() +
#                   scale_x_date() +
#                   ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
#                   theme(title = element_text(size = 12, face = 'bold')) +
#                   theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
#                   theme(legend.background = element_rect(fill = "gray90")) +
#                   theme(legend.key.size = unit(0.7, "cm"))
#                 print(g)
#                 
#                 ExportPlot(g,outputDataPath,my_result_spread_name)
#                 
#                 
#                 toplot_df <-  melt(results[,c("DATES","FIRST_WEIGHT","SECOND_WEIGHT")],"DATES")
#                 
#                 my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
#                 g<-ggplot(
#                   toplot_df,aes(
#                     x = DATES,y = value,group = variable,color = variable
#                   )
#                 ) +
#                   geom_line() +
#                   scale_x_date() +
#                   ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
#                   theme(title = element_text(size = 12, face = 'bold')) +
#                   theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
#                   theme(legend.background = element_rect(fill = "gray90")) +
#                   theme(legend.key.size = unit(0.7, "cm"))
#                 print(g)
#                 
#                 ExportPlot(g,outputDataPath,paste(my_result_spread_name,"_weights",sep=""))
#                 
#                 toplot_df <-  melt(results[,c("DATES","FIRST_LEG","SECOND_LEG")],"DATES")
#                 
#                 my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
#                 g<-ggplot(
#                   toplot_df,aes(
#                     x = DATES,y = value,group = variable,color = variable
#                   )
#                 ) +
#                   geom_line() +
#                   scale_x_date() +
#                   ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
#                   theme(title = element_text(size = 12, face = 'bold')) +
#                   theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
#                   theme(legend.background = element_rect(fill = "gray90")) +
#                   theme(legend.key.size = unit(0.7, "cm"))
#                 print(g)
#                 
#                 ExportPlot(g,outputDataPath,paste(my_result_spread_name,"_legs",sep=""))
#                 
#                 
#                 
#                 toplot_df <-  melt(results[,c("DATES","STRATEGY_TODAY", "STRATEGY_YESTERDAY","STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
#                 
#                 my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
#                 g<-ggplot(
#                   toplot_df,aes(
#                     x = DATES,y = value,group = variable,color = variable
#                   )
#                 ) +
#                   geom_line() +
#                   scale_x_date() +
#                   ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
#                   theme(title = element_text(size = 12, face = 'bold')) +
#                   theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
#                   theme(legend.background = element_rect(fill = "gray90")) +
#                   theme(legend.key.size = unit(0.7, "cm"))
#                 print(g)          
#                 ExportPlot(g,outputDataPath,paste(my_result_spread_name,"bis"))
#                 
#                 my_result_sentiment_spread_name <- paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon, "spread_sentiments_2013",sep="")
#                 my_result_sentiment_spread_filename <- paste(my_result_sentiment_spread_name, ".rds",sep="")
#                 
#                 sentiment_results <- readRDS(paste(outputDataPath,my_result_sentiment_spread_filename, sep = ""))
#                 
#                 
#                 ### Biggest weights inspection
#                 print(" First biggest weights inspection")
#                 max_first_weight <- which.max(results$FIRST_WEIGHT)
#                 biggest_first_weight <- results[max_first_weight,]
#                 biggest_first_weight_sentiment <- sentiment_results[max_first_weight,]
#                 results <- results[-max_first_weight,]
#                 sentiment_results <- sentiment_results[-max_first_weight,]
#                 
#                 print(biggest_first_weight)
#                 print(biggest_first_weight_sentiment[,colSums(biggest_first_weight_sentiment)!=0])
#                 
#                 max_second_weight <- which.max(results$SECOND_WEIGHT)
#                 biggest_second_weight <- results[max_second_weight,]
#                 biggest_second_weight_sentiment <- sentiment_results[max_second_weight,]
#                 results <- results[-max_second_weight,]
#                 sentiment_results <- sentiment_results[-max_second_weight,]
#                 
#                 
#                 print(biggest_second_weight)
#                 print(biggest_second_weight_sentiment[,colSums(biggest_second_weight_sentiment)!=0])
#                 
#                 print(" Second biggest weights inspection")
#                 max_first_weight <- which.max(results$FIRST_WEIGHT)
#                 biggest_first_weight <- results[max_first_weight,]
#                 biggest_first_weight_sentiment <- sentiment_results[max_first_weight,]
#                 results <- results[-max_first_weight,]
#                 sentiment_results <- sentiment_results[-max_first_weight,]
#                 
#                 print(biggest_first_weight)
#                 print(biggest_first_weight_sentiment[,colSums(biggest_first_weight_sentiment)!=0])
#                 
#                 max_second_weight <- which.max(results$SECOND_WEIGHT)
#                 biggest_second_weight <- results[max_second_weight,]
#                 biggest_second_weight_sentiment <- sentiment_results[max_second_weight,]
#                 results <- results[-max_second_weight,]
#                 sentiment_results <- sentiment_results[-max_second_weight,]
#                 
#                 
#                 print(biggest_second_weight)
#                 print(biggest_second_weight_sentiment)
#                 
#                 print(" Third biggest weights inspection")
#                 max_first_weight <- which.max(results$FIRST_WEIGHT)
#                 biggest_first_weight <- results[max_first_weight,]
#                 biggest_first_weight_sentiment <- sentiment_results[max_first_weight,]
#                 results <- results[-max_first_weight,]
#                 sentiment_results <- sentiment_results[-max_first_weight,]
#                 
#                 print(biggest_first_weight)
#                 print(biggest_first_weight_sentiment[,colSums(biggest_first_weight_sentiment)!=0])
#                 
#                 max_second_weight <- which.max(results$SECOND_WEIGHT)
#                 biggest_second_weight <- results[max_second_weight,]
#                 biggest_second_weight_sentiment <- sentiment_results[max_second_weight,]
#                 results <- results[-max_second_weight,]
#                 sentiment_results <- sentiment_results[-max_second_weight,]
#                 
#                 
#                 print(biggest_second_weight)
#                 print(biggest_second_weight_sentiment[,colSums(biggest_first_weight_sentiment)!=0])
#                 
#               }
            }
          }
        }
      }
    }
  }
}
SaveDataFrame(my_pair_IR_results_df,outputDataPath,my_pair_IR_results_df_name)

#### results

all_pair_results <- readRDS(paste(outputDataPath,"all_pairs_month_2013.rds", sep = ""))
colnames(us_de_pair_results)

for (my_pair in my_pairs){
  
  my_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
  for (nb in 1:3){
    max_today_IR <- which.max(abs(my_pair_results$IR_spreadbidir_today))
    biggest_today_IR <- my_pair_results[max_today_IR,]
    print("today")
    print(biggest_today_IR$IR_spreadbidir_today)
    print("tomorrow")
    print(biggest_today_IR$IR_spreadbidir_tomorrow)
    print("All")
    print(biggest_today_IR)
    my_pair_results <- my_pair_results[-max_today_IR,]
  }
}


all_pair_results <- readRDS(paste(outputDataPath,"all_pairs_month_2013.rds", sep = ""))
colnames(us_de_pair_results)

for (my_pair in my_pairs){
  my_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
  for (nb in 1:3){
    max_tomorrow_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_tomorrow))
    biggest_tomorrow_IR <- month_us_de_pair_results[max_tomorrow_IR,]
    print("today")
    print(biggest_today_IR$IR_spreadbidir_today)
    print("tomorrow")
    print(biggest_today_IR$IR_spreadbidir_tomorrow)
    print("All")
    print(biggest_today_IR)
    month_us_de_pair_results <- month_us_de_pair_results[-max_tomorrow_IR,]
  }
}

# 
# 
# for (nb in 1:3){
#   max_today_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_today))
#   biggest_today_IR <- month_us_de_pair_results[max_today_IR,]
#   print("today")
#   print(biggest_today_IR$IR_spreadbidir_today)
#   print("tomorrow")
#   print(biggest_today_IR$IR_spreadbidir_tomorrow)
#   print("All")
#   print(biggest_today_IR)
#   month_us_de_pair_results <- month_us_de_pair_results[-max_today_IR,]
# }
# # ================> threshold of one
# 
# 
# my_pair = c("US","DE")
# my_pair_IR_results_df_name <- paste(my_pair[1], my_pair[2],"_2013", sep="")
# us_de_pair_results <- readRDS(paste(outputDataPath,paste(my_pair_IR_results_df_name,".rds",sep=""), sep = ""))
# colnames(us_de_pair_results)
# print(" First biggest today monthly IRs")
# month_us_de_pair_results <- us_de_pair_results[us_de_pair_results$horizon==21,]
# 
# 
# for (nb in 1:3){
#   max_tomorrow_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_tomorrow))
#   biggest_tomorrow_IR <- month_us_de_pair_results[max_tomorrow_IR,]
#   print("today")
#   print(biggest_today_IR$IR_spreadbidir_today)
#   print("tomorrow")
#   print(biggest_today_IR$IR_spreadbidir_tomorrow)
#   print("All")
#   print(biggest_today_IR)
#   month_us_de_pair_results <- month_us_de_pair_results[-max_tomorrow_IR,]
# }
# 
# # ================> threshold of one point two
