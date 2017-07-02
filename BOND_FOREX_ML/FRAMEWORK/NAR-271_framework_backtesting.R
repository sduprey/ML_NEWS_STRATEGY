### Launching all pair spread computations
### Trying to forecast the spread between ?/? bonds futures
# library("SIT")
# library("RPQuantUtils")
# library("RPToolsDB")
require(ggplot2)
require("ppcor")
require(graphics)
require("TTR")
require(plyr)
# server fails if we not require this package loading
require(labeling)
# require(reshape)
require(reshape2)
require(RColorBrewer)
require(stats)
require(Rsolnp)
require(zoo)
require(xts)
require(vars)
# require(Quandl)
## for the modeling
require(rpart)
require(randomForest)
require(xgboost)
## require caret (for dummy variables)
# require(caret)
## require Metrics to compute error
require(Metrics)


source("./RCode/RP_Plotting_Utils.R")
# source("./RCode/RP_Macro_Monthly_Utils.R")
# source("./RCode/RP_Spread_Utils.R")
source("./RCode/RP_Dates_Utils.R")
source("./RCode/RP_Df_Utils.R")


user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
# repoPath = RP_GetSharedPath(user)
# # Input Data Path
# inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# # Output Data Path
# outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")
# 
# outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")


inputDataPath  <- "C://My_RP_Data/"



my_total_predictors_df <- readRDS(paste(inputDataPath,"my_total_predictors_df.rds", sep = ""))
my_total_outputs_df <- readRDS(paste(inputDataPath,"my_total_outputs_df.rds", sep = ""))

print(dim(my_total_predictors_df))
print(dim(my_total_outputs_df))
head(my_total_outputs_df$DATE)
tail(my_total_outputs_df$DATE)


backtesting_starting_date <- "2002-01-01"
backtesting_ending_date <- "2015-10-01"



# 
# 
# my_total_predictors_df <- my_total_predictors_df[my_total_predictors_df$DATE >= backtesting_starting_date & my_total_predictors_df$DATE <= backtesting_ending_date,]
# my_total_outputs_df <- my_total_outputs_df[my_total_outputs_df$DATE >= backtesting_starting_date & my_total_outputs_df$DATE <= backtesting_ending_date,]
# 
# 
# 
# 
# 
# sum(colSums(my_total_predictors_df[,-1]) == 0)
# 
# 
# # my_pairs = list (c("US","DE"),c("US","JP"),c("US","GB"))
# my_pairs = list (c("US","DE"),c("US","JP"),c("US","GB"))
# 
# options(warn=-1)
# 
# for (my_pair in my_pairs){
#   # investment_horizon <- c(5,21,1)
#   investment_horizon <- c(21)
#   
#   # my_depths <- c(3,4)
#   # my_depths <- c(4)
#   my_depths <- c(4)
#   # my_algorithms <- c("xgboost_cv","xgboost_caret_cv","rpart_cv","rpart_pruned","xgboost","rpart","random_forest","svm")
#   # my_algorithms <- c("xgboost_cv")
#   my_algorithms <- c("xgboost_cv")
#   #   
#   # europe_handling <- c(TRUE,FALSE)
#   # europe_handling <- c(TRUE)
#   # we use it for economy only or not
#   europe_handling <- c(FALSE)
#   
#   # to_zscore <- c(TRUE,FALSE)
#   # we use zscore to add or not RP_entity_id to our predictor
#   to_zscore <- c(TRUE)
#   # spread_amplification_factors <- c(0.05,0.1,0.5,1,5) 
#   spread_amplification_factors <- c(0.5,1,0.05,0.1,1.5) 
#   # backtesting rolling window in year !!!
#   # rolling_windows <- c(1,2,3,5,10,-1)
#   rolling_windows <- c(1,2,3,5,10)
#   # Here we recalibrate each 6 invesment horizon period (here months)
#   # recalibration_frequency <- c(1,3,6,9)
#   # recalibration_frequencies <- 12*rolling_windows
#   # we calibrate our model just once
#   # then we only retrain it
#   # recalibration_frequencies <- 12*rolling_windows
#   recalibration_frequencies <- 12*rev(rolling_windows)
#   for(recalibration_frequency in recalibration_frequencies){
#     for (my_rolling_window in rolling_windows){
#       for (depth in my_depths){
#         for (algorithm in my_algorithms){
#           for (europe_as_third_country in europe_handling){
#             for (zscore in to_zscore){
#               for (my_horizon in investment_horizon) {
#                 for (spread_amplification_factor in spread_amplification_factors){
#                   
#                   # capture.output(
#                   all_results <- compute_spread_strategy_west_first_horizon_withRPID(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, spread_amplification_factor,algorithm, europe_as_third_country, zscore, depth, my_horizon, my_rolling_window, recalibration_frequency)
#                   # )
#                   
#                   SaveDataFrame(all_results$results,outputDataPathMonth,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"roll_win",my_rolling_window,"rec_freq",recalibration_frequency,"spread_results_month_2007",sep=""))
#                   # SaveDataFrame(all_results$sentiments,outputDataPathMonth,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"spread_sentiments_month_2007",sep=""))
#                   SaveDataFrame(all_results$first_leg,outputDataPathMonth,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"roll_win",my_rolling_window,"rec_freq",recalibration_frequency,"spread_first_leg_month_2007",sep=""))
#                   SaveDataFrame(all_results$second_leg,outputDataPathMonth,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"roll_win",my_rolling_window,"rec_freq",recalibration_frequency,"spread_second_leg_month_2007",sep=""))
#                   SaveDataFrame(all_results$features_names,outputDataPathMonth,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"roll_win",my_rolling_window,"rec_freq",recalibration_frequency,"spread_features_month_2007",sep=""))
#                   
#                   SaveDataFrame(all_results$calibration_parameters,outputDataPathMonth,paste(my_pair[1], my_pair[2], algorithm,"EU",europe_as_third_country,"ZS",zscore,"depth",depth,"amp",spread_amplification_factor,"inv_hor",my_horizon,"roll_win",my_rolling_window,"rec_freq",recalibration_frequency,"spread_calibration_month_2007",sep=""))
#                   
#                   
#                   #               importance_matrix <- xgb.importance(all_results$predicting_columns, model = all_results$first_leg)
#                   #               xgb.plot.importance(importance_matrix)
#                   
#                   #######toplot_df <-  melt(results[,c("DATES","SPREAD_STRATEGY_TODAY", "SPREAD_STRATEGY_YESTERDAY", "SPREAD_STRATEGY_TOMORROW", "STRATEGY_TODAY", "STRATEGY_YESTERDAY","STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
#                   results <- all_results$results
#                   toplot_df <-  melt(results[,c("DATES","STRATEGY_TODAY", "STRATEGY_YESTERDAY","STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
#                   
#                   my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
#                   g<-ggplot(
#                     toplot_df,aes(
#                       x = DATES,y = value,group = variable,color = variable
#                     )
#                   ) +
#                     geom_line() +
#                     scale_x_date() +
#                     ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
#                     theme(title = element_text(size = 12, face = 'bold')) +
#                     theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
#                     theme(legend.background = element_rect(fill = "gray90")) +
#                     theme(legend.key.size = unit(0.7, "cm"))
#                   print(g)
#                   
#                   
#                   IR <- computeIR(results$STRATEGY_RETURN, my_horizon)
#                   print("Information ratio for Ravenpack news trading strategy today s night return")
#                   print(IR)
#                   
#                   gc()
#                 }
#               }
#             }
#           }
#         }
#       }
#     }
#   }
# }
