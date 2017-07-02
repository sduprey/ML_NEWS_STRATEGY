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
library(ROCR)
# require(rpart.plot)
# require(rattle)
# install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
# require(caret)
require(xgboost)
library("RPBackTesting", lib.loc="C:/Program Files/R/R-3.2.2/library")


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
outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")
# outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007/01_11_2015/",sep="")
outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007/",sep="")

backtesting_starting_date <- "2007-01-01"
backtesting_ending_date <- "2015-10-01"


# my_pairs = list (c("US","DE"),c("US","EU"),c("US","FR"),c("US","GB"),c("US","JP"))
my_pairs = list(c("US","DE"),c("US","GB"),c("US","JP"))

#### results

all_pair_results <- readRDS(paste(outputDataPathMonth,"all_pairs_month_2007.rds", sep = ""))

USPrediction <- NULL
USNextReturn <- NULL

GBPrediction <- NULL
GBNextReturn <- NULL

DEPrediction <- NULL
DENextReturn <- NULL

JPPrediction <- NULL
JPNextReturn <- NULL

for (my_pair in my_pairs){
  
  month_us_de_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
  my_result_spread_name_root <- paste(my_pair[2],"month_2007_all_pairs",sep="")
  nb_deep_solution <- 1
  if (my_pair[2] == "DE" ){
    nb_deep_solution <- 2
  }
  for (nb in 1:nb_deep_solution){
    my_result_spread_name <- paste(my_result_spread_name_root, nb,sep="")
    # max_today_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_today))
    max_today_IR <- which.max(month_us_de_pair_results$IR_spreadbidir_today)
    
    biggest_today_IR <- month_us_de_pair_results[max_today_IR,]
    print("today")
    print(biggest_today_IR$IR_spreadbidir_today)
    print("tomorrow")
    print(biggest_today_IR$IR_spreadbidir_tomorrow)
    print("All")
    print(biggest_today_IR)
    
    month_us_de_pair_results <- month_us_de_pair_results[-max_today_IR,]
    
    if (nb == nb_deep_solution){
      # we do not redo the computation
      #     capture.output(
      # all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon, biggest_today_IR$rolling_window, biggest_today_IR$recalibration_frequency)
      #     )
      # we load the result file
      # SaveDataFrame(all_results$results,outputDataPathMonth,paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency,"unipred_spread_results_month_2007",sep=""))
      #     
      #         # no more of that : it now comes from the same way as the standard results
      #         my_result_prediction_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency, "unipred_spread_results_month_2007",sep="")
      #         my_result_prediction_spread_filename <- paste(my_result_prediction_spread_name, ".rds",sep="")
      #         results_prediction <- tryCatch( readRDS(paste(outputDataPathMonth,my_result_prediction_spread_filename, sep = "")),
      #                                         error = function(e) {NULL})
      # no more of that : it now comes from the same way as the standard results
      my_result_prediction_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency, "spread_results_month_2007",sep="")
      my_result_prediction_spread_filename <- paste(my_result_prediction_spread_name, ".rds",sep="")
      all_results <- tryCatch( readRDS(paste(outputDataPathMonth,my_result_prediction_spread_filename, sep = "")),
                               error = function(e) {NULL})
      
      results_prediction <- all_results
      
      
      if (!is.null(results_prediction)){
        if (my_pair[2] == "GB" ){
          USGBPrediction <- results_prediction[,"FIRST_PREDICTION"]
          USGBNextReturn <- results_prediction[,"FIRST_BOND_NEXT_RETURN"]
          corUSGB <- cor(USGBPrediction, USGBNextReturn)
          print("US GB correlation")
          print(corUSGB)
          GBPrediction <- results_prediction[,"SECOND_PREDICTION"]
          GBNextReturn <- results_prediction[,"SECOND_BOND_NEXT_OPEN_RETURN"]
        }
        
        if (my_pair[2] == "DE" ){
          USDEPrediction <- results_prediction[,"FIRST_PREDICTION"]
          USDENextReturn <- results_prediction[,"FIRST_BOND_NEXT_RETURN"]
          corUSDE <- cor(USDEPrediction, USDENextReturn)
          print("US DE correlation")
          print(corUSDE)
          
          DEPrediction <- results_prediction[,"SECOND_PREDICTION"]
          DENextReturn <- results_prediction[,"SECOND_BOND_NEXT_OPEN_RETURN"]
        }
        
        if (my_pair[2] == "JP" ){
          USJPPrediction <- results_prediction[,"FIRST_PREDICTION"]
          USJPNextReturn <- results_prediction[,"FIRST_BOND_NEXT_RETURN"]
          corUSJP <- cor(USJPPrediction, USJPNextReturn)
          print("US JP correlation")
          print(corUSJP)
          JPPrediction <- results_prediction[,"SECOND_PREDICTION"]
          JPNextReturn <- results_prediction[,"SECOND_BOND_NEXT_OPEN_RETURN"]
        }
      }
    }
  }
}
aggregated_df <- data.frame(USPrediction=USGBPrediction,USEffective=USGBNextReturn,
                            #                             USDEPrediction=USDEPrediction,USDEEffective=USDENextReturn,
                            #                             USGBPrediction=USGBPrediction,USGBEffective=USGBNextReturn,
                            #                             USJPPrediction=USJPPrediction,USJPEffective=USJPNextReturn,
                            GBPrediction=GBPrediction,GBEffective=GBNextReturn,DEPrediction=DEPrediction,DEEffective=DENextReturn,JPPrediction=JPPrediction,JPEffective=JPNextReturn)
# aggregated_df <- data.frame(USPrediction=USPrediction,USEffective=USNextReturn,GBPrediction=GBPrediction,GBEffective=GBNextReturn,DEPrediction=DEPrediction,DEEffective=DENextReturn)
SaveDataFrame(aggregated_df,outputDataPathStrategyMonth,"all_pairs_prediction_results_month_2007")
# 
# for (my_pair in my_pairs){
#   
#   month_us_de_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
#   my_result_spread_name_root <- paste(my_pair[2],"month_2007_all_pairs",sep="")
#   for (nb in 1:1){
#     my_result_spread_name <- paste(my_result_spread_name_root, nb,sep="")
#     # max_today_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_today))
#     max_today_IR <- which.max(month_us_de_pair_results$IR_spreadbidir_today)
#     
#     biggest_today_IR <- month_us_de_pair_results[max_today_IR,]
#     print("today")
#     print(biggest_today_IR$IR_spreadbidir_today)
#     print("tomorrow")
#     print(biggest_today_IR$IR_spreadbidir_tomorrow)
#     print("All")
#     print(biggest_today_IR)
#     
#     month_us_de_pair_results <- month_us_de_pair_results[-max_today_IR,]
#     
#     # we do not redo the computation
#     #     capture.output(
#     # all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon, biggest_today_IR$rolling_window, biggest_today_IR$recalibration_frequency)
#     #     )
#     # we load the result file
#     # SaveDataFrame(all_results$results,outputDataPathMonth,paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency,"unipred_spread_results_month_2007",sep=""))
#     
#     my_result_prediction_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency, "unipred_spread_results_month_2007",sep="")
#     my_result_prediction_spread_filename <- paste(my_result_prediction_spread_name, ".rds",sep="")
#     
#     results_prediction <- tryCatch( readRDS(paste(outputDataPathMonth,my_result_prediction_spread_filename, sep = "")),
#                                     error = function(e) {NULL})
#     
#     
#     USprediction <- results_prediction[,"FIRST_PREDICTION"]
#     USNextReturn <- results_prediction[,"FIRST_BOND_NEXT_RETURN"]
#     # correlation between prediction and returns for US
#     print("Correlation")
#     print(cor(USprediction,USNextReturn))
#     USpredictionbullish <- USprediction >= 0 
#     USNextReturnbullish <- USNextReturn >= 0 
#     
#     library(caret)
#     print(confusionMatrix(USpredictionbullish, USNextReturnbullish))
#     
#     
#     #     pred_discrete <- prediction(as.numeric(USpredictionbullish),USNextReturnbullish)
#     #     
#     #     perf <- performance(pred_discrete,"tpr","fpr")
#     #     plot(perf)
#     #     ## precision/recall curve (x-axis: recall, y-axis: precision)
#     #     perf1 <- performance(pred_discrete, "prec", "rec")
#     #     plot(perf1)
#     #     ## sensitivity/specificity curve (x-axis: specificity,
#     #     ## y-axis: sensitivity)
#     #     perf1 <- performance(pred_discrete, "sens", "spec")
#     #     plot(perf1)
#     #     
#     #     
#     #     pred_continuous <- prediction(USprediction,USNextReturnbullish)
#     
#     bullishGoodPrediction <- sum(USpredictionbullish * USNextReturnbullish)/sum(USNextReturnbullish)
#     print("Good long predictions")
#     print(bullishGoodPrediction)
#     USpredictionbearish <- USprediction <= 0 
#     USNextReturnbearish <- USNextReturn <= 0 
#     
#     library(caret)
#     print(confusionMatrix(USpredictionbearish, USNextReturnbearish))
#     
#     bearishGoodPrediction <- sum(USpredictionbearish * USNextReturnbearish)/sum(USpredictionbearish)
#     print("Good short predictions")
#     print(bearishGoodPrediction)
#     
#     
#     SBprediction <- results_prediction[,"SECOND_PREDICTION"]
#     SBNextReturn <- results_prediction[,"SECOND_BOND_NEXT_OPEN_RETURN"]
#     SBpredictionbullish <- SBprediction >= 0 
#     SBNextReturnbullish <- SBNextReturn >= 0 
#     library(caret)
#     print(confusionMatrix(SBpredictionbullish, SBNextReturnbullish))
#     
#     bullishGoodPrediction <- sum(SBpredictionbullish * SBNextReturnbullish)/sum(SBNextReturnbullish)
#     print("Good long predictions")
#     print(bullishGoodPrediction)
#     SBpredictionbearish <- SBprediction <= 0 
#     SBNextReturnbearish <- SBNextReturn <= 0 
#     library(caret)
#     print(confusionMatrix(SBpredictionbearish, SBNextReturnbearish))
#     bearishGoodPrediction <- sum(SBpredictionbearish * SBNextReturnbearish)/sum(SBNextReturnbearish)
#     print("Good short predictions")
#     print(bearishGoodPrediction)
#     
#     # correlation between prediction and returns for other bond
#     print("Correlation")
#     print(cor(SBprediction,SBNextReturn))
#     
#   }
# }
