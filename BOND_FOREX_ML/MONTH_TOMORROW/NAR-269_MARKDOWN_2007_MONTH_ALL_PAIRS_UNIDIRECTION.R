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
outputDataPathMonth <- paste(outputDataPath,"Month_2007/01_11_2015/",sep="")

backtesting_starting_date <- "2007-01-01"
backtesting_ending_date <- "2015-10-01"


# my_pairs = list (c("US","DE"),c("US","EU"),c("US","FR"),c("US","GB"),c("US","JP"))
my_pairs = list(c("US","DE"),c("US","GB"),c("US","JP"))

#### results

all_pair_results <- readRDS(paste(outputDataPathMonth,"all_pairs_month_2007.rds", sep = ""))
colnames(all_pair_results)

for (my_pair in my_pairs){
  my_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
  for (nb in 1:3){
    max_today_IR <- which.max(abs(my_pair_results$IR_spreadbidir_today))
    biggest_today_IR <- my_pair_results[max_today_IR,]
    print("Second leg")
    print(my_pair[2])
    print("today")
    print(biggest_today_IR$IR_spreadbidir_today)
    print("tomorrow")
    print(biggest_today_IR$IR_spreadbidir_tomorrow)
    print("All")
    print(biggest_today_IR)
    my_pair_results <- my_pair_results[-max_today_IR,]
  }
}

# for (my_pair in my_pairs){
#   
#   my_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
#   for (nb in 1:3){
#     max_today_IR <- which.max(abs(my_pair_results$IR_spreadbidir_today))
#     biggest_today_IR <- my_pair_results[max_today_IR,]
#     print("Second leg")
#     print(my_pair[2])
#     print("today")
#     print(biggest_today_IR$IR_spreadbidir_today)
#     print("tomorrow")
#     print(biggest_today_IR$IR_spreadbidir_tomorrow)
#     print("All")
#     print(biggest_today_IR)
#     my_pair_results <- my_pair_results[-max_today_IR,]
#   }
#   my_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
#   for (nb in 1:3){
#     max_tomorrow_IR <- which.max(abs(my_pair_results$IR_spreadbidir_tomorrow))
#     biggest_tomorrow_IR <- my_pair_results[max_tomorrow_IR,]
#     print("Second leg")
#     print(my_pair[2])
#     print("today")
#     print(biggest_today_IR$IR_spreadbidir_today)
#     print("tomorrow")
#     print(biggest_today_IR$IR_spreadbidir_tomorrow)
#     print("All")
#     print(biggest_today_IR)
#     my_pair_results <- my_pair_results[-max_tomorrow_IR,]
#   }
# }





for (my_pair in my_pairs){
  
  month_us_de_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
  my_result_spread_name_root <- paste(my_pair[2],"month_2007_all_pairs",sep="")
  nb_deep_solution <- 1
  if (my_pair[2] == "DE" ){
    nb_deep_solution <- 2
  } else {
    nb_deep_solution <- 1
  }
  for (nb in nb_deep_solution:nb_deep_solution){
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
      all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon, biggest_today_IR$rolling_window, biggest_today_IR$recalibration_frequency)
      #     )
      # we load the result file
      SaveDataFrame(all_results$results,outputDataPathMonth,paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency,"unipred_spread_results_month_2007",sep=""))
      
      
      USprediction <- all_results$results[,"FIRST_PREDICTION"]
      USNextReturn <- all_results$results[,"FIRST_BOND_NEXT_RETURN"]
      # correlation between prediction and returns for US
      cor(USprediction,USNextReturn)
      USpredictionbullish <- USprediction >= 0 
      USNextReturnbullish <- USNextReturn >= 0 
      bullishGoodPrediction <- sum(USpredictionbullish * USNextReturnbullish)/sum(USNextReturnbullish)
      print(bullishGoodPrediction)
      USpredictionbearish <- USprediction <= 0 
      USNextReturnbearish <- USNextReturn <= 0 
      bearishGoodPrediction <- sum(USpredictionbearish * USNextReturnbearish)/sum(USNextReturnbullish)
      print(bearishGoodPrediction)
      
      
      SBprediction <- all_results$results[,"SECOND_PREDICTION"]
      SBNextReturn <- all_results$results[,"SECOND_BOND_NEXT_OPEN_RETURN"]
      SBpredictionbullish <- SBprediction >= 0 
      SBNextReturnbullish <- SBNextReturn >= 0 
      bullishGoodPrediction <- sum(SBpredictionbullish * SBNextReturnbullish)/sum(SBNextReturnbullish)
      print(bullishGoodPrediction)
      SBpredictionbearish <- USprediction <= 0 
      SBNextReturnbearish <- USNextReturn <= 0 
      bearishGoodPrediction <- sum(SBpredictionbearish * SBNextReturnbearish)/sum(SBNextReturnbullish)
      print(bearishGoodPrediction)
      
      
      # correlation between prediction and returns for other bond
      cor(SBprediction,SBNextReturn)
      
      results <-   all_results$results  
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
      
      results$FIRST_LEG <- results$FIRST_WEIGHT*results$FIRST_BOND_NEXT_RETURN 
      results$SECOND_LEG <- results$SECOND_WEIGHT*results$SECOND_BOND_NEXT_OPEN_RETURN
      
      STRATEGY_RETURN <- FIRST_WEIGHT*FIRST_BOND_NEXT_RETURN+SECOND_WEIGHT*SECOND_BOND_NEXT_OPEN_RETURN
      sum(STRATEGY_RETURN - results$STRATEGY_RETURN)
      
      # Decomposing our strategy over two legs
      sum(results$FIRST_LEG - results$FIRST_WEIGHT*results$FIRST_BOND_NEXT_RETURN)
      sum(results$SECOND_LEG - results$SECOND_WEIGHT*results$SECOND_BOND_NEXT_OPEN_RETURN)
      #### Outputing some turn over ratios results
      print("Outputing statistical results for our strategy")
      WeightMatrix <- results[,c("DATES","FIRST_WEIGHT","SECOND_WEIGHT")]
      colnames(WeightMatrix) <- c("DATE","FIRST_WEIGHT","SECOND_WEIGHT")
      # ReturnSerie <- results[,c("DATES","STRATEGY_RETURN")]
      # colnames(ReturnSerie) <- c("Date","STRATEGY_RETURN")
      ReturnSerie <- results[,c("STRATEGY_RETURN")]
      turnover <- RP_GetTurnOver(WeightMatrix,-1)
      RP_ReturnStats(ReturnSerie,21,TRUE,WeightMatrix,0.05,F)
      
      
      
      #######toplot_df <-  melt(results[,c("DATES", "STRATEGY_TODAY","SPREAD_STRATEGY_YESTERDAY", "SPREAD_STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
      toplot_df <-  melt(results[,c("DATES", "STRATEGY_TODAY","FIRST_BOND","SECOND_BOND")],"DATES")
      
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
      
      ExportPlot(g,outputDataPathMonth,my_result_spread_name)
      
      
      toplot_df <-  melt(results[,c("DATES","FIRST_WEIGHT","SECOND_WEIGHT")],"DATES")
      
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
      
      ExportPlot(g,outputDataPathMonth,paste(my_result_spread_name,"_weights",sep=""))
      
      toplot_df <-  melt(results[,c("DATES","FIRST_LEG","SECOND_LEG")],"DATES")
      
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
      
      ExportPlot(g,outputDataPathMonth,paste(my_result_spread_name,"_legs",sep=""))
      
      
      
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
      ExportPlot(g,outputDataPathMonth,paste(my_result_spread_name,"bis"))
      
      
      
      #     ### Biggest weights inspection
      #     print(" First biggest weights inspection")
      #     max_first_weight <- which.max(results$FIRST_WEIGHT)
      #     biggest_first_weight <- results[max_first_weight,]
      #     biggest_first_weight_sentiment <- sentiment_results[max_first_weight,]
      #     results <- results[-max_first_weight,]
      #     sentiment_results <- sentiment_results[-max_first_weight,]
      #     
      #     print(biggest_first_weight)
      #     print(biggest_first_weight_sentiment[,colSums(biggest_first_weight_sentiment)!=0])
      #     
      #     max_second_weight <- which.max(results$SECOND_WEIGHT)
      #     biggest_second_weight <- results[max_second_weight,]
      #     biggest_second_weight_sentiment <- sentiment_results[max_second_weight,]
      #     results <- results[-max_second_weight,]
      #     sentiment_results <- sentiment_results[-max_second_weight,]
      #     
      #     
      #     print(biggest_second_weight)
      #     print(biggest_second_weight_sentiment[,colSums(biggest_second_weight_sentiment)!=0])
      #     
      #     print(" Second biggest weights inspection")
      #     max_first_weight <- which.max(results$FIRST_WEIGHT)
      #     biggest_first_weight <- results[max_first_weight,]
      #     biggest_first_weight_sentiment <- sentiment_results[max_first_weight,]
      #     results <- results[-max_first_weight,]
      #     sentiment_results <- sentiment_results[-max_first_weight,]
      #     
      #     print(biggest_first_weight)
      #     print(biggest_first_weight_sentiment[,colSums(biggest_first_weight_sentiment)!=0])
      #     
      #     max_second_weight <- which.max(results$SECOND_WEIGHT)
      #     biggest_second_weight <- results[max_second_weight,]
      #     biggest_second_weight_sentiment <- sentiment_results[max_second_weight,]
      #     results <- results[-max_second_weight,]
      #     sentiment_results <- sentiment_results[-max_second_weight,]
      #     
      #     
      #     print(biggest_second_weight)
      #     print(biggest_second_weight_sentiment[,colSums(biggest_second_weight_sentiment)!=0])
      #     
      #     print(" Third biggest weights inspection")
      #     max_first_weight <- which.max(results$FIRST_WEIGHT)
      #     biggest_first_weight <- results[max_first_weight,]
      #     biggest_first_weight_sentiment <- sentiment_results[max_first_weight,]
      #     results <- results[-max_first_weight,]
      #     sentiment_results <- sentiment_results[-max_first_weight,]
      #     
      #     print(biggest_first_weight)
      #     print(biggest_first_weight_sentiment[,colSums(biggest_first_weight_sentiment)!=0])
      #     
      #     max_second_weight <- which.max(results$SECOND_WEIGHT)
      #     biggest_second_weight <- results[max_second_weight,]
      #     biggest_second_weight_sentiment <- sentiment_results[max_second_weight,]
      #     results <- results[-max_second_weight,]
      #     sentiment_results <- sentiment_results[-max_second_weight,]
      #     
      #     
      #     print(biggest_second_weight)
      #     print(biggest_second_weight_sentiment[,colSums(biggest_second_weight_sentiment)!=0])
      #     
      #     month_us_de_pair_results <- month_us_de_pair_results[-max_today_IR,]
    }
  }
}
