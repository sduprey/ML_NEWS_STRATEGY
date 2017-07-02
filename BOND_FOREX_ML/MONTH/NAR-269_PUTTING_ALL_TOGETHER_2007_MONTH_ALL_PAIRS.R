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
# outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007/01_11_2015/",sep="")
outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007/",sep="")
outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")

backtesting_starting_date <- "2007-01-01"
backtesting_ending_date <- "2015-10-01"


# my_pairs = list (c("US","DE"),c("US","EU"),c("US","FR"),c("US","GB"),c("US","JP"))
my_pairs = list(c("US","DE"),c("US","GB"),c("US","JP"))

#### results

all_pair_results <- readRDS(paste(outputDataPathMonth,"all_pairs_month_2007.rds", sep = ""))
all_results_df <- NULL

for (my_pair in my_pairs){
  
  month_us_de_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
  my_result_spread_name_root <- paste(my_pair[2],"month_2007_all_pairs",sep="")
  nb_deep_solution <- 1
  if (my_pair[2] == "DE" ){
    nb_deep_solution <- 2
  } else {
    nb_deep_solution <- 1
  }
  for (nb in 1:nb_deep_solution){
    my_result_spread_name <- paste(my_result_spread_name_root, nb,sep="")
    max_today_IR <- which.max(month_us_de_pair_results$IR_spreadbidir_today)
    biggest_today_IR <- month_us_de_pair_results[max_today_IR,]
    month_us_de_pair_results<-month_us_de_pair_results[-max_today_IR,]
    print("today")
    print(biggest_today_IR$IR_spreadbidir_today)
    print("tomorrow")
    print(biggest_today_IR$IR_spreadbidir_tomorrow)
    print("All")
    print(biggest_today_IR)
    if (nb == nb_deep_solution){
      
      # we do not redo the computation
      #     capture.output(
      #       all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon)
      #     )
      # we load the result file
      my_result_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency,"spread_results_month_2007",sep="")
      my_result_spread_filename <- paste(my_result_spread_name, ".rds",sep="")
      
      
      results <- tryCatch( readRDS(paste(outputDataPathMonth,my_result_spread_filename, sep = "")),
                           error = function(e) {NULL})
      
      
      
      #     results <- all_results$results
      #     sentiment_results <- all_results$sentiments
      #     tree_one <- all_results$first_leg
      #     tree_two <- all_results$second_leg
      #     print(tree_one)
      #     print(tree_one)
      #     text(tree_one)
      #     
      #     ### plotting the tree and assessing the importance of the 
      #     print(tree_one)
      #     plot(tree_one)
      #     text(tree_one)
      #     names(tree_one)
      #     tree_one$cptable
      #     plot(tree_one$variable.importance)
      #     summary(tree_one)
      #     
      #     png(paste(outputDataPath,paste("My_tree_pictures/",my_pair[2],"_month_2007_tree.png",sep=""),sep=""), width = 1200, height = 800)
      #     post(tree_one, file = "", title. = paste("Regressing US/",my_pair[2],"on Ravenpack news sentiment",sep=""), bp = 18)
      #     dev.off()
      #     
      
      
      IR_spreadbidir_today <- computeIR(results$STRATEGY_RETURN, biggest_today_IR$horizon)
      IR_spreadbidir_today_bis <- computeIRfromPrice(results$STRATEGY_TODAY, biggest_today_IR$horizon)
      print((IR_spreadbidir_today_bis - IR_spreadbidir_today))
      IR_spreadbidir_tomorrow <- computeIRfromPrice(results$STRATEGY_TOMORROW, biggest_today_IR$horizon)
      IR_spreadbidir_yesterday <- computeIRfromPrice(results$STRATEGY_YESTERDAY, biggest_today_IR$horizon)
      
      
      # fact checking
      STRATEGY_RETURN <- CumfromPricesToRet(results$STRATEGY_TODAY)
      sum(STRATEGY_RETURN - results$STRATEGY_RETURN)
      
      FIRST_BOND_RETURN <- CumfromPricesToRet(results$FIRST_BOND)
      sum(FIRST_BOND_RETURN - results$FIRST_BOND_RETURN)
      
      SECOND_BOND_RETURN <- CumfromPricesToRet(results$SECOND_BOND)
      sum(SECOND_BOND_RETURN - results$SECOND_BOND_RETURN)
      
      FIRST_BOND <- CumFromRetToPricesStart(FIRST_BOND_RETURN)
      sum(FIRST_BOND - results$FIRST_BOND)
      
      SECOND_BOND <- CumFromRetToPricesStart(SECOND_BOND_RETURN)
      sum(SECOND_BOND - results$SECOND_BOND)
      
      FIRST_WEIGHT <- results$FIRST_WEIGHT
      SECOND_WEIGHT <- results$SECOND_WEIGHT
      
      FIRST_BOND_NEXT_RETURN<-results$FIRST_BOND_NEXT_RETURN
      SECOND_BOND_NEXT_OPEN_RETURN<-results$SECOND_BOND_NEXT_OPEN_RETURN
      
      #   head(results$DATES,8)
      #   head(results$FIRST_BOND_NEXT_RETURN,8)
      #   head(FIRST_BOND_RETURN,8)
      #   
      #   head(results$DATES,5)
      #   head(results$SECOND_BOND_NEXT_OPEN_RETURN,5)
      #   head(results$SECOND_BOND_OPEN_RETURN,5)
      #   
      
      STRATEGY_RETURN <- FIRST_WEIGHT*FIRST_BOND_NEXT_RETURN+SECOND_WEIGHT*SECOND_BOND_NEXT_OPEN_RETURN
      sum(STRATEGY_RETURN - results$STRATEGY_RETURN)
      
      
      
      results$FIRST_LEG <- results$FIRST_WEIGHT*results$FIRST_BOND_NEXT_RETURN
      results$SECOND_LEG <- results$SECOND_WEIGHT*results$SECOND_BOND_NEXT_OPEN_RETURN
      
      
      colnames(results) <- c("DATES",paste(my_pair[2],"_",colnames(results[-1]),sep=""))
      
      if (is.null(all_results_df)){
        all_results_df <- results
      } else {
        all_results_df <- merge(all_results_df,results,by="DATES",all=T)
      }
      
      
    }
  }
}

old_columns <- colnames(all_results_df)
results <- all_results_df[complete.cases(all_results_df),]

# Computing the equally weighted strategy for benchmark
results$STRATEGY_RETURN <- (results$JP_STRATEGY_RETURN+results$GB_STRATEGY_RETURN+results$DE_STRATEGY_RETURN)/3
results$STRATEGY_TODAY <- CumFromRetToPricesStart(results$STRATEGY_RETURN)

results$US_WEIGHT <- (results$DE_FIRST_WEIGHT + results$GB_FIRST_WEIGHT + results$JP_FIRST_WEIGHT)/3
results$DE_WEIGHT <- results$DE_SECOND_WEIGHT/3
results$JP_WEIGHT <- results$JP_SECOND_WEIGHT/3
results$GB_WEIGHT <- results$GB_SECOND_WEIGHT/3

results$EQ_WEIGHTS_BOND <- (results$DE_FIRST_BOND + results$JP_SECOND_BOND + results$GB_SECOND_BOND +results$DE_SECOND_BOND)/4


# results$STRATEGY_RETURN <- (results$JP_STRATEGY_RETURN+results$GB_STRATEGY_RETURN+results$DE_STRATEGY_RETURN)/3
# results$STRATEGY_TODAY <- CumFromRetToPricesStart(results$STRATEGY_RETURN)
# 
# results$US_WEIGHT <- (results$DE_FIRST_WEIGHT + results$GB_FIRST_WEIGHT + results$JP_FIRST_WEIGHT)/3
# results$DE_WEIGHT <- results$DE_SECOND_WEIGHT/3
# results$JP_WEIGHT <- results$JP_SECOND_WEIGHT/3
# results$GB_WEIGHT <- results$GB_SECOND_WEIGHT/3
colnames(results) <- c(old_columns,"STRATEGY_RETURN","STRATEGY_TODAY","US_WEIGHT","DE_WEIGHT","JP_WEIGHT","GB_WEIGHT","EQ_WEIGHTS_BOND")


SaveDataFrame(results, outputDataPathStrategyMonth, "all_pairs_results_month_2007")
all_pair_combined_results <- readRDS(paste(outputDataPathStrategyMonth,"all_pairs_results_month_2007.rds", sep = ""))

write.csv(all_pair_combined_results, file = paste(outputDataPathStrategyMonth,"all_pairs_results_month_2007.csv",sep=""), row.names = FALSE)



