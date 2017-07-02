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

#### results

all_pair_results <- readRDS(paste(outputDataPath,"all_pairs_week_2013.rds", sep = ""))
colnames(all_pair_results)

all_results_df <- NULL

for (my_pair in my_pairs){
  
  month_us_de_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
  if( my_pair[2] == "JP"){
    max_today_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_today))
    month_us_de_pair_results <- month_us_de_pair_results[-max_today_IR,]
    max_today_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_today))
    month_us_de_pair_results <- month_us_de_pair_results[-max_today_IR,]
    max_today_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_today))
    month_us_de_pair_results <- month_us_de_pair_results[-max_today_IR,]
    max_today_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_today))
    month_us_de_pair_results <- month_us_de_pair_results[-max_today_IR,]
  }
  my_result_spread_name_root <- paste(my_pair[2],"week_2013_all_pairs",sep="")
  for (nb in 1:1){
    my_result_spread_name <- paste(my_result_spread_name_root, nb,sep="")
    max_today_IR <- which.max(abs(month_us_de_pair_results$IR_spreadbidir_today))
    biggest_today_IR <- month_us_de_pair_results[max_today_IR,]
    print("today")
    print(biggest_today_IR$IR_spreadbidir_today)
    print("tomorrow")
    print(biggest_today_IR$IR_spreadbidir_tomorrow)
    print("All")
    print(biggest_today_IR)
    capture.output(
      all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon)
    )
    results <- all_results$results
    sentiment_results <- all_results$sentiments
    tree_one <- all_results$first_leg
    tree_two <- all_results$second_leg
    print(tree_one)
    print(tree_one)
    plot(tree_one)
    text(tree_one)
    names(tree_one)
    tree_one$cptable
    plot(tree_one$variable.importance)
    summary(tree_one)
    
    png(paste(outputDataPath,paste("My_tree_pictures/",my_pair[2],"_week_2013_tree.png",sep=""),sep=""), width = 1200, height = 800)
    post(tree_one, file = "", title. = paste("Regressing US/",my_pair[2],"on Ravenpack news sentiment",sep=""), bp = 18)
    dev.off()
    
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

all_results_df <- all_results_df[complete.cases(all_results_df),]

SaveDataFrame(all_results_df, outputDataPath, "all_pairs_results_week_2013")
all_pair_combined_results <- readRDS(paste(outputDataPath,"all_pairs_results_week_2013.rds", sep = ""))

write.csv(all_pair_combined_results, file = paste(outputDataPath,"all_pairs_results_week_2013.csv",sep=""), row.names = FALSE)



