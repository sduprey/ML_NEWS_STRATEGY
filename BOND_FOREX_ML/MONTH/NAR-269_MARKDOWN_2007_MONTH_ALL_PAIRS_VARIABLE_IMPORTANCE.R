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
outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")

backtesting_starting_date <- "2007-01-01"
backtesting_ending_date <- "2015-10-01"


# my_pairs = list (c("US","DE"),c("US","EU"),c("US","FR"),c("US","GB"),c("US","JP"))
my_pairs = list(c("US","JP"),c("US","DE"),c("US","GB"))

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
  if (my_pair[2] == "DE" ){
    nb_deep_solution <- 2
  }
  for (nb in 1:nb_deep_solution){
    # for (nb in 1:1){
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
    
    # we do not redo the computation
    #     capture.output(
    #       all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon)
    #     )
    # we load the result file
    
    # Computing the variable importance over time
    # all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon)
    # all_results <- compute_spread_strategy_west_first_horizon_resubstitution(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, "xgboost", biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon, biggest_today_IR$rolling_window, biggest_today_IR$recalibration_frequency)
    # all_results <- compute_spread_strategy_west_first_horizon_resubstitution(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon, biggest_today_IR$rolling_window, biggest_today_IR$recalibration_frequency)
    # all_results <- compute_spread_strategy_west_first_horizon_resubstitution(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon, biggest_today_IR$rolling_window, 120)
    
    
    
    #     my_result_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency, "spread_results_month_2007",sep="")
    #     my_result_spread_filename <- paste(my_result_spread_name, ".rds",sep="")
    #     
    #     results <- tryCatch( readRDS(paste(outputDataPathMonth,my_result_spread_filename, sep = "")),
    #                          error = function(e) {NULL})
    #     
    #     
    #     ### Dealing with parameters stability
    #     
    #     my_calibration_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency,"spread_calibration_month_2007",sep="")
    #     my_calibration_filename <- paste(my_calibration_name, ".rds",sep="")
    #     
    #     calibration <- tryCatch( readRDS(paste(outputDataPathMonth,my_calibration_filename, sep = "")),
    #                              error = function(e) {NULL})
    #     
    #     my_features_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency,"spread_features_month_2007",sep="")
    #     my_features_filename <- paste(my_features_name, ".rds",sep="")
    #     
    #     features_names <- tryCatch( readRDS(paste(outputDataPathMonth,my_features_filename, sep = "")),
    #                                 error = function(e) {NULL})
    #     
    #     if(!is.null(calibration)){
    #       print(" Calibration value")
    #       print(calibration)
    #     }
    #     
    #     
    my_first_leg_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency, "spread_first_leg_month_2007",sep="")
    my_first_leg_spread_filename <- paste(my_first_leg_spread_name, ".rds",sep="")
    first_leg <- tryCatch( readRDS(paste(outputDataPathMonth,my_first_leg_spread_filename, sep = "")),
                           error = function(e) {NULL})
    
    my_second_leg_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency, "spread_second_leg_month_2007",sep="")
    my_second_leg_spread_filename <- paste(my_second_leg_spread_name, ".rds",sep="")
    second_leg <- tryCatch( readRDS(paste(outputDataPathMonth,my_second_leg_spread_filename, sep = "")),
                           error = function(e) {NULL})
    
    
    my_features_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency, "spread_features_month_2007",sep="")
    my_features_spread_filename <- paste(my_features_spread_name, ".rds",sep="")
    features_names <- tryCatch( readRDS(paste(outputDataPathMonth,my_features_spread_filename, sep = "")),
                           error = function(e) {NULL})

    if (!is.null(first_leg) && !is.null(second_leg) && !is.null(features_names) && !is.null(results) ){
      if ((length(first_leg) == length(second_leg))){
        # if ((length(first_leg) == length(second_leg)) &&  (length(second_leg)== length(results$DATES))){
          
        # We here begin to accumulate the histograms over time
        my_total_df <- NULL
        for (my_iteration in 1:length(results$DATES)){
          
#           my_date <- results$DATES[my_iteration]
#           print("Dealing with date : ")
#           print(my_date)
          print("My iteration")
          print(my_iteration)
          
          my_first_model <- first_leg[[my_iteration]]
          my_second_model <- second_leg[[my_iteration]]
          
          
                    first_importance_df <- tryCatch(  as.data.frame(xgb.importance(features_names, model = my_first_model)),
                                                error = function(e) {NULL})
                    second_importance_df <- tryCatch(  as.data.frame(xgb.importance(features_names, model = my_second_model)),
                                                      error = function(e) {NULL})
          
                    if (!is.null(first_importance_df) && !is.null(second_importance_df)){
                      print("Not null")
#                       first_importance_df <- as.data.frame(xgb.importance(features_names, model = my_first_model))
#                       second_importance_df <- as.data.frame(xgb.importance(features_names, model = my_second_model))
                      # if (!is.null(first_importance_df) && !is.null(second_importance_df)){
                      first_importance_df <- first_importance_df[,c("Feature","Gain")]
                      second_importance_df <- second_importance_df[,c("Feature","Gain")]
                      
                      merged_dataframe <- data.frame(Feature = features_names, Gain = rep(0,length(features_names)))
                      
                      merged_dataframe <- merge(merged_dataframe, first_importance_df, by = "Feature",all=TRUE)
                      merged_dataframe <- merge(merged_dataframe, second_importance_df, by = "Feature",all=TRUE)
                      
                      
                      if(is.null(my_total_df)){
                        final_column_df <- data.frame(Feature=features_names,Weigth=rowSums(merged_dataframe[,-1],na.rm = TRUE))
                        my_total_df <- final_column_df
                      } else {
                        final_column_df <- data.frame(Weigth=rowSums(merged_dataframe[,-1],na.rm = TRUE))
                        my_total_df <- cbind(my_total_df,final_column_df)
                      }
                    } else {
                      print("Null")
                    }
           

          #           } else {
          #             print("Issue")
          #           }
          
          #           populateList <- function(mycolumnlist, myrow){
          #             mycolumnlist[[myrow$Feature]] <- myrow$Gain
          #           }
          #           
          #           mycolumnlist <- vector("list", length(features_names))
          #           names(mycolumnlist) <- features_names
        }
        
        # colnames(my_total_df) <- c("Feature",results$DATES)
        colnames(my_total_df) <- c("Feature",1:(dim(my_total_df)[2]-1))
        rownames(my_total_df) <- features_names
        
        SaveDataFrame(my_total_df,outputDataPathMonth,paste(my_pair[1], my_pair[2],"variable_importance_spread_results_month_2007",sep=""))
      }  
    }
    
#     if (!is.null(first_leg) && !is.null(features_names) ){
#       print(class(first_leg))
#       
#       if (class(first_leg[[1]]) == "xgb.Booster"){
#         #### Exploring the model
#         nb_trees <- 1
#         for (my_model in first_leg){
#           model <- xgb.dump(my_model, with.stats = T)
#           print(model[1:10])
#           print(model)
#           # Compute feature importance matrix
#           #           importance_matrix <- xgb.importance(features_names, model = my_model)
#           #           # Nice graph
#           #           # filtered
#           #           # xgb.plot.importance(importance_matrix[1:10,])
#           #           # unfiltered
#           #           imp_plot <- xgb.plot.importance(importance_matrix)
#           #           print(imp_plot)
#           #           ExportPlot(imp_plot,outputDataPathMonth,paste0("first",my_pair[2],nb_trees,"histogram"))
#           
#           tree_plot <- xgb.plot.tree(feature_names = features_names, model = my_model)
#           print(tree_plot)
#           # does not work because this is a html widget
#           # ExportPlot(tree_plot,outputDataPathMonth,paste0("first",my_pair[2],nb_trees,"tree"))
#           
#           nb_trees <- nb_trees + 1
#         }
#       }
#       
#       if (class(first_leg) == "rpart"){
#         # some plotting to assess performance after fact checking
#         #     sentiment_results <- all_results$sentiments
#         #     tree_one <- all_results$first_leg
#         #     tree_two <- all_results$second_leg
#         #     print(tree_one)
#         #     print(tree_one)
#         #     text(tree_one)
#         #     
#         #     ### plotting the tree and assessing the importance of the 
#         #     print(tree_one)
#         #     plot(tree_one)
#         #     text(tree_one)
#         #     names(tree_one)
#         #     tree_one$cptable
#         #     plot(tree_one$variable.importance)
#         #     summary(tree_one)
#         #     
#         #     png(paste(outputDataPath,paste("My_tree_pictures/",my_pair[2],"_month_2007_tree.png",sep=""),sep=""), width = 1200, height = 800)
#         #     post(tree_one, file = "", title. = paste("Regressing US/",my_pair[2],"on Ravenpack news sentiment",sep=""), bp = 18)
#         #     dev.off()
#       }
#     }
    
    #     
    #     results <- all_results$results
    #     sentiment_results <- all_results$sentiments
    #     tree_one <- all_results$first_leg
    #     tree_two <- all_results$second_leg
    #     
    
    #     
    #     
    #     IR_spreadbidir_today <- computeIR(results$STRATEGY_RETURN, biggest_today_IR$horizon)
    #     IR_spreadbidir_tomorrow <- computeIRfromPrice(results$STRATEGY_TOMORROW, biggest_today_IR$horizon)
    #     IR_spreadbidir_yesterday <- computeIRfromPrice(results$STRATEGY_YESTERDAY, biggest_today_IR$horizon)
    #     
    #     # fact checking
    #     STRATEGY_RETURN <- CumfromPricesToRet(results$STRATEGY_TODAY)
    #     sum(STRATEGY_RETURN-results$STRATEGY_RETURN[2:length(results$STRATEGY_RETURN)])
    #     
    #     FIRST_BOND_RETURN <- CumfromPricesToRet(results$FIRST_BOND)
    #     sum(FIRST_BOND_RETURN - results$FIRST_BOND_RETURN[2:length(results$STRATEGY_RETURN)])
    #     
    #     SECOND_BOND_RETURN <- CumfromPricesToRet(results$SECOND_BOND)
    #     sum(SECOND_BOND_RETURN - results$SECOND_BOND_RETURN[2:length(results$STRATEGY_RETURN)])
    #     
    #     FIRST_BOND <- CumFromRetToPricesStart(results$FIRST_BOND_RETURN)
    #     sum(FIRST_BOND - results$FIRST_BOND)
    #     
    #     SECOND_BOND <- CumFromRetToPricesStart(results$SECOND_BOND_RETURN)
    #     sum(SECOND_BOND - results$SECOND_BOND)
    #     
    #     FIRST_WEIGHT <- results$FIRST_WEIGHT
    #     SECOND_WEIGHT <- results$SECOND_WEIGHT
    #     
    #     FIRST_BOND_NEXT_RETURN<-results$FIRST_BOND_NEXT_RETURN
    #     SECOND_BOND_NEXT_OPEN_RETURN<-results$SECOND_BOND_NEXT_OPEN_RETURN
    #     
    #     results$FIRST_LEG <- results$FIRST_WEIGHT*results$FIRST_BOND_NEXT_RETURN 
    #     results$SECOND_LEG <- results$SECOND_WEIGHT*results$SECOND_BOND_NEXT_OPEN_RETURN
    #     
    #     STRATEGY_RETURN <- FIRST_WEIGHT*FIRST_BOND_NEXT_RETURN+SECOND_WEIGHT*SECOND_BOND_NEXT_OPEN_RETURN
    #     sum(STRATEGY_RETURN - results$STRATEGY_RETURN)
    #     
    #     # Decomposing our strategy over two legs
    #     sum(results$FIRST_LEG - results$FIRST_WEIGHT*results$FIRST_BOND_NEXT_RETURN)
    #     sum(results$SECOND_LEG - results$SECOND_WEIGHT*results$SECOND_BOND_NEXT_OPEN_RETURN)
    #     #### Outputing some turn over ratios results
    #     print("Outputing statistical results for our strategy")
    #     WeightMatrix <- results[,c("DATES","FIRST_WEIGHT","SECOND_WEIGHT")]
    #     colnames(WeightMatrix) <- c("DATE","FIRST_WEIGHT","SECOND_WEIGHT")
    #     # ReturnSerie <- results[,c("DATES","STRATEGY_RETURN")]
    #     # colnames(ReturnSerie) <- c("Date","STRATEGY_RETURN")
    #     ReturnSerie <- results[,c("STRATEGY_RETURN")]
    #     turnover <- RP_GetTurnOver(WeightMatrix,-1)
    #     RP_ReturnStats(ReturnSerie,21,TRUE,WeightMatrix,0.05,F)
    #     
    #     
    #     
    #     #######toplot_df <-  melt(results[,c("DATES", "STRATEGY_TODAY","SPREAD_STRATEGY_YESTERDAY", "SPREAD_STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
    #     toplot_df <-  melt(results[,c("DATES", "STRATEGY_TODAY","FIRST_BOND","SECOND_BOND")],"DATES")
    #     
    #     my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
    #     g<-ggplot(
    #       toplot_df,aes(
    #         x = DATES,y = value,group = variable,color = variable
    #       )
    #     ) +
    #       geom_line() +
    #       scale_x_date() +
    #       ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
    #       theme(title = element_text(size = 12, face = 'bold')) +
    #       theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
    #       theme(legend.background = element_rect(fill = "gray90")) +
    #       theme(legend.key.size = unit(0.7, "cm"))
    #     print(g)
    #     
    #     ExportPlot(g,outputDataPathMonth,my_result_spread_name)
    #     
    #     
    #     toplot_df <-  melt(results[,c("DATES","FIRST_WEIGHT","SECOND_WEIGHT")],"DATES")
    #     
    #     my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
    #     g<-ggplot(
    #       toplot_df,aes(
    #         x = DATES,y = value,group = variable,color = variable
    #       )
    #     ) +
    #       geom_line() +
    #       scale_x_date() +
    #       ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
    #       theme(title = element_text(size = 12, face = 'bold')) +
    #       theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
    #       theme(legend.background = element_rect(fill = "gray90")) +
    #       theme(legend.key.size = unit(0.7, "cm"))
    #     print(g)
    #     
    #     ExportPlot(g,outputDataPathMonth,paste(my_result_spread_name,"_weights",sep=""))
    #     
    #     toplot_df <-  melt(results[,c("DATES","FIRST_LEG","SECOND_LEG")],"DATES")
    #     
    #     my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
    #     g<-ggplot(
    #       toplot_df,aes(
    #         x = DATES,y = value,group = variable,color = variable
    #       )
    #     ) +
    #       geom_line() +
    #       scale_x_date() +
    #       ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
    #       theme(title = element_text(size = 12, face = 'bold')) +
    #       theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
    #       theme(legend.background = element_rect(fill = "gray90")) +
    #       theme(legend.key.size = unit(0.7, "cm"))
    #     print(g)
    #     
    #     ExportPlot(g,outputDataPathMonth,paste(my_result_spread_name,"_legs",sep=""))
    #     
    #     
    #     
    #     toplot_df <-  melt(results[,c("DATES","STRATEGY_TODAY", "STRATEGY_YESTERDAY","STRATEGY_TOMORROW","FIRST_BOND","SECOND_BOND")],"DATES")
    #     
    #     my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
    #     g<-ggplot(
    #       toplot_df,aes(
    #         x = DATES,y = value,group = variable,color = variable
    #       )
    #     ) +
    #       geom_line() +
    #       scale_x_date() +
    #       ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
    #       theme(title = element_text(size = 12, face = 'bold')) +
    #       theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
    #       theme(legend.background = element_rect(fill = "gray90")) +
    #       theme(legend.key.size = unit(0.7, "cm"))
    #     print(g)          
    #     ExportPlot(g,outputDataPathMonth,paste(my_result_spread_name,"bis"))
    #     
    #     
    
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
