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
outputDataPathWeek <- paste(outputDataPath,"Week_2007/",sep="")
outputDataPathMergingWeek <- paste(outputDataPath,"Week_2007/01_15_2016/",sep="")


# 
# backtesting_starting_date <- "2007-01-01"
# backtesting_ending_date <- "2015-10-01"
# 
# 
# # my_pairs = list (c("US","DE"),c("US","EU"),c("US","FR"),c("US","GB"),c("US","JP"))
# my_pairs = list(c("US","DE"),c("US","GB"),c("US","JP"))
# 
# #### results
# 
# all_pair_results <- readRDS(paste(outputDataPathWeek,"all_pairs_Week_2007.rds", sep = ""))
# all_results_df <- NULL
# 
# for (my_pair in my_pairs){
#   
#   Week_us_de_pair_results <- all_pair_results[all_pair_results$second_leg == my_pair[2],]
#   my_result_spread_name_root <- paste(my_pair[2],"month_2007_all_pairs",sep="")
#   for (nb in 1:1){
#     my_result_spread_name <- paste(my_result_spread_name_root, nb,sep="")
#     max_today_IR <- which.max(month_us_de_pair_results$IR_spreadbidir_today)
#     biggest_today_IR <- month_us_de_pair_results[max_today_IR,]
#     print("today")
#     print(biggest_today_IR$IR_spreadbidir_today)
#     print("tomorrow")
#     print(biggest_today_IR$IR_spreadbidir_tomorrow)
#     print("All")
#     print(biggest_today_IR)
#     
#     # we do not redo the computation
#     #     capture.output(
#     #       all_results <- compute_spread_strategy_west_first_horizon(inputDataPath, outputDataPath, my_pair[1], my_pair[2], backtesting_starting_date, backtesting_ending_date, biggest_today_IR$spread_amplification_factor, biggest_today_IR$algorithm, biggest_today_IR$europe_as_third_country, biggest_today_IR$zscore, biggest_today_IR$depth, biggest_today_IR$horizon)
#     #     )
#     # we load the result file
#     my_result_spread_name <- paste(my_pair[1], my_pair[2], biggest_today_IR$algorithm,"EU",biggest_today_IR$europe_as_third_country,"ZS",biggest_today_IR$zscore,"depth",biggest_today_IR$depth,"amp",biggest_today_IR$spread_amplification_factor,"inv_hor",biggest_today_IR$horizon,"roll_win",biggest_today_IR$rolling_window,"rec_freq",biggest_today_IR$recalibration_frequency,"spread_results_month_2007",sep="")
#     my_result_spread_filename <- paste(my_result_spread_name, ".rds",sep="")
#     
#     
#     results <- tryCatch( readRDS(paste(outputDataPathMonth,my_result_spread_filename, sep = "")),
#                          error = function(e) {NULL})
#     
#     
#     
#     #     results <- all_results$results
#     #     sentiment_results <- all_results$sentiments
#     #     tree_one <- all_results$first_leg
#     #     tree_two <- all_results$second_leg
#     #     print(tree_one)
#     #     print(tree_one)
#     #     text(tree_one)
#     #     
#     #     ### plotting the tree and assessing the importance of the 
#     #     print(tree_one)
#     #     plot(tree_one)
#     #     text(tree_one)
#     #     names(tree_one)
#     #     tree_one$cptable
#     #     plot(tree_one$variable.importance)
#     #     summary(tree_one)
#     #     
#     #     png(paste(outputDataPath,paste("My_tree_pictures/",my_pair[2],"_month_2007_tree.png",sep=""),sep=""), width = 1200, height = 800)
#     #     post(tree_one, file = "", title. = paste("Regressing US/",my_pair[2],"on Ravenpack news sentiment",sep=""), bp = 18)
#     #     dev.off()
#     #     
#     
#     
#     IR_spreadbidir_today <- computeIR(results$STRATEGY_RETURN, biggest_today_IR$horizon)
#     IR_spreadbidir_today_bis <- computeIRfromPrice(results$STRATEGY_TODAY, biggest_today_IR$horizon)
#     print((IR_spreadbidir_today_bis - IR_spreadbidir_today))
#     IR_spreadbidir_tomorrow <- computeIRfromPrice(results$STRATEGY_TOMORROW, biggest_today_IR$horizon)
#     IR_spreadbidir_yesterday <- computeIRfromPrice(results$STRATEGY_YESTERDAY, biggest_today_IR$horizon)
#     
#     
#     # fact checking
#     STRATEGY_RETURN <- CumfromPricesToRet(results$STRATEGY_TODAY)
#     sum(STRATEGY_RETURN - results$STRATEGY_RETURN)
#     
#     FIRST_BOND_RETURN <- CumfromPricesToRet(results$FIRST_BOND)
#     sum(FIRST_BOND_RETURN - results$FIRST_BOND_RETURN)
#     
#     SECOND_BOND_RETURN <- CumfromPricesToRet(results$SECOND_BOND)
#     sum(SECOND_BOND_RETURN - results$SECOND_BOND_RETURN)
#     
#     FIRST_BOND <- CumFromRetToPricesStart(FIRST_BOND_RETURN)
#     sum(FIRST_BOND - results$FIRST_BOND)
#     
#     SECOND_BOND <- CumFromRetToPricesStart(SECOND_BOND_RETURN)
#     sum(SECOND_BOND - results$SECOND_BOND)
#     
#     FIRST_WEIGHT <- results$FIRST_WEIGHT
#     SECOND_WEIGHT <- results$SECOND_WEIGHT
#     
#     FIRST_BOND_NEXT_RETURN<-results$FIRST_BOND_NEXT_RETURN
#     SECOND_BOND_NEXT_OPEN_RETURN<-results$SECOND_BOND_NEXT_OPEN_RETURN
#     
#     #   head(results$DATES,8)
#     #   head(results$FIRST_BOND_NEXT_RETURN,8)
#     #   head(FIRST_BOND_RETURN,8)
#     #   
#     #   head(results$DATES,5)
#     #   head(results$SECOND_BOND_NEXT_OPEN_RETURN,5)
#     #   head(results$SECOND_BOND_OPEN_RETURN,5)
#     #   
#     
#     STRATEGY_RETURN <- FIRST_WEIGHT*FIRST_BOND_NEXT_RETURN+SECOND_WEIGHT*SECOND_BOND_NEXT_OPEN_RETURN
#     sum(STRATEGY_RETURN - results$STRATEGY_RETURN)
#     
#     
#     
#     results$FIRST_LEG <- results$FIRST_WEIGHT*results$FIRST_BOND_NEXT_RETURN
#     results$SECOND_LEG <- results$SECOND_WEIGHT*results$SECOND_BOND_NEXT_OPEN_RETURN
#     
#     colnames(results) <- c("DATES",paste(my_pair[2],"_",colnames(results[-1]),sep=""))
#     
#     if (is.null(all_results_df)){
#       all_results_df <- results
#     } else {
#       all_results_df <- merge(all_results_df,results,by="DATES",all=T)
#     }
#     
#   }
# }
# 
# all_results_df <- all_results_df[complete.cases(all_results_df),]
# 
# SaveDataFrame(all_results_df, outputDataPath, "all_pairs_results_month_2007")
all_pair_combined_results_12_28 <- readRDS(paste(outputDataPathMergingWeek,"all_pairs_results_week_2007_12_28.rds", sep = ""))
all_pair_combined_results_01_01 <- readRDS(paste(outputDataPathMergingWeek,"all_pairs_results_week_2007_01_15.rds", sep = ""))


IR_spreadbidir_today <- computeIR(all_pair_combined_results_12_28$DE_STRATEGY_RETURN, 21)
print(IR_spreadbidir_today)
IR_spreadbidir_today <- computeIR(all_pair_combined_results_12_28$GB_STRATEGY_RETURN, 21)
print(IR_spreadbidir_today)
IR_spreadbidir_today <- computeIR(all_pair_combined_results_12_28$JP_STRATEGY_RETURN, 21)
print(IR_spreadbidir_today)

IR_spreadbidir_today <- computeIR(all_pair_combined_results_01_01$DE_STRATEGY_RETURN, 21)
print(IR_spreadbidir_today)
IR_spreadbidir_today <- computeIR(all_pair_combined_results_01_01$GB_STRATEGY_RETURN, 21)
print(IR_spreadbidir_today)
IR_spreadbidir_today <- computeIR(all_pair_combined_results_01_01$JP_STRATEGY_RETURN, 21)
print(IR_spreadbidir_today)


plot(all_pair_combined_results_12_28$DE_STRATEGY_TODAY)
plot(all_pair_combined_results_12_28$GB_STRATEGY_TODAY)
plot(all_pair_combined_results_12_28$JP_STRATEGY_TODAY)

plot(all_pair_combined_results_01_01$DE_STRATEGY_TODAY)
plot(all_pair_combined_results_01_01$GB_STRATEGY_TODAY)
plot(all_pair_combined_results_01_01$JP_STRATEGY_TODAY)


all_pair_combined_results_12_28$JP_STRATEGY_RETURN <- all_pair_combined_results_01_01$JP_STRATEGY_RETURN
all_pair_combined_results_12_28$JP_STRATEGY_YESTERDAY <- all_pair_combined_results_01_01$JP_STRATEGY_YESTERDAY     
all_pair_combined_results_12_28$JP_STRATEGY_TODAY <- all_pair_combined_results_01_01$JP_STRATEGY_TODAY             
all_pair_combined_results_12_28$JP_STRATEGY_TOMORROW <- all_pair_combined_results_01_01$JP_STRATEGY_TOMORROW
all_pair_combined_results_12_28$JP_SECOND_BOND <- all_pair_combined_results_01_01$JP_SECOND_BOND           
all_pair_combined_results_12_28$JP_FIRST_BOND <- all_pair_combined_results_01_01$JP_FIRST_BOND            
all_pair_combined_results_12_28$JP_FIRST_WEIGHT <- all_pair_combined_results_01_01$JP_FIRST_WEIGHT
all_pair_combined_results_12_28$JP_SECOND_WEIGHT <- all_pair_combined_results_01_01$JP_SECOND_WEIGHT      
all_pair_combined_results_12_28$JP_FIRST_BOND_RETURN <- all_pair_combined_results_01_01$JP_FIRST_BOND_RETURN         
all_pair_combined_results_12_28$JP_SECOND_BOND_OPEN_RETURN <- all_pair_combined_results_01_01$JP_SECOND_BOND_OPEN_RETURN
all_pair_combined_results_12_28$JP_FIRST_BOND_NEXT_RETURN <- all_pair_combined_results_01_01$JP_FIRST_BOND_NEXT_RETURN   
all_pair_combined_results_12_28$JP_SECOND_BOND_NEXT_OPEN_RETURN <- all_pair_combined_results_01_01$JP_SECOND_BOND_NEXT_OPEN_RETURN
all_pair_combined_results_12_28$JP_FIRST_LEG <- all_pair_combined_results_01_01$JP_FIRST_LEG         
all_pair_combined_results_12_28$JP_SECOND_LEG <- all_pair_combined_results_01_01$JP_SECOND_LEG

# all_pair_combined_results_12_28$DE_STRATEGY_RETURN <- all_pair_combined_results_01_01$DE_STRATEGY_RETURN
# all_pair_combined_results_12_28$DE_STRATEGY_YESTERDAY <- all_pair_combined_results_01_01$DE_STRATEGY_YESTERDAY     
# all_pair_combined_results_12_28$DE_STRATEGY_TODAY <- all_pair_combined_results_01_01$DE_STRATEGY_TODAY             
# all_pair_combined_results_12_28$DE_STRATEGY_TOMORROW <- all_pair_combined_results_01_01$DE_STRATEGY_TOMORROW
# all_pair_combined_results_12_28$DE_SECOND_BOND <- all_pair_combined_results_01_01$DE_SECOND_BOND           
# all_pair_combined_results_12_28$DE_FIRST_BOND <- all_pair_combined_results_01_01$DE_FIRST_BOND            
# all_pair_combined_results_12_28$DE_FIRST_WEIGHT <- all_pair_combined_results_01_01$DE_FIRST_WEIGHT
# all_pair_combined_results_12_28$DE_SECOND_WEIGHT <- all_pair_combined_results_01_01$DE_SECOND_WEIGHT      
# all_pair_combined_results_12_28$DE_FIRST_BOND_RETURN <- all_pair_combined_results_01_01$DE_FIRST_BOND_RETURN         
# all_pair_combined_results_12_28$DE_SECOND_BOND_OPEN_RETURN <- all_pair_combined_results_01_01$DE_SECOND_BOND_OPEN_RETURN
# all_pair_combined_results_12_28$DE_FIRST_BOND_NEXT_RETURN <- all_pair_combined_results_01_01$DE_FIRST_BOND_NEXT_RETURN   
# all_pair_combined_results_12_28$DE_SECOND_BOND_NEXT_OPEN_RETURN <- all_pair_combined_results_01_01$DE_SECOND_BOND_NEXT_OPEN_RETURN
# all_pair_combined_results_12_28$DE_FIRST_LEG <- all_pair_combined_results_01_01$DE_FIRST_LEG         
# all_pair_combined_results_12_28$DE_SECOND_LEG <- all_pair_combined_results_01_01$DE_SECOND_LEG
results <- all_pair_combined_results_12_28

### Deutschland visualization
my_result_spread_name <- "US_DE"
my_pair[1]<-"US"
my_pair[2]<-"DE"
toplot_df <-  melt(results[,c("DATES", "DE_STRATEGY_TODAY","DE_FIRST_BOND","DE_SECOND_BOND")],"DATES")
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

ExportPlot(g,outputDataPathMergingWeek,my_result_spread_name)


toplot_df <-  melt(results[,c("DATES","DE_FIRST_WEIGHT","DE_SECOND_WEIGHT")],"DATES")

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

ExportPlot(g,outputDataPathMergingWeek,paste(my_result_spread_name,"_weights",sep=""))

toplot_df <-  melt(results[,c("DATES","DE_FIRST_LEG","DE_SECOND_LEG")],"DATES")

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

ExportPlot(g,outputDataPathMergingWeek,paste(my_result_spread_name,"_legs",sep=""))



### Great Britain visualization
my_result_spread_name <- "US_GB"
my_pair[1]<-"US"
my_pair[2]<-"GB"
toplot_df <-  melt(results[,c("DATES", "GB_STRATEGY_TODAY","GB_FIRST_BOND","GB_SECOND_BOND")],"DATES")

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

ExportPlot(g,outputDataPathMergingWeek,my_result_spread_name)


toplot_df <-  melt(results[,c("DATES","GB_FIRST_WEIGHT","GB_SECOND_WEIGHT")],"DATES")

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

ExportPlot(g,outputDataPathMergingWeek,paste(my_result_spread_name,"_weights",sep=""))

toplot_df <-  melt(results[,c("DATES","GB_FIRST_LEG","GB_SECOND_LEG")],"DATES")

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

ExportPlot(g,outputDataPathMergingWeek,paste(my_result_spread_name,"_legs",sep=""))


### Japan visualization
my_result_spread_name <- "US_JP"
my_pair[1]<-"US"
my_pair[2]<-"JP"
toplot_df <-  melt(results[,c("DATES", "JP_STRATEGY_TODAY","JP_FIRST_BOND","JP_SECOND_BOND")],"DATES")

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

ExportPlot(g,outputDataPathMergingWeek,my_result_spread_name)


toplot_df <-  melt(results[,c("DATES","JP_FIRST_WEIGHT","JP_SECOND_WEIGHT")],"DATES")

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

ExportPlot(g,outputDataPathMergingWeek,paste(my_result_spread_name,"_weights",sep=""))

toplot_df <-  melt(results[,c("DATES","JP_FIRST_LEG","JP_SECOND_LEG")],"DATES")

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

ExportPlot(g,outputDataPathMergingWeek,paste(my_result_spread_name,"_legs",sep=""))


SaveDataFrame(all_pair_combined_results_12_28, outputDataPathMergingWeek, "all_pairs_results_month_2007")
write.csv(all_pair_combined_results_12_28, file = paste(outputDataPathMergingWeek,"all_pairs_results_month_2007.csv",sep=""), row.names = FALSE)
