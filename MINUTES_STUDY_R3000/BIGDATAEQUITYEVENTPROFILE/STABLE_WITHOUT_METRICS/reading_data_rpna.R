library("RPToolsDB")
library(shiny)
library(DT)
library("RPPlotUtils")
library(dplyr)

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")



# 
# 
# ##############
# ##############
# ##############
# ############## Computing the spread sentiment category
# bootstrap_CI <- FALSE
# data <- readRDS(file=paste0(outputDataPath,bootstrap_CI,"prod_r1000_rpna_abvol_abvol_corrado_df.rds"))
# 
# data$localSource[data$localSource == "DJPR"] <- "DJPR"
# data$localSource[data$localSource == "PREMIUM"] <- "PREMIUM_PACK"
# data$localSource[data$localSource == "WEBNONPREMIUM"] <- "WEB_NON_PREMIUM"
# 
# print(dim(data))
# 
# stats_sign <- colnames(data)[which(!is.na(as.numeric(colnames(data))))]
# ord <- paste0("ORD",stats_sign)
# rets <- paste0("RET",stats_sign)
# vol <- paste0("VOLU",stats_sign)
# vola <- paste0("VOLA",stats_sign)
# numeric_columns <- c(stats_sign,rets,ord,vol,vola)
# mean_columns <- c(stats_sign,ord,vol,vola)
# spread_columns <- c(rets)
# 
# 
# joining_columns <- setdiff(colnames(data), numeric_columns)
# 
# joining_columns <- setdiff(joining_columns, c("sentiment_criteria","corrado_methodo","event_number_event_filtering"))
# 
# 
# computeSpread <- function(df,toMean , toSpread){
#   
#   if(sum(df$sentiment_criteria == "NEGATIVE") & sum(df$sentiment_criteria == "POSITIVE")){
#     
#     negativeSentiment <- df[df$sentiment_criteria == "NEGATIVE",]
#     positiveSentiment <- df[df$sentiment_criteria == "POSITIVE",]
#     
#     if(dim(negativeSentiment)[1] >1){
#       print("to investigate")
#       negativeSentiment <- negativeSentiment[which.max(negativeSentiment$event_number_event_filtering),]
#     } 
#     if(dim(positiveSentiment)[1] >1){
#       print("to investigate")
#       positiveSentiment <- positiveSentiment[which.max(positiveSentiment$event_number_event_filtering),]
#     } 
#     
#     rowToAdd <- negativeSentiment
#     rowToAdd[,toMean] <- (positiveSentiment[,toMean] + negativeSentiment[,toMean])/2
#     rowToAdd[,toSpread] <- (positiveSentiment[,toSpread] - negativeSentiment[,toSpread])
#     rowToAdd$sentiment_criteria <- "SPREAD"
#     return(rbind(df,rowToAdd))
#   }
#   return(df)
# }
# 
# print("computing the spread")
# data_spread_augmented <- ddply(.data = data, .variables = joining_columns, .fun = function(x){computeSpread(x,mean_columns,spread_columns)})
# print("spread computed")
# data <- data_spread_augmented
# print(dim(data))
# 
# # data$infinity_return <- abs(data$RET180)
# # data$infinity_confidence <- data$`180`
# ##########################################################
# #############################
# #############################
# #############################
# #############################
# ############################# Computing ranking metrics
# 
# data$correcting_factor <- 2*(data$sentiment_criteria == "POSITIVE"  | data$sentiment_criteria == "SPREAD")-1
# data$correcting_factor[data$sentiment_criteria == "ALL"] <- 0
# #### first one
# # data$infinity_return <- (data$RET180)*data$correcting_factor
# #### second one
# # data$infinity_return <- (data$RET180)*(data$`180`)*data$correcting_factor
# #### third one
# stats_post_sign <- colnames(data)[which(as.numeric(colnames(data)) >= 0)]
# rets_post <- paste0("RET",stats_post_sign)
# # data$infinity_return <- rowSums(data[,rets_post]*data$correcting_factor*data[,stats_post_sign],na.rm = TRUE)
# 
# 
# infinity_return <- rowSums(data[,rets_post]*data$correcting_factor*data[,stats_post_sign],na.rm = TRUE)
# 
# infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
# significance <- (data$event_number_event_filtering-min(data$event_number_event_filtering))/(max(data$event_number_event_filtering)-min(data$event_number_event_filtering))
# 
# infinity_return_global <- infinity_return
# infinity_return <- infinity_return*significance
# 
# 
# data$infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
# 
# data$infinity_return_global <- (infinity_return_global-min(infinity_return_global))/(max(infinity_return_global)-min(infinity_return_global))
# 
# data$infinity_return[grepl("technical",data$my_event)] <- data$infinity_return[grepl("technical",data$my_event)]*0.01
# data$infinity_return[grepl("imbalance",data$my_event)] <- data$infinity_return[grepl("imbalance",data$my_event)]*0.01
# data$infinity_return[grepl("stock-loss",data$my_event)] <- data$infinity_return[grepl("stock-loss",data$my_event)]*0.01
# data$infinity_return[grepl("stock-gain",data$my_event)] <- data$infinity_return[grepl("stock-gain",data$my_event)]*0.01
# data$infinity_return[grepl("stock-prices",data$my_event)] <- data$infinity_return[grepl("stock-prices",data$my_event)]*0.01
# 
# data$infinity_return_global[grepl("technical",data$my_event)] <- data$infinity_return_global[grepl("technical",data$my_event)]*0.01
# data$infinity_return_global[grepl("imbalance",data$my_event)] <- data$infinity_return_global[grepl("imbalance",data$my_event)]*0.01
# data$infinity_return_global[grepl("stock-loss",data$my_event)] <- data$infinity_return_global[grepl("stock-loss",data$my_event)]*0.01
# data$infinity_return_global[grepl("stock-gain",data$my_event)] <- data$infinity_return_global[grepl("stock-gain",data$my_event)]*0.01
# data$infinity_return_global[grepl("stock-prices",data$my_event)] <- data$infinity_return_global[grepl("stock-prices",data$my_event)]*0.01
# 
# 
# 
# 
# 
# RP_SaveDataFrame(data, outputDataPath = outputDataPath, filename = "prod_spr_r1000_rpna_abvol_abvol_corrado_df")
# print("spread saved")
# 
# 
# # test <- data[data$my_event == "analyst-ratings" & data$similarity_gap_filter == 1 & data$localSource == "DJ",]
# # test[,joining_columns]
# # test <- data[data$my_event == "analyst-rating" & data$similarity_gap_filter == "1" & data$localSource == "DJ",]
# 
# ##############
# ##############
# ##############
# ##############  End of computing the spread sentiment category
# 
# 
# ##############
# ##############
# ##############
# ############## Ordering the best profiles
# 
# data <- readRDS(file=paste0(outputDataPath,"prod_spr_r1000_rpna_abvol_abvol_corrado_df.rds"))
# # data <- data[data$event_number_event_filtering >= 50,]
# print(dim(data))
# ##############
# print("Getting rid of the flat profiles")
# stats_sign <- colnames(data)[which(!is.na(as.numeric(colnames(data))))]
# ord <- paste0("ORD",stats_sign)
# rets <- paste0("RET",stats_sign)
# vol <- paste0("VOLU",stats_sign)
# vola <- paste0("VOLA",stats_sign)
# numeric_columns <- c(stats_sign,rets,ord,vol,vola)
# mean_columns <- c(stats_sign,ord,vol,vola)
# spread_columns <- c(rets)
# 
# trimFlat <- function(row,rets){
#   # print(row[,rets])
#   numeric_rets <- row[,rets]
#   numeric_rets[is.na(numeric_rets)] <- 0
#   
#   diff_rets <- diff(as.vector(as.matrix(numeric_rets)))
#   
#   nullNumber <- sum(abs(diff_rets) <= .Machine$double.eps)
#   proportion <- nullNumber/dim(row)[2]
#   if(proportion >= 0.05){
#     return(row$Id)
#   }
#   return(NULL)
# }
# 
# 
# 
# print("before")
# print(dim(data))
# 
# data$Id <- 1:dim(data)[1]
# noFlatData  <- ddply(.data = data,.variables = "Id",.fun =  function(x){trimFlat(x, spread_columns)})
# data <- data[!(data$Id %in% noFlatData$Id),]
# data$Id <- NULL
# 
# print("after")
# print(dim(data))
# print(dim(noFlatData))
# 
# 
# print("Done")
# 
# 
# RP_SaveDataFrame(data, outputDataPath = outputDataPath, filename = "clean_prod_spr_r1000_rpna_abvol_abvol_corrado_df")
# print("clean saved")
# 



##########################################################
#############################
#############################
#############################
#############################
############################# Group category best profiles


####### all generics
data <- readRDS(file=paste0(outputDataPath,"clean_prod_spr_r1000_rpna_abvol_abvol_corrado_df.rds"))

all_group_events <- sort(unique(data$my_event[data$aggregate_criteria == "GROUP"]))
all_category_events <- sort(unique(data$my_event[data$aggregate_criteria == "CATEGORY"]))

RP_SaveDataFrame(all_group_events, outputDataPath = outputDataPath, filename = "prod_rpna_all_group_events")
RP_SaveDataFrame(all_category_events, outputDataPath = outputDataPath, filename = "prod_rpna_all_category_events")


###########
########### Fro groups
dataGroup <- data[data$aggregate_criteria =="GROUP",]
trimBest <- function(dfrow){
  return(dfrow[dfrow$infinity_return >= max(dfrow$infinity_return),])
}


print("before")
print(dim(dataGroup))

dataGroupBest  <- ddply(.data = dataGroup,.variables = "my_event",.fun =  trimBest)

print("after")
print(dim(dataGroupBest))
dataGroupBest <- dataGroupBest[order(dataGroupBest$infinity_return,decreasing = TRUE),]

RP_SaveDataFrame(dataGroupBest, outputDataPath = outputDataPath, filename = "clean_prod_rpna_best_profile_group_ordered_r1000_corrado_df")




#########
########### For categories
dataCategory <- data[data$aggregate_criteria =="CATEGORY",]

print("before")
print(dim(dataCategory))

dataCategoryBest  <- ddply(.data = dataCategory,.variables = "my_event",.fun =  trimBest)

print("after")
print(dim(dataCategoryBest))
dataCategoryBest <- dataCategoryBest[order(dataCategoryBest$infinity_return,decreasing = TRUE),]

RP_SaveDataFrame(dataCategoryBest, outputDataPath = outputDataPath, filename = "clean_prod_rpna_best_profile_category_ordered_r1000_corrado_df")

print("best profiles computed")



##############
##############
##############
############## end of ordering the best profiles

