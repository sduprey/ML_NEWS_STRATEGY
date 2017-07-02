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





##############
##############

my_lapses_files <- 
  list(
    "-1" = "FALSElprod_r1000_bigdata_abvol_abvol_corrado_df_authentic.rds",
    "2" ="FALSEl2prod_r1000_bigdata_abvol_abvol_corrado_df.rds",
    "4" = "FALSEl4prod_r1000_bigdata_abvol_abvol_corrado_df.rds",
    "5" = "FALSEl5prod_r1000_bigdata_abvol_abvol_corrado_df.rds"
  )


aggregated_data <- NULL
# for (my_lapse in c(-1,2,4,5)){
for (my_lapse in c(-1,2,4,5)){
    
  my_file_name <- my_lapses_files[[as.character(my_lapse)]]
  print(my_file_name)
  data <- readRDS(file=paste0(outputDataPath,my_file_name))
  print(dim(data))
  if(my_lapse == -1){
    data <- data[data$lapse == 1,]
    data$lapse <- -1
  } else {
    data$lapse <- my_lapse
  }
  
  
  data$event_number_event_filtering <- 1.5*data$event_number_event_filtering
  
  
  data$event_number_event_filtering[data$my_event == "analyst-ratings"] <- 2.5*data$event_number_event_filtering[data$my_event == "analyst-ratings"]
  
  data$localSource[data$localSource == "DJPR"] <- "DJPR"
  data$localSource[data$localSource == "PREMIUM"] <- "PREMIUM_PACK"
  data$localSource[data$localSource == "WEBNONPREMIUM"] <- "WEB_NON_PREMIUM"
  
  print(dim(data))
  print(unique(data$lapse))
  
  
  
  
  stats_sign <- colnames(data)[which(!is.na(as.numeric(colnames(data))))]
  ord <- paste0("ORD",stats_sign)
  rets <- paste0("RET",stats_sign)
  vol <- paste0("VOLU",stats_sign)
  vola <- paste0("VOLA",stats_sign)
  numeric_columns <- c(stats_sign,rets,ord,vol,vola)
  mean_columns <- c(stats_sign,ord,vol,vola)
  spread_columns <- c(rets)
  
  
  joining_columns <- setdiff(colnames(data), numeric_columns)
  
  joining_columns <- setdiff(joining_columns, c("sentiment_criteria","corrado_methodo","event_number_event_filtering"))
  
  
  computeSpread <- function(df,toMean , toSpread){
    
    if(sum(df$sentiment_criteria == "NEGATIVE") & sum(df$sentiment_criteria == "POSITIVE")){
      
      negativeSentiment <- df[df$sentiment_criteria == "NEGATIVE",]
      positiveSentiment <- df[df$sentiment_criteria == "POSITIVE",]
      
      if(dim(negativeSentiment)[1] >1){
        print("to investigate")
        negativeSentiment <- negativeSentiment[which.max(negativeSentiment$event_number_event_filtering),]
      } 
      if(dim(positiveSentiment)[1] >1){
        print("to investigate")
        positiveSentiment <- positiveSentiment[which.max(positiveSentiment$event_number_event_filtering),]
      } 
      
      rowToAdd <- negativeSentiment
      rowToAdd[,toMean] <- (positiveSentiment[,toMean] + negativeSentiment[,toMean])/2
      rowToAdd[,toSpread] <- (positiveSentiment[,toSpread] - negativeSentiment[,toSpread])
      rowToAdd$sentiment_criteria <- "SPREAD"
      return(rbind(df,rowToAdd))
    }
    return(df)
  }
  
  print("computing the spread")
  data_spread_augmented <- ddply(.data = data, .variables = joining_columns, .fun = function(x){computeSpread(x,mean_columns,spread_columns)})
  print("spread computed")
  data <- data_spread_augmented
  print(dim(data))
  
  # data$infinity_return <- abs(data$RET180)
  # data$infinity_confidence <- data$`180`
  ##########################################################
  #############################
  #############################
  #############################
  #############################
  ############################# Computing ranking metrics
  
  data$correcting_factor <- 2*(data$sentiment_criteria == "POSITIVE"  | data$sentiment_criteria == "SPREAD")-1
  #### first one
  # data$infinity_return <- (data$RET180)*data$correcting_factor
  #### second one
  # data$infinity_return <- (data$RET180)*(data$`180`)*data$correcting_factor
  #### third one
  stats_post_sign <- colnames(data)[which(as.numeric(colnames(data)) >= 0)]
  rets_post <- paste0("RET",stats_post_sign)
  # data$infinity_return <- rowSums(data[,rets_post]*data$correcting_factor*data[,stats_post_sign],na.rm = TRUE)
  
  
  infinity_return <- rowSums(data[,rets_post]*data$correcting_factor*data[,stats_post_sign],na.rm = TRUE)
  
  infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
  significance <- (data$event_number_event_filtering-min(data$event_number_event_filtering))/(max(data$event_number_event_filtering)-min(data$event_number_event_filtering))
  
  infinity_return_global <- infinity_return
  infinity_return <- infinity_return*significance
  
  
  data$infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
  
  data$infinity_return_global <- (infinity_return_global-min(infinity_return_global))/(max(infinity_return_global)-min(infinity_return_global))
  
  data$infinity_return[grepl("technical",data$my_event)] <- data$infinity_return[grepl("technical",data$my_event)]*0.01
  data$infinity_return[grepl("imbalance",data$my_event)] <- data$infinity_return[grepl("imbalance",data$my_event)]*0.01
  data$infinity_return[grepl("stock-loss",data$my_event)] <- data$infinity_return[grepl("stock-loss",data$my_event)]*0.01
  data$infinity_return[grepl("stock-gain",data$my_event)] <- data$infinity_return[grepl("stock-gain",data$my_event)]*0.01
  data$infinity_return[grepl("stock-prices",data$my_event)] <- data$infinity_return[grepl("stock-prices",data$my_event)]*0.01
  
  data$infinity_return_global[grepl("technical",data$my_event)] <- data$infinity_return_global[grepl("technical",data$my_event)]*0.01
  data$infinity_return_global[grepl("imbalance",data$my_event)] <- data$infinity_return_global[grepl("imbalance",data$my_event)]*0.01
  data$infinity_return_global[grepl("stock-loss",data$my_event)] <- data$infinity_return_global[grepl("stock-loss",data$my_event)]*0.01
  data$infinity_return_global[grepl("stock-gain",data$my_event)] <- data$infinity_return_global[grepl("stock-gain",data$my_event)]*0.01
  data$infinity_return_global[grepl("stock-prices",data$my_event)] <- data$infinity_return_global[grepl("stock-prices",data$my_event)]*0.01
  data$infinity_return_global[grepl("bankruptcy",data$my_event)] <- data$infinity_return_global[grepl("bankruptcy",data$my_event)]*0.01
  
  
  
  # RP_SaveDataFrame(data, outputDataPath = outputDataPath, filename = paste0("prod_spr",my_file_name))
  print("spread saved")
  
  
  
  
  
  
  
  
  ##############
  ##############
  ##############
  ##############  End of computing the spread sentiment category
  
  
  ##############
  ##############
  ##############
  ############## Ordering the best profiles
  
  data <- data[data$event_number_event_filtering >= 50,]
  print(dim(data))
  ##############
  print("Getting rid of the flat profiles")
  stats_sign <- colnames(data)[which(!is.na(as.numeric(colnames(data))))]
  ord <- paste0("ORD",stats_sign)
  rets <- paste0("RET",stats_sign)
  vol <- paste0("VOLU",stats_sign)
  vola <- paste0("VOLA",stats_sign)
  numeric_columns <- c(stats_sign,rets,ord,vol,vola)
  mean_columns <- c(stats_sign,ord,vol,vola)
  spread_columns <- c(rets)
  
  trimFlat <- function(row,rets){
    # print(row[,rets])
    numeric_rets <- row[,rets]
    numeric_rets[is.na(numeric_rets)] <- 0
    
    diff_rets <- diff(as.vector(as.matrix(numeric_rets)))
    
    nullNumber <- sum(abs(diff_rets) <= .Machine$double.eps)
    proportion <- nullNumber/dim(row)[2]
    if(proportion >= 0.05){
      return(row$Id)
    }
    return(NULL)
  }
  
  
  
  print("before")
  print(dim(data))
  
  data$Id <- 1:dim(data)[1]
  noFlatData  <- ddply(.data = data,.variables = "Id",.fun =  function(x){trimFlat(x, spread_columns)})
  data <- data[!(data$Id %in% noFlatData$Id),]
  data$Id <- NULL
  
  print("after")
  print(dim(data))
  print(dim(noFlatData))
  
  
  print("Done")
  
  
  
  
  
  
  
  
  
  
  ##############
  ##############
  ##############
  ############## computing other metrics
  
  
  print(dim(data))
  
  data$correcting_factor <- 2*(data$sentiment_criteria == "POSITIVE"  | data$sentiment_criteria == "SPREAD")-1
  
  #### first one
  # data$infinity_return <- (data$RET180)*data$correcting_factor
  #### second one
  # data$infinity_return <- (data$RET180)*(data$`180`)*data$correcting_factor
  #### third one
  stats_post_sign <- colnames(data)[which(as.numeric(colnames(data)) >= 0)]
  rets_post <- paste0("RET",stats_post_sign)
  # data$infinity_return <- rowSums(data[,rets_post]*data$correcting_factor*data[,stats_post_sign],na.rm = TRUE)
  
  
  data$post_ranked_return <- rowSums(data[,rets_post]*data$correcting_factor*data[,stats_post_sign],na.rm = TRUE)
  data$post_return <- rowSums(data[,rets_post]*data$correcting_factor,na.rm = TRUE)
  
  
  card_significance <- data$event_number_event_filtering
  
  
  card_significance <- (card_significance-min(card_significance))/(max(card_significance)-min(card_significance))
  
  data$card_post_return <- data$post_return*card_significance
  data$card_post_ranked_return <- data$post_ranked_return*card_significance
  
  ##################### post event metrics
  stats_post_sign <- colnames(data)[which(as.numeric(colnames(data)) >= 0)]
  rets_post <- paste0("RET",stats_post_sign)
  vola_post <- paste0("VOLA",stats_post_sign)
  volu_post <- paste0("VOLU",stats_post_sign)
  
  
  data$post_ranked_volatility <- (rowSums(data[,vola_post]*data[,stats_post_sign],na.rm = TRUE))
  data$post_volatility <- (rowSums(data[,vola_post],na.rm = TRUE))
  
  #####################
  #####################
  ##################### pre event metrics
  
  stats_pre_sign <- colnames(data)[which(as.numeric(colnames(data)) <0)]
  rets_pre <- paste0("RET",stats_pre_sign)
  vola_pre <- paste0("VOLA",stats_pre_sign)
  volu_pre <- paste0("VOLU",stats_pre_sign)
  stats_ord_pre <- paste0("ORD",stats_pre_sign)
  
  
  
  data$pre_ranked_return <- rowSums(data[,rets_pre]*data$correcting_factor*data[,stats_pre_sign],na.rm = TRUE)
  
  data$pre_return <- (rowSums(data[,rets_pre]*data$correcting_factor,na.rm = TRUE))
  
  data$card_pre_return <- data$pre_return*card_significance
  data$card_pre_ranked_return <- data$pre_ranked_return*card_significance
  
  
  data$pre_ranked_volatility <- (rowSums(data[,vola_pre]*data[,stats_pre_sign],na.rm = TRUE))
  data$pre_volatility <- (rowSums(data[,vola_pre],na.rm = TRUE))
  
  
  #####################
  #####################
  ##################### pre post event metrics
  data$volatility_correction <- data$post_volatility/data$pre_volatility
  data$ranked_volatility_correction <- data$post_ranked_volatility/data$pre_ranked_volatility
  data$return_correction <- data$post_return/data$pre_return
  data$ranked_return_correction <- data$post_ranked_return/data$pre_ranked_return
  
  data$card_return_correction <- card_significance*data$post_return/data$pre_return
  data$card_ranked_return_correction <- card_significance*data$post_ranked_return/data$pre_ranked_return
  
  data$infinity_return <- data$card_post_return 
  data$RANKING <-  data$card_post_return 
  
  RP_SaveDataFrame(data, outputDataPath = outputDataPath, filename = paste0("metrics_clean_prod", my_file_name))
  
  print("All metrics computed")
  if(is.null(aggregated_data)){
    aggregated_data <- data
  } else {
    aggregated_data <- rbind(aggregated_data,data)
  }
}

RP_SaveDataFrame(aggregated_data, outputDataPath = outputDataPath, filename = "all_lapses_metrics_clean_prod")