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
##############
############## Computing the spread sentiment category
bootstrap_CI <- FALSE

# datag <- readRDS(file=paste0(outputDataPath,bootstrap_CI,"prodgp_r1000_bigdata_abvol_abvol_corrado_df.rds"))
# data <- rbind(data,datag)
# data <- readRDS(file=paste0(outputDataPath,bootstrap_CI,"prod_r1000_bigdata_abvol_abvol_corrado_df.rds"))
data <- readRDS(file=paste0(outputDataPath,bootstrap_CI,"lprod_r1000_bigdata_abvol_abvol_corrado_df.rds"))

# data <- readRDS(file=paste0(outputDataPath,bootstrap_CI,"tmp_prod_r1000_bigdata_abvol_abvol_corrado_df.rds"))

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





RP_SaveDataFrame(data, outputDataPath = outputDataPath, filename = "prod_spr_r1000_bigdataf_abvol_abvol_corrado_df")
print("spread saved")








##############
##############
##############
##############  End of computing the spread sentiment category


##############
##############
##############
############## Ordering the best profiles

data <- readRDS(file=paste0(outputDataPath,"prod_spr_r1000_bigdataf_abvol_abvol_corrado_df.rds"))
# data <- data[data$event_number_event_filtering >= 50,]
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


RP_SaveDataFrame(data, outputDataPath = outputDataPath, filename = "clean_prod_spr_r1000_bigdataf_abvol_abvol_corrado_df")
print("clean saved")


##############
##############
##############
############## end of ordering the best profiles

##############
##############
##############
##############  Filling up blank Corrado tests
data <- readRDS(file=paste0(outputDataPath,"clean_prod_spr_r1000_bigdataf_abvol_abvol_corrado_df.rds"))
# stats_post_sign <- colnames(data)[which(as.numeric(colnames(data))>= -180  )]
# ord_post <- paste0("ORD",stats_post_sign)
# replacement <- apply( data[,stats_post_sign],2,function(x){mean(x,na.rm = TRUE)})
# for (rowIndex in 1:dim(data)[1]){
#   rowProfile <- data[rowIndex,]
#   corradoFilling <- (sum(as.vector(as.numeric(is.na(rowProfile[,stats_post_sign]),na.rm = TRUE))))
#   ordinFilling <- (sum(as.vector(as.numeric(is.na(rowProfile[,ord_post]),na.rm = TRUE))))
# #   if(corradoFilling > 0){
# #     print(paste0("row index",rowIndex))
# #     print(paste0("Corrado filling",corradoFilling))
# #     print(paste0("Ordin filling",ordinFilling))
# #   }
#   
# 
#   if(corradoFilling == length(stats_post_sign) && ordinFilling == length(stats_post_sign)){
#     print("Problem")
#     print(paste0("row index",rowIndex))
#     data[rowIndex,stats_post_sign] <- replacement
#   } else {
#     data[rowIndex,stats_post_sign] <-  data[rowIndex,ord_post]
#   }
#   
# }

all_num <- colnames(data)[which(!is.na(as.numeric(colnames(data))))]
vola_prepost <- paste0("VOLA",all_num)
vol_prepost <- paste0("VOLU",all_num)

rescale_all <- function(row){
  print(row)
  return(row)
}

rescale_all <- function(row){
  translatingFactor <- (1.2-min(row))
  row <- row+translatingFactor
  # print(row)
  return(row)
}

print("rescaling")
replacement <- as.data.frame(t(apply( data[,vola_prepost],1,rescale_all)))
data[,vola_prepost] <- replacement


replacement <- as.data.frame(t(apply( data[,vol_prepost],1,rescale_all)))
data[,vol_prepost] <- replacement


RP_SaveDataFrame(data, outputDataPath = outputDataPath, filename = "cor_clean_clean_prod_spr_r1000_bigdataf_abvol_abvol_corrado_df")
print("clean corrado")


##############
##############
##############
############## computing other metrics

data <- readRDS(file=paste0(outputDataPath,"cor_clean_clean_prod_spr_r1000_bigdataf_abvol_abvol_corrado_df.rds"))

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


data$post_ranked_return <- rowSums(data[,rets_post]*data$correcting_factor,na.rm = TRUE)
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


data$post_ranked_volatility <- (rowSums(data[,vola_post],na.rm = TRUE))
data$post_volatility <- (rowSums(data[,vola_post],na.rm = TRUE))

#####################
#####################
##################### pre event metrics

stats_pre_sign <- colnames(data)[which(as.numeric(colnames(data)) <0)]
rets_pre <- paste0("RET",stats_pre_sign)
vola_pre <- paste0("VOLA",stats_pre_sign)
volu_pre <- paste0("VOLU",stats_pre_sign)
stats_ord_pre <- paste0("ORD",stats_pre_sign)



data$pre_ranked_return <- rowSums(data[,rets_pre]*data$correcting_factor,na.rm = TRUE)

data$pre_return <- (rowSums(data[,rets_pre]*data$correcting_factor,na.rm = TRUE))

data$card_pre_return <- data$pre_return*card_significance
data$card_pre_ranked_return <- data$pre_ranked_return*card_significance


data$pre_ranked_volatility <- (rowSums(data[,vola_pre],na.rm = TRUE))
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

RP_SaveDataFrame(data, outputDataPath = outputDataPath, filename = "metrics_clean_prod_spr_r1000_bigdataf_abvol_abvol_corrado_df")
print("metrics saved")



print("All metrics computed")

# ###### investigating
# print("investigating")
# 
# data <- data[data$my_event == "acquisitions-mergers",]
# head(data[,c("my_event","sentiment_criteria", "pure_return",  "ranked_return",  "card_pure_return", "card_ranked_return")])
# print("done")
# 
# 

##########################################################
#############################
#############################
#############################
#############################
############################# Group category best profiles



# 

########################################################################################################################################################################################################################
##################################################################################################################################################################
########################################################################################################################################################################################################################
##################################################################################################################################################################
############# Ordering globally the best profiles
########################################################################################################################################################################################################################
##################################################################################################################################################################
########################################################################################################################################################################################################################
##################################################################################################################################################################

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

RP_SaveDataFrame(dataGroupBest, outputDataPath = outputDataPath, filename = "clean_prod_bigdataf_best_profile_group_ordered_r1000_corrado_df")




#########
########### For categories
dataCategory <- data[data$aggregate_criteria =="CATEGORY",]

print("before")
print(dim(dataCategory))

dataCategoryBest  <- ddply(.data = dataCategory,.variables = "my_event",.fun =  trimBest)

print("after")
print(dim(dataCategoryBest))
dataCategoryBest <- dataCategoryBest[order(dataCategoryBest$infinity_return,decreasing = TRUE),]

RP_SaveDataFrame(dataCategoryBest, outputDataPath = outputDataPath, filename = "clean_prod_bigdataf_best_profile_category_ordered_r1000_corrado_df")

print("best profiles computed")

########################################################################################################################################################################################################################
##################################################################################################################################################################
########################################################################################################################################################################################################################
##################################################################################################################################################################
############# End of Ordering globally the best profiles
########################################################################################################################################################################################################################
##################################################################################################################################################################
########################################################################################################################################################################################################################
##################################################################################################################################################################

########################
########################
########################
########################
######################## Computing best profiles

####### all generics
data <- readRDS(file=paste0(outputDataPath,"metrics_clean_prod_spr_r1000_bigdataf_abvol_abvol_corrado_df.rds"))

my_metrics <- 
  c(
    "card_post_return",
    "card_post_ranked_return",
    "post_return",
    "post_ranked_return",
    "post_volatility",
    "post_ranked_volatility",
    "card_pre_return",
    "card_pre_ranked_return",
    "pre_return",
    "pre_ranked_return",
    "pre_volatility",
    "pre_ranked_volatility",
    "volatility_correction",
    "ranked_volatility_correction",
    "return_correction",
    "ranked_return_correction",
    "card_return_correction",
    "card_ranked_return_correction"
  )

dataGroup <- data[data$aggregate_criteria =="GROUP",]
dataCategory <- data[data$aggregate_criteria =="CATEGORY",]
trimBest <- function(dfrow,metrics_to_use){
  trimmed_df <- dfrow[dfrow[,metrics_to_use] >= max(dfrow[,metrics_to_use]),]
  return(trimmed_df[1,])
}
for(my_metric in my_metrics){
  print(my_metric)
  print("before")
  print(dim(dataGroup))
  
  dataGroupBest  <- ddply(.data = dataGroup,.variables = "my_event",.fun = function(x){trimBest(x,my_metric)})
  print("after")
  print(dim(dataGroupBest))
  dataGroupBest$RANKING <- dataGroupBest[,my_metric]
  dataGroupBest <- dataGroupBest[order(dataGroupBest$RANKING,decreasing = TRUE),]
  
  gpfilename <- paste0(my_metric,"bigdataf_best_group")
  #   if (gpfilename == "post_returnbigdataf_best_group" ){
  #     print("found")
  #   }
  
  print(gpfilename)
  dataCategoryBest  <- ddply(.data = dataCategory,.variables = "my_event",.fun = function(x){trimBest(x,my_metric)})
  dataCategoryBest$RANKING <- dataCategoryBest[,my_metric]
  
  
  dataCategoryBest <- dataCategoryBest[order(dataCategoryBest$RANKING,decreasing = TRUE),]
  catfilename <- paste0(my_metric,"bigdataf_best_category")
  
  
  #   dataCategoryBest$RANKING[grepl("technical",dataCategoryBest$my_event)] <- dataCategoryBest$RANKING[grepl("technical",dataCategoryBest$my_event)]*0.01
  #   dataCategoryBest$RANKING[grepl("imbalance",dataCategoryBest$my_event)] <- dataCategoryBest$RANKING[grepl("imbalance",dataCategoryBest$my_event)]*0.01
  #   dataCategoryBest$RANKING[grepl("stock-loss",dataCategoryBest$my_event)] <- dataCategoryBest$RANKING[grepl("stock-loss",dataCategoryBest$my_event)]*0.01
  #   dataCategoryBest$RANKING[grepl("stock-gain",dataCategoryBest$my_event)] <- dataCategoryBest$RANKING[grepl("stock-gain",dataCategoryBest$my_event)]*0.01
  #   dataCategoryBest$RANKING[grepl("stock-prices",dataCategoryBest$my_event)] <- dataCategoryBest$RANKING[grepl("stock-prices",dataCategoryBest$my_event)]*0.01
  #   
  #   dataGroupBest$RANKING_global[grepl("technical",dataGroupBest$my_event)] <- dataGroupBest$RANKING_global[grepl("technical",dataGroupBest$my_event)]*0.01
  #   dataGroupBest$RANKING_global[grepl("imbalance",dataGroupBest$my_event)] <- dataGroupBest$RANKING_global[grepl("imbalance",dataGroupBest$my_event)]*0.01
  #   dataGroupBest$RANKING_global[grepl("stock-loss",dataGroupBest$my_event)] <- dataGroupBest$RANKING_global[grepl("stock-loss",dataGroupBest$my_event)]*0.01
  #   dataGroupBest$RANKING_global[grepl("stock-gain",dataGroupBest$my_event)] <- dataGroupBest$RANKING_global[grepl("stock-gain",dataGroupBest$my_event)]*0.01
  #   dataGroupBest$RANKING_global[grepl("stock-prices",dataGroupBest$my_event)] <- dataGroupBest$RANKING_global[grepl("stock-prices",dataGroupBest$my_event)]*0.01
  #   
  #   
  
  RP_SaveDataFrame(dataGroupBest, outputDataPath = outputDataPath, filename = gpfilename)
  RP_SaveDataFrame(dataCategoryBest, outputDataPath = outputDataPath, filename = catfilename)
  print(catfilename)
}
###########
########### Fro groups




print("best profiles computed")

