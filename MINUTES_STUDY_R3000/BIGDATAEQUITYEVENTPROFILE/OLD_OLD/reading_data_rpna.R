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



# ####################
# ####################
# ####################
# ####################
# #################### Merging to get the two statistical tests
# data <- readRDS(file=paste0(outputDataPath,"src_r1000_corrado_df.rds"))
# stats_sign <- colnames(data)[which(!is.na(as.numeric(colnames(data))))]
# rets <- paste0("RET",colnames(data)[which(!is.na(as.numeric(colnames(data))))])
# joining_columns <- setdiff(colnames(data), c(stats_sign,rets))
# joining_columns_stats_test <- setdiff(joining_columns, "corrado_methodo")
# dataDT <- as.data.table(data)
# 
# stackStatsTest <- function(df){
#   print(dim(df))
# }
# 
# corradoAbnDT <- dataDT[,stackStatsTest,by=joining_columns_stats_test]
# 
# print(joining_columns)

# ####################
# ####################
# ####################
# ####################
# #################### End of merging the two statist


# ####################
# ####################
# ####################
# ####################
# #################### Merging to get the best
# data2 <- readRDS(file=paste0(outputDataPath,"r2000_corrado_df.rds"))
# # data1 <- readRDS(file=paste0(outputDataPath,"r1000_corrado_df.rds"))
# data1 <- readRDS(file=paste0(outputDataPath,"src_r1000_corrado_df.rds"))
# 
# data1$RET180[is.na(data1$RET180)] <- 0
# data2$RET180[is.na(data2$RET180)] <- 0
# 
# data1$RET180 <- rnorm(n=dim(data1)[1])
# stats_sign <- colnames(data2)[which(!is.na(as.numeric(colnames(data2))))]
# rets <- paste0("RET",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
# joining_columns <- setdiff(colnames(data2), c(stats_sign,rets))
# 
# data <- merge(data1,data2, all.x = TRUE, by= joining_columns,suffixes=c('_r1000','_r2000'))
# print(dim(data))
# print(colnames(data))
# 
# 
# selectTheBest <- function(row){
#   to_keep <- NULL
#   if (row[,"sentiment_criteria"] == "POSITIVE" | row[,"sentiment_criteria"] == "ALL" ){
#     if( row[,"RET180_r1000"] >= row[,"RET180_r2000"] ){
#       to_keep <- grepl('_r1000' ,colnames(row))
#     } else {
#       to_keep <- grepl('_r2000' ,colnames(row))
#     }
#   } else {
#     if( row[,"RET180_r1000"] >= row[,"RET180_r2000"] ){
#       to_keep <- grepl('_r2000' ,colnames(row))    
#     } else {
#       to_keep <- grepl('_r1000' ,colnames(row))
#     }
#   }
#   toReturn <- row[,to_keep]
#   colnames(toReturn) <- gsub("_r1000","",colnames(toReturn))
#   colnames(toReturn) <- gsub("_r2000","",colnames(toReturn))
#   return(toReturn)
#  
# }
# 
# print("selecting best row by row")
# dataBest <- ddply(.data = data, .variables = joining_columns, .fun = selectTheBest)
# print("end of selection")
# 
# print(dim(dataBest))

############
############
############
############ my functions



##############
##############
##############
############## Computing the spread sentiment category
bootstrap_CI <- FALSE
data <- readRDS(file=paste0(outputDataPath,bootstrap_CI,"r1000_rpna_abvol_abvol_corrado_df.rds"))

data$localSource[data$localSource == "DJ"] <- "DJPR"
data$localSource[data$localSource == "PREMIUM"] <- "PREMIUM_PACK"
data$localSource[data$localSource == "WEBNONPREMIUM"] <- "WEB_NON_PREMIUM"

print(dim(data))

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
RP_SaveDataFrame(data_spread_augmented, outputDataPath = outputDataPath, filename = "spr_r1000_rpna_abvol_abvol_corrado_df")
print("spread saved")


# test <- data[data$my_event == "analyst-ratings" & data$similarity_gap_filter == 1 & data$localSource == "DJ",]
# test[,joining_columns]
# test <- data[data$my_event == "analyst-rating" & data$similarity_gap_filter == "1" & data$localSource == "DJ",]

##############
##############
##############
##############  End of computing the spread sentiment category


##############
##############
##############
############## Ordering the best profiles

data <- readRDS(file=paste0(outputDataPath,"spr_r1000_rpna_abvol_abvol_corrado_df.rds"))
data <- data[data$event_number_event_filtering >= 50,]


# data$infinity_return <- abs(data$RET180)
# data$infinity_confidence <- data$`180`

data$correcting_factor <- 2*(data$sentiment_criteria == "POSITIVE"  | data$sentiment_criteria == "SPREAD")-1
data$correcting_factor[data$sentiment_criteria == "ALL"] <- 0
#### first one
# data$infinity_return <- (data$RET180)*data$correcting_factor
#### second one
# data$infinity_return <- (data$RET180)*(data$`180`)*data$correcting_factor
#### third one
stats_post_sign <- colnames(data)[which(as.numeric(colnames(data)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)
data$infinity_return <- rowSums(data[,rets_post]*data$correcting_factor*data[,stats_post_sign],na.rm = TRUE)


####### all generics

all_group_events <- sort(unique(data$my_event[data$aggregate_criteria == "GROUP"]))
all_category_events <- sort(unique(data$my_event[data$aggregate_criteria == "CATEGORY"]))

RP_SaveDataFrame(all_group_events, outputDataPath = outputDataPath, filename = "rpna_all_group_events")
RP_SaveDataFrame(all_category_events, outputDataPath = outputDataPath, filename = "rpna_all_category_events")


###########
########### Fro groups
dataGroup <- data[data$aggregate_criteria =="GROUP",]

dataGroupDT <- as.data.table(dataGroup)
selectBestProfile <- function(discriminating_value){
  
  return(max(discriminating_value))
}
print(dim(dataGroupDT))
# setkey(dataDT,"my_event")
dataGroupDT[,max_prof := selectBestProfile(infinity_return),by=c("my_event")]
print(dim(dataGroupDT))
dataGroupDT <- as.data.frame(unique(dataGroupDT[infinity_return >= max_prof,]))
print(dim(dataGroupDT))
dataGroupDT <- unique(dataGroupDT[,c("my_event","infinity_return")])
dataGroupDT <- dataGroupDT[order(dataGroupDT$infinity_return,decreasing = TRUE),]
RP_SaveDataFrame(dataGroupDT, outputDataPath = outputDataPath, filename = "rpna_best_profile_group_ordered_r1000_corrado_df")

###########
########### For categories
dataCategory <- data[data$aggregate_criteria =="CATEGORY",]

dataCategoryDT <- as.data.table(dataCategory)
selectBestProfile <- function(discriminating_value){
  
  return(max(discriminating_value))
}
print(dim(dataCategoryDT))
# setkey(dataDT,"my_event")
dataCategoryDT[,max_prof := selectBestProfile(infinity_return),by=c("my_event")]
print(dim(dataCategoryDT))
dataCategoryDT <- as.data.frame(unique(dataCategoryDT[infinity_return >= max_prof,]))
print(dim(dataCategoryDT))
dataCategoryDT <- unique(dataCategoryDT[,c("my_event","infinity_return")])
dataCategoryDT <- dataCategoryDT[order(dataCategoryDT$infinity_return,decreasing = TRUE),]
RP_SaveDataFrame(dataCategoryDT, outputDataPath = outputDataPath, filename = "rpna_best_profile_category_ordered_r1000_corrado_df")
print("best profiles computed")



##############
##############
##############
############## end of ordering the best profiles

