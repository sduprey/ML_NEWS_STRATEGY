# Data collection and flat file saving for all graphics
library("RPToolsDB")
library("abind")
library("RPPlotUtils")
library("RPostgreSQL")
library("sqldf")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")

# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

Sys.setenv(TZ = "UTC")



data <- readRDS(file=paste0(outputDataPath,"new_spr_r1000_rpna_abvol_abvol_corrado_df.rds"))
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


stats_sign <- colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))]
rets <- paste0("RET",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
ord_stats_sign <- paste0("ORD",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
vol_stats_sign <- paste0("VOLU",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
vola_stats_sign <- paste0("VOLA",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])


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
RP_SaveDataFrame(dataGroupDT, outputDataPath = outputDataPath, filename = "new_rpna_best_profile_group_ordered_r1000_corrado_df")

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
RP_SaveDataFrame(dataCategoryDT, outputDataPath = outputDataPath, filename = "new_rpna_best_profile_category_ordered_r1000_corrado_df")
print("best profiles computed")

