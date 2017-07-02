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
vol_prepost <- paste0("VOL",all_num)

rescale_all <- function(row){
  translatingFactor <- (1.2-min(row))
  row <- row+translatingFactor
  # print(row)
  return(row)
}

print("rescaling")
replacement <- as.data.frame(t(apply( data[,vola_prepost],1,rescale_all)))
data[,vola_prepost] <- replacement

print(head(replacement))
print(tail(replacement))
