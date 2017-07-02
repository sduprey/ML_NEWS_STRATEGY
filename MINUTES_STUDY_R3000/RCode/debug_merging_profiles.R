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

source("./RCode/RP_Utilities.R")
source("./RCode/RP_BigData_EventStudy_Utilities.R")

indiceType <- "R1000"

newly_processed_profiles <- readRDS(file = paste0(outputDataPath,"debugging_update_profiles.rds"))


RDSFileName = paste0(RP_GetUpdatedDBPath(user = user, region = 'US', type = "INDICE_MINUTES", Level = "Production"), "_",paste0("PROFILES_",indiceType), ".rds")
historicalProfilesData <- readRDS(file = RDSFileName)
print(dim(historicalProfilesData))
print(dim(newly_processed_profiles))

###### petty calculation to match the historic : to withdraw
newly_processed_profiles$correcting_factor <- 2*(newly_processed_profiles$sentiment_criteria == "POSITIVE"  | newly_processed_profiles$sentiment_criteria == "SPREAD")-1
stats_post_sign <- colnames(newly_processed_profiles)[which(as.numeric(colnames(newly_processed_profiles)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)
infinity_return <- rowSums(newly_processed_profiles[,rets_post]*newly_processed_profiles$correcting_factor*newly_processed_profiles[,stats_post_sign],na.rm = TRUE)
infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
significance <- (newly_processed_profiles$event_number_event_filtering-min(newly_processed_profiles$event_number_event_filtering))/(max(newly_processed_profiles$event_number_event_filtering)-min(newly_processed_profiles$event_number_event_filtering))
infinity_return_global <- infinity_return
infinity_return <- infinity_return*significance
newly_processed_profiles$infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
newly_processed_profiles$infinity_return_global <- (infinity_return_global-min(infinity_return_global))/(max(infinity_return_global)-min(infinity_return_global))

print("Merging the profiles here")

oldColumns <- colnames(historicalProfilesData)
# print(identical(colnames(historicalProfilesData),colnames(newly_processed_profiles)))

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
stats_sign <- colnames(historicalProfilesData)[which(!is.na(as.numeric(colnames(historicalProfilesData))))]

ords <- paste0("ORD",stats_sign)
rets <- paste0("RET",stats_sign)
volas <- paste0("VOLA",stats_sign)
volus <- paste0("VOLU",stats_sign)

additional_col_to_drop <- c("corrado_methodo","event_number_event_filtering","lapse",
                            "correcting_factor","infinity_return","infinity_return_global",
"post_ranked_return","post_return","card_post_return","card_post_ranked_return","post_ranked_volatility","post_volatility",              
"pre_ranked_return","pre_return","card_pre_return","card_pre_ranked_return", "pre_ranked_volatility","pre_volatility",              
"volatility_correction","ranked_volatility_correction","return_correction","ranked_return_correction","card_return_correction","card_ranked_return_correction",
"RANKING")

joining_columns <- setdiff(colnames(historicalProfilesData), c(c(stats_sign,ords,rets,volas,volus),additional_col_to_drop))

# new_columns <- paste0("NEW", colnames(newly_processed_profiles)[which(!(colnames(newly_processed_profiles) :%in% joining_columns))])

newData <- paste0("NEW",c(stats_sign,ords,rets,volas,volus))

colnames(newly_processed_profiles)[which(!(colnames(newly_processed_profiles) %in% joining_columns))] <- paste0("NEW", colnames(newly_processed_profiles)[which(!(colnames(newly_processed_profiles) %in% joining_columns))])

print("colliding datasets")
MergedProfilesData <- merge(historicalProfilesData,newly_processed_profiles,all.x = TRUE, by = joining_columns)

mergeProfiles <- function(row){
  newCount <- row[,"NEWevent_number_event_filtering"]
  if(!is.na(newCount)){
    oldCount <- row[,"event_number_event_filtering"]
    
    stats_sign <- colnames(row)[which(!is.na(as.numeric(colnames(row))))]
    ords <- paste0("ORD",stats_sign)
    rets <- paste0("RET",stats_sign)
    volas <- paste0("VOLA",stats_sign)
    volus <- paste0("VOLU",stats_sign)
    
    new_ords <- paste0("NEW", ords)
    new_stats_sign <- paste0("NEW", stats_sign)
    new_rets <- paste0("NEW", rets)
    new_volas <- paste0("NEW", volas)
    new_volus <- paste0("NEW", volus)
    
    weightnew <- newCount/(newCount+oldCount)
    weightold <- oldCount/(newCount+oldCount)
    
    row[,ords]       <- weightold*row[,ords] + weightnew*row[,new_ords]
    row[,rets]       <- weightold*row[,rets] + weightnew*row[,new_rets]
    row[,volas]      <- weightold*row[,volas] + weightnew*row[,new_volas]
    row[,volus]      <- weightold*row[,volus] + weightnew*row[,new_volus]
    row[,stats_sign] <- weightold*row[,stats_sign] + weightnew*row[,new_stats_sign]
  }
  return(row)
}

print("Updating profiles")
UpdatedProfiles <- ddply(.data = MergedProfilesData, .variables = joining_columns, .fun = function(x){mergeProfiles(x)})

if(recreateArchive){
  RDSFileName = paste0(RP_GetUpdatedDBPath(user = user, region = 'US', type = "INDICE_MINUTES", Level = "Production"), "_",paste0("PROFILES_",indiceType), ".rds")
  saveRDS(object = UpdatedProfiles, file = RDSFileName)
}

UpdatedProfiles <- UpdatedProfiles[,oldColumns]
