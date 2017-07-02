# Data collection and flat file saving for all graphics
library("RPToolsDB")
library("abind")
library("RPPlotUtils")
library("boot")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")

# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

source("./RCode/RP_Utilities.R")
source("./RCode/RP_BigData_EventStudy_Utilities.R")

my_count <- 0

print("2007_1")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2007_1_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)

print("2007_2")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2007_2_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))

print("2008_1")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2008_1_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))

print("2008_2")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2008_2_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))

#   
print("2009_1")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2009_1_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))

print("2009_2")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2009_2_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))


print("2010_1")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2010_1_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))


print("2010_2")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2010_2_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))


print("2011")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2011_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))


print("2012_1")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2012_1_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))
print("2012_2")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2012_2_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))

print("2013_1")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2013_1_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))
#     print("2013_2")
#     total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2013_2_bigdata_events.rds"))
#     total_processed_minute_event_df <- filterDataLight(total_processed_minute_event_df,my_source, aggregate_criteria,my_event_relevance_combo,similarity_gap_filter,sentiment_criteria)
#     total_processed_minute_event_df <- rbind(total_processed_minute_event_df,total_processed_minute_event_df)
#   
print("2014_1")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2014_1_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))

print("2014_2")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2014_2_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))

print("2015_1")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2015_1_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))


print("2015_2")
total_processed_minute_event_df <- readRDS(file = paste0(outputDataPath,"2015_2_bigdata_events.rds"))
total_processed_minute_event_df$profile_quality <- rowSums(is.na(total_processed_minute_event_df[,as.character(1:361)]))
total_processed_minute_event_df$to_keep_first <- (total_processed_minute_event_df$profile_quality != 361)
# my_count <- my_count+ sum(total_processed_minute_event_df$to_keep_first)
my_count <- my_count+ dim(total_processed_minute_event_df)[1]
print(my_count)
print(rm(total_processed_minute_event_df))