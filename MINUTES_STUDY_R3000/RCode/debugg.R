
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
load(file = paste0(outputDataPath, "debugKernel.RData"))

print(Day)
print(dim(EventsDT))
print(dim(MinutesDT))
print(dim(MinutesIndiceDT))

minute_event_df <- RP_CalibrateDailyMinuteBetaMarket(Day, EventsDT, MinutesDT, MinutesIndiceDT)

print(dim(minute_event_df))
