# Data collection and flat file saving for all graphics
library("RPToolsDB")
library("abind")
library("RPPlotUtils")
library("RPostgreSQL")
library("sqldf")
library("utils")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")

# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

Sys.setenv(TZ = "UTC")

# source("./RCode/RP_Utilities.R")
# source("./RCode/RP_BigData_EventStudy_Utilities.R")

# # save(day, CompanyRPDataDaily, rday_minute_data_RPData, indice_minutes_daily_RPData, minute_range,file = paste0(outputDataPath,"damnedBug.RData"))
load(file = paste0(outputDataPath,"damnedBug.RData"))
minute_event_df <- RPDailyMinutesEventMatrixBetaMarketConstruction(Day =day, EventsDF = CompanyRPDataDaily, MinutesDF = rday_minute_data_RPData, MinutesIndiceDF = indice_minutes_daily_RPData,  minute_range = minute_range)


save(Day,  EventsDT, MinutesDT, MinutesIndiceDT,file = paste0(outputDataPath,"damnedBugBis.RData"))
load(file = paste0(outputDataPath,"damnedBugBis.RData"))
RP_CalibrateDailyMinuteBetaMarket(Day,  EventsDT, MinutesDT, MinutesIndiceDT)
    