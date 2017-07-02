### Mapping categories / group




# Data collection and flat file saving for all graphics
library("RPToolsDB")

library("abind")
library("RPPlotUtils")
library("RPostgreSQL")
library("sqldf")
library("lubridate")

# RavenPack
library("RPQuantUtils")
library("RPToolsDB")
library("RPIndicatorCreation")
library("RPStatisticalFitting")
library('RPBackTesting')
library('RPJIRAUtils')
library('RPMLUtils')
library('RPPlotUtils')
library("RPQuantUtils")
library("RPToolsDB")
library("RPIndicatorCreation")
library("RPStatisticalFitting")
library('RPBackTesting')
library('RPJIRAUtils')
library('RPMLUtils')
library('RPPlotUtils')
Sys.setenv(TZ = "UTC")


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



mapping_group_category_request <- 'SELECT distinct "GROUP", CATEGORY FROM analytics.rbda_100_analytics_beta_1 group by "GROUP", CATEGORY'

dbCon<-RP_DatabaseConnect(user)
MappingGroupCategory_RPData <- dbGetQuery(dbCon, mapping_group_category_request)

RP_SaveDataFrame(dataframe = MappingGroupCategory_RPData,outputDataPath =outputDataPath ,filename = "bigdata_group_category")
