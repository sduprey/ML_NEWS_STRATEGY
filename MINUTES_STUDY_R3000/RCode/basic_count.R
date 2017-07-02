##### Basic ount of events per category
###########################################
#####        PACKAGES LOADING         #####
###########################################
# General purpose
# library("lubridate")
# library("zoo")
# library("plyr")
# library("dplyr")
# library('car')
# library('rgl')
# library(plotly)

# RavenPack
library("RPQuantUtils")
library("RPToolsDB")
# library("RPIndicatorCreation")
# library("RPStatisticalFitting")
# library('RPBackTesting')
# library('RPJIRAUtils')
# library(RPMLUtils)
# library(RPPlotUtils)
# library(RPMappingTools)
Sys.setenv(TZ = "UTC")



###########################################
#####         PROJECT INFO            #####
###########################################
# User name
user = 'sduprey'
# region
region = "US"
# CAP
CAP = "R1000"
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path

# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

# ------------------------------------------------------------------------
Pull <- FALSE
# ------------------------------------------------------------------------

# Categories by group by year RBDA
if (Pull){
  DBQuery <- paste0("SELECT distinct category, COUNT(*)
                    FROM analytics.rbda_100_analytics_beta_1
                    WHERE to_char(timestamp_utc,'hh24:mi') >= '15:30' and to_char(timestamp_utc,'hh24:mi') <= '20:00'
                    GROUP by category
                    ORDER by category")
  
  
  dbCon <- RP_DatabaseConnect(user)
  CategoriesDT <- dbGetQuery(dbCon,DBQuery)
  RP_DatabaseDisconnect(dbCon)
  
  colnames(CategoriesDT) <- toupper(colnames(CategoriesDT))
  CategoriesDT <- as.data.table(CategoriesDT)
  saveRDS(CategoriesDT, paste0(outputDataPath,"BigCategoriesDT.rds"))
  print("Big saved")

  DBQuery <- paste0("SELECT distinct category, COUNT(*)
                    FROM analytics.rpna_400_eqt
                    WHERE to_char(timestamp_utc,'hh24:mi') >= '15:30' and to_char(timestamp_utc,'hh24:mi') <= '20:00'
                    GROUP by category
                    ORDER by category")
  
  
  dbCon <- RP_DatabaseConnect(user)
  CategoriesDT <- dbGetQuery(dbCon,DBQuery)
  RP_DatabaseDisconnect(dbCon)
  
  colnames(CategoriesDT) <- toupper(colnames(CategoriesDT))
  CategoriesDT <- as.data.table(CategoriesDT)
  saveRDS(CategoriesDT, paste0(outputDataPath,"RPNACategoriesDT.rds"))
  print("RPNA saved")
  
  

}else{
  BigCategoriesDT <- readRDS(paste0(outputDataPath,"BigCategoriesDT.rds"))
  RPNACategoriesDT <- readRDS(paste0(outputDataPath,"RPNACategoriesDT.rds"))
  setnames(BigCategoriesDT, "COUNT", "BIG_COUNT")
  setnames(RPNACategoriesDT, "COUNT", "RPNA_COUNT")
  ComparisonCategory <- merge(BigCategoriesDT,RPNACategoriesDT,by="CATEGORY")
}

########### counting my files
# print("Running version : close excluded")
# total_processed_minute_event_dfWhole <- readRDS(file = paste0(outputDataPath,"ALLtmp_events.rds"))
# 
# total_processed_minute_event2010_dfWhole <- readRDS(file = paste0(outputDataPath,"ALL_2010events.rds"))
# total_processed_minute_event_dfWhole <- rbind(total_processed_minute_event_dfWhole,total_processed_minute_event2010_dfWhole)
# rm(total_processed_minute_event2010_dfWhole)
# total_processed_minute_event2012_dfWhole <- readRDS(file = paste0(outputDataPath,"ALL_2012events.rds"))
# total_processed_minute_event_dfWhole <- rbind(total_processed_minute_event_dfWhole,total_processed_minute_event2012_dfWhole)
# rm(total_processed_minute_event2012_dfWhole)
# 
# print("Total dimension")
# print(dim(total_processed_minute_event_dfWhole))
# 
# saveRDS(count(total_processed_minute_event_dfWhole$CATEGORY), file=paste0(outputDataPath,"count_big.rds"))
# 
# 
# total_processed_minute_event_dfWhole <- readRDS(file = paste0(outputDataPath,"NewAbVolAbVolR1000From2007_total_processed_minute_event_beta_market_df.rds"))
# saveRDS(count(total_processed_minute_event_dfWhole$CATEGORY), file=paste0(outputDataPath,"count_rpna.rds"))


count_rpna <- readRDS(file=paste0(outputDataPath,"count_rpna.rds"))
colnames(count_rpna) <- c("CATEGORY","COUNT_RPNA")
count_big <- readRDS(file=paste0(outputDataPath,"count_big.rds"))
colnames(count_big) <- c("CATEGORY","COUNT_BIG")

Comparisoncount_rpna <- merge(count_big,count_rpna,by="CATEGORY")

count_rpna <- readRDS(file=paste0(outputDataPath,"count_group_rpna.rds"))
colnames(count_rpna) <- c("GROUP","COUNT_RPNA")
count_big <- readRDS(file=paste0(outputDataPath,"count_group_big.rds"))
colnames(count_big) <- c("GROUP","COUNT_BIG")

ComparisonGroupcount_rpna <- merge(count_big,count_rpna,by="GROUP")






#############################
#############################
#############################
#############################
#############################
############################# Advanced checking

# Data collection and flat file saving for all graphics

library("RPToolsDB")
library("abind")
library("RPPlotUtils")
library("RPostgreSQL")
library("sqldf")
library("RPQuantUtils")
library("RPToolsDB")

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




region = 'US'
CAP = 'R1000'

# startDate <- "2007-02-01"
# endDate <-  "2016-09-01"

startDate <- "2007-02-01"
endDate <-  "2016-09-01"


version ='RPNA'

# CustomFilter = "RELEVANCE >= 70"
CustomFilter = "ENS == 100 & RELEVANCE == 100"
# 
# customColumns <- 
#   c(
#     "RP_ENTITY_ID", 
#     "TIMESTAMP_UTC",       
#     "GROUP",                   
#     "CATEGORY",     
#     "RELEVANCE",            
#     "EVENT_SENTIMENT_SCORE",   
#     "EVENT_RELEVANCE", 
#     "EVENT_SIMILARITY_DAYS",              
#     "FACT_LEVEL",       
#     "RP_SOURCE_ID",      			
#     "PRODUCT_KEY"                  
#   )
customColumns <- 
  c(
    "RP_ENTITY_ID", 
    "TIMESTAMP_UTC",       
    "GROUP",                   
    "CATEGORY",     
    "RELEVANCE",            
    "ESS",   
    "G_ENS_SIMILARITY_GAP",              
    "RP_SOURCE_ID",      			
    "PRODUCT_KEY"                  
  )




print("Dealing with all groups together")

CompanyRPData <- RP_GetAnalyticsDataLocalDB(user = user, region = region, CAP = CAP, Groups = "analyst-ratings", startDate = startDate,endDate = endDate,version = version,CustomFilter = CustomFilter)#,colSubset =customColumns)
minute_range <- 180
excluding_range <- 0
###### Filtering the events which happen during intraday minutes only
library(lubridate)
TIMESTAMP_REGION <- as.POSIXlt(CompanyRPData$TIMESTAMP_UTC, "America/New_York")

TIMESTAMP_REGION_FORM <- ymd_hms(TIMESTAMP_REGION)
LOCAL_MARKET_HOUR <- hour(TIMESTAMP_REGION_FORM) 
LOCAL_MARKET_HOUR_MINUTE <- hour(TIMESTAMP_REGION_FORM) + minute(TIMESTAMP_REGION_FORM)/60

# print("Filtering events whose minute ranges are inside market hours without close and open")
beforeDimension <- dim(CompanyRPData)[1]
print("Filtering onyl events within trading hours")
print("We keep overlapping events ")
CompanyRPData <- CompanyRPData[LOCAL_MARKET_HOUR_MINUTE>=(9+(30+excluding_range)/60),]
CompanyRPData <- CompanyRPData[LOCAL_MARKET_HOUR_MINUTE<=(16-(excluding_range)/60),]
RPNACompanyRPData <- as.data.frame(CompanyRPData)

####### RBDA

version ='RBDA'

CustomFilter = "RELEVANCE >= 70"

customColumns <- 
  c(
    "RP_ENTITY_ID", 
    "TIMESTAMP_UTC",       
    "GROUP",                   
    "CATEGORY",     
    "RELEVANCE",            
    "EVENT_SENTIMENT_SCORE",   
    "EVENT_RELEVANCE", 
    "EVENT_SIMILARITY_DAYS",              
    "FACT_LEVEL",       
    "RP_SOURCE_ID",      			
    "PRODUCT_KEY"                  
  )





CompanyRPData <- RP_GetAnalyticsDataLocalDB(user = user, region = region, CAP = CAP, Groups =  "analyst-ratings", startDate = startDate,endDate = endDate,version = version,CustomFilter = CustomFilter,colSubset =customColumns)
minute_range <- 180
excluding_range <- 0
###### Filtering the events which happen during intraday minutes only
library(lubridate)
TIMESTAMP_REGION <- as.POSIXlt(CompanyRPData$TIMESTAMP_UTC, "America/New_York")

TIMESTAMP_REGION_FORM <- ymd_hms(TIMESTAMP_REGION)
LOCAL_MARKET_HOUR <- hour(TIMESTAMP_REGION_FORM) 
LOCAL_MARKET_HOUR_MINUTE <- hour(TIMESTAMP_REGION_FORM) + minute(TIMESTAMP_REGION_FORM)/60

# print("Filtering events whose minute ranges are inside market hours without close and open")
beforeDimension <- dim(CompanyRPData)[1]
print("Filtering onyl events within trading hours")
print("We keep overlapping events ")
CompanyRPData <- CompanyRPData[LOCAL_MARKET_HOUR_MINUTE>=(9+(30+excluding_range)/60),]
CompanyRPData <- CompanyRPData[LOCAL_MARKET_HOUR_MINUTE<=(16-(excluding_range)/60),]
BigCompanyRPData <- as.data.frame(CompanyRPData)
  
print(table(BigCompanyRPData$CATEGORY))
print(table(RPNACompanyRPData$CATEGORY))


print(table(BigCompanyRPData$GROUP))
print(table(RPNACompanyRPData$GROUP))


