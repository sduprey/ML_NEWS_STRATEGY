###########################################
#####        PACKAGES LOADING         #####
###########################################
# General purpose
#library("ROracle")
library("lubridate")
library("zoo")
library("plyr")
# require(igraph)
# library(data.table)
library('car')
library('rgl')
library(plotly)
# library(caret)
# library(xgboost)
# library(Matrix)

# RavenPack
library("RPQuantUtils")
library("RPToolsDB")
library("RPIndicatorCreation")
library("RPStatisticalFitting")
library('RPBackTesting')
library('RPJIRAUtils')
library(RPMLUtils)
library(RPPlotUtils)
Sys.setenv(TZ = "UTC")





###########################################
#####         PROJECT INFO            #####
###########################################
# User name
user = 'sduprey'
# region
region = "US"
# CAP
CAP = "R3000"
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

# FiguresPath
figuresPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/Figures/',sep="")
##############################################

###############################
# Load Bloomberg Mapping File #
###############################

load(paste0(outputDataPath,"Bloomberg.RData" ))
Bloomberg <- as.data.table(Bloomberg)

###############################
# Load Historical Index Split #
###############################
load(paste0(outputDataPath,"History_IndexSplit.RData" ))
History <- as.data.table(History)

##############################
# Load AlgoSeek Mapping File #
##############################
AlgoSeekMapping <- read.csv(paste0(outputDataPath,"AlgoSeek_RP_Mapping.csv"))
AlgoSeekMapping <- as.data.table(AlgoSeekMapping)
AlgoSeekMapping$dates <- as.Date(AlgoSeekMapping$dates)

##################################
# Load AlgoSeek Counts for R1000 #
##################################
AlgoSeek_R1000_COUNT <- readRDS(paste0(outputDataPath,'Algoseek_R1000_Tickers_RP_IDs'))
AlgoSeek_R1000_COUNT <- as.data.table(AlgoSeek_R1000_COUNT)
AlgoSeek_R1000_COUNT$date <- as.Date(AlgoSeek_R1000_COUNT$date)


# Look into dates where we have no RP_ENTITY_IDs
SuspectDates <- AlgoSeek_R1000_COUNT$date[which(AlgoSeek_R1000_COUNT$rp_entity_id == 0)]


# Check the Mapping file in these dates
DatesInMapping <- unique(AlgoSeekMapping$dates)
sum(DatesInMapping %in% SuspectDates)



DatesInData <- unique(AlgoSeek_R1000_COUNT$date)

DatesInData[!(DatesInData %in% DatesInMapping)]

length(DatesInData) - sum(DatesInData %in% DatesInMapping)
weekdays(DatesInData[!(DatesInData %in% DatesInMapping)])

# Dates in History File
DatesInHistoryFile <- unique(History$dates)
length(DatesInData) - sum(DatesInData %in% DatesInHistoryFile)

tmp <- History[,length(unique(rp_entity_id)),by = dates]

# Check Add and Delete in the History File
tmp <- History
tmp[,ADD := sum(Type == 'Add'), by = dates]
tmp[,DELETE := sum(Type == 'Delete'), by = dates]

setkey(tmp,dates)
tmp <- unique(tmp)
RP_PlotDataFrame((as.data.frame(tmp[,.(dates,ADD,DELETE)])), AxisIncluded = T)

# Dates in Bloomberg
DatesInBloomberg <- unique(Bloomberg$dates)
length(DatesInData) - sum(DatesInData %in% DatesInBloomberg)


############################
# Get the Dates in Russell #
############################
# Open DB Connection
dbCon <- RP_DatabaseConnect(user)

# DBQuery <- paste0("select * from algoseek.v_algoseek_r1000
#                   where date_est = '",snapshotDate,"' and rp_entity_id = '",MainCompany,"'")

DBQuery <- paste0("select distinct(dates) from research.v_rp_russell_global")

RussellDates <- dbGetQuery(dbCon,DBQuery)

RP_DatabaseDisconnect(dbCon)

RussellDates$dates <- as.Date(as.character(RussellDates$dates),"%Y%m%d")

# Dates in Russell
DatesInRussell <- unique(RussellDates$dates)
length(DatesInData) - sum(DatesInData %in% DatesInRussell)

# Look at Suspect Dates in the Mapping File
SuspectDates <- DatesInData[!(DatesInData %in% DatesInRussell)]

tmp <- AlgoSeekMapping[dates %in% SuspectDates]

SuspectDates <- unique(AlgoSeek_R1000_COUNT[rp_entity_id <= 200]$date)

Match <- DatesInData %in% DatesInRussell

DatesData <- data.table(DATES = DatesInData, IN_RUSSELL = Match, BACKFILL_DATE = DatesInData)

RP_GetMostRecentMappingDate <- function(x,i){
  if (x$IN_RUSSELL[i]){
    BACKFILL_DATE <- x$DATES[i]
  }else{
    BACKFILL_DATE <- max(x[DATES< x$DATES[i] & IN_RUSSELL == T,DATES])
  }
  return(BACKFILL_DATE)
}

for (i in 1:dim(DatesData)[1]){
  DatesData[i, ]$BACKFILL_DATE <- RP_GetMostRecentMappingDate(DatesData,i)
}

DatesData <- DatesData[IN_RUSSELL == F]

for (i in 1:dim(DatesData)[1]){
  tmp <- AlgoSeekMapping[dates == DatesData$BACKFILL_DATE[i]]
  tmp$dates <- DatesData$DATES[i]
  if (i == 1){
    BackFilledData <- tmp
  }else{
    BackFilledData <- rbind(BackFilledData,tmp)
  }
}

PatchedMapping <- rbind(AlgoSeekMapping, BackFilledData)

setkey(PatchedMapping,dates,rp_entity_id)

sum(!(unique(AlgoSeek_R1000_COUNT$date) %in% unique(PatchedMapping$dates)))

saveRDS(PatchedMapping,paste0(outputDataPath,"Algoseek_RP_Mapping_FILLED"))

###############################################################################
# Pull the the tickers that appear in the Algoseek Table in the Suspect Dates #
###############################################################################
# Open DB Connection
dbCon <- RP_DatabaseConnect(user)

# DBQuery <- paste0("select * from algoseek.v_algoseek_r1000
#                   where date_est = '",snapshotDate,"' and rp_entity_id = '",MainCompany,"'")
SuspectDatesList <- paste("'",SuspectDates,"'",sep ="", collapse = ",")
SuspectDatesList <- paste0("(",SuspectDatesList,")")

DBQuery <- paste0("select distinct ticker, date_est
                  from algoseek.algoseek_r1000
                  where date_est in ",SuspectDatesList," order by date_est")

SuspectDatesTickers <- dbGetQuery(dbCon,DBQuery)

RP_DatabaseDisconnect(dbCon)

# Check the tickers in the suspect dates
SuspectDatesTickers <- as.data.table(SuspectDatesTickers)
tmp <- SuspectDatesTickers[,length(unique(ticker)), by = date_est]
tmp <- merge(tmp,AlgoSeek_R1000_COUNT, by.x = c("date_est"),by.y = c("date"))

# Try to merge the new Fixed mapping with the SuspectDatesTickers to check how many 
# unique RP_ENTITY_IDs we are able to Map
tmp <- merge(SuspectDatesTickers,PatchedMapping,by.x = c("date_est","ticker"),
             by.y = c("dates","Symbol"))
tmp[,NUM_RP_ENTITY_ID := length(unique(rp_entity_id)), by = date_est]
setkey(tmp,date_est)
tmp <- unique(tmp[,.(date_est,NUM_TICKERS,NUM_RP_ENTITY_ID)])
RP_PlotDataFrame(as.data.frame(tmp), AxisIncluded = T)


###################################
# Check Uniqueness of the Mapping #
###################################
Mapping <- readRDS(paste0(outputDataPath,"Algoseek_RP_Mapping_FILLED"))

# R1000
R1000_Daily_VWAP <- readRDS(paste0(outputDataPath,"VWAP_R1000_TradingHours.rds"))
tmp <- as.data.table(R1000_Daily_VWAP)
colnames(tmp) <- c("dates", "Symbol","vwap")

tmp <- merge(tmp,Mapping, by = c("dates","Symbol"))
tmp

sum(is.na(tmp$Symbol))
sum(is.na(tmp$rp_entity_id))
sum(is.na(tmp$vwap))

tmp[,COUNT := .N, by = .(dates,rp_entity_id)]
unique(tmp$COUNT)
table(tmp$COUNT)
# View(tmp[COUNT == 2])


cat((paste("We have", dim(tmp[COUNT >1])[1],"cases of not unique VWAP 
      for a RP_ENTITY_ID")))
cat((paste(length(unique(tmp[COUNT >1,]$rp_entity_id)), "RP_ENTITY_IDs map into
           multiple (2) tickers")))
unique(tmp[COUNT >1,]$rp_entity_id)
tmp <- tmp[COUNT>1]
setkey(tmp,rp_entity_id,Symbol)
tmp <- unique(tmp)

# R2000
R2000_Daily_VWAP <- readRDS(paste0(outputDataPath,"VWAP_R2000_TradingHours.rds"))
tmp <- as.data.table(R2000_Daily_VWAP)
colnames(tmp) <- c("dates", "Symbol","vwap")

tmp <- merge(tmp,Mapping, by = c("dates","Symbol"))
tmp

sum(is.na(tmp$Symbol))
sum(is.na(tmp$rp_entity_id))
sum(is.na(tmp$vwap))

tmp[,COUNT := .N, by = .(dates,rp_entity_id)]
unique(tmp$COUNT)
table(tmp$COUNT)


cat((paste("We have", dim(tmp[COUNT >1])[1],"cases of not unique VWAP 
      for a RP_ENTITY_ID")))
cat((paste(length(unique(tmp[COUNT >1,]$rp_entity_id)), "RP_ENTITY_IDs map into
           multiple (2) tickers")))
unique(tmp[COUNT >1,]$rp_entity_id)

