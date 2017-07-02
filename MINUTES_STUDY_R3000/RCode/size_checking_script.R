#### size checking script
################################
####################################
####################################


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


###############################################
###############################################
###############################################
###############################################
############################################### common parameters

region = 'US'
indiceType = 'R1000'

startDate <- "2015-01-01"
endDate <-  "2016-01-01"

groups <- "analyst-ratings"

###################### big data size ###############################################################
################################################################################################### 
##############################################################################################################

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
version ='RBDA'
CustomFilter = "EVENT_RELEVANCE >= 90 & RELEVANCE == 100 & EVENT_SENTIMENT_SCORE>0 & EVENT_SIMILARITY_DAYS>=0"

RBDACompanyRPData <- RP_GetAnalyticsDataLocalDB(user = user, region = region, CAP = indiceType, Groups = groups, startDate = startDate,endDate = endDate,version = version, CustomFilter = CustomFilter, colSubset =customColumns)
# CompanyRPData <- CompanyRPData[CompanyRPData$RELEVANCE == 100,]
# CompanyRPData <- CompanyRPData[CompanyRPData$EVENT_RELEVANCE >= 90,]
### source hardcoding : to remove
RBDACompanyRPData$SOURCE <- "WEBNONPREMIUM"
RBDACompanyRPData$SOURCE[RBDACompanyRPData$PRODUCT_KEY=='DJ-BD' | RBDACompanyRPData$PRODUCT_KEY=='PR-BD' ] <- "DJPR"
RBDACompanyRPData$SOURCE[RBDACompanyRPData$RP_SOURCE_ID%in%c('5A5702','9D69F1','86BD04','CBEE83','ED68DC','B5235B')] <- "PREMIUM"


RBDACompanyRPData <- RBDACompanyRPData[RBDACompanyRPData$SOURCE == "DJPR",]
RBDACompanyRPData <- as.data.frame(RBDACompanyRPData)
print("big data size")
print(dim(RBDACompanyRPData))


###################### end of big data size ###############################################################
################################################################################################### 
##############################################################################################################

###################### rpna size ###############################################################
################################################################################################### 
##############################################################################################################


#### hard parameters : to remove
customColumns <- 
  c(
    "RP_ENTITY_ID", 
    "TIMESTAMP_UTC",       
    "GROUP",                   
    "CATEGORY",     
    "RELEVANCE",            
    "ESS",   
    "G_ENS_SIMILARITY_GAP",              
    "SOURCE",      			
    "PRODUCT_KEY"                  
  )

version ='RPNA'
CustomFilter = "ENS == 100 & RELEVANCE == 100 & ESS>50 & G_ENS_SIMILARITY_GAP >= 0"



RPNACompanyRPData <- RP_GetAnalyticsDataLocalDB(user = user, region = region, CAP = indiceType, Groups = groups, startDate = startDate,endDate = endDate,version = version, CustomFilter = CustomFilter, colSubset =customColumns)
RPNACompanyRPData <- as.data.frame(RPNACompanyRPData)

RP_SOURCE_ID <- RPNACompanyRPData$SOURCE 


RPNACompanyRPData$SOURCE <- "WEBNONPREMIUM"
RPNACompanyRPData$SOURCE[RPNACompanyRPData$PRODUCT_KEY=='DJ-EQ' | RPNACompanyRPData$PRODUCT_KEY=='PR-EQ' ] <- "DJPR"
RPNACompanyRPData$SOURCE[RP_SOURCE_ID %in%c('5A5702','9D69F1','86BD04','CBEE83','ED68DC','B5235B')] <- "PREMIUM"


RPNACompanyRPData <- RPNACompanyRPData[RPNACompanyRPData$SOURCE == "DJPR",]

print("rpna size")
print(dim(RPNACompanyRPData))

###################### end of rpna size ###############################################################
################################################################################################### 
##############################################################################################################


print("done")