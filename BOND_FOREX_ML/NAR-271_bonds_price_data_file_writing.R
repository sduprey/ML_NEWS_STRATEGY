##### Fetching commodity data from RP database
######## Gold modeling
# library("RPQuantUtils")
# library("RPToolsDB")
require(ggplot2)
require("ppcor")
require(graphics)
require("TTR")
require(plyr)
# require(reshape)
require(reshape2)
require(RColorBrewer)
require(stats)
require(Rsolnp)
require(zoo)
require(xts)
require(vars)
require(Quandl)

source("./RCode/RP_Plotting_Utils.R")
source("./RCode/RP_Macro_Monthly_Utils.R")
source("./RCode/RP_Price_Utils.R")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
# repoPath = RP_GetSharedPath(user)
# Input Data Path
# inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
# outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

inputDataPath  <- "C://My_RP_Data/"


# Getting data from Quandl
# CBOT 10-year US Treasury Note Futures #1 (TY1) - Unadjusted Prices, Roll On Last Trading Day, Continuous Contract History
US_10Y_TN_Rolled_Futures <- readRDS(paste(inputDataPath,"US_10Y_TN_Rolled_Futures.rds", sep = ""))
US_10Y_TN_Rolled_Futures <- US_10Y_TN_Rolled_Futures[,1:(dim(US_10Y_TN_Rolled_Futures)[2]-1)]
colnames(US_10Y_TN_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
print(min(US_10Y_TN_Rolled_Futures$DATE))
print(maxUS_10Y <- max(US_10Y_TN_Rolled_Futures$DATE))


US_5Y_TN_Rolled_Futures <- readRDS(paste(inputDataPath,"US_5Y_TN_Rolled_Futures.rds", sep = ""))
US_5Y_TN_Rolled_Futures <- US_5Y_TN_Rolled_Futures[,1:(dim(US_5Y_TN_Rolled_Futures)[2]-1)]
colnames(US_5Y_TN_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
print(min(US_5Y_TN_Rolled_Futures$DATE))
print(maxUS_5Y <- max(US_5Y_TN_Rolled_Futures$DATE))


US_2Y_TN_Rolled_Futures <- readRDS(paste(inputDataPath,"US_2Y_TN_Rolled_Futures.rds", sep = ""))
US_2Y_TN_Rolled_Futures <- US_2Y_TN_Rolled_Futures[,1:(dim(US_2Y_TN_Rolled_Futures)[2]-1)]
colnames(US_2Y_TN_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
print(min(US_2Y_TN_Rolled_Futures$DATE))
print(maxUS_2Y <-max(US_2Y_TN_Rolled_Futures$DATE))

US_30Y_TN_Rolled_Futures <- readRDS(paste(inputDataPath,"US_30Y_TN_Rolled_Futures.rds", sep = ""))
US_30Y_TN_Rolled_Futures <- US_30Y_TN_Rolled_Futures[,1:(dim(US_30Y_TN_Rolled_Futures)[2]-1)]
colnames(US_30Y_TN_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
print(min(US_30Y_TN_Rolled_Futures$DATE))
print(maxUK_30Y <- max(US_30Y_TN_Rolled_Futures$DATE))


GER_10Y_BUND_Rolled_Futures <- readRDS(paste(inputDataPath,"GER_10Y_BUND_Rolled_Futures.rds", sep = ""))
GER_10Y_BUND_Rolled_Futures <- GER_10Y_BUND_Rolled_Futures[,1:(dim(GER_10Y_BUND_Rolled_Futures)[2]-1)]
colnames(GER_10Y_BUND_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
min(GER_10Y_BUND_Rolled_Futures$DATE)
print(min(GER_10Y_BUND_Rolled_Futures$DATE))
print(maxGER_10Y <-max(GER_10Y_BUND_Rolled_Futures$DATE))


UK_10Y_GILT_Rolled_Futures <- readRDS(paste(inputDataPath,"UK_10Y_GILT_Rolled_Futures.rds", sep = ""))
UK_10Y_GILT_Rolled_Futures <- UK_10Y_GILT_Rolled_Futures[,1:(dim(UK_10Y_GILT_Rolled_Futures)[2]-1)]
colnames(UK_10Y_GILT_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
min(UK_10Y_GILT_Rolled_Futures$DATE)
print(min(UK_10Y_GILT_Rolled_Futures$DATE))
print(maxUK_10Y <-max(UK_10Y_GILT_Rolled_Futures$DATE))


JP_10Y_YEN_Rolled_Futures <- readRDS(paste(inputDataPath,"JP_10Y_YEN_Rolled_Futures.rds", sep = ""))
JP_10Y_YEN_Rolled_Futures <- JP_10Y_YEN_Rolled_Futures[,1:(dim(JP_10Y_YEN_Rolled_Futures)[2]-1)]
colnames(JP_10Y_YEN_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
min(JP_10Y_YEN_Rolled_Futures$DATE)
print(min(JP_10Y_YEN_Rolled_Futures$DATE))
print(maxJP_10Y <- max(JP_10Y_YEN_Rolled_Futures$DATE))


GER_5Y_BOBL_Rolled_Futures <- readRDS(paste(inputDataPath,"GER_5Y_BOBL_Rolled_Futures.rds", sep = ""))
GER_5Y_BOBL_Rolled_Futures <- GER_5Y_BOBL_Rolled_Futures[,1:(dim(GER_5Y_BOBL_Rolled_Futures)[2]-1)]
colnames(GER_5Y_BOBL_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")

min(GER_5Y_BOBL_Rolled_Futures$DATE)
print(min(GER_5Y_BOBL_Rolled_Futures$DATE))
print(maxGER_5Y <- max(GER_5Y_BOBL_Rolled_Futures$DATE))


GER_2Y_SCHATZ_Rolled_Futures <- readRDS(paste(inputDataPath,"GER_2Y_SCHATZ_Rolled_Futures.rds", sep = ""))
GER_2Y_SCHATZ_Rolled_Futures <- GER_2Y_SCHATZ_Rolled_Futures[,1:(dim(GER_2Y_SCHATZ_Rolled_Futures)[2]-1)]
colnames(GER_2Y_SCHATZ_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")

min(GER_2Y_SCHATZ_Rolled_Futures$DATE)
print(min(GER_2Y_SCHATZ_Rolled_Futures$DATE))
print(maxGER_2Y <- max(GER_2Y_SCHATZ_Rolled_Futures$DATE))


# FR_10Y_OAT_Rolled_Futures <- readRDS(paste(inputDataPath,"FR_10Y_OAT_Rolled_Futures.rds", sep = ""))
# ################### drop them not enough historic
# min(FR_10Y_OAT_Rolled_Futures$Date)


# EU_10Y_BTP_Rolled_Futures <- readRDS(paste(inputDataPath,"EU_10Y_BTP_Rolled_Futures.rds", sep = ""))
# ################### drop them not enough historic
# min(EU_10Y_BTP_Rolled_Futures$Date)


CA_10Y_Rolled_Futures <- readRDS(paste(inputDataPath,"CA_10Y_Rolled_Futures.rds", sep = ""))
CA_10Y_Rolled_Futures <- CA_10Y_Rolled_Futures[,1:(dim(CA_10Y_Rolled_Futures)[2]-1)]
colnames(CA_10Y_Rolled_Futures) <-  c("DATE","OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
min(CA_10Y_Rolled_Futures$DATE)
print(min(CA_10Y_Rolled_Futures$DATE))
print(maxCA_10Y <- max(CA_10Y_Rolled_Futures$DATE))

# EU_30Y_BUXL_Rolled_Futures <- readRDS(paste(inputDataPath,"EU_30Y_BUXL_Rolled_Futures.rds", sep = ""))
# ################### drop them not enough historic
# min(EU_30Y_BUXL_Rolled_Futures$Date)

# # EUREX BOBL
# Quandl("SCF/EUREX_FGBM2_FN", authcode="18dndgL4fDkTE-r7zMrV")
# Quandl("SCF/EUREX_FGBM1_FN", authcode="18dndgL4fDkTE-r7zMrV")
# # MEDIUM LIFFE
# # Quandl.get('CHRIS/LIFFE_H1', 'authcode', '18dndgL4fDkTE-r7zMrV')

### Merging all data frames alltogether
# we here just make sure that we have each day

backtesting_starting_date <- "2000-01-01"
backtesting_ending_date <- min(maxCA_10Y,maxGER_2Y,maxGER_5Y,maxJP_10Y,maxUK_10Y, maxGER_10Y, maxUK_30Y, maxUS_2Y, maxUS_5Y, maxUS_10Y)
all_predictors_df <- data.frame(DATE=seq(as.Date(backtesting_starting_date), as.Date(backtesting_ending_date), by="days"))
all_output_df <- data.frame(DATE=seq(as.Date(backtesting_starting_date), as.Date(backtesting_ending_date), by="days"))
all_data_df <- data.frame(DATE=seq(as.Date(backtesting_starting_date), as.Date(backtesting_ending_date), by="days"))

## Computing all prices related data for Canada
# CANADA 10 Y
print("Computing Canada 10 years")
aggregations <- computeAllPriceData(CA_10Y_Rolled_Futures)
CA_10Y_Rolled_Futures_All <- aggregations$all
CA_10Y_Rolled_Futures_Output <- aggregations$output
CA_10Y_Rolled_Futures_Predictors <- computeAllPriceMomentumData(CA_10Y_Rolled_Futures_All)
# we generate three datasets : one with everything, one with only what is known at day d and one with the output we try to forecast at day d

CA_10Y_Rolled_Futures_All <- CA_10Y_Rolled_Futures_All[(CA_10Y_Rolled_Futures_All$DATE >= backtesting_starting_date) & (CA_10Y_Rolled_Futures_All$DATE <= backtesting_ending_date),]
colnames(CA_10Y_Rolled_Futures_All) <-  c("DATE",paste0("CA_10Y_", colnames(CA_10Y_Rolled_Futures_All[,-1])))
all_data_df <- merge(CA_10Y_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)

CA_10Y_Rolled_Futures_Output <- CA_10Y_Rolled_Futures_Output[(CA_10Y_Rolled_Futures_Output$DATE >= backtesting_starting_date) & (CA_10Y_Rolled_Futures_Output$DATE <= backtesting_ending_date),]
colnames(CA_10Y_Rolled_Futures_Output) <-  c("DATE",paste0("CA_10Y_", colnames(CA_10Y_Rolled_Futures_Output[,-1])))
all_output_df <- merge(CA_10Y_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)

CA_10Y_Rolled_Futures_Predictors <- CA_10Y_Rolled_Futures_Predictors[(CA_10Y_Rolled_Futures_Predictors$DATE >= backtesting_starting_date) & (CA_10Y_Rolled_Futures_Predictors$DATE <= backtesting_ending_date),]
colnames(CA_10Y_Rolled_Futures_Predictors) <-  c("DATE",paste0("CA_10Y_", colnames(CA_10Y_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(CA_10Y_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)

# GERMANY 2 Y
print("Computing Germany 2 years")
aggregations <- computeAllPriceData(GER_2Y_SCHATZ_Rolled_Futures)
GER_2Y_SCHATZ_Rolled_Futures_All <- aggregations$all
GER_2Y_SCHATZ_Rolled_Futures_Output <- aggregations$output
GER_2Y_SCHATZ_Rolled_Futuress_Predictors <- computeAllPriceMomentumData(GER_2Y_SCHATZ_Rolled_Futures_All)


GER_2Y_SCHATZ_Rolled_Futures_All <- GER_2Y_SCHATZ_Rolled_Futures_All[GER_2Y_SCHATZ_Rolled_Futures_All$DATE >= backtesting_starting_date & GER_2Y_SCHATZ_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(GER_2Y_SCHATZ_Rolled_Futures_All) <-  c("DATE",paste0("GER_2Y_", colnames(GER_2Y_SCHATZ_Rolled_Futures_All[,-1])))
all_data_df <- merge(GER_2Y_SCHATZ_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)


GER_2Y_SCHATZ_Rolled_Futures_Output <- GER_2Y_SCHATZ_Rolled_Futures_Output[GER_2Y_SCHATZ_Rolled_Futures_Output$DATE >= backtesting_starting_date & GER_2Y_SCHATZ_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(GER_2Y_SCHATZ_Rolled_Futures_Output) <-  c("DATE",paste0("GER_2Y_", colnames(GER_2Y_SCHATZ_Rolled_Futures_Output[,-1])))
all_output_df <- merge(GER_2Y_SCHATZ_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)

GER_2Y_SCHATZ_Rolled_Futuress_Predictors <- GER_2Y_SCHATZ_Rolled_Futuress_Predictors[GER_2Y_SCHATZ_Rolled_Futuress_Predictors$DATE >= backtesting_starting_date & GER_2Y_SCHATZ_Rolled_Futuress_Predictors$DATE <= backtesting_ending_date,]
colnames(GER_2Y_SCHATZ_Rolled_Futuress_Predictors) <-  c("DATE",paste0("GER_2Y_", colnames(GER_2Y_SCHATZ_Rolled_Futuress_Predictors[,-1])))
all_predictors_df <- merge(GER_2Y_SCHATZ_Rolled_Futuress_Predictors,all_predictors_df,by="DATE",all=TRUE)

# GERMANY 5Y
print("Computing Germany 5 years")
aggregations <- computeAllPriceData(GER_5Y_BOBL_Rolled_Futures)
GER_5Y_BOBL_Rolled_Futures_All <- aggregations$all
GER_5Y_BOBL_Rolled_Futures_Output <- aggregations$output
GER_5Y_BOBL_Rolled_Futures_Predictors <- computeAllPriceMomentumData(GER_5Y_BOBL_Rolled_Futures_All)

GER_5Y_BOBL_Rolled_Futures_All <- GER_5Y_BOBL_Rolled_Futures_All[GER_5Y_BOBL_Rolled_Futures_All$DATE >= backtesting_starting_date & GER_5Y_BOBL_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(GER_5Y_BOBL_Rolled_Futures_All) <-  c("DATE",paste0("GER_5Y_", colnames(GER_5Y_BOBL_Rolled_Futures_All[,-1])))
all_data_df <- merge(GER_5Y_BOBL_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)

GER_5Y_BOBL_Rolled_Futures_Output <- GER_5Y_BOBL_Rolled_Futures_Output[GER_5Y_BOBL_Rolled_Futures_Output$DATE >= backtesting_starting_date & GER_5Y_BOBL_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(GER_5Y_BOBL_Rolled_Futures_Output) <-  c("DATE",paste0("GER_5Y_", colnames(GER_5Y_BOBL_Rolled_Futures_Output[,-1])))
all_output_df <- merge(GER_5Y_BOBL_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)

GER_5Y_BOBL_Rolled_Futures_Predictors <- GER_5Y_BOBL_Rolled_Futures_Predictors[GER_5Y_BOBL_Rolled_Futures_Predictors$DATE >= backtesting_starting_date & GER_5Y_BOBL_Rolled_Futures_Predictors$DATE <= backtesting_ending_date,]
colnames(GER_5Y_BOBL_Rolled_Futures_Predictors) <-  c("DATE",paste0("GER_5Y_", colnames(GER_5Y_BOBL_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(GER_5Y_BOBL_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)

# GERMANY 10 YEARS
print("Computing Germany 10 years")
aggregations <- computeAllPriceData(GER_10Y_BUND_Rolled_Futures)
GER_10Y_BUND_Rolled_Futures_All <- aggregations$all
GER_10Y_BUND_Rolled_Futures_Output <- aggregations$output
GER_10Y_BUND_Rolled_Futures_Predictors <- computeAllPriceMomentumData(GER_10Y_BUND_Rolled_Futures_All)

GER_10Y_BUND_Rolled_Futures_All <- GER_10Y_BUND_Rolled_Futures_All[GER_10Y_BUND_Rolled_Futures_All$DATE >= backtesting_starting_date & GER_10Y_BUND_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(GER_10Y_BUND_Rolled_Futures_All) <-  c("DATE",paste0("DE_10Y_", colnames(GER_10Y_BUND_Rolled_Futures_All[,-1])))
all_data_df <- merge(GER_10Y_BUND_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)

GER_10Y_BUND_Rolled_Futures_Output <- GER_10Y_BUND_Rolled_Futures_Output[GER_10Y_BUND_Rolled_Futures_Output$DATE >= backtesting_starting_date & GER_10Y_BUND_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(GER_10Y_BUND_Rolled_Futures_Output) <-  c("DATE",paste0("DE_10Y_", colnames(GER_10Y_BUND_Rolled_Futures_Output[,-1])))
all_output_df <- merge(GER_10Y_BUND_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)

GER_10Y_BUND_Rolled_Futures_Predictors <- GER_10Y_BUND_Rolled_Futures_Predictors[GER_10Y_BUND_Rolled_Futures_Predictors$DATE >= backtesting_starting_date & GER_10Y_BUND_Rolled_Futures_Predictors$DATE <= backtesting_ending_date,]
colnames(GER_10Y_BUND_Rolled_Futures_Predictors) <-  c("DATE",paste0("DE_10Y_", colnames(GER_10Y_BUND_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(GER_10Y_BUND_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)

### JAPAN 10 YEARS
print("Computing Japan 10 years")
aggregations <- computeAllPriceData(JP_10Y_YEN_Rolled_Futures)
JP_10Y_YEN_Rolled_Futures_All <- aggregations$all
JP_10Y_YEN_Rolled_Futures_Output <- aggregations$output
JP_10Y_YEN_Rolled_Futures_Predictors <- computeAllPriceMomentumData(JP_10Y_YEN_Rolled_Futures_All)

JP_10Y_YEN_Rolled_Futures_All <- JP_10Y_YEN_Rolled_Futures_All[JP_10Y_YEN_Rolled_Futures_All$DATE >= backtesting_starting_date & JP_10Y_YEN_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(JP_10Y_YEN_Rolled_Futures_All) <-  c("DATE",paste0("JP_10Y_", colnames(JP_10Y_YEN_Rolled_Futures_All[,-1])))
all_data_df <- merge(JP_10Y_YEN_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)

JP_10Y_YEN_Rolled_Futures_Output <- JP_10Y_YEN_Rolled_Futures_Output[JP_10Y_YEN_Rolled_Futures_Output$DATE >= backtesting_starting_date & JP_10Y_YEN_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(JP_10Y_YEN_Rolled_Futures_Output) <-  c("DATE",paste0("JP_10Y_", colnames(JP_10Y_YEN_Rolled_Futures_Output[,-1])))
all_output_df <- merge(JP_10Y_YEN_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)

JP_10Y_YEN_Rolled_Futures_Predictors <- JP_10Y_YEN_Rolled_Futures_Predictors[JP_10Y_YEN_Rolled_Futures_Predictors$DATE >= backtesting_starting_date & JP_10Y_YEN_Rolled_Futures_Predictors$DATE <= backtesting_ending_date,]
colnames(JP_10Y_YEN_Rolled_Futures_Predictors) <-  c("DATE",paste0("JP_10Y_", colnames(JP_10Y_YEN_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(JP_10Y_YEN_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)

### UK 10 YEARS
print("Computing UK 10 years")
aggregations <- computeAllPriceData(UK_10Y_GILT_Rolled_Futures)
UK_10Y_GILT_Rolled_Futures_All <- aggregations$all
UK_10Y_GILT_Rolled_Futures_Output <- aggregations$output
UK_10Y_GILT_Rolled_Futures_Predictors <- computeAllPriceMomentumData(UK_10Y_GILT_Rolled_Futures_All)

UK_10Y_GILT_Rolled_Futures_All <- UK_10Y_GILT_Rolled_Futures_All[UK_10Y_GILT_Rolled_Futures_All$DATE >= backtesting_starting_date & UK_10Y_GILT_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(UK_10Y_GILT_Rolled_Futures_All) <-  c("DATE",paste0("GB_10Y_", colnames(UK_10Y_GILT_Rolled_Futures_All[,-1])))
all_data_df <- merge(UK_10Y_GILT_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)

UK_10Y_GILT_Rolled_Futures_Output <- UK_10Y_GILT_Rolled_Futures_Output[UK_10Y_GILT_Rolled_Futures_Output$DATE >= backtesting_starting_date & UK_10Y_GILT_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(UK_10Y_GILT_Rolled_Futures_Output) <-  c("DATE",paste0("GB_10Y_", colnames(UK_10Y_GILT_Rolled_Futures_Output[,-1])))
all_output_df <- merge(UK_10Y_GILT_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)


UK_10Y_GILT_Rolled_Futures_Predictors <- UK_10Y_GILT_Rolled_Futures_Predictors[UK_10Y_GILT_Rolled_Futures_Predictors$DATE >= backtesting_starting_date & UK_10Y_GILT_Rolled_Futures_Predictors$DATE <= backtesting_ending_date,]
colnames(UK_10Y_GILT_Rolled_Futures_Predictors) <-  c("DATE",paste0("GB_10Y_", colnames(UK_10Y_GILT_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(UK_10Y_GILT_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)

# US 30 YEARS
print("Computing US 30 years")
aggregations <- computeAllPriceData(US_30Y_TN_Rolled_Futures)
US_30Y_TN_Rolled_Futures_All <- aggregations$all
US_30Y_TN_Rolled_Futures_Output <- aggregations$output
US_30Y_TN_Rolled_Futures_Predictors <- computeAllPriceMomentumData(US_30Y_TN_Rolled_Futures_All)

US_30Y_TN_Rolled_Futures_All <- US_30Y_TN_Rolled_Futures_All[US_30Y_TN_Rolled_Futures_All$DATE >= backtesting_starting_date & US_30Y_TN_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(US_30Y_TN_Rolled_Futures_All) <-  c("DATE",paste0("US_30Y_", colnames(US_30Y_TN_Rolled_Futures_All[,-1])))
all_data_df <- merge(US_30Y_TN_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)

US_30Y_TN_Rolled_Futures_Output <- US_30Y_TN_Rolled_Futures_Output[US_30Y_TN_Rolled_Futures_Output$DATE >= backtesting_starting_date & US_30Y_TN_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(US_30Y_TN_Rolled_Futures_Output) <-  c("DATE",paste0("US_30Y_", colnames(US_30Y_TN_Rolled_Futures_Output[,-1])))
all_output_df <- merge(US_30Y_TN_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)

US_30Y_TN_Rolled_Futures_Predictors <- US_30Y_TN_Rolled_Futures_Predictors[US_30Y_TN_Rolled_Futures_Predictors$DATE >= backtesting_starting_date & US_30Y_TN_Rolled_Futures_Predictors$DATE <= backtesting_ending_date,]
colnames(US_30Y_TN_Rolled_Futures_Predictors) <-  c("DATE",paste0("US_30Y_", colnames(US_30Y_TN_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(US_30Y_TN_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)


# US 2Y YEARS 
print("Computing US 2 years")
aggregations <- computeAllPriceData(US_2Y_TN_Rolled_Futures)
US_2Y_TN_Rolled_Futures_All <- aggregations$all
US_2Y_TN_Rolled_Futures_Output <- aggregations$output
US_2Y_TN_Rolled_Futures_Predictors <- computeAllPriceMomentumData(US_2Y_TN_Rolled_Futures_All)

US_2Y_TN_Rolled_Futures_All <-  US_2Y_TN_Rolled_Futures_All[US_2Y_TN_Rolled_Futures_All$DATE >= backtesting_starting_date & US_2Y_TN_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(US_2Y_TN_Rolled_Futures_All) <-  c("DATE",paste0("US_2Y_", colnames(US_2Y_TN_Rolled_Futures_All[,-1])))
all_data_df <- merge(US_2Y_TN_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)

US_2Y_TN_Rolled_Futures_Output <-  US_2Y_TN_Rolled_Futures_Output[US_2Y_TN_Rolled_Futures_Output$DATE >= backtesting_starting_date & US_2Y_TN_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(US_2Y_TN_Rolled_Futures_Output) <-  c("DATE",paste0("US_2Y_", colnames(US_2Y_TN_Rolled_Futures_Output[,-1])))
all_output_df <- merge(US_2Y_TN_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)

US_2Y_TN_Rolled_Futures_Predictors <-  US_2Y_TN_Rolled_Futures_Predictors[US_2Y_TN_Rolled_Futures_Predictors$DATE >= backtesting_starting_date & US_2Y_TN_Rolled_Futures_Predictors$DATE <= backtesting_ending_date,]
colnames(US_2Y_TN_Rolled_Futures_Predictors) <-  c("DATE",paste0("US_2Y_", colnames(US_2Y_TN_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(US_2Y_TN_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)

# US 10 YEARS
print("Computing US 10 years")
aggregations <- computeAllPriceData(US_10Y_TN_Rolled_Futures)
US_10Y_TN_Rolled_Futures_All <- aggregations$all
US_10Y_TN_Rolled_Futures_Output <- aggregations$output
US_10Y_TN_Rolled_Futures_Predictors <- computeAllPriceMomentumData(US_10Y_TN_Rolled_Futures_All)

US_10Y_TN_Rolled_Futures_All <- US_10Y_TN_Rolled_Futures_All[US_10Y_TN_Rolled_Futures_All$DATE >= backtesting_starting_date & US_10Y_TN_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(US_10Y_TN_Rolled_Futures_All) <-  c("DATE",paste0("US_10Y_", colnames(US_10Y_TN_Rolled_Futures_All[,-1])))
all_data_df <- merge(US_10Y_TN_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)

US_10Y_TN_Rolled_Futures_Output <- US_10Y_TN_Rolled_Futures_Output[US_10Y_TN_Rolled_Futures_Output$DATE >= backtesting_starting_date & US_10Y_TN_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(US_10Y_TN_Rolled_Futures_Output) <-  c("DATE",paste0("US_10Y_", colnames(US_10Y_TN_Rolled_Futures_Output[,-1])))
all_output_df <- merge(US_10Y_TN_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)

US_10Y_TN_Rolled_Futures_Predictors <- US_10Y_TN_Rolled_Futures_Predictors[US_10Y_TN_Rolled_Futures_Predictors$DATE >= backtesting_starting_date & US_10Y_TN_Rolled_Futures_Predictors$DATE <= backtesting_ending_date,]
colnames(US_10Y_TN_Rolled_Futures_Predictors) <-  c("DATE",paste0("US_10Y_", colnames(US_10Y_TN_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(US_10Y_TN_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)

# US 5 YEARS

print("Computing US 5 years")
aggregations <- computeAllPriceData(US_5Y_TN_Rolled_Futures)
US_5Y_TN_Rolled_Futures_All <- aggregations$all
US_5Y_TN_Rolled_Futures_Output <- aggregations$output
US_5Y_TN_Rolled_Futures_Predictors <- computeAllPriceMomentumData(US_5Y_TN_Rolled_Futures_All)

US_5Y_TN_Rolled_Futures_All <- US_5Y_TN_Rolled_Futures_All[US_5Y_TN_Rolled_Futures_All$DATE >= backtesting_starting_date & US_5Y_TN_Rolled_Futures_All$DATE <= backtesting_ending_date,]
colnames(US_5Y_TN_Rolled_Futures_All) <-  c("DATE",paste0("US_5Y_", colnames(US_5Y_TN_Rolled_Futures_All[,-1])))
all_data_df <- merge(US_5Y_TN_Rolled_Futures_All,all_data_df,by="DATE",all=TRUE)


US_5Y_TN_Rolled_Futures_Output <- US_5Y_TN_Rolled_Futures_Output[US_5Y_TN_Rolled_Futures_Output$DATE >= backtesting_starting_date & US_5Y_TN_Rolled_Futures_Output$DATE <= backtesting_ending_date,]
colnames(US_5Y_TN_Rolled_Futures_Output) <-  c("DATE",paste0("US_5Y_", colnames(US_5Y_TN_Rolled_Futures_Output[,-1])))
all_output_df <- merge(US_5Y_TN_Rolled_Futures_Output,all_output_df,by="DATE",all=TRUE)


US_5Y_TN_Rolled_Futures_Predictors <- US_5Y_TN_Rolled_Futures_Predictors[US_5Y_TN_Rolled_Futures_Predictors$DATE >= backtesting_starting_date & US_5Y_TN_Rolled_Futures_Predictors$DATE <= backtesting_ending_date,]
colnames(US_5Y_TN_Rolled_Futures_Predictors) <-  c("DATE",paste0("US_5Y_", colnames(US_5Y_TN_Rolled_Futures_Predictors[,-1])))
all_predictors_df <- merge(US_5Y_TN_Rolled_Futures_Predictors,all_predictors_df,by="DATE",all=TRUE)

colnames(all_data_df)
head(all_data_df)
colnames(all_predictors_df)
head(all_predictors_df)
colnames(all_output_df)
head(all_output_df)


SaveDataFrame(all_data_df,inputDataPath,"all_data_df")
SaveDataFrame(all_predictors_df,inputDataPath,"all_predictors_df")
SaveDataFrame(all_output_df,inputDataPath,"all_output_df")

#### saving all prices data

### saving all prices data 




