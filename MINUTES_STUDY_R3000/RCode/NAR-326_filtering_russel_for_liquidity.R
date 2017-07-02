# Load Trade Volume Data
# Data collection and flat file saving for all graphics
library("RPToolsDB")
library("abind")
library("RPPlotUtils")
library("boot")
library("zoo")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")

# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

RP_GetRollMean <- function(x, n = 21) {
  
  x <- as.numeric(x)
  N <- length(x)
  z <- which(!is.na(x))
  Q <- length(z)
  
  out <- rep(NA, N)
  if (n < Q) out[z] <- rollmean(x[z], k = n, fill = NA, align = "right")
  
  return(as.numeric(out))
  
}

TradeVolumePanel <- readRDS(paste0(outputDataPath,"VWAPReturns_R3000_TradingHours.rds"))


# R1000 not filtered, only R2000 companies filtered: keeping top 80% according to 30 trailing mean of Total Volume
# tmp <- copy(TradeVolumePanel)
# colnames(tmp)
# tmp[,COUNT := .N, by = .(DATE,RP_ENTITY_ID)]
TradeVolumePanel[,AVG_TRAD_VOL := RP_GetRollMean(TOTAL_VOLUME), by = RP_ENTITY_ID]
TradeVolumePanel[,QUANTILE_TRAD_VOL := RP_QuantileRank(AVG_TRAD_VOL,minCompanies = 5,
                                          step = 0.2), by = .(DATE,INDEX)] 
TradeVolumePanel[INDEX == 'R2000',]$QUANTILE_TRAD_VOL <- 5



#################################
#################################
#################################
#################################
#################################
#################################
################################# Old stuff
# # Load Trade Volume Data
# TradeVolumePanel <- readRDS(paste0(vwapFolder,"VWAPReturns_R3000_TradingHours.rds"))
# 
# 
# # R1000 not filtered, only R2000 companies filtered: keeping top 80% according to 30 trailing mean of Total Volume
# tmp <- copy(TradeVolumePanel)
# colnames(tmp)
# tmp[,COUNT := .N, by = .(DATE,RP_ENTITY_ID)]
# tmp[,AVG_TRAD_VOL := RP_GetRollMean(TOTAL_VOLUME), by = RP_ENTITY_ID]
# tmp[,QUANTILE_TRAD_VOL := RP_QuantileRank(AVG_TRAD_VOL,minCompanies = 5,
#                                           step = 0.2), by = .(DATE,INDEX)] 
# tmp[INDEX == 'R1000',]$QUANTILE_TRAD_VOL <- 5