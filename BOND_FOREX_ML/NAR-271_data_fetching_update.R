### Ravenpack Redshift data fetching

# Data collection and flat file saving for all graphics

library("RPToolsDB")


user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")

# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

dbCon<-RP_DatabaseConnect(user)

#  Euro Area: FB83C5
#  European Union Area: 9DBA1F
#  Eureopean Central Bank: 3A9CFF
#  Eurozone: F30C5D
#  European Financial Stability Facility: DD3194
#  European Commission: 562F10
#  European Union: E5FA3A
#  European Council: 2E4B1B

# saveRDS(global_macroRPData, file=paste(inputDataPath, "rp_global_macro_data_dj_full_advanced.rds"))
global_macroRPData <- readRDS(paste(inputDataPath,"rp_global_macro_data_dj_full_advanced.rds", sep = ""))

global_macroRPData[global_macroRPData$rp_entity_id == "FB83C5",c("country_code")] <- "EU"
global_macroRPData[global_macroRPData$rp_entity_id == "9DBA1F",c("country_code")] <- "EU"
global_macroRPData[global_macroRPData$rp_entity_id == "3A9CFF",c("country_code")] <- "EU"
global_macroRPData[global_macroRPData$rp_entity_id == "F30C5D",c("country_code")] <- "EU"
global_macroRPData[global_macroRPData$rp_entity_id == "DD3194",c("country_code")] <- "EU"
global_macroRPData[global_macroRPData$rp_entity_id == "562F10",c("country_code")] <- "EU"
global_macroRPData[global_macroRPData$rp_entity_id == "E5FA3A",c("country_code")] <- "EU"
global_macroRPData[global_macroRPData$rp_entity_id == "2E4B1B",c("country_code")] <- "EU"


print(global_macroRPData[global_macroRPData$rp_entity_id == "FB83C5",c("country_code")])
