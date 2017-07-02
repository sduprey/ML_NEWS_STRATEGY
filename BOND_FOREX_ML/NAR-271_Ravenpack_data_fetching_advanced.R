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

print("Fetching global macro news event since 2000 RavenPack Dow Jones Edition only")
print("Relevance 100 only")
global_macrorequest <- "select rpna.timestamp_utc, rpna.rp_entity_id, rpna.country_code, rpna.relevance, rpna.topic, rpna.group, rpna.type, rpna.sub_type, rpna.ess, rpna.ens, rpna.ens_similarity_gap, rpna.aes, rpna.aev, rpna.news_type, rpna.product_key from analytics.rpna_400_mgp rpna WHERE rpna.relevance = 100 and rpna.product_key = 'DJ-GM'"
global_macroRPData <- dbGetQuery(dbCon, global_macrorequest)
saveRDS(global_macroRPData, file=paste(inputDataPath, "rp_global_macro_data_dj_full_advanced.rds"))

# dbCon<-RP_DatabaseConnect(user)
# print("Fetching global macro news event since 2015")
# global_macrorequest <- "select rpna.timestamp_utc, rpna.rp_entity_id, rpna.entity_type, rpna.entity_name, rpna.country_code, rpna.relevance, rpna.topic, rpna.group, rpna.type, rpna.sub_type, rpna.ess, rpna.ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key from analytics.rpna_400_mgp rpna WHERE rpna.relevance = 100 and rpna.timestamp_utc >= '2015-01-01'"
# global_macroRPData <- dbGetQuery(dbCon, global_macrorequest)
# saveRDS(global_macroRPData, file=paste(inputDataPath, "rp_global_macro_data_limited.rds"))
# print("Fetching global macro news event since 2010")
# global_macrorequest <- "select rpna.timestamp_utc, rpna.rp_entity_id, rpna.entity_type, rpna.entity_name, rpna.country_code, rpna.relevance, rpna.topic, rpna.group, rpna.type, rpna.sub_type, rpna.ess, rpna.ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key from analytics.rpna_400_mgp rpna WHERE rpna.relevance = 100 and rpna.timestamp_utc >= '2010-01-01'"
# global_macroRPData <- dbGetQuery(dbCon, global_macrorequest)
# saveRDS(global_macroRPData, file=paste(inputDataPath, "rp_global_macro_data.rds"))
# 
# print("Fetching global macro news event since 2000")
# global_macrorequest <- "select rpna.timestamp_utc, rpna.country_code, rpna.relevance, rpna.topic, rpna.group, rpna.type, rpna.sub_type, rpna.ess, rpna.ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key from analytics.rpna_400_mgp rpna WHERE rpna.relevance = 100"
# global_macroRPData <- dbGetQuery(dbCon, global_macrorequest)
# saveRDS(global_macroRPData, file=paste(inputDataPath, "rp_global_macro_data_full.rds"))

# print("Fetching global corporate news event since 2007 RavenPack Web Edition")
# global_macrorequest <- "select rpna.timestamp_utc, rpna.country_code, rpna.relevance, rpna.topic, rpna.group, rpna.type, rpna.sub_type, rpna.ess, rpna.ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key from analytics.rpna_400_eqt rpna WHERE rpna.relevance = 100 and rpna.timestamp_utc >= '2007-01-01'"
# global_macroRPData <- dbGetQuery(dbCon, global_macrorequest)
# saveRDS(global_macroRPData, file=paste(inputDataPath, "rp_global_corporate_data_full.rds"))
# 

# print("Fetching global macro news event since 2000 RavenPack Dow Jones Edition only")
# global_macrorequest <- "select rpna.timestamp_utc, rpna.country_code, rpna.relevance, rpna.topic, rpna.group, rpna.type, rpna.sub_type, rpna.ess, rpna.ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key from analytics.rpna_400_mgp rpna WHERE rpna.relevance = 100 and rpna.product_key = 'DJ-GM'"
# global_macroRPData <- dbGetQuery(dbCon, global_macrorequest)
# saveRDS(global_macroRPData, file=paste(inputDataPath, "rp_global_macro_data_dj_full.rds"))
# 
# print("Fetching global corporate news event since 2000 RavenPack Dow Jones Edition only")
# global_macrorequest <- "select rpna.timestamp_utc, rpna.country_code, rpna.relevance, rpna.topic, rpna.group, rpna.type, rpna.sub_type, rpna.ess, rpna.ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key from analytics.rpna_400_eqt rpna WHERE rpna.relevance = 100 and rpna.product_key = 'DJ-EQ'"
# global_macroRPData <- dbGetQuery(dbCon, global_macrorequest)
# saveRDS(global_macroRPData, file=paste(inputDataPath, "rp_global_corporate_data_dj_full.rds"))
# 
# 
# print("Fetching countries")
# european_country_code_list_request <- "SELECT DISTINCT country_code FROM countries WHERE continent_code like 'EU'"
# european_country_code_list_RPData <- dbGetQuery(dbCon, european_country_code_list_request)
# saveRDS(european_country_code_list_RPData, file=paste(outputDataPath, "european_country_code_list.rds"))
# 
# 
# ###### Sophisticated average daily event restrained to from 2014
# print("Fetching equity data restrained to from 2014")
# sophisticated_average_daily_event_request <-"select rpna.timestamp_utc, rpna.rp_entity_id, rpna.country_code, rpna.relevance, rpna.ens, rpna.g_ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key, rpna.group, rgb.industry_level_1, rgb.market_cap from  analytics.rpna_400_eqt rpna inner join (SELECT russell.rp_entity_id, russell.industry_level_1, russell.market_cap FROM research.v_rp_russell_global russell  INNER JOIN (SELECT rp_entity_id,MAX(dates) AS maxDate FROM research.v_rp_russell_global GROUP BY rp_entity_id) bis ON russell.rp_entity_id = bis.rp_entity_id AND russell.dates = bis.maxDate) rgb on rgb.rp_entity_id  = rpna.rp_entity_id and rpna.timestamp_utc  >= '2014-01-01' and rpna.relevance >= 70"
# sophisticated_average_daily_event_RPData <- dbGetQuery(dbCon, sophisticated_average_daily_event_request)
# saveRDS(sophisticated_average_daily_event_RPData, file=paste(outputDataPath, "sophisticated_average_daily_event_company_limited.rds"))
# # Fetching data from a R flat file
# #sophisticated_average_daily_event_RPData <- readRDS(paste(outputDataPath, "sophisticated_average_daily_event_company_limited.rds"))
# rm(sophisticated_average_daily_event_RPData)
# 
# print("Fetching macroeconomic news restrained to up 2014")
# sophisticated_macro_average_daily_event_request <- "select timestamp_utc, country_code, rpna.relevance, rpna.ens, rpna.g_ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key, rpna.group from analytics.rpna_400_mgp rpna where timestamp_utc  >= '2014-01-01' and relevance = 100"
# sophisticated_macro_average_daily_eventRPData <- dbGetQuery(dbCon, sophisticated_macro_average_daily_event_request)
# saveRDS(sophisticated_macro_average_daily_eventRPData,  file=paste(outputDataPath, "sophisticated_macro_average_daily_event_company_limited.rds"))
# # Fetching data from a R flat file
# # sophisticated_macro_average_daily_eventRPData <- readRDS(paste(outputDataPath, "sophisticated_macro_average_daily_event_company_limited.rds"))
# # rm(sophisticated_macro_average_daily_eventRPData)
# 
# 
# 
# ###### Sophisticated average daily event
# print("Fetching equity data")
# sophisticated_average_daily_event_request <-"select rpna.timestamp_utc, rpna.rp_entity_id, rpna.country_code, rpna.relevance, rpna.ens, rpna.g_ens, rpna.ens_similarity_gap, rpna.news_type, rpna.product_key, rpna.group, rgb.industry_level_1, rgb.market_cap from  analytics.rpna_400_eqt rpna inner join (SELECT russell.rp_entity_id, russell.industry_level_1, russell.market_cap FROM research.v_rp_russell_global russell  INNER JOIN (SELECT rp_entity_id,MAX(dates) AS maxDate FROM research.v_rp_russell_global GROUP BY rp_entity_id) bis ON russell.rp_entity_id = bis.rp_entity_id AND russell.dates = bis.maxDate) rgb on rgb.rp_entity_id  = rpna.rp_entity_id and rpna.relevance >= 70"
# sophisticated_average_daily_event_RPData <- dbGetQuery(dbCon, sophisticated_average_daily_event_request)
# saveRDS(sophisticated_average_daily_event_RPData, file=paste(outputDataPath, "sophisticated_average_daily_event_company.rds"))
# # Fetching data from a R flat file
# #sophisticated_average_daily_event_RPData <- readRDS(paste(outputDataPath, "sophisticated_average_daily_event_company.rds"))
# rm(sophisticated_average_daily_event_RPData)
# 
# 
# #### Macro news per hour distribution
# print("Fetching macroeconomic news")
# sophisticated_macro_average_daily_event_request <- "select  rpna.timestamp_utc,  rpna.country_code, rpna.relevance, rpna.relevance, rpna.ens, rpna.g_ens, rpna.ens_similarity_gap, rpna.news_type from analytics.rpna_400_mgp rpna where rpna.relevance = 100"
# sophisticated_macro_average_daily_eventRPData <- dbGetQuery(dbCon, sophisticated_macro_average_daily_event_request)
# saveRDS(sophisticated_macro_average_daily_eventRPData,  file=paste(outputDataPath, "sophisticated_macro_average_daily_event_company.rds"))
# # Fetching data from a R flat file
# sophisticated_macro_average_daily_eventRPData <- readRDS(paste(outputDataPath, "sophisticated_macro_average_daily_event_company.rds"))
# rm(sophisticated_macro_average_daily_eventRPData)
# 
# 
# ###### Sentiment ratio cross sectional study
# print("Fetching sentiment score and novelty for equities")
# sentiment_ratio_per_source_per_ens_request <- "SELECT timestamp_utc, ess, ens, ens_similarity_gap, news_type FROM analytics.rpna_400_eqt WHERE ess IS NOT NULL"
# sentiment_ratio_RPData <- dbGetQuery(dbCon, sentiment_ratio_per_source_per_ens_request)
# saveRDS(sentiment_ratio_RPData, file=paste(outputDataPath, "sentiment_ratio.rds"))
# # Fetching data from a R flat file
# sentiment_ratio_RPData <- readRDS(paste(outputDataPath, "sentiment_ratio.rds"))
# rm(sentiment_ratio_RPData)
# 
# print("Fetching sentiment score and novelty for equities")
# sentiment_ratio_per_source_per_ens_request <- "SELECT timestamp_utc, ess, ens, ens_similarity_gap, news_type FROM analytics.rpna_400_mgp WHERE ess IS NOT NULL"
# sentiment_ratio_RPData <- dbGetQuery(dbCon, sentiment_ratio_per_source_per_ens_request)
# saveRDS(sentiment_ratio_RPData, file=paste(outputDataPath, "sentiment_ratio.rds"))
# # Fetching data from a R flat file
# sentiment_ratio_RPData <- readRDS(paste(outputDataPath, "sentiment_ratio_macro.rds"))
# rm(sentiment_ratio_RPData)
# 
# 
# ###### Sentiment ratio cross sectional study
# print("Fetching sentiment score and novelty for equities for Dow Jones edition solely")
# sentiment_ratio_per_source_per_ens_request <- "SELECT timestamp_utc, ess, ens, ens_similarity_gap, news_type FROM analytics.rpna_400_eqt WHERE ess IS NOT NULL AND RELEVANCE>=90 AND PRODUCT_KEY='DJ-EQ'"
# sentiment_ratio_RPData <- dbGetQuery(dbCon, sentiment_ratio_per_source_per_ens_request)
# saveRDS(sentiment_ratio_RPData, file=paste(outputDataPath, "sentiment_ratio_dj.rds"))
# # Fetching data from a R flat file
# sentiment_ratio_RPData <- readRDS(paste(outputDataPath, "sentiment_ratio_dj.rds"))
# rm(sentiment_ratio_RPData)
# 
# 
# ######################## Lighter data for debug
# ############ Average event per company per industry
# ######average_event_per_company_per_industry_request <-"select rpna.timestamp_utc, rpna.rp_entity_id, rgb.industry_level_1 from  analytics.rpna_400_eqt rpna inner join (select distinct rp_entity_id, industry_level_1  from  research.v_rp_russell_global group by rp_entity_id, industry_level_1) rgb on rgb.rp_entity_id  = rpna.rp_entity_id"
# ######average_event_per_company_per_industry_RPData <- dbGetQuery(dbCon, average_event_per_company_per_industry_request)
# ######saveRDS(average_event_per_company_per_industry_RPData, file=paste(outputDataPath, "average_event_company_per_industry.rds"))
# ####### Fetching data from a R flat file
# #######average_event_per_company_per_industry_RPData <- readRDS(paste(outputDataPath, "average_event_company_per_industry.rds")
# ######rm(average_event_per_company_per_industry_RPData)
# 
# ############ Average daily event per company
# ######average_daily_event_request <-"select rpna.timestamp_utc, rpna.rp_entity_id from  analytics.rpna_400_eqt rpna"
# ######average_daily_event_request_RPData <- dbGetQuery(dbCon, average_daily_event_request)
# ######saveRDS(average_daily_event_request_RPData, file=paste(outputDataPath, "average_daily_event_company.rds"))
# ####### Fetching data from a R flat file
# #######average_daily_event_request_RPData <- readRDS(paste(outputDataPath, "average_daily_event_company.rds"))
# ######rm(average_daily_event_request_RPData)
# 
# 
# # ###### News per hour data
# # ###### News per month data
# # ###### News per week day data
# # ###### News per month day data
# # 
# # us_equity_timestamp_request <- "select timestamp_utc from analytics.rpna_400_eqt where country_code = 'US'"
# # us_equity_timestampRPData <- dbGetQuery(dbCon, us_equity_timestamp_request)
# # 
# # saveRDS(us_equity_timestampRPData, file=paste(outputDataPath, "us_equity_timestamps.rds"))
# # # Fetching data from a R flat file
# # #us_equity_timestampRPData <- readRDS(paste(outputDataPath, "us_equity_timestamps.rds"))
# # rm(us_equity_timestampRPData)
# # 
# # eu_equity_timestamp_request <- "select timestamp_utc from analytics.rpna_400_eqt where country_code in (SELECT DISTINCT country_code FROM countries WHERE continent_code like 'EU')"
# # eu_equity_timestampRPData <- dbGetQuery(dbCon, eu_equity_timestamp_request)
# # saveRDS(eu_equity_timestampRPData,  file=paste(outputDataPath, "eu_equity_timestamps.rds"))
# # # Fetching data from a R flat file
# # #eu_equity_timestampRPData <- readRDS(paste(outputDataPath, "eu_equity_timestamps.rds"))
# # rm(eu_equity_timestampRPData)
# # 
# # #### Macro news per hour distribution
# # us_macro_timestamp_request <- "select timestamp_utc from analytics.rpna_400_mgp where country_code =  'US'"
# # us_macro_timestampRPData <- dbGetQuery(dbCon, us_macro_timestamp_request)
# # saveRDS(us_macro_timestampRPData,  file=paste(outputDataPath, "us_macro_timestamps.rds"))
# # # Fetching data from a R flat file
# # #us_macro_timestampRPData <- readRDS(paste(outputDataPath, "us_macro_timestamps.rds"))
# # rm(us_macro_timestampRPData)
# # 
# # eu_macro_timestamp_request <- "select timestamp_utc from analytics.rpna_400_mgp where country_code in (SELECT DISTINCT country_code FROM countries WHERE continent_code like 'EU')"
# # eu_macro_timestampRPData <- dbGetQuery(dbCon, eu_macro_timestamp_request)
# # saveRDS(eu_macro_timestampRPData,  file=paste(outputDataPath, "eu_macro_timestamps.rds"))
# # # Fetching data from a R flat file
# # #eu_macro_timestampRPData <- readRDS(paste(outputDataPath, "eu_macro_timestamps.rds"))
# # rm(eu_macro_timestampRPData)
# # 
# # ###### Sentiment ratio cross sectional study
# # sentiment_ratio_per_source_per_ens_request <- "SELECT ess, ens, ens_similarity_gap, news_type FROM analytics.rpna_400_eqt WHERE ess IS NOT NULL"
# # sentiment_ratio_RPData <- dbGetQuery(dbCon, sentiment_ratio_per_source_per_ens_request)
# # saveRDS(sentiment_ratio_RPData, file=paste(outputDataPath, "sentiment_ratio.rds"))
# # # Fetching data from a R flat file
# # #sentiment_ratio_RPData <- readRDS(paste(outputDataPath, "sentiment_ratio.rds"))
# # rm(sentiment_ratio_RPData)
# 
# 
# ###### ESS histogram
# ###### CSS histogram
# ess_request <-"select ess from analytics.rpna_400_eqt  WHERE ess is not null AND RELEVANCE>=90"
# essRPData <- dbGetQuery(dbCon, ess_request)
# saveRDS(essRPData, file=paste(outputDataPath, "ess_relevance_greater_90.rds"))
# # Fetching data from a R flat file
# essRPData <- readRDS(paste(outputDataPath, "ess_relevance_greater_90.rds"))
# 
# css_request <-"select css from analytics.rpna_400_eqt  WHERE css is not null AND RELEVANCE>=90"
# cssRPData <- dbGetQuery(dbCon, css_request)
# saveRDS(cssRPData,  file=paste(outputDataPath, "css_relevance_greater_90.rds"))
# # Fetching data from a R flat file
# cssRPData <- readRDS(paste(outputDataPath, "css_relevance_greater_90.rds"))
# 
# 
# 
# ###### ESS histogram
# ###### CSS histogram
# ess_request <-"select ess from analytics.rpna_400_eqt  WHERE ess is not null AND RELEVANCE>=90 and PRODUCT_KEY='DJ-EQ'"
# essRPData <- dbGetQuery(dbCon, ess_request)
# saveRDS(essRPData, file=paste(outputDataPath, "ess_relevance_greater_90_dj.rds"))
# # Fetching data from a R flat file
# essRPData <- readRDS(paste(outputDataPath, "ess_relevance_greater_90_dj.rds"))
# 
# css_request <-"select css from analytics.rpna_400_eqt  WHERE css is not null AND RELEVANCE>=90 and PRODUCT_KEY='DJ-EQ'"
# cssRPData <- dbGetQuery(dbCon, css_request)
# saveRDS(cssRPData,  file=paste(outputDataPath, "css_relevance_greater_90_dj.rds"))
# # Fetching data from a R flat file
# cssRPData <- readRDS(paste(outputDataPath, "css_relevance_greater_90_dj.rds"))
# 
