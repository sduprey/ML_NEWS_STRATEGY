RP_RPNAUpdateMinuteProfile <- function(user, region = "US", indiceType = "R1000", startDate, endDate, groups = "ALL", recreateArchive = TRUE, updateOnly = TRUE){
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
  CustomFilter = "ENS == 100 & RELEVANCE == 100"
  
  
  ##### Daily mapping to algo seek tickers
  dbCon<-RP_DatabaseConnect(user)
  mapping_request <- paste0("select * FROM algoseek.daily_symbol_mapping where date_utc >= '",startDate,"' and date_utc <= '",endDate,"'")
  mapping_request_RPData <- dbGetQuery(dbCon, mapping_request)
  RP_DatabaseDisconnect(dbCon)
  
  
  CompanyRPData <- RP_GetAnalyticsDataLocalDB(user = user, region = region, CAP = indiceType, Groups = groups, startDate = startDate,endDate = endDate,version = version, CustomFilter = CustomFilter, colSubset =customColumns)
  CompanyRPData <- as.data.frame(CompanyRPData)
  
  CompanyRPData$EVENT_RELEVANCE <- 100
  CompanyRPData$EVENT_SIMILARITY_DAYS <- CompanyRPData$G_ENS_SIMILARITY_GAP
  CompanyRPData$EVENT_SENTIMENT_SCORE <- CompanyRPData$ESS
  
  CompanyRPData$G_ENS_SIMILARITY_GAP <- NULL
  CompanyRPData$ESS <- NULL
  
  ### source hardcoding : to remove
  
  RP_SOURCE_ID <- CompanyRPData$SOURCE 
  
  CompanyRPData$SOURCE <- "WEBNONPREMIUM"
  CompanyRPData$SOURCE[CompanyRPData$PRODUCT_KEY=='DJ-EQ' | CompanyRPData$PRODUCT_KEY=='PR-EQ' ] <- "DJPR"
  CompanyRPData$SOURCE[RP_SOURCE_ID %in% c('5A5702','9D69F1','86BD04','CBEE83','ED68DC','B5235B','CCE591')] <- "PREMIUM"
  
  print("Number of sources")
  print(length(unique(CompanyRPData$SOURCE)))
  
  ### releasing memory
  CompanyRPData$PRODUCT_KEY <- NULL
  
  minute_range <- 180
  excluding_range <- 0
  ###### Filtering the events which happen during intraday minutes only
  library(lubridate)
  TIMESTAMP_REGION <- as.POSIXlt(CompanyRPData$TIMESTAMP_UTC, "America/New_York")
  TIMESTAMP_REGION_FORM <- ymd_hms(TIMESTAMP_REGION)
  LOCAL_MARKET_HOUR <- hour(TIMESTAMP_REGION_FORM) 
  LOCAL_MARKET_HOUR_MINUTE <- hour(TIMESTAMP_REGION_FORM) + minute(TIMESTAMP_REGION_FORM)/60
  
  
  CompanyRPData <- CompanyRPData[LOCAL_MARKET_HOUR_MINUTE>=(9+(30+excluding_range)/60),]
  CompanyRPData <- CompanyRPData[LOCAL_MARKET_HOUR_MINUTE<=(16-(excluding_range)/60),]
  
  
  rm(TIMESTAMP_REGION) 
  rm(TIMESTAMP_REGION_FORM)
  rm(LOCAL_MARKET_HOUR)
  
  beta_market_estimation_period <- 30
  indice_minutes_RPData <- RP_GetMinutesIndiceLocalDB(user=user, indiceType = "R1000", startDate =(as.Date(startDate)-2*beta_market_estimation_period), endDate = endDate)
  
  
  total_processed_minute_event_df <- NULL
  
  looping_days <- seq(as.Date(startDate), as.Date(endDate), "days")
  
  loop_counter <- 1
  current_month <- NULL
  rmonth_minute_data_RPData <- NULL
  
  for(day in  looping_days){
    indice_minutes_daily_RPData <- indice_minutes_RPData[(indice_minutes_RPData$date_utc <= day) & (indice_minutes_RPData$date_utc >= (day - 2*beta_market_estimation_period)),]
    
    CompanyRPData$DATE <- as.Date(trunc(CompanyRPData$TIMESTAMP_UTC, "day"))
    
    CompanyRPDataDaily <- CompanyRPData[CompanyRPData$DATE == day,]
    Entities <- unique(CompanyRPDataDaily$RP_ENTITY_ID)
    Entities <- Entities[!is.na(Entities)]
    
    daily_mapping <- mapping_request_RPData[mapping_request_RPData$date_utc == day,]
    SymbolEntities <- unique(daily_mapping$symbol[daily_mapping$rp_entity_id %in% Entities])
    print("Reduction due to mapping issues")
    print(1-length(SymbolEntities)/length(Entities))
    CompanyRPDataDaily$TICKER <- daily_mapping$symbol[match(CompanyRPDataDaily$RP_ENTITY_ID, daily_mapping$rp_entity_id, nomatch = NA)]
    CompanyRPDataDaily <- CompanyRPDataDaily[!is.na(CompanyRPDataDaily$TICKER),]
    
    
    month_format <- format(as.Date(day, origin="1970-01-01"), "%Y-%m")
    month_minus_one_format <- format(as.Date(day-10, origin="1970-01-01"), "%Y-%m")
    if (is.null(current_month) || (month_format != current_month)){
      rmonth_minute_data_RPData <- readRDS(file=paste0(outputDataPath,"MINUTE_FILES/", month_format,"MonthR1000_minutes_data_beta_market.rds"))
      rday_minute_data_month_before_RPData <- readRDS(file=paste0(outputDataPath,"MINUTE_FILES/", month_minus_one_format,"MonthR1000_minutes_data_beta_market.rds"))
      rmonth_minute_data_RPData <- rbind(rmonth_minute_data_RPData,rday_minute_data_month_before_RPData)
      rmonth_minute_data_RPData$date_utc <- as.Date(trunc(rmonth_minute_data_RPData$timestamp_utc, "day"))
      current_month <- month_format
    }
    
    rday_minute_data_RPData <- rmonth_minute_data_RPData[rmonth_minute_data_RPData$date_utc <= day,]
    
    
    rday_minute_data_RPData$rp_entity_id <- daily_mapping$rp_entity_id[match(rday_minute_data_RPData$ticker, daily_mapping$symbol, nomatch = NA)]
    rday_minute_data_RPData <- rday_minute_data_RPData[rday_minute_data_RPData$rp_entity_id %in% unique(CompanyRPDataDaily$RP_ENTITY_ID),]
    rday_minute_data_RPData <- rday_minute_data_RPData[!is.na(rday_minute_data_RPData$rp_entity_id),]
    
    if ((dim(rday_minute_data_RPData)[1] > 0)   & (dim(CompanyRPDataDaily)[1] > 0)){
      
      colnames(rday_minute_data_RPData) <- toupper(colnames(rday_minute_data_RPData))
      CompanyRPDataDaily$TIMESTAMP_UTC_MIN <- trunc(CompanyRPDataDaily$TIMESTAMP_UTC, "min")
      
      if (dim(indice_minutes_daily_RPData)[1] >0){
        print("Processing day")
        print(as.Date(day, origin="1970-01-01"))
        print("Processing Events")
        print(dim(CompanyRPDataDaily)[1])
        minute_event_df <- RPDailyMinutesEventMatrixBetaMarketConstruction(Day =day, EventsDF = CompanyRPDataDaily, MinutesDF = rday_minute_data_RPData, MinutesIndiceDF = indice_minutes_daily_RPData,  minute_range = minute_range)
        if (!is.null(minute_event_df)){
          print("Outputing Events")
          print(dim(minute_event_df)[1]/8)
        } else {
          print("Outputing Events")
          print(0)
        }
        total_processed_minute_event_df <- rbind(total_processed_minute_event_df, minute_event_df)
      }
      gc(verbose = FALSE)
      loop_counter <- loop_counter + 1
    } 
    gc(verbose = FALSE)
  }
  
  if(recreateArchive){
    if(!updateOnly){
      RDSFileName = paste0(RP_GetUpdatedDBPath(user = user, region = 'US', type = "INDICE_MINUTES"), "_",paste0("EVENTS_",indiceType), ".rds")
      print(paste0("Saving all archive : ",RDSFileName))
      saveRDS(object =total_processed_minute_event_df, file=RDSFileName)
    } else {
      ##### Getting historical data
      historicalEventProfileData <- RP_GetMinutesIndiceLocalDB(user=user, indiceType = paste0("EVENTS_",indiceType), startDate =NULL, endDate = NULL)
      ####### overwriting the data file 
      RDSFileName = paste0(RP_GetUpdatedDBPath(user = user, region = 'US', type = "INDICE_MINUTES"), "_",paste0("EVENTS_",indiceType), ".rds")
      print(paste0("Updating archive : ",RDSFileName))
      saveRDS(object = rbind(historicalEventProfileData, total_processed_minute_event_df), file=RDSFileName)
    }
  }
  return(total_processed_minute_event_df)
}

RP_UpdateMinuteProfile <- function(user, region = "US", indiceType = "R1000", startDate, endDate, groups = "ALL", recreateArchive = FALSE, updateOnly = FALSE){
  #### hard parameters : to remove
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
  CustomFilter = "RELEVANCE >= 70"
  
  ##### Daily mapping to algo seek tickers
  dbCon<-RP_DatabaseConnect(user)
  mapping_request <- paste0("select * FROM algoseek.daily_symbol_mapping where date_utc >= '",startDate,"' and date_utc <= '",endDate,"'")
  mapping_request_RPData <- dbGetQuery(dbCon, mapping_request)
  RP_DatabaseDisconnect(dbCon)
  
  
  CompanyRPData <- RP_GetAnalyticsDataLocalDB(user = user, region = region, CAP = indiceType, Groups = groups, startDate = startDate,endDate = endDate,version = version, CustomFilter = CustomFilter, colSubset =customColumns, CustomSIMGAP = "CUSTOM")
  CompanyRPData <- as.data.frame(CompanyRPData)
  
  ### source hardcoding : to remove
  CompanyRPData$SOURCE <- "WEBNONPREMIUM"
  CompanyRPData$SOURCE[CompanyRPData$PRODUCT_KEY=='DJ-BD' | CompanyRPData$PRODUCT_KEY=='PR-BD' ] <- "DJPR"
  CompanyRPData$SOURCE[CompanyRPData$RP_SOURCE_ID%in% c('5A5702','9D69F1','86BD04','CBEE83','ED68DC','B5235B','CCE591')] <- "PREMIUM"
  
  
  print("Number of sources")
  print(length(unique(CompanyRPData$SOURCE)))
  ### releasing memory
  CompanyRPData$PRODUCT_KEY <- NULL
  CompanyRPData$RP_SOURCE_ID <- NULL
  
  minute_range <- 180
  excluding_range <- 0
  ###### Filtering the events which happen during intraday minutes only
  library(lubridate)
  TIMESTAMP_REGION <- as.POSIXlt(CompanyRPData$TIMESTAMP_UTC, "America/New_York")
  TIMESTAMP_REGION_FORM <- ymd_hms(TIMESTAMP_REGION)
  LOCAL_MARKET_HOUR <- hour(TIMESTAMP_REGION_FORM) 
  LOCAL_MARKET_HOUR_MINUTE <- hour(TIMESTAMP_REGION_FORM) + minute(TIMESTAMP_REGION_FORM)/60
  
  
  CompanyRPData <- CompanyRPData[LOCAL_MARKET_HOUR_MINUTE>=(9+(30+excluding_range)/60),]
  CompanyRPData <- CompanyRPData[LOCAL_MARKET_HOUR_MINUTE<=(16-(excluding_range)/60),]
  
  rm(TIMESTAMP_REGION) 
  rm(TIMESTAMP_REGION_FORM) 
  rm(LOCAL_MARKET_HOUR) 
  
  beta_market_estimation_period <- 30
  indice_minutes_RPData <- RP_GetMinutesIndiceLocalDB(user=user, indiceType = "R1000", startDate =(as.Date(startDate)-2*beta_market_estimation_period), endDate = endDate)
  
  total_processed_minute_event_df <- NULL
  
  looping_days <- seq(as.Date(startDate), as.Date(endDate), "days")
  
  loop_counter <- 1
  current_month <- NULL
  rmonth_minute_data_RPData <- NULL
  
  for(day in  looping_days){
    indice_minutes_daily_RPData <- indice_minutes_RPData[(indice_minutes_RPData$date_utc <= day) & (indice_minutes_RPData$date_utc >= (day - 2*beta_market_estimation_period)),]
    
    CompanyRPData$DATE <- as.Date(trunc(CompanyRPData$TIMESTAMP_UTC, "day"))
    
    CompanyRPDataDaily <- CompanyRPData[CompanyRPData$DATE == day,]
    Entities <- unique(CompanyRPDataDaily$RP_ENTITY_ID)
    Entities <- Entities[!is.na(Entities)]
    
    daily_mapping <- mapping_request_RPData[mapping_request_RPData$date_utc == day,]
    SymbolEntities <- unique(daily_mapping$symbol[daily_mapping$rp_entity_id %in% Entities])
    
    print("Reduction due to mapping issues")
    print(1-length(SymbolEntities)/length(Entities))
    
    CompanyRPDataDaily$TICKER <- daily_mapping$symbol[match(CompanyRPDataDaily$RP_ENTITY_ID, daily_mapping$rp_entity_id, nomatch = NA)]
    CompanyRPDataDaily <- CompanyRPDataDaily[!is.na(CompanyRPDataDaily$TICKER),]
    
    
    month_format <- format(as.Date(day, origin="1970-01-01"), "%Y-%m")
    month_minus_one_format <- format(as.Date(day-10, origin="1970-01-01"), "%Y-%m")
    
    #### we here fettch the one month estimation window once per month
    if (is.null(current_month) || (month_format != current_month)){
      print("Fetching 2 previous months (30 days history) history for all entities")
      rmonth_minute_data_RPData <- RP_FetchMonthMinutes(user, month = month_format, mapping = mapping_request_RPData)
      
      # rmonth_minute_data_RPData <- readRDS(file=paste0(outputDataPath,"MINUTE_FILES/", month_format,"MonthR1000_minutes_data_beta_market.rds"))
      # rday_minute_data_month_before_RPData <- readRDS(file=paste0(outputDataPath,"MINUTE_FILES/", month_minus_one_format,"MonthR1000_minutes_data_beta_market.rds"))
      
      rday_minute_data_month_before_RPData <- RP_FetchMonthMinutes(user, month = month_minus_one_format, mapping = mapping_request_RPData)
      rmonth_minute_data_RPData <- rbind(rmonth_minute_data_RPData,rday_minute_data_month_before_RPData)
      
      rmonth_minute_data_RPData$date_utc <- as.Date(trunc(rmonth_minute_data_RPData$timestamp_utc, "day"))
      
      current_month <- month_format
    }
    
    rday_minute_data_RPData <- rmonth_minute_data_RPData[rmonth_minute_data_RPData$date_utc <= day,]
    rday_minute_data_RPData <- rday_minute_data_RPData[rday_minute_data_RPData$date_utc >= (day-beta_market_estimation_period),]
    
    rday_minute_data_RPData$rp_entity_id <- daily_mapping$rp_entity_id[match(rday_minute_data_RPData$ticker, daily_mapping$symbol, nomatch = NA)]
    rday_minute_data_RPData <- rday_minute_data_RPData[rday_minute_data_RPData$rp_entity_id %in% unique(CompanyRPDataDaily$RP_ENTITY_ID),]
    rday_minute_data_RPData <- rday_minute_data_RPData[!is.na(rday_minute_data_RPData$rp_entity_id),]
    
    rday_minute_data_RPData <- rday_minute_data_RPData[!is.na(rday_minute_data_RPData$rp_entity_id),]
    
    if ((dim(rday_minute_data_RPData)[1] > 0)   & (dim(CompanyRPDataDaily)[1] > 0)){
      colnames(rday_minute_data_RPData) <- toupper(colnames(rday_minute_data_RPData))
      CompanyRPDataDaily$TIMESTAMP_UTC_MIN <- trunc(CompanyRPDataDaily$TIMESTAMP_UTC, "min")
      
      if (dim(indice_minutes_daily_RPData)[1] >0){
        print("Processing day")
        print(as.Date(day, origin="1970-01-01"))
        print("Processing Events")
        print(dim(CompanyRPDataDaily)[1])
        minute_event_df <- RPDailyMinutesEventMatrixBetaMarketConstruction(Day =day, EventsDF = CompanyRPDataDaily, MinutesDF = rday_minute_data_RPData, MinutesIndiceDF = indice_minutes_daily_RPData,  minute_range = minute_range)
        if (!is.null(minute_event_df)){
          print("Outputing Events")
          ### divide by 9 because a row for volatility, volume, market models, statistics test ...
          print(dim(minute_event_df)[1]/9)
        } else {
          print("Outputing Events")
          print(0)
        }
        total_processed_minute_event_df <- rbind(total_processed_minute_event_df, minute_event_df)
      }
      gc(verbose = FALSE)
      loop_counter <- loop_counter + 1
    } 
    gc(verbose = FALSE)
  }
  
  
  if(recreateArchive){
    if(!updateOnly){
      RDSFileName = paste0(RP_GetUpdatedDBPath(user = user, region = 'US', type = "INDICE_MINUTES"), "_",paste0("EVENTS_",indiceType), ".rds")
      print(paste0("Saving all archive : ",RDSFileName))
      saveRDS(object =total_processed_minute_event_df, file=RDSFileName)
    } else {
      ##### Getting historical data
     
      ####### overwriting the data file 
      RDSFileName = paste0(RP_GetUpdatedDBPath(user = user, region = 'US', type = "INDICE_MINUTES"), "_",paste0("EVENTS_",indiceType), ".rds")
      historicalEventProfileData <- readRDS(file = RDSFileName)
      historicalEventProfileData <- rbind(historicalEventProfileData, total_processed_minute_event_df)
      historicalEventProfileData <- RP_GetMinutesIndiceLocalDB(user=user, indiceType = paste0("EVENTS_",indiceType), startDate =NULL, endDate = NULL)
      
      print(paste0("Updating archive : ",RDSFileName))
      saveRDS(object = rbind(historicalEventProfileData, total_processed_minute_event_df), file=RDSFileName)
    }
  }
  
  all_profiles_df <- RP_ComputeAllProfile(total_processed_minute_event_df)
  
  mergedProfiles <- RP_UpdateShinyAppsProfiles(all_profiles_df)
  
  RP_UpdateAverageProfile(mergedProfiles)
}


RP_FetchMonthMinutes <- function(user, month, mapping){
  dbCon<-RP_DatabaseConnect(user)
  SymbolEntities <- unique(mapping$symbol)
  EntitiesList <- paste("'",SymbolEntities,"'",collapse=",",sep='')
  EntitiesList <- paste0("(" ,EntitiesList,")")
  dailyRequest <- paste0("select * FROM algoseek.algoseek_r1000 WHERE to_char(timestamp_utc,'yyyy-mm') =  '",month,"' and ticker in ",EntitiesList)
  rday_minute_data_RPData <- dbGetQuery(dbCon, dailyRequest)
  RP_DatabaseDisconnect(dbCon)
  return(rday_minute_data_RPData)
}


RP_ComputeAllProfile <- function(processed_minute_event_df){
  #### to be updated to take into account time lapses
  lapse <- -1  
  corradoStatsAll <- NULL
  returnsStatsAll <- NULL
  volumeStatsAll <- NULL
  volatilityStatsAll <- NULL
  
  sentiment_criterias <- c("POSITIVE","NEGATIVE")
  # sentiment_criterias <- c("POSITIVE")
  # aggregate_criterias <- c("GROUP")
  aggregate_criterias <- c("GROUP","CATEGORY")
  
  RELEVANCE_EVENT_RELEVANCE_COMBO <- c("HIGH_HIGH","HIGH_MEDIUM","HIGH_LOW","MEDIUM_MEDIUM","MEDIUM_LOW","LOW_LOW" )
  
  SOURCE_CRITERIA <-  c("DJPR","PREMIUM","WEBNONPREMIUM")
  
  EVENT_SIMILARITY_DAYS <-  c(0,1,7,30,90,186,365)
  # saveRDS(object = unique(total_processed_minute_event_df$GROUP), file=paste0(outputDataPath,"allbigeventsGroup.rds"))
  # saveRDS(object = unique(total_processed_minute_event_df$CATEGORY), file=paste0(outputDataPath,"allbigeventsCategory.rds"))
  # ALL_GROUPS <- readRDS(file=paste0(outputDataPath,"allbigeventsGroup.rds"))
  # ALL_CATEGORIES <- readRDS(file=paste0(outputDataPath,"allbigeventsCategory.rds"))
  
  ALL_GROUPS <- unique(processed_minute_event_df$GROUP)
  ALL_CATEGORIES <- unique(processed_minute_event_df$CATEGORY)
  
  print("groups categories dimensions")
  print(length(ALL_GROUPS))
  print(length(ALL_CATEGORIES))
  
  
  for (my_source in SOURCE_CRITERIA){
    #   for (my_relevance in RELEVANCE){
    #     for (my_event_relevance in EVENT_RELEVANCE){
    for (my_event_relevance_combo in RELEVANCE_EVENT_RELEVANCE_COMBO){
      my_relevance <- NULL
      my_event_relevance <- NULL
      if(my_event_relevance_combo == "HIGH_HIGH"){
        my_relevance <- "HIGH"
        my_event_relevance <- "HIGH"
      }
      
      if(my_event_relevance_combo == "HIGH_MEDIUM"){
        my_relevance <- "HIGH"
        my_event_relevance <- "MEDIUM"
      }
      
      if(my_event_relevance_combo == "HIGH_LOW"){
        my_relevance <- "HIGH"
        my_event_relevance <- "LOW"
      }
      
      
      if(my_event_relevance_combo == "MEDIUM_MEDIUM"){
        my_relevance <- "MEDIUM"
        my_event_relevance <- "MEDIUM"
      }
      
      if(my_event_relevance_combo == "MEDIUM_LOW"){
        my_relevance <- "MEDIUM"
        my_event_relevance <- "LOW"
      }
      
      if(my_event_relevance_combo == "LOW_LOW"){
        my_relevance <- "LOW"
        my_event_relevance <- "LOW"
      }
      
      for (similarity_gap_filter in EVENT_SIMILARITY_DAYS){
        for (aggregate_criteria in aggregate_criterias){
          for (sentiment_criteria in sentiment_criterias){
            
            ALL_EVENTS <- NULL
            if(aggregate_criteria == "GROUP"){
              ALL_EVENTS <- ALL_GROUPS
            }
            if(aggregate_criteria == "CATEGORY"){
              ALL_EVENTS <- ALL_CATEGORIES
            }
            all_event_filtered_df <- RP_FilterDataLight(processed_minute_event_df, my_source, aggregate_criteria,my_event_relevance_combo,similarity_gap_filter,sentiment_criteria)
            
            print("all events dimension")
            print(length(ALL_EVENTS))
            
            for(my_event in ALL_EVENTS){
              filtered_df <- all_event_filtered_df[all_event_filtered_df$EVENT == my_event,]
              
              event_number_event_filtering <- dim(filtered_df)[1]
              print(event_number_event_filtering)
              # event_number_event_filtering <- event_number_event_filtering*4/3
              if(event_number_event_filtering >0){
                volatilityStats <- RP_ComputeVolatility(lapse,my_relevance, my_event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering,my_event, localSource = my_source, dataFrame = filtered_df, Russell_version = "R1000")
                volatilityStatsAll <- rbind(volatilityStatsAll,volatilityStats)
                
                
                volumeStats <- RP_ComputeVolumes(lapse,my_relevance, my_event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering,my_event, localSource = my_source, dataFrame = filtered_df, Russell_version = "R1000")
                volumeStatsAll <- rbind(volumeStatsAll,volumeStats)
                
                
                returnStats <- RP_ComputeReturns(lapse,my_relevance, my_event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering,my_event, localSource = my_source, dataFrame = filtered_df, Russell_version = "R1000")
                returnsStatsAll <- rbind(returnsStatsAll,returnStats)
                
                print("dealing with event")
                print(my_event)
                
                corradoCenteredStats <- RP_ComputeCorradoStatistics(lapse,my_relevance, my_event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering,my_event, localSource = my_source, dataFrame = filtered_df, Russell_version = "R1000")
                ordinCenteredStats <- RP_ComputeORDINStatistics(lapse,my_relevance, my_event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering,my_event, localSource = my_source, dataFrame = filtered_df, Russell_version = "R1000")
                
                ####### 73 5 minutes bucket in our estimation window interval
                retToAdd <- returnStats[,1:73]
                colnames(retToAdd) <- paste0("RET",colnames(retToAdd))
                
                ordToAdd <- ordinCenteredStats$event_minutes_matrix_all_methodo[,1:73]
                colnames(ordToAdd) <- paste0("ORD",colnames(ordToAdd))
                
                voluToAdd <- volumeStats[,1:73]
                colnames(voluToAdd) <- paste0("VOLU",colnames(voluToAdd))
                
                volaToAdd <- volatilityStats[,1:73]
                colnames(volaToAdd) <- paste0("VOLA",colnames(volaToAdd))
                
                corradoCenteredStats$event_minutes_matrix_all_methodo <- cbind(corradoCenteredStats$event_minutes_matrix_all_methodo,retToAdd,ordToAdd,voluToAdd,volaToAdd)
                # ordinCenteredStats$event_minutes_matrix_all_methodo <- cbind(ordinCenteredStats$event_minutes_matrix_all_methodo,retToAdd,corrToAdd)
                corradoStatsAll <- rbind(corradoStatsAll,corradoCenteredStats$event_minutes_matrix_all_methodo)
                # corradoStatsAll <- rbind(corradoStatsAll,ordinCenteredStats$event_minutes_matrix_all_methodo)
                
                print("Adding event")
                print(my_event)
                print("Results size increasing to")
                print(dim(corradoStatsAll))
              } else {
                print(my_event)
                # print(paste0(lapse,my_relevance, my_event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering,my_event,my_source))
                print(paste0("No data for event : ",my_event))
              }
            }
            rm(all_event_filtered_df)
            gc()
          }
        }
      }
    }
  }
  return(corradoStatsAll)
}


RP_FilterDataLight  <- function(dataframe, my_source, aggregate_criteria,my_event_relevance_combo,similarity_gap_filter,sentiment_criteria){
  dataframe <- dataframe[(dataframe$SOURCE == my_source),]
  if (aggregate_criteria == "CATEGORY"){
    dataframe$EVENT <- dataframe$CATEGORY
    # total_processed_volume_minute_event_df$EVENT <- total_processed_volume_minute_event_df$CATEGORY      
  }
  
  if (aggregate_criteria == "GROUP"){
    dataframe$EVENT <- dataframe$GROUP
    # total_processed_volume_minute_event_df$EVENT <- total_processed_volume_minute_event_df$GROUP
  }
  
  if(my_event_relevance_combo == "HIGH_HIGH"){
    dataframe <- dataframe[dataframe$RELEVANCE == 100,]
    dataframe <- dataframe[dataframe$EVENT_RELEVANCE >= 90,]
  }
  
  if(my_event_relevance_combo == "HIGH_MEDIUM"){
    dataframe <- dataframe[dataframe$RELEVANCE == 100,]
    dataframe <- dataframe[dataframe$EVENT_RELEVANCE > 70 & dataframe$EVENT_RELEVANCE < 90,]
  }
  
  if(my_event_relevance_combo == "HIGH_LOW"){
    dataframe <- dataframe[dataframe$RELEVANCE == 100,]
    dataframe <- dataframe[dataframe$EVENT_RELEVANCE <= 70,]
  }
  
  
  if(my_event_relevance_combo == "MEDIUM_MEDIUM"){
    dataframe <- dataframe[dataframe$RELEVANCE >= 90 &  dataframe$RELEVANCE < 100,]
    dataframe <- dataframe[dataframe$EVENT_RELEVANCE > 70 & dataframe$EVENT_RELEVANCE < 90,]
  }
  
  if(my_event_relevance_combo == "MEDIUM_LOW"){
    dataframe <- dataframe[dataframe$RELEVANCE >= 90 &  dataframe$RELEVANCE < 100,]
    dataframe <- dataframe[dataframe$EVENT_RELEVANCE <= 70,]
  }
  
  if(my_event_relevance_combo == "LOW_LOW"){
    dataframe <- dataframe[dataframe$RELEVANCE < 90,]
    dataframe <- dataframe[dataframe$EVENT_RELEVANCE <= 70,]
  }
  
  dataframe <- dataframe[dataframe$EVENT_SIMILARITY_DAYS >= similarity_gap_filter,]
  # total_processed_volume_minute_event_df <- total_processed_volume_minute_event_df[total_processed_volume_minute_event_df$SIMILARITY_GAP >= similarity_gap_filter,]
  
  if (sentiment_criteria == "POSITIVE"){
    dataframe <- dataframe[dataframe$EVENT_SENTIMENT_SCORE > 0,]
    # total_processed_volume_minute_event_df <- total_processed_volume_minute_event_df[total_processed_volume_minute_event_df$ESS>50,]
    
  }
  
  if (sentiment_criteria == "NEGATIVE"){
    dataframe <- dataframe[dataframe$EVENT_SENTIMENT_SCORE < 0,]
    # total_processed_volume_minute_event_df <- total_processed_volume_minute_event_df[total_processed_volume_minute_event_df$ESS<50,]
  }
  #   
  #   dataframe <- dataframe[dataframe$EVENT == my_event,]
  return(dataframe)
}


RP_UpdateShinyAppsProfiles <- function(newly_processed_profiles, recreateArchive = FALSE){
  
  RDSFileName = paste0(RP_GetUpdatedDBPath(user = user, region = 'US', type = "INDICE_MINUTES", Level = "Production"), "_",paste0("PROFILES_",indiceType), ".rds")
  historicalProfilesData <- readRDS(file = RDSFileName)
  print(dim(historicalProfilesData))
  print(dim(newly_processed_profiles))
  
  ###### petty calculation to match the historic : to withdraw
  newly_processed_profiles$correcting_factor <- 2*(newly_processed_profiles$sentiment_criteria == "POSITIVE"  | newly_processed_profiles$sentiment_criteria == "SPREAD")-1
  stats_post_sign <- colnames(newly_processed_profiles)[which(as.numeric(colnames(newly_processed_profiles)) >= 0)]
  rets_post <- paste0("RET",stats_post_sign)
  infinity_return <- rowSums(newly_processed_profiles[,rets_post]*newly_processed_profiles$correcting_factor*newly_processed_profiles[,stats_post_sign],na.rm = TRUE)
  infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
  significance <- (newly_processed_profiles$event_number_event_filtering-min(newly_processed_profiles$event_number_event_filtering))/(max(newly_processed_profiles$event_number_event_filtering)-min(newly_processed_profiles$event_number_event_filtering))
  infinity_return_global <- infinity_return
  infinity_return <- infinity_return*significance
  newly_processed_profiles$infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
  newly_processed_profiles$infinity_return_global <- (infinity_return_global-min(infinity_return_global))/(max(infinity_return_global)-min(infinity_return_global))
  
  print("Merging the profiles here")
  
  oldColumns <- colnames(historicalProfilesData)
  # print(identical(colnames(historicalProfilesData),colnames(newly_processed_profiles)))
  
  # ####################
  # ####################
  # ####################
  # ####################
  # #################### Merging to get the best
  # data2 <- readRDS(file=paste0(outputDataPath,"r2000_corrado_df.rds"))
  # # data1 <- readRDS(file=paste0(outputDataPath,"r1000_corrado_df.rds"))
  # data1 <- readRDS(file=paste0(outputDataPath,"src_r1000_corrado_df.rds"))
  # 
  # data1$RET180[is.na(data1$RET180)] <- 0
  # data2$RET180[is.na(data2$RET180)] <- 0
  # 
  # data1$RET180 <- rnorm(n=dim(data1)[1])
  stats_sign <- colnames(historicalProfilesData)[which(!is.na(as.numeric(colnames(historicalProfilesData))))]
  
  ords <- paste0("ORD",stats_sign)
  rets <- paste0("RET",stats_sign)
  volas <- paste0("VOLA",stats_sign)
  volus <- paste0("VOLU",stats_sign)
  
  additional_col_to_drop <- c("lapse","correcting_factor","infinity_return","infinity_return_global","corrado_methodo","event_number_event_filtering")
  
  joining_columns <- setdiff(colnames(historicalProfilesData), c(c(stats_sign,ords,rets,volas,volus),additional_col_to_drop))
  
  # new_columns <- paste0("NEW", colnames(newly_processed_profiles)[which(!(colnames(newly_processed_profiles) %in% joining_columns))])
  
  newData <- paste0("NEW",c(stats_sign,ords,rets,volas,volus))
  
  colnames(newly_processed_profiles)[which(!(colnames(newly_processed_profiles) %in% joining_columns))] <- paste0("NEW", colnames(newly_processed_profiles)[which(!(colnames(newly_processed_profiles) %in% joining_columns))])
  
  print("colliding datasets")
  MergedProfilesData <- merge(historicalProfilesData,newly_processed_profiles,all.x = TRUE, by = joining_columns)
  
  mergeProfiles <- function(row){
    newCount <- row[,"NEWevent_number_event_filtering"]
    if(!is.na(newCount)){
      oldCount <- row[,"event_number_event_filtering"]
      
      stats_sign <- colnames(row)[which(!is.na(as.numeric(colnames(row))))]
      ords <- paste0("ORD",stats_sign)
      rets <- paste0("RET",stats_sign)
      volas <- paste0("VOLA",stats_sign)
      volus <- paste0("VOLU",stats_sign)
      
      new_ords <- paste0("NEW", ords)
      new_stats_sign <- paste0("NEW", stats_sign)
      new_rets <- paste0("NEW", rets)
      new_volas <- paste0("NEW", volas)
      new_volus <- paste0("NEW", volus)
      
      weightnew <- newCount/(newCount+oldCount)
      weightold <- oldCount/(newCount+oldCount)
      
      row[,ords]       <- weightold*row[,ords] + weightnew*row[,new_ords]
      row[,rets]       <- weightold*row[,rets] + weightnew*row[,new_rets]
      row[,volas]      <- weightold*row[,volas] + weightnew*row[,new_volas]
      row[,volus]      <- weightold*row[,volus] + weightnew*row[,new_volus]
      row[,stats_sign] <- weightold*row[,stats_sign] + weightnew*row[,new_stats_sign]
    }
    return(row)
  }
  
  print("Updating profiles")
  UpdatedProfiles <- ddply(.data = MergedProfilesData, .variables = joining_columns, .fun = function(x){mergeProfiles(x)})
  
  if(recreateArchive){
    RDSFileName = paste0(RP_GetUpdatedDBPath(user = user, region = 'US', type = "INDICE_MINUTES", Level = "Production"), "_",paste0("PROFILES_",indiceType), ".rds")
    saveRDS(object = UpdatedProfiles, file = RDSFileName)
  }
  
  UpdatedProfiles <- UpdatedProfiles[,oldColumns]
  return(UpdatedProfiles)
}



RP_UpdateAverageProfiles <- function(newly_processed_profiles){
  
}

RP_CalibrateMarketModel <- function(LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN,CLOSE_PRICE){
  LAGGED_CLOSE_PRICE <- mlag(CLOSE_PRICE,1)
  LOG_MINUTES_RETURN <- log(CLOSE_PRICE/LAGGED_CLOSE_PRICE)
  market_model <- tryCatch(lm(LOG_MINUTES_RETURN~LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN+LAGGED_CLOSE_PRICE), error = function(e) {NULL})
  if(!is.null(market_model)){
    return (list(betam=market_model$coefficients[2], gamma=market_model$coefficients[3], sigma=sd(market_model$residuals,na.rm = TRUE)))
  } else {
    return(list(betam=1, gamma=0, sigma=1))
  }
}  


RP_CalibrateDailyMinuteBetaMarket <- function(Day,  EventsDT, MinutesDT, MinutesIndiceDT){
  incomingNBEvents <- dim(EventsDT)[1]
  setkey(MinutesDT, TIMESTAMP_UTC_MIN_NUM)
  setkey(MinutesIndiceDT, TIMESTAMP_UTC_MIN_NUM)
  
  AllIn <- merge(MinutesDT,MinutesIndiceDT,all.x = TRUE, by = "TIMESTAMP_UTC_MIN_NUM")
  setkey(EventsDT, TICKER, TIMESTAMP_UTC_MIN_NUM)
  setkey(AllIn, TICKER, TIMESTAMP_UTC_MIN_NUM)
  
  AllIn <- merge(AllIn,EventsDT,all.x = TRUE,all.y = TRUE, by=c("TICKER","TIMESTAMP_UTC_MIN_NUM"))
  setnames(AllIn, "RP_ENTITY_ID.x", "RP_ENTITY_ID")
  setnames(AllIn, "RP_ENTITY_ID.y", "RP_ENTITY_ID_EVENT")
  setkey(AllIn, TICKER, TIMESTAMP_UTC_MIN_NUM)
  
  corradoAbnDT <- AllIn[,computeCorradoAbnormalRets(TIMESTAMP_UTC_MIN_NUM,MINUTE,HOUR,DAY,HIGH_PRICE,LOW_PRICE,LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN,CLOSE_PRICE,RP_ENTITY_ID_EVENT,SOURCE,TOTAL_VOLUME),by=c("TICKER")]
  if(dim(corradoAbnDT)[1] == 0){
    if(incomingNBEvents){
      print(paste0("Problem Outputing events : ", 0))
    }
    return(NULL)
  } else {
    corradoAbnDF <- as.data.frame(corradoAbnDT)
    EventsDTF <- as.data.frame(EventsDT)
    minute_event_df <- merge(corradoAbnDF,EventsDTF[,c("DATE","TIMESTAMP_UTC_MIN_NUM","RP_ENTITY_ID","GROUP","CATEGORY","RELEVANCE","EVENT_RELEVANCE","EVENT_SIMILARITY_DAYS","EVENT_SENTIMENT_SCORE","SOURCE")],all.x=TRUE,by=c("TIMESTAMP_UTC_MIN_NUM","RP_ENTITY_ID","SOURCE"))
    outputingEvents <- dim(minute_event_df)[1]/8
    if (outputingEvents != incomingNBEvents){
      print(paste0("Problem Outputing events : ", outputingEvents, " instead of ",incomingNBEvents))
    }
    return(minute_event_df)
  }
  return(minute_event_df)
}


RPDailyMinutesEventMatrixBetaMarketConstruction <- function(Day,EventsDF, MinutesDF, MinutesIndiceDF, minute_range = 180){
  colnames(MinutesIndiceDF) <- toupper(colnames(MinutesIndiceDF))
  
  EventsDF$REF_MINUTE <- minute(ymd_hms(EventsDF$TIMESTAMP_UTC_MIN))
  EventsDF$REF_HOUR <- hour(ymd_hms(EventsDF$TIMESTAMP_UTC_MIN))
  EventsDF$REF_DAY <- day(ymd_hms(EventsDF$TIMESTAMP_UTC_MIN))
  
  
  EventsDF$TIMESTAMP_UTC_MIN_NUM <- as.integer(as.numeric(EventsDF$TIMESTAMP_UTC_MIN))
  
  EventsDF$TIMESTAMP_UTC <- NULL
  EventsDF$TIMESTAMP_UTC_MIN <- NULL
  EventsDF$TIMESTAMP_UTC_FORM <- NULL
  EventsDF$TIMESTAMP_REGION <- NULL
  EventsDF$TIMESTAMP_REGION_FORM <- NULL
  
  EventsDT <-  as.data.table(EventsDF)
  
  
  ##################### Trimming by keeping at the minute level the most novel one
  trim_events <- function(sameMinuteEvents){
    uniqueRow <- sameMinuteEvents[which(sameMinuteEvents$EVENT_SIMILARITY_DAYS == max(sameMinuteEvents$EVENT_SIMILARITY_DAYS)),]
    if(dim(uniqueRow)[1] > 1){
      uniqueRow <- uniqueRow[which(abs(uniqueRow$EVENT_SENTIMENT_SCORE) == max(abs(uniqueRow$EVENT_SENTIMENT_SCORE))),]
    }
    return(uniqueRow[1,])
  }
  ######################
  
  
  EventsDT <- as.data.table(ddply(EventsDT, .(TICKER, TIMESTAMP_UTC_MIN_NUM,SOURCE), trim_events))
  
  if(dim(EventsDT)[1] != dim(unique(EventsDT[,.(TICKER, TIMESTAMP_UTC_MIN_NUM,SOURCE)]))[1]){
    print("we have a problem")
  }
  
  
  MinutesDF$TIMESTAMP_REGION_FORM <- ymd_hms(MinutesDF$TIMESTAMP_EST)
  MinutesDF$NO_EVENT_LOCAL_MARKET_HOUR <- hour(MinutesDF$TIMESTAMP_REGION_FORM) 
  MinutesDF$NO_EVENT_LOCAL_MARKET_HOUR_MINUTE <- hour(MinutesDF$TIMESTAMP_REGION_FORM) + minute(MinutesDF$TIMESTAMP_REGION_FORM)/60
  
  MinutesDF <- MinutesDF[MinutesDF$NO_EVENT_LOCAL_MARKET_HOUR_MINUTE>=9,]
  MinutesDF <- MinutesDF[MinutesDF$NO_EVENT_LOCAL_MARKET_HOUR_MINUTE<=16,]
  
  
  MinutesDF$MINUTE <- minute(ymd_hms(MinutesDF$TIMESTAMP_UTC))
  MinutesDF$HOUR <- hour(ymd_hms(MinutesDF$TIMESTAMP_UTC))
  MinutesDF$DAY <- day(ymd_hms(MinutesDF$TIMESTAMP_UTC))
  
  MinutesDF$TIMESTAMP_UTC_MIN_NUM <- as.integer(as.numeric(trunc(MinutesDF$TIMESTAMP_UTC, "min")))
  MinutesIndiceDF$TIMESTAMP_UTC_MIN_NUM <- as.integer(as.numeric(trunc(MinutesIndiceDF$TIMESTAMP_UTC, "min")))
  
  
  MinutesDF$TIMESTAMP_EST <- NULL
  MinutesDF$TIMESTAMP_REGION_FORM <- NULL
  MinutesDF$TIMESTAMP_UTC_FORM <- NULL
  MinutesDF$TIMESTAMP_UTC <- NULL
  
  MinutesIndiceDF$TIMESTAMP_EST <- NULL
  MinutesIndiceDF$TIMESTAMP_REGION_FORM <- NULL
  MinutesIndiceDF$TIMESTAMP_UTC_FORM <- NULL
  MinutesIndiceDF$TIMESTAMP_UTC <- NULL
  
  MinutesDT <- as.data.table(MinutesDF)
  MinutesIndiceDT <- as.data.table(MinutesIndiceDF)
  if( dim(EventsDT)[1] > 0 ){
    minute_event_df <- RP_CalibrateDailyMinuteBetaMarket(Day, EventsDT, MinutesDT, MinutesIndiceDT)
    return(minute_event_df)
  } else {
    return(NULL)
  }
  
}

RP_ComputeVolatility <- function(lapse,relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, Russell_version = "R1000"){
  event_minutes_matrix_all_methodo <- NULL
  
  for (beta_methodo in  c("VOLATILITY")){
    print("Event :")
    print(my_event)
    print("Market model :")
    print(beta_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
    print("Volatility")
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    print(dim(event_minutes_matrix))
    
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    print(dim(event_minutes_matrix))
    
    print("we nullify when no data")#### algoseek spec : no transaction no volatility
    event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
    ########## ignoring outliers
    volatility_threshold <- 20 ### got from the empirical distribution of abnormal volatility
    event_minutes_matrix[event_minutes_matrix > volatility_threshold] <- volatility_threshold
    
    event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        return(list(max(index),mean(value,na.rm=TRUE)))
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      # print(tt)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    # event_minutes_vector <- cumsum(event_minutes_vector)
    
    
    event_minutes_matrix <- melt(event_minutes_vector)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    
    colnames(event_minutes_matrix)[2] <- beta_methodo
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
  }
  
  event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
  nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]
  
  event_minutes_matrix_all_methodo$relevance <- relevance
  event_minutes_matrix_all_methodo$lapse <- lapse
  event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  event_minutes_matrix_all_methodo$my_event <- my_event
  event_minutes_matrix_all_methodo$localSource <- localSource
  
  
  
  event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
  colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_ONE","BETA_DAY","BETA_MINUTE")
  event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_MINUTE")
  
  return(event_minutes_matrix_all_methodo)
  
}

outputVolatility <- function(lapse,relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, asymmetric = FALSE, plotInArborescence = FALSE, toPlot= FALSE,Russell_version = "R1000"){
  event_minutes_matrix_all_methodo <- NULL
  
  for (beta_methodo in  c("VOLATILITY")){
    print("Event :")
    print(my_event)
    print("Market model :")
    print(beta_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
    print("Volatility")
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    print(dim(event_minutes_matrix))
    
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    print(dim(event_minutes_matrix))
    
    print("we nullify when no data")
    event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
    volatility_threshold <- 20 ### got from the empirical distribution of abnormal volatility
    event_minutes_matrix[event_minutes_matrix > volatility_threshold] <- volatility_threshold
    
    event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        return(list(max(index),mean(value,na.rm=TRUE)))
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      # print(tt)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    # event_minutes_vector <- cumsum(event_minutes_vector)
    
    
    event_minutes_matrix <- melt(event_minutes_vector)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    
    colnames(event_minutes_matrix)[2] <- beta_methodo
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
  }
  
  if(is.null(localSource)){
    localSource <- "DJ-EQ"
  }
  
  
  if(asymmetric){
    event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[event_minutes_matrix_all_methodo$MINUTES >= -60,]
  }
  
  event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0
  
  if(toPlot){
    
    
    if (plotInArborescence){
      pathToSave <- NULL
      filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_volume_minute_event_plot")
      if(product_criteria == "DJ-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      if(product_criteria == "WE-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[my_source]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      
      
      g <- tryCatch(RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T, XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
                    , error = function(e) {NULL})
      
      if(is.null(g)){
        print("Trouble printing : ")
        print(pathToSave)
      }
      
    }
    
    pathToSave <- NULL
    if(product_criteria == "DJ-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER/",product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_volume_minute_event_plot")
    }
    
    if(product_criteria == "WE-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER/",product_criteria,"_",premium_sources_libelle[[my_source]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_volume_minute_event_plot")
    }
    
    g <- tryCatch(
      RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
      
      , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
  }
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
  nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]
  
  event_minutes_matrix_all_methodo$relevance <- relevance
  event_minutes_matrix_all_methodo$lapse <- lapse
  event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  event_minutes_matrix_all_methodo$my_event <- my_event
  event_minutes_matrix_all_methodo$localSource <- localSource
  
  
  
  event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
  colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_ONE","BETA_DAY","BETA_MINUTE")
  event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_MINUTE")
  
  return(event_minutes_matrix_all_methodo)
  
}


RP_ComputeVolumes <- function(lapse, relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180,Russell_version = "R1000"){  
  event_minutes_matrix_all_methodo <- NULL
  # for (beta_methodo in  c("BETA_ONE","BETA_DAY","BETA_MINUTE")){
  for (beta_methodo in  c("VOLUME")){
    # print("Event :")
    # print(my_event)
    # print("Market model :")
    # print(beta_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
    # print("Volumes")
    # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    # print(dim(event_minutes_matrix))
    
    #########################
    #########################
    #########################
    ######################### ACTUAL PLOTTING ACTUAL PLOTTING
    event_minutes_vector <- NULL
    ###### computing Corrado test here
    # print(dim(event_minutes_matrix))
    
    print("we nullify when no data")
    event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
    volume_threshold <- 20 ### got from the empirical distribution of abnormal volatility
    event_minutes_matrix[event_minutes_matrix > volume_threshold] <- volume_threshold
    
    event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        return(list(max(index),mean(value,na.rm=TRUE)))
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      # print(tt)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    event_minutes_matrix <- melt(event_minutes_vector)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    #     
    #     event_minutes_matrix$value <- event_minutes_matrix$value*1000
    #     event_minutes_matrix$value <- event_minutes_matrix$value-event_minutes_matrix$value[(dim(event_minutes_matrix)[1]- 1)/2+1]
    
    colnames(event_minutes_matrix)[2] <- beta_methodo
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
  }
  
  event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
  nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]
  
  event_minutes_matrix_all_methodo$lapse <- lapse
  event_minutes_matrix_all_methodo$relevance <- relevance
  event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  event_minutes_matrix_all_methodo$my_event <- my_event
  event_minutes_matrix_all_methodo$localSource <- localSource
  
  
  event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
  colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_ONE","BETA_DAY","BETA_MINUTE")
  event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_MINUTE")
  
  return(event_minutes_matrix_all_methodo)
  
}


outputVolumes <- function(lapse, relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, asymmetric = FALSE, plotInArborescence = FALSE, toPlot= FALSE,Russell_version = "R1000"){  event_minutes_matrix_all_methodo <- NULL
# for (beta_methodo in  c("BETA_ONE","BETA_DAY","BETA_MINUTE")){
for (beta_methodo in  c("VOLUME")){
  # print("Event :")
  # print(my_event)
  # print("Market model :")
  # print(beta_methodo)
  dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
  # print("Volumes")
  # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
  event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
  # print(dim(event_minutes_matrix))
  
  
  
  #########################
  #########################
  #########################
  ######################### ACTUAL PLOTTING ACTUAL PLOTTING
  event_minutes_vector <- NULL
  
  ###### computing Corrado test here
  # print(dim(event_minutes_matrix))
  
  print("we nullify when no data")
  event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
  volume_threshold <- 20 ### got from the empirical distribution of abnormal volatility
  event_minutes_matrix[event_minutes_matrix > volume_threshold] <- volume_threshold
  
  event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
  # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
  event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
  if (nb_minutes_bucket > 1){
    print("Bucketing by")
    print(nb_minutes_bucket)
    
    
    column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
    up_bound <- round((2*nb_range+1)/5)+1
    just_after_event_index <- which(column_index == 0)+1
    
    
    
    buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
    buckets_index_change <- c(0,diff(buckets_index))
    
    while(buckets_index_change[just_after_event_index] != 1){
      buckets_index_change <- buckets_index_change[-1]
      buckets_index <- buckets_index[-1]
    }
    to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
    bucket_value <- function(index,value){
      return(list(max(index),mean(value,na.rm=TRUE)))
    }
    
    tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
    tt <- unique(tt[,.(buck_index,buck_return)])
    # print(tt)
    event_minutes_vector <- tt$buck_return
    event_minutes_index <- tt$buck_index
  } 
  
  event_minutes_matrix <- melt(event_minutes_vector)
  event_minutes_matrix$MINUTES <- event_minutes_index
  event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
  #     
  #     event_minutes_matrix$value <- event_minutes_matrix$value*1000
  #     event_minutes_matrix$value <- event_minutes_matrix$value-event_minutes_matrix$value[(dim(event_minutes_matrix)[1]- 1)/2+1]
  
  colnames(event_minutes_matrix)[2] <- beta_methodo
  
  if(is.null(event_minutes_matrix_all_methodo)){
    event_minutes_matrix_all_methodo <- event_minutes_matrix
  } else {
    event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
  }
  
}

if(is.null(localSource)){
  localSource <- "DJ-EQ"
}


if(asymmetric){
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[event_minutes_matrix_all_methodo$MINUTES >= -60,]
}

event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0

if(toPlot){
  
  
  if (plotInArborescence){
    pathToSave <- NULL
    filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_volume_minute_event_plot")
    if(product_criteria == "DJ-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
    }
    
    if(product_criteria == "WE-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[my_source]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
    }
    
    
    
    g <- tryCatch(RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T, XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
                  , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
    
  }
  
  pathToSave <- NULL
  if(product_criteria == "DJ-EQ"){
    pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER/",product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_volume_minute_event_plot")
  }
  
  if(product_criteria == "WE-EQ"){
    pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER/",product_criteria,"_",premium_sources_libelle[[my_source]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_volume_minute_event_plot")
  }
  
  g <- tryCatch(
    RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
    
    , error = function(e) {NULL})
  
  if(is.null(g)){
    print("Trouble printing : ")
    print(pathToSave)
  }
}

# product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]

event_minutes_matrix_all_methodo$lapse <- lapse
event_minutes_matrix_all_methodo$relevance <- relevance
event_minutes_matrix_all_methodo$event_relevance <- event_relevance
event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
event_minutes_matrix_all_methodo$my_event <- my_event
event_minutes_matrix_all_methodo$localSource <- localSource



event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]

# event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_ONE","BETA_DAY","BETA_MINUTE")
event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_MINUTE")

return(event_minutes_matrix_all_methodo)

}

RP_ComputeReturns <- function(lapse, relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, Russell_version = "R1000"){
  event_minutes_matrix_all_methodo <- NULL
  # for (beta_methodo in  c("BETA_ONE","BETA_DAY","BETA_MINUTE")){
  for (beta_methodo in  c("BETA_MINUTE")){
    # print("Event :")
    # print(my_event)
    # print("Market model :")
    # print(beta_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
    # print("Returns")
    # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    # print(dim(event_minutes_matrix))
    
    #########################
    #########################
    #########################
    ######################### ACTUAL PLOTTING ACTUAL PLOTTING
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    # print(dim(event_minutes_matrix))
    
    ################# algo seek : no value in algoseek : 0 return  
    event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
    ##### we get rid of the crazy outliers : threshold got from empirical distribution
    
    return_threshold <- 0.02
    event_minutes_matrix[event_minutes_matrix >  return_threshold] <- return_threshold
    event_minutes_matrix[event_minutes_matrix < -return_threshold] <- -return_threshold
    
    event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        ### is it right here to average ??????????
        # toRet <- list(max(index),mean(value,na.rm=TRUE)) 
        toRet <- list(max(index),sum(value,na.rm=TRUE)) 
        return(toRet)
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      # print(tt)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    event_minutes_vector <- cumsum(event_minutes_vector)
    
    # event_minutes_vector <- event_minutes_vector-event_minutes_vector[(length(event_minutes_vector)[1]- 1)/2]
    
    
    event_minutes_matrix <- melt(event_minutes_vector)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    event_minutes_matrix$value <- event_minutes_matrix$value*10000
    
    event_minutes_matrix$value <- event_minutes_matrix$value-event_minutes_matrix$value[(dim(event_minutes_matrix)[1]- 1)/2+1]
    
    colnames(event_minutes_matrix)[2] <- beta_methodo
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
  }
  
  event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
  nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]
  
  event_minutes_matrix_all_methodo$lapse <- lapse
  event_minutes_matrix_all_methodo$relevance <- relevance
  event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  event_minutes_matrix_all_methodo$my_event <- my_event
  event_minutes_matrix_all_methodo$localSource <- localSource
  
  event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
  colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_ONE","BETA_DAY","BETA_MINUTE")
  event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_MINUTE")
  
  return(event_minutes_matrix_all_methodo)
  
}


outputGraphics <- function(lapse, relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, asymmetric = FALSE, plotInArborescence = FALSE, toPlot= FALSE,Russell_version = "R1000"){
  event_minutes_matrix_all_methodo <- NULL
  # for (beta_methodo in  c("BETA_ONE","BETA_DAY","BETA_MINUTE")){
  for (beta_methodo in  c("BETA_MINUTE")){
    # print("Event :")
    # print(my_event)
    # print("Market model :")
    # print(beta_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
    # print("Returns")
    # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    # print(dim(event_minutes_matrix))
    
    
    
    #########################
    #########################
    #########################
    ######################### ACTUAL PLOTTING ACTUAL PLOTTING
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    # print(dim(event_minutes_matrix))
    
    # print("we nullify when no data")
    event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
    return_threshold <- 0.02
    event_minutes_matrix[event_minutes_matrix >  return_threshold] <- return_threshold
    event_minutes_matrix[event_minutes_matrix < -return_threshold] <- -return_threshold
    
    event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        ### is it right here to average ??????????
        # toRet <- list(max(index),mean(value,na.rm=TRUE)) 
        toRet <- list(max(index),sum(value,na.rm=TRUE)) 
        return(toRet)
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      # print(tt)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    event_minutes_vector <- cumsum(event_minutes_vector)
    
    # event_minutes_vector <- event_minutes_vector-event_minutes_vector[(length(event_minutes_vector)[1]- 1)/2]
    
    
    event_minutes_matrix <- melt(event_minutes_vector)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    event_minutes_matrix$value <- event_minutes_matrix$value*10000
    
    event_minutes_matrix$value <- event_minutes_matrix$value-event_minutes_matrix$value[(dim(event_minutes_matrix)[1]- 1)/2+1]
    
    colnames(event_minutes_matrix)[2] <- beta_methodo
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
  }
  
  if(is.null(localSource)){
    localSource <- "DJ-EQ"
  }
  
  
  if(asymmetric){
    event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[event_minutes_matrix_all_methodo$MINUTES >= -60,]
  }
  
  event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0
  
  if(toPlot){
    
    
    if (plotInArborescence){
      pathToSave <- NULL
      filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_event_plot")
      if(product_criteria == "DJ-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      if(product_criteria == "WE-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[my_source]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      
      
      g <- tryCatch(RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T, XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
                    , error = function(e) {NULL})
      
      if(is.null(g)){
        print("Trouble printing : ")
        print(pathToSave)
      }
      
    }
    
    pathToSave <- NULL
    if(product_criteria == "DJ-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER/",product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_event_plot")
    }
    
    if(product_criteria == "WE-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER/",product_criteria,"_",premium_sources_libelle[[my_source]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_event_plot")
    }
    
    g <- tryCatch(
      RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
      
      , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
  }
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
  nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]
  
  event_minutes_matrix_all_methodo$lapse <- lapse
  event_minutes_matrix_all_methodo$relevance <- relevance
  event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  event_minutes_matrix_all_methodo$my_event <- my_event
  event_minutes_matrix_all_methodo$localSource <- localSource
  
  
  
  event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
  colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_ONE","BETA_DAY","BETA_MINUTE")
  event_minutes_matrix_all_methodo$corrado_methodo <- c("BETA_MINUTE")
  
  return(event_minutes_matrix_all_methodo)
  
}

outputCenteredCorradoStatistics <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, asymmetric = FALSE, plotInArborescence = FALSE, Russell_version = "R1000"){
  event_minutes_matrix_all_methodo <- NULL
  # for (beta_methodo in  c("BETA_ONE","BETA_DAY","BETA_MINUTE")){
  for (beta_methodo in  c("BETA_MINUTE")){
    
    print("Event :")
    print(my_event)
    print("Market model :")
    print(beta_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
    print("Returns")
    # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    print(dim(event_minutes_matrix))
    
    
    
    #########################
    #########################
    #########################
    ######################### ACTUAL PLOTTING ACTUAL PLOTTING
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    print(dim(event_minutes_matrix))
    
    print("we nullify when no data")
    event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
    
    event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        return(list(max(index),mean(value,na.rm=TRUE)))
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      # print(tt)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    
    ##############
    ##############
    ##############
    ################ old way
    #     event_minutes_point_vector <- (event_minutes_vector-mean(event_minutes_vector,na.rm=TRUE))/sd(event_minutes_vector,na.rm=TRUE)
    #     event_minutes_vector_probabilities <- pnorm(abs(event_minutes_point_vector),lower.tail = FALSE)
    #     event_minutes_vector_probabilities <- 1 - event_minutes_vector_probabilities
    #     
    #     results_event_minutes_vector <- event_minutes_vector_probabilities
    #     ##### first half
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     first_half_indice_inverted <- c(event_indice:1)
    #     first_half_indice <- c(1:event_indice)
    #     
    #     results_event_minutes_vector[first_half_indice_inverted] <- cumsum(event_minutes_vector_probabilities[first_half_indice_inverted])/first_half_indice
    #     ##### second half
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     
    #     results_event_minutes_vector[second_half_indice] <- cumsum(event_minutes_vector_probabilities[second_half_indice])/c(1:length(second_half_indice))
    #     
    #     event_minutes_matrix <- melt(results_event_minutes_vector)
    #     event_minutes_matrix$MINUTES <- event_minutes_index
    #     event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    ##############
    ##############
    ##############
    ################ end of old way
    
    event_indice <- (length(event_minutes_vector)- 1)/2
    cum_event_minutes_vector <-  cumsum(event_minutes_vector)
    
    
    cum_event_minutes_vector_reset <- cum_event_minutes_vector-cum_event_minutes_vector[event_indice]
    
    first_half_indice <- c(1:event_indice)
    second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    
    
    
    
    event_minutes_normalized_vector <- (event_minutes_vector-mean(event_minutes_vector,na.rm=TRUE))/sd(event_minutes_vector,na.rm=TRUE)
    
    results_event_minutes_normalized_vector <- event_minutes_normalized_vector
    ##### first half
    event_indice <- (length(event_minutes_vector)- 1)/2
    first_half_indice_inverted <- c(event_indice:1)
    first_half_indice <- c(1:event_indice)
    
    results_event_minutes_normalized_vector[first_half_indice_inverted] <- cumsum(event_minutes_normalized_vector[first_half_indice_inverted])/sqrt(first_half_indice)
    ##### second half
    second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    results_event_minutes_normalized_vector[second_half_indice] <- cumsum(event_minutes_normalized_vector[second_half_indice])/sqrt(c(1:length(second_half_indice)))
    
    
    event_minutes_vector_probabilities <- pnorm(abs(results_event_minutes_normalized_vector),lower.tail = FALSE)
    event_minutes_vector_probabilities <- 1 - event_minutes_vector_probabilities
    
    
    
    event_minutes_matrix <- melt(event_minutes_vector_probabilities)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    ##############
    ##############
    ##############
    ################
    
    if (beta_methodo == "BETA_ONE"){
      colnames(event_minutes_matrix)[2] <- "CORRADO_ONE"
    }
    
    if (beta_methodo == "BETA_DAY"){
      colnames(event_minutes_matrix)[2] <- "CORRADO_DAY"
    }
    
    if (beta_methodo == "BETA_MINUTE"){
      colnames(event_minutes_matrix)[2] <- "CORRADO_MINUTE"
    }
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
  }
  
  if(is.null(localSource)){
    localSource <- "DJ-EQ"
  }
  
  if(asymmetric){
    event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[event_minutes_matrix_all_methodo$MINUTES >= -60,]
  }
  
  pathToSave <- NULL
  if(product_criteria == "DJ-EQ"){
    filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
    pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER_CORRADO/",filename)
  }
  
  if(product_criteria == "WE-EQ"){
    filename <- paste0(product_criteria,"_",premium_sources_libelle[[my_source]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
    filename <- gsub("-","",filename)
    pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER_CORRADO/",filename)
  }
  significance_threshold <- 1 - 0.05
  event_minutes_matrix_all_methodo$ABNORMAL_THRESHOLD <- significance_threshold
  
  event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0
  g <- tryCatch(
    RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
    , error = function(e) {NULL})
  
  if(is.null(g)){
    print("Trouble printing : ")
    print(pathToSave)
  }
  
  
  
  if (plotInArborescence){
    pathToSave <- NULL
    filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
    if(product_criteria == "DJ-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
    }
    
    if(product_criteria == "WE-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[my_source]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
    }
    
    g <- tryCatch(
      RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
      , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
    
  }
  
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
  nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]
  event_minutes_matrix_all_methodo$product_criteria <- product_criteria
  event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  event_minutes_matrix_all_methodo$ens_filter <- ens_filter
  event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  event_minutes_matrix_all_methodo$my_event <- my_event
  event_minutes_matrix_all_methodo$localSource <- localSource
  event_minutes_matrix_all_methodo$gics_sector <- gics_sector
  
  
  
  event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
  colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  event_minutes_matrix_all_methodo$corrado_methodo <- c("CORRADO_ONE","CORRADO_DAY","CORRADO_MINUTE")
  
  
  return(event_minutes_matrix_all_methodo)
}


RP_ComputeCorradoStatistics <- function(lapse, relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, Russell_version = "R1000"){
  centerEvent <- TRUE
  corrado_all_methodo <- NULL
  event_minutes_matrix_all_methodo <- NULL
  cum_event_minutes_matrix_all_methodo <- NULL
  # for (corrado_methodo in  c("CORRADO_ONE","CORRADO_DAY","CORRADO_MINUTE")){
  for (corrado_methodo in  c("CORRADO_MINUTE")){
    # print("Event :")
    # print(my_event)
    # print("Market model :")
    # print(corrado_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == corrado_methodo,]
    # print("Returns")
    # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    # print(dim(event_minutes_matrix))
    
    
    #     
    #     print("we nullify when no data")
    #     event_minutes_matrix[is.na(event_minutes_matrix)] <- 0.5
    
    
    #########################
    #########################
    #########################
    ######################### ACTUAL PLOTTING ACTUAL PLOTTING
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    print(dim(event_minutes_matrix))
    
    event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        return(list(max(index),mean(value,na.rm=TRUE)))
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      setkey(tt,buck_index)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    ######################################
    ######################################
    ######################################
    ######################################
    ######################################
    ###################################### Ccentered way
    
    #     dropping_count <- 20
    #     sd_comp_vec <- event_minutes_vector[order(event_minutes_vector,decreasing = TRUE)][-c(1:dropping_count)]
    #     sd_over_time <-  sqrt(sum((sd_comp_vec-1/2)^2)/length(sd_comp_vec))
    #     
    #     
    #     results_event_minutes_vector <- event_minutes_vector
    #     ##### first half
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     first_half_indice_inverted <- c(event_indice:1)
    #     
    #     sd_minutes_vector_first_half <- sqrt(length(first_half_indice_inverted):1) * sd_over_time
    #     results_event_minutes_vector[first_half_indice_inverted] <- cumsum(event_minutes_vector[first_half_indice_inverted]-1/2)/sd_minutes_vector_first_half
    #     
    #     ##### second half
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     
    #     sd_minutes_vector_second_half <- sqrt(1:length(second_half_indice)) * sd_over_time
    #     results_event_minutes_vector[second_half_indice] <- cumsum(event_minutes_vector[second_half_indice]-1/2)/sd_minutes_vector_second_half
    #     
    #     
    #     cumulated_event_minutes_vector <- results_event_minutes_vector
    #     event_minutes_vector <- (event_minutes_vector-1/2)/sd_over_time
    #     
    ######################################
    ######################################
    ######################################
    ###################################### NOT centered way
    
    
    ############# NOT CENTERED CUMULATIVE FROM BEGINNING
    #############
    #############
    #############
    #     sd_over_time <-  sqrt(sum((event_minutes_vector-1/2)^2)/length(event_minutes_vector))
    #     cumulated_event_minutes_vector <- cumsum((event_minutes_vector-1/2))
    #     sd_minutes_vector <- sqrt(1:length(cumulated_event_minutes_vector)) * sd_over_time
    #     cumulated_event_minutes_vector <- cumulated_event_minutes_vector/sd_minutes_vector   
    #############
    #############
    #############
    #############
    
    event_minutes_vector[is.na(event_minutes_vector)] <- 0.5
    if (!centerEvent){
      sd_over_time <-  sqrt(sum((event_minutes_vector-1/2)^2)/length(event_minutes_vector))
      
      event_indice <- (length(event_minutes_vector)- 1)/2
      first_half_indice <- c(1:event_indice)
      first_half_indice_inverted <- c(event_indice:1)
      second_half_indice <- c((event_indice+1):length(event_minutes_vector))
      
      cumulated_event_minutes_vector <- cumsum((event_minutes_vector-1/2))
      sd_minutes_vector_first_half <- sqrt(1:length(first_half_indice)) * sd_over_time
      cumulated_event_minutes_vector[first_half_indice_inverted] <- cumsum((event_minutes_vector[first_half_indice_inverted]-1/2))/sd_minutes_vector_first_half
      sd_minutes_vector_second_half <- sqrt(1:length(second_half_indice)) * sd_over_time
      cumulated_event_minutes_vector[second_half_indice] <- cumsum((event_minutes_vector[second_half_indice]-1/2))/sd_minutes_vector_second_half
      
    } else {
      sd_over_time <-  sqrt(sum((event_minutes_vector-1/2)^2)/length(event_minutes_vector))
      # event_indice <- (length(event_minutes_vector)- 1)/2
      event_indice <- (length(event_minutes_vector)-1)/2+1
      first_half_indice <- c(1:event_indice)
      first_half_indice_inverted <- c(event_indice:1)
      
      second_half_indice <- c((event_indice+1):length(event_minutes_vector))
      
      cumulated_event_minutes_vector <- cumsum((event_minutes_vector-1/2))
      sd_minutes_vector_first_half <- sqrt(1:length(first_half_indice)) * sd_over_time
      
      # cumulated_event_minutes_vector[first_half_indice] <- cumsum((event_minutes_vector[first_half_indice]-1/2))/sd_minutes_vector_first_half
      cumulated_event_minutes_vector[first_half_indice_inverted] <- cumsum((event_minutes_vector[first_half_indice_inverted]-1/2))/sd_minutes_vector_first_half
      
      sd_minutes_vector_second_half <- sqrt(1:length(second_half_indice)) * sd_over_time
      cumulated_event_minutes_vector[second_half_indice] <- cumsum((event_minutes_vector[second_half_indice]-1/2))/sd_minutes_vector_second_half
    }
    
    event_minutes_vector <- (event_minutes_vector-1/2)/sd_over_time
    ######################################
    ######################################
    ######################################
    ######################################
    ######################################
    ###################################### End of the CAR Corrado test computation
    
    
    
    ######################################
    ######################################
    ######################################
    ######################################
    ###################################### Computing the pre post event average Corrado bucket
    event_indice <- (length(event_minutes_vector)- 1)/2
    ######################################
    ######################################
    ######################################
    ######################################
    ###################################### End   Computing the pre post event average Corrado bucket
    
    
    
    # event_indice <- (length(event_minutes_vector)- 1)/2
    #     event_minutes_vector_light <- event_minutes_vector[-c(event_indice-1,event_indice,event_indice+1)]
    #     sd_over_time= sqrt(sum((event_minutes_vector_light-1/2)^2)/length(event_minutes_vector_light))
    #     event_minutes_vector_light <- (event_minutes_vector_light-1/2)/sd_over_time
    #     event_minutes_vector_probabilities <- (pnorm(event_minutes_vector_light,lower.tail = TRUE)<0.05 | pnorm(event_minutes_vector_light,lower.tail = FALSE)<0.05)
    
    ######################################
    ######################################
    ######################################
    ###################################### CHOOSE THE METHODOLOGY
    # cumulated_event_minutes_vector <- cumulated_event_minutes_vector_bis
    ######################################
    ######################################
    ######################################
    ######################################
    
    
    # event_minutes_vector_probabilities <- (pnorm(event_minutes_vector,lower.tail = TRUE)<0.05 | pnorm(event_minutes_vector,lower.tail = FALSE)<0.05)
    event_minutes_vector_probabilities <- pnorm(abs(event_minutes_vector),lower.tail = FALSE)
    event_minutes_vector_probabilities <- 1 - event_minutes_vector_probabilities
    
    cumulated_event_minutes_vector_probabilities <- pnorm(abs(cumulated_event_minutes_vector),lower.tail = FALSE)
    cumulated_event_minutes_vector_probabilities <- 1 - cumulated_event_minutes_vector_probabilities
    
    
    
    cum_event_minutes_matrix <- melt(cumulated_event_minutes_vector_probabilities)
    cum_event_minutes_matrix$MINUTES <- event_minutes_index
    cum_event_minutes_matrix <- cum_event_minutes_matrix[,c("MINUTES","value")]
    
    if (centerEvent){
      colnames(cum_event_minutes_matrix)[2] <- paste0("CENTER_",corrado_methodo)
    } else {
      colnames(cum_event_minutes_matrix)[2] <- corrado_methodo
    }
    
    event_minutes_matrix <- melt(event_minutes_vector_probabilities)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    colnames(event_minutes_matrix)[2] <- corrado_methodo
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
    if(is.null(cum_event_minutes_matrix_all_methodo)){
      cum_event_minutes_matrix_all_methodo <- cum_event_minutes_matrix
    } else {
      cum_event_minutes_matrix_all_methodo <- merge(cum_event_minutes_matrix_all_methodo, cum_event_minutes_matrix, by = "MINUTES")
    }
    
    
    
  }
  
  
  ####################
  ####################
  #################### END OF CUMULATED CORRADO EVENT PLOTTING
  results <- list()
  results$toplot_event_minutes_matrix_all_methodo <- cum_event_minutes_matrix_all_methodo
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  cum_event_minutes_matrix_all_methodo <- as.data.frame(t(cum_event_minutes_matrix_all_methodo))
  nb_minutes <- dim(cum_event_minutes_matrix_all_methodo)[2]
  
  
  #   
  
  cum_event_minutes_matrix_all_methodo_minutes <- cum_event_minutes_matrix_all_methodo[1,]
  cum_event_minutes_matrix_all_methodo <- cum_event_minutes_matrix_all_methodo[-c(1),]
  colnames(cum_event_minutes_matrix_all_methodo)[1:nb_minutes] <- cum_event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # cum_event_minutes_matrix_all_methodo$corrado_methodo <- c("CORRADO_ONE","CORRADO_DAY","CORRADO_MINUTE")
  
  label <- "CORRADO_MINUTE"
  if(centerEvent){
    label <- paste0("CENTER_",label)
  }
  cum_event_minutes_matrix_all_methodo$corrado_methodo <- c(label)
  cum_event_minutes_matrix_all_methodo$lapse <- lapse
  cum_event_minutes_matrix_all_methodo$relevance <- relevance
  cum_event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  cum_event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  cum_event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  cum_event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  cum_event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  cum_event_minutes_matrix_all_methodo$my_event <- my_event
  cum_event_minutes_matrix_all_methodo$localSource <- localSource
  
  
  results$event_minutes_matrix_all_methodo <- cum_event_minutes_matrix_all_methodo
  
  
  
  return(results)
}

outputCorradoStatistics <- function(lapse, relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, asymmetric = FALSE, plotInArborescence = FALSE, toPlot= FALSE,Russell_version = "R1000"){
  centerEvent <- TRUE
  corrado_all_methodo <- NULL
  event_minutes_matrix_all_methodo <- NULL
  cum_event_minutes_matrix_all_methodo <- NULL
  # for (corrado_methodo in  c("CORRADO_ONE","CORRADO_DAY","CORRADO_MINUTE")){
  for (corrado_methodo in  c("CORRADO_MINUTE")){
    # print("Event :")
    # print(my_event)
    # print("Market model :")
    # print(corrado_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == corrado_methodo,]
    # print("Returns")
    # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    # print(dim(event_minutes_matrix))
    
    
    #     
    #     print("we nullify when no data")
    #     event_minutes_matrix[is.na(event_minutes_matrix)] <- 0.5
    
    
    #########################
    #########################
    #########################
    ######################### ACTUAL PLOTTING ACTUAL PLOTTING
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    print(dim(event_minutes_matrix))
    
    event_minutes_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        return(list(max(index),mean(value,na.rm=TRUE)))
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      setkey(tt,buck_index)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    ######################################
    ######################################
    ######################################
    ######################################
    ######################################
    ###################################### Ccentered way
    
    #     dropping_count <- 20
    #     sd_comp_vec <- event_minutes_vector[order(event_minutes_vector,decreasing = TRUE)][-c(1:dropping_count)]
    #     sd_over_time <-  sqrt(sum((sd_comp_vec-1/2)^2)/length(sd_comp_vec))
    #     
    #     
    #     results_event_minutes_vector <- event_minutes_vector
    #     ##### first half
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     first_half_indice_inverted <- c(event_indice:1)
    #     
    #     sd_minutes_vector_first_half <- sqrt(length(first_half_indice_inverted):1) * sd_over_time
    #     results_event_minutes_vector[first_half_indice_inverted] <- cumsum(event_minutes_vector[first_half_indice_inverted]-1/2)/sd_minutes_vector_first_half
    #     
    #     ##### second half
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     
    #     sd_minutes_vector_second_half <- sqrt(1:length(second_half_indice)) * sd_over_time
    #     results_event_minutes_vector[second_half_indice] <- cumsum(event_minutes_vector[second_half_indice]-1/2)/sd_minutes_vector_second_half
    #     
    #     
    #     cumulated_event_minutes_vector <- results_event_minutes_vector
    #     event_minutes_vector <- (event_minutes_vector-1/2)/sd_over_time
    #     
    ######################################
    ######################################
    ######################################
    ###################################### NOT centered way
    
    
    ############# NOT CENTERED CUMULATIVE FROM BEGINNING
    #############
    #############
    #############
    #     sd_over_time <-  sqrt(sum((event_minutes_vector-1/2)^2)/length(event_minutes_vector))
    #     cumulated_event_minutes_vector <- cumsum((event_minutes_vector-1/2))
    #     sd_minutes_vector <- sqrt(1:length(cumulated_event_minutes_vector)) * sd_over_time
    #     cumulated_event_minutes_vector <- cumulated_event_minutes_vector/sd_minutes_vector   
    #############
    #############
    #############
    #############
    
    event_minutes_vector[is.na(event_minutes_vector)] <- 0.5
    if (!centerEvent){
      sd_over_time <-  sqrt(sum((event_minutes_vector-1/2)^2)/length(event_minutes_vector))
      
      event_indice <- (length(event_minutes_vector)- 1)/2
      first_half_indice <- c(1:event_indice)
      first_half_indice_inverted <- c(event_indice:1)
      second_half_indice <- c((event_indice+1):length(event_minutes_vector))
      
      cumulated_event_minutes_vector <- cumsum((event_minutes_vector-1/2))
      sd_minutes_vector_first_half <- sqrt(1:length(first_half_indice)) * sd_over_time
      cumulated_event_minutes_vector[first_half_indice_inverted] <- cumsum((event_minutes_vector[first_half_indice_inverted]-1/2))/sd_minutes_vector_first_half
      sd_minutes_vector_second_half <- sqrt(1:length(second_half_indice)) * sd_over_time
      cumulated_event_minutes_vector[second_half_indice] <- cumsum((event_minutes_vector[second_half_indice]-1/2))/sd_minutes_vector_second_half
      
    } else {
      sd_over_time <-  sqrt(sum((event_minutes_vector-1/2)^2)/length(event_minutes_vector))
      # event_indice <- (length(event_minutes_vector)- 1)/2
      event_indice <- (length(event_minutes_vector)-1)/2+1
      first_half_indice <- c(1:event_indice)
      first_half_indice_inverted <- c(event_indice:1)
      
      second_half_indice <- c((event_indice+1):length(event_minutes_vector))
      
      cumulated_event_minutes_vector <- cumsum((event_minutes_vector-1/2))
      sd_minutes_vector_first_half <- sqrt(1:length(first_half_indice)) * sd_over_time
      
      # cumulated_event_minutes_vector[first_half_indice] <- cumsum((event_minutes_vector[first_half_indice]-1/2))/sd_minutes_vector_first_half
      cumulated_event_minutes_vector[first_half_indice_inverted] <- cumsum((event_minutes_vector[first_half_indice_inverted]-1/2))/sd_minutes_vector_first_half
      
      sd_minutes_vector_second_half <- sqrt(1:length(second_half_indice)) * sd_over_time
      cumulated_event_minutes_vector[second_half_indice] <- cumsum((event_minutes_vector[second_half_indice]-1/2))/sd_minutes_vector_second_half
    }
    
    event_minutes_vector <- (event_minutes_vector-1/2)/sd_over_time
    ######################################
    ######################################
    ######################################
    ######################################
    ######################################
    ###################################### End of the CAR Corrado test computation
    
    
    
    ######################################
    ######################################
    ######################################
    ######################################
    ###################################### Computing the pre post event average Corrado bucket
    event_indice <- (length(event_minutes_vector)- 1)/2
    ######################################
    ######################################
    ######################################
    ######################################
    ###################################### End   Computing the pre post event average Corrado bucket
    
    
    
    # event_indice <- (length(event_minutes_vector)- 1)/2
    #     event_minutes_vector_light <- event_minutes_vector[-c(event_indice-1,event_indice,event_indice+1)]
    #     sd_over_time= sqrt(sum((event_minutes_vector_light-1/2)^2)/length(event_minutes_vector_light))
    #     event_minutes_vector_light <- (event_minutes_vector_light-1/2)/sd_over_time
    #     event_minutes_vector_probabilities <- (pnorm(event_minutes_vector_light,lower.tail = TRUE)<0.05 | pnorm(event_minutes_vector_light,lower.tail = FALSE)<0.05)
    
    ######################################
    ######################################
    ######################################
    ###################################### CHOOSE THE METHODOLOGY
    # cumulated_event_minutes_vector <- cumulated_event_minutes_vector_bis
    ######################################
    ######################################
    ######################################
    ######################################
    
    
    # event_minutes_vector_probabilities <- (pnorm(event_minutes_vector,lower.tail = TRUE)<0.05 | pnorm(event_minutes_vector,lower.tail = FALSE)<0.05)
    event_minutes_vector_probabilities <- pnorm(abs(event_minutes_vector),lower.tail = FALSE)
    event_minutes_vector_probabilities <- 1 - event_minutes_vector_probabilities
    
    cumulated_event_minutes_vector_probabilities <- pnorm(abs(cumulated_event_minutes_vector),lower.tail = FALSE)
    cumulated_event_minutes_vector_probabilities <- 1 - cumulated_event_minutes_vector_probabilities
    
    
    
    cum_event_minutes_matrix <- melt(cumulated_event_minutes_vector_probabilities)
    cum_event_minutes_matrix$MINUTES <- event_minutes_index
    cum_event_minutes_matrix <- cum_event_minutes_matrix[,c("MINUTES","value")]
    
    if (centerEvent){
      colnames(cum_event_minutes_matrix)[2] <- paste0("CENTER_",corrado_methodo)
    } else {
      colnames(cum_event_minutes_matrix)[2] <- corrado_methodo
    }
    
    event_minutes_matrix <- melt(event_minutes_vector_probabilities)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    colnames(event_minutes_matrix)[2] <- corrado_methodo
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
    if(is.null(cum_event_minutes_matrix_all_methodo)){
      cum_event_minutes_matrix_all_methodo <- cum_event_minutes_matrix
    } else {
      cum_event_minutes_matrix_all_methodo <- merge(cum_event_minutes_matrix_all_methodo, cum_event_minutes_matrix, by = "MINUTES")
    }
    
    
    
  }
  
  if(is.null(localSource)){
    localSource <- "DJ-EQ"
  }
  
  
  ####################
  ####################
  #################### NON CUMULATED CORRADO EVENT PLOTTING
  
  
  if(asymmetric){
    event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[event_minutes_matrix_all_methodo$MINUTES >= -60,]
  }
  
  if(toPlot){
    
    
    pathToSave <- NULL
    if(product_criteria == "DJ-EQ"){
      filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER_CORRADO/",filename)
    }
    
    if(product_criteria == "WE-EQ"){
      filename <- paste0(product_criteria,"_",premium_sources_libelle[[my_source]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
      filename <- gsub("-","",filename)
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER_CORRADO/",filename)
    }
    significance_threshold <- 1 - 0.05
    event_minutes_matrix_all_methodo$ABNORMAL_THRESHOLD <- significance_threshold
    
    event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0
    g <- tryCatch(
      RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
      , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
    
    
    
    if (plotInArborescence){
      pathToSave <- NULL
      filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
      if(product_criteria == "DJ-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      if(product_criteria == "WE-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[my_source]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      g <- tryCatch(
        RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
        , error = function(e) {NULL})
      
      if(is.null(g)){
        print("Trouble printing : ")
        print(pathToSave)
      }
      
    }
  }
  ####################
  ####################
  #################### END OF NON CUMULATED CORRADO EVENT PLOTTING
  
  ####################
  ####################
  #################### CUMULATED CORRADO EVENT PLOTTING
  
  if(asymmetric){
    cum_event_minutes_matrix_all_methodo <- cum_event_minutes_matrix_all_methodo[cum_event_minutes_matrix_all_methodo$MINUTES >= -60,]
  }
  
  if(toPlot){
    pathToSave <- NULL
    if(product_criteria == "DJ-EQ"){
      filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_cum_corrado_event_plot")
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER_CORRADO/",filename)
    }
    
    if(product_criteria == "WE-EQ"){
      filename <- paste0(product_criteria,"_",premium_sources_libelle[[my_source]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_cum_corrado_event_plot")
      filename <- gsub("-","",filename)
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER_CORRADO/",filename)
    }
    significance_threshold <- 1 - 0.05
    cum_event_minutes_matrix_all_methodo$ABNORMAL_THRESHOLD <- significance_threshold
    
    cum_event_minutes_matrix_all_methodo[is.na(cum_event_minutes_matrix_all_methodo)] <- 0
    g <- tryCatch(
      RP_PlotDataFrame(cum_event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
      , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
    
    
    
    if (plotInArborescence){
      pathToSave <- NULL
      filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_cum_corrado_event_plot")
      if(product_criteria == "DJ-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      if(product_criteria == "WE-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[my_source]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      g <- tryCatch(
        RP_PlotDataFrame(cum_event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
        , error = function(e) {NULL})
      
      if(is.null(g)){
        print("Trouble printing : ")
        print(pathToSave)
      }
      
    }
  }
  
  ####################
  ####################
  #################### END OF CUMULATED CORRADO EVENT PLOTTING
  results <- list()
  results$toplot_event_minutes_matrix_all_methodo <- cum_event_minutes_matrix_all_methodo
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  cum_event_minutes_matrix_all_methodo <- as.data.frame(t(cum_event_minutes_matrix_all_methodo))
  nb_minutes <- dim(cum_event_minutes_matrix_all_methodo)[2]
  
  
  #   
  
  cum_event_minutes_matrix_all_methodo_minutes <- cum_event_minutes_matrix_all_methodo[1,]
  cum_event_minutes_matrix_all_methodo <- cum_event_minutes_matrix_all_methodo[-c(1),]
  colnames(cum_event_minutes_matrix_all_methodo)[1:nb_minutes] <- cum_event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # cum_event_minutes_matrix_all_methodo$corrado_methodo <- c("CORRADO_ONE","CORRADO_DAY","CORRADO_MINUTE")
  
  label <- "CORRADO_MINUTE"
  if(centerEvent){
    label <- paste0("CENTER_",label)
  }
  cum_event_minutes_matrix_all_methodo$corrado_methodo <- c(label)
  cum_event_minutes_matrix_all_methodo$lapse <- lapse
  cum_event_minutes_matrix_all_methodo$relevance <- relevance
  cum_event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  cum_event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  cum_event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  cum_event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  cum_event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  cum_event_minutes_matrix_all_methodo$my_event <- my_event
  cum_event_minutes_matrix_all_methodo$localSource <- localSource
  
  
  results$event_minutes_matrix_all_methodo <- cum_event_minutes_matrix_all_methodo
  
  
  
  return(results)
}

computeCIbounds <- function(my_return_vector){
  theta.boot.mean <- boot(my_return_vector, bootThetaMean, R=5000) 
  mean_ci <- boot.ci(theta.boot.mean, conf=0.9)
  return(c(mean_ci$normal[2],mean(my_return_vector),mean_ci$normal[3]))  
}


bootThetaMean <- function(x,i) {
  mean(x[i])
}


RP_ComputeORDINStatistics <- function(lapse, relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, Russell_version = "R1000"){
  centerEvent <- TRUE
  event_minutes_matrix_all_methodo <- NULL
  # for (beta_methodo in  c("BETA_ONE","BETA_DAY","BETA_MINUTE")){
  for (beta_methodo in  c("BETA_MINUTE")){
    
    # print("Event :")
    # print(my_event)
    # print("Market model :")
    # print(beta_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
    # print("Returns")
    # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    # print(dim(event_minutes_matrix))
    
    
    
    #########################
    #########################
    #########################
    ######################### ACTUAL PLOTTING ACTUAL PLOTTING
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    print(dim(event_minutes_matrix))
    
    print("we nullify when no data")
    event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
    
    #     ################## old classic way
    #     event_minutes_matrix <- t(apply(event_minutes_matrix,1,FUN=cumsum))
    
    if(!centerEvent){
      cumsumaroundevent <- function(row_vector){
        event_indice <- (length(row_vector)- 1)/2
        first_half_indice <- c(1:event_indice)
        first_half_indice_inverted <- c(event_indice:1)
        
        second_half_indice <- c((event_indice+1):length(row_vector))
        
        cumulated_event_minutes_vector <- cumsum(row_vector)
        cumulated_event_minutes_vector[first_half_indice_inverted] <- cumsum((row_vector[first_half_indice_inverted]))
        
        cumulated_event_minutes_vector[second_half_indice] <- cumsum((row_vector[second_half_indice]))
        return(cumulated_event_minutes_vector)
      }
      event_minutes_matrix <- t(apply(event_minutes_matrix,1,FUN=cumsumaroundevent))
    } else {
      # save(event_minutes_matrix,file = paste0(outputDataPath,"event_minutes_matrix")) 
      cumsumresetaroundevent <- function(row_vector){
        row_vector[is.na(row_vector)] <- 0
        event_indice <- (length(row_vector)- 1)/2+1
        first_half_indice <- c(1:event_indice)
        first_half_indice_inverted <- c(event_indice:1)
        
        second_half_indice <- c((event_indice+1):length(row_vector))
        
        cumulated_event_minutes_vector <- cumsum(row_vector)
        # cumulated_event_minutes_vector[first_half_indice] <- cumsum(row_vector[first_half_indice])
        cumulated_event_minutes_vector[first_half_indice_inverted] <- cumsum(row_vector[first_half_indice_inverted])
        
        cumulated_event_minutes_vector[second_half_indice] <- cumsum(row_vector[second_half_indice])
        return(cumulated_event_minutes_vector)
      }
      event_minutes_matrix <- t(apply(event_minutes_matrix,1,FUN=cumsumresetaroundevent))
      
    }
    
    
    event_minutes_mean_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    event_minutes_sd_vector <- apply(event_minutes_matrix,2,FUN=function(x){sd(x,na.rm = TRUE)/sqrt(length(x))})
    
    t_ordin <- event_minutes_mean_vector/event_minutes_sd_vector
    
    event_minutes_vector <- pnorm(abs(t_ordin),lower.tail = FALSE)
    event_minutes_vector <- 1 - event_minutes_vector
    event_minutes_vector[is.na(event_minutes_vector)] <- 0 
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        return(list(max(index),mean(value,na.rm=TRUE)))
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      # print(tt)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    
    ##############
    ##############
    ##############
    ################ old way
    #     event_minutes_point_vector <- (event_minutes_vector-mean(event_minutes_vector,na.rm=TRUE))/sd(event_minutes_vector,na.rm=TRUE)
    #     event_minutes_vector_probabilities <- pnorm(abs(event_minutes_point_vector),lower.tail = FALSE)
    #     event_minutes_vector_probabilities <- 1 - event_minutes_vector_probabilities
    #     
    #     results_event_minutes_vector <- event_minutes_vector_probabilities
    #     ##### first half
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     first_half_indice_inverted <- c(event_indice:1)
    #     first_half_indice <- c(1:event_indice)
    #     
    #     results_event_minutes_vector[first_half_indice_inverted] <- cumsum(event_minutes_vector_probabilities[first_half_indice_inverted])/first_half_indice
    #     ##### second half
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     
    #     results_event_minutes_vector[second_half_indice] <- cumsum(event_minutes_vector_probabilities[second_half_indice])/c(1:length(second_half_indice))
    #     
    #     event_minutes_matrix <- melt(results_event_minutes_vector)
    #     event_minutes_matrix$MINUTES <- event_minutes_index
    #     event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    ##############
    ##############
    ##############
    ################ end of old way
    
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     cum_event_minutes_vector <-  cumsum(event_minutes_vector)
    #     
    #     
    #     cum_event_minutes_vector_reset <- cum_event_minutes_vector-cum_event_minutes_vector[event_indice]
    #     
    #     first_half_indice <- c(1:event_indice)
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     
    #     
    #     
    #     
    #     event_minutes_normalized_vector <- (event_minutes_vector-mean(event_minutes_vector,na.rm=TRUE))/sd(event_minutes_vector,na.rm=TRUE)
    #     
    #     results_event_minutes_normalized_vector <- event_minutes_normalized_vector
    #     ##### first half
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     first_half_indice_inverted <- c(event_indice:1)
    #     first_half_indice <- c(1:event_indice)
    #     
    #     results_event_minutes_normalized_vector[first_half_indice_inverted] <- cumsum(event_minutes_normalized_vector[first_half_indice_inverted])/sqrt(first_half_indice)
    #     ##### second half
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     results_event_minutes_normalized_vector[second_half_indice] <- cumsum(event_minutes_normalized_vector[second_half_indice])/sqrt(c(1:length(second_half_indice)))
    #     
    #     
    #     event_minutes_vector_probabilities <- pnorm(abs(results_event_minutes_normalized_vector),lower.tail = FALSE)
    #     event_minutes_vector_probabilities <- 1 - event_minutes_vector_probabilities
    #     
    
    
    event_minutes_matrix <- melt(event_minutes_vector)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    ##############
    ##############
    ##############
    ################
    
    if(centerEvent){
      if (beta_methodo == "BETA_ONE"){
        colnames(event_minutes_matrix)[2] <- "CENTER_ORDIN_ONE"
      }
      
      if (beta_methodo == "BETA_DAY"){
        colnames(event_minutes_matrix)[2] <- "CENTER_ORDIN_DAY"
      }
      
      if (beta_methodo == "BETA_MINUTE"){
        colnames(event_minutes_matrix)[2] <- "CENTER_ORDIN_MINUTE"
      }
      
    } else {
      if (beta_methodo == "BETA_ONE"){
        colnames(event_minutes_matrix)[2] <- "ORDIN_ONE"
      }
      
      if (beta_methodo == "BETA_DAY"){
        colnames(event_minutes_matrix)[2] <- "ORDIN_DAY"
      }
      
      if (beta_methodo == "BETA_MINUTE"){
        colnames(event_minutes_matrix)[2] <- "ORDIN_MINUTE"
      }
      
    }
    
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
  }
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  
  results <- list()
  results$toplot_event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo
  
  event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
  nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]
  
  event_minutes_matrix_all_methodo$lapse <- lapse
  event_minutes_matrix_all_methodo$relevance <- relevance
  event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  event_minutes_matrix_all_methodo$my_event <- my_event
  event_minutes_matrix_all_methodo$localSource <- localSource
  
  
  
  event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
  colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # event_minutes_matrix_all_methodo$corrado_methodo <- c("ORDIN_ONE","ORDIN_DAY","ORDIN_MINUTE")
  
  label <- "ORDIN_MINUTE"
  if(centerEvent){
    label <- paste0("CENTERED",label)
  }
  event_minutes_matrix_all_methodo$corrado_methodo <- c(label)
  
  
  results$event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo
  return(results)
}


outputORDINStatistics <- function(lapse, relevance, event_relevance, aggregate_criteria,sentiment_criteria,similarity_gap_filter,event_number_event_filtering, my_event, localSource, dataFrame, nb_minutes_bucket=5, nb_range = 180, asymmetric = FALSE, plotInArborescence = FALSE, toPlot= FALSE,Russell_version = "R1000"){
  centerEvent <- TRUE
  event_minutes_matrix_all_methodo <- NULL
  # for (beta_methodo in  c("BETA_ONE","BETA_DAY","BETA_MINUTE")){
  for (beta_methodo in  c("BETA_MINUTE")){
    
    # print("Event :")
    # print(my_event)
    # print("Market model :")
    # print(beta_methodo)
    dataFrameBeta <- dataFrame[dataFrame$BETA_METHODO == beta_methodo,]
    # print("Returns")
    # event_minutes_matrix <- as.matrix(filtered_df[,-c(metadata_column)])
    event_minutes_matrix <- as.matrix(dataFrameBeta[,as.character(1:(2*nb_range + 1))])
    # print(dim(event_minutes_matrix))
    
    
    
    #########################
    #########################
    #########################
    ######################### ACTUAL PLOTTING ACTUAL PLOTTING
    event_minutes_vector <- NULL
    
    ###### computing Corrado test here
    print(dim(event_minutes_matrix))
    
    print("we nullify when no data")
    event_minutes_matrix[is.na(event_minutes_matrix)] <- 0
    
    #     ################## old classic way
    #     event_minutes_matrix <- t(apply(event_minutes_matrix,1,FUN=cumsum))
    
    if(!centerEvent){
      cumsumaroundevent <- function(row_vector){
        event_indice <- (length(row_vector)- 1)/2
        first_half_indice <- c(1:event_indice)
        first_half_indice_inverted <- c(event_indice:1)
        
        second_half_indice <- c((event_indice+1):length(row_vector))
        
        cumulated_event_minutes_vector <- cumsum(row_vector)
        cumulated_event_minutes_vector[first_half_indice_inverted] <- cumsum((row_vector[first_half_indice_inverted]))
        
        cumulated_event_minutes_vector[second_half_indice] <- cumsum((row_vector[second_half_indice]))
        return(cumulated_event_minutes_vector)
      }
      event_minutes_matrix <- t(apply(event_minutes_matrix,1,FUN=cumsumaroundevent))
    } else {
      # save(event_minutes_matrix,file = paste0(outputDataPath,"event_minutes_matrix")) 
      cumsumresetaroundevent <- function(row_vector){
        row_vector[is.na(row_vector)] <- 0
        event_indice <- (length(row_vector)- 1)/2+1
        first_half_indice <- c(1:event_indice)
        first_half_indice_inverted <- c(event_indice:1)
        
        second_half_indice <- c((event_indice+1):length(row_vector))
        
        cumulated_event_minutes_vector <- cumsum(row_vector)
        # cumulated_event_minutes_vector[first_half_indice] <- cumsum(row_vector[first_half_indice])
        cumulated_event_minutes_vector[first_half_indice_inverted] <- cumsum(row_vector[first_half_indice_inverted])
        
        cumulated_event_minutes_vector[second_half_indice] <- cumsum(row_vector[second_half_indice])
        return(cumulated_event_minutes_vector)
      }
      event_minutes_matrix <- t(apply(event_minutes_matrix,1,FUN=cumsumresetaroundevent))
      
    }
    
    
    event_minutes_mean_vector <- apply(event_minutes_matrix,2,FUN=function(x){mean(x,na.rm = TRUE)})
    event_minutes_sd_vector <- apply(event_minutes_matrix,2,FUN=function(x){sd(x,na.rm = TRUE)/sqrt(length(x))})
    
    t_ordin <- event_minutes_mean_vector/event_minutes_sd_vector
    
    event_minutes_vector <- pnorm(abs(t_ordin),lower.tail = FALSE)
    event_minutes_vector <- 1 - event_minutes_vector
    event_minutes_vector[is.na(event_minutes_vector)] <- 0 
    # event_minutes_vector <- apply(event_minutes_matrix,2,FUN=mean)
    
    event_minutes_index <- c(seq(-nb_range,-1),0,seq(1,nb_range)) 
    if (nb_minutes_bucket > 1){
      print("Bucketing by")
      print(nb_minutes_bucket)
      
      
      column_index <- c(seq(-nb_range,-1),0,seq(1,nb_range))
      up_bound <- round((2*nb_range+1)/5)+1
      just_after_event_index <- which(column_index == 0)+1
      
      
      
      buckets_index <- rep(1:up_bound,rep(5,length(1:up_bound)))
      buckets_index_change <- c(0,diff(buckets_index))
      
      while(buckets_index_change[just_after_event_index] != 1){
        buckets_index_change <- buckets_index_change[-1]
        buckets_index <- buckets_index[-1]
      }
      to_aggregate_dt <- as.data.table(data.frame(index=column_index, buckets_index= buckets_index[1:length(column_index)], value=event_minutes_vector))
      bucket_value <- function(index,value){
        return(list(max(index),mean(value,na.rm=TRUE)))
      }
      
      tt <- to_aggregate_dt[,c("buck_index","buck_return"):=bucket_value(index,value), by=.(buckets_index)]
      tt <- unique(tt[,.(buck_index,buck_return)])
      # print(tt)
      event_minutes_vector <- tt$buck_return
      event_minutes_index <- tt$buck_index
    } 
    
    
    ##############
    ##############
    ##############
    ################ old way
    #     event_minutes_point_vector <- (event_minutes_vector-mean(event_minutes_vector,na.rm=TRUE))/sd(event_minutes_vector,na.rm=TRUE)
    #     event_minutes_vector_probabilities <- pnorm(abs(event_minutes_point_vector),lower.tail = FALSE)
    #     event_minutes_vector_probabilities <- 1 - event_minutes_vector_probabilities
    #     
    #     results_event_minutes_vector <- event_minutes_vector_probabilities
    #     ##### first half
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     first_half_indice_inverted <- c(event_indice:1)
    #     first_half_indice <- c(1:event_indice)
    #     
    #     results_event_minutes_vector[first_half_indice_inverted] <- cumsum(event_minutes_vector_probabilities[first_half_indice_inverted])/first_half_indice
    #     ##### second half
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     
    #     results_event_minutes_vector[second_half_indice] <- cumsum(event_minutes_vector_probabilities[second_half_indice])/c(1:length(second_half_indice))
    #     
    #     event_minutes_matrix <- melt(results_event_minutes_vector)
    #     event_minutes_matrix$MINUTES <- event_minutes_index
    #     event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    ##############
    ##############
    ##############
    ################ end of old way
    
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     cum_event_minutes_vector <-  cumsum(event_minutes_vector)
    #     
    #     
    #     cum_event_minutes_vector_reset <- cum_event_minutes_vector-cum_event_minutes_vector[event_indice]
    #     
    #     first_half_indice <- c(1:event_indice)
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     
    #     
    #     
    #     
    #     event_minutes_normalized_vector <- (event_minutes_vector-mean(event_minutes_vector,na.rm=TRUE))/sd(event_minutes_vector,na.rm=TRUE)
    #     
    #     results_event_minutes_normalized_vector <- event_minutes_normalized_vector
    #     ##### first half
    #     event_indice <- (length(event_minutes_vector)- 1)/2
    #     first_half_indice_inverted <- c(event_indice:1)
    #     first_half_indice <- c(1:event_indice)
    #     
    #     results_event_minutes_normalized_vector[first_half_indice_inverted] <- cumsum(event_minutes_normalized_vector[first_half_indice_inverted])/sqrt(first_half_indice)
    #     ##### second half
    #     second_half_indice <- c((event_indice+1):length(event_minutes_vector))
    #     results_event_minutes_normalized_vector[second_half_indice] <- cumsum(event_minutes_normalized_vector[second_half_indice])/sqrt(c(1:length(second_half_indice)))
    #     
    #     
    #     event_minutes_vector_probabilities <- pnorm(abs(results_event_minutes_normalized_vector),lower.tail = FALSE)
    #     event_minutes_vector_probabilities <- 1 - event_minutes_vector_probabilities
    #     
    
    
    event_minutes_matrix <- melt(event_minutes_vector)
    event_minutes_matrix$MINUTES <- event_minutes_index
    event_minutes_matrix <- event_minutes_matrix[,c("MINUTES","value")]
    
    ##############
    ##############
    ##############
    ################
    
    if(centerEvent){
      if (beta_methodo == "BETA_ONE"){
        colnames(event_minutes_matrix)[2] <- "CENTER_ORDIN_ONE"
      }
      
      if (beta_methodo == "BETA_DAY"){
        colnames(event_minutes_matrix)[2] <- "CENTER_ORDIN_DAY"
      }
      
      if (beta_methodo == "BETA_MINUTE"){
        colnames(event_minutes_matrix)[2] <- "CENTER_ORDIN_MINUTE"
      }
      
    } else {
      if (beta_methodo == "BETA_ONE"){
        colnames(event_minutes_matrix)[2] <- "ORDIN_ONE"
      }
      
      if (beta_methodo == "BETA_DAY"){
        colnames(event_minutes_matrix)[2] <- "ORDIN_DAY"
      }
      
      if (beta_methodo == "BETA_MINUTE"){
        colnames(event_minutes_matrix)[2] <- "ORDIN_MINUTE"
      }
      
    }
    
    
    if(is.null(event_minutes_matrix_all_methodo)){
      event_minutes_matrix_all_methodo <- event_minutes_matrix
    } else {
      event_minutes_matrix_all_methodo <- merge(event_minutes_matrix_all_methodo, event_minutes_matrix, by = "MINUTES")
    }
    
  }
  
  if(is.null(localSource)){
    localSource <- "DJ-EQ"
  }
  
  
  if(asymmetric){
    event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[event_minutes_matrix_all_methodo$MINUTES >= -60,]
  }
  if(toPlot){
    pathToSave <- NULL
    if(product_criteria == "DJ-EQ"){
      filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER_CORRADO/",filename)
    }
    
    if(product_criteria == "WE-EQ"){
      filename <- paste0(product_criteria,"_",premium_sources_libelle[[my_source]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
      filename <- gsub("-","",filename)
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER_CORRADO/",filename)
    }
    significance_threshold <- 1 - 0.05
    event_minutes_matrix_all_methodo$ABNORMAL_THRESHOLD <- significance_threshold
    
    event_minutes_matrix_all_methodo[is.na(event_minutes_matrix_all_methodo)] <- 0
    g <- tryCatch(
      RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
      , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
    
    
    
    if (plotInArborescence){
      pathToSave <- NULL
      filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_corrado_event_plot")
      if(product_criteria == "DJ-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      if(product_criteria == "WE-EQ"){
        pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[my_source]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
      }
      
      g <- tryCatch(
        RP_PlotDataFrame(event_minutes_matrix_all_methodo,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
        , error = function(e) {NULL})
      
      if(is.null(g)){
        print("Trouble printing : ")
        print(pathToSave)
      }
      
    }
  }
  
  
  # product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering,my_event, localSourc
  
  results <- list()
  results$toplot_event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo
  
  event_minutes_matrix_all_methodo <- as.data.frame(t(event_minutes_matrix_all_methodo))
  nb_minutes <- dim(event_minutes_matrix_all_methodo)[2]
  
  event_minutes_matrix_all_methodo$lapse <- lapse
  event_minutes_matrix_all_methodo$relevance <- relevance
  event_minutes_matrix_all_methodo$event_relevance <- event_relevance
  event_minutes_matrix_all_methodo$aggregate_criteria <- aggregate_criteria
  event_minutes_matrix_all_methodo$sentiment_criteria <- sentiment_criteria
  event_minutes_matrix_all_methodo$similarity_gap_filter <- similarity_gap_filter
  event_minutes_matrix_all_methodo$event_number_event_filtering <- event_number_event_filtering
  event_minutes_matrix_all_methodo$my_event <- my_event
  event_minutes_matrix_all_methodo$localSource <- localSource
  
  
  
  event_minutes_matrix_all_methodo_minutes <- event_minutes_matrix_all_methodo[1,]
  event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo[-c(1,5),]
  colnames(event_minutes_matrix_all_methodo)[1:nb_minutes] <- event_minutes_matrix_all_methodo_minutes[1:nb_minutes]
  
  # event_minutes_matrix_all_methodo$corrado_methodo <- c("ORDIN_ONE","ORDIN_DAY","ORDIN_MINUTE")
  
  label <- "ORDIN_MINUTE"
  if(centerEvent){
    label <- paste0("CENTERED",label)
  }
  event_minutes_matrix_all_methodo$corrado_methodo <- c(label)
  
  
  results$event_minutes_matrix_all_methodo <- event_minutes_matrix_all_methodo
  return(results)
}


outputGraphicsTogether <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame, plotInArborescence, Russell_version = "R1000"){
  
  pathToSave <- NULL
  if(product_criteria == "DJ-EQ"){
    filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_signif_event_plot")
    pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER/",filename)
  }
  
  if(product_criteria == "WE-EQ"){
    filename <- paste0(product_criteria,"_",premium_sources_libelle[[my_source]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_signif_event_plot")
    filename <- gsub("-","",filename)
    pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHER/",filename)
  }
  significance_threshold <- 1 - 0.05
  dataFrame$ABNORMAL_THRESHOLD <- significance_threshold
  
  dataFrame[is.na(dataFrame)] <- 0
  g <- tryCatch(
    RP_PlotDataFrame(dataFrame,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
    , error = function(e) {NULL})
  
  if(is.null(g)){
    print("Trouble printing : ")
    print(pathToSave)
  }
  
  
  
  if (plotInArborescence){
    pathToSave <- NULL
    filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_signif_event_plot")
    if(product_criteria == "DJ-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
    }
    
    if(product_criteria == "WE-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[my_source]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
    }
    
    g <- tryCatch(
      RP_PlotDataFrame(dataFrame,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
      , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
    
  }
  
  
}



outputGraphicsTogetherBestProfile <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000"){
  
  dataframestats <- dataFrame[,c("MINUTES","STATS_SIGN")]
  colnames(dataframestats) <- c("MINUTES","SIGNIFICANCE")
  dataframerets <- dataFrame[,c("MINUTES","RETS")]
  colnames(dataframerets) <- c("MINUTES","RETURNS")
  
  premium_sources_libelle <- list("5A5702"="Benzinga",
                                  "9D69F1"="MT_Newswires",
                                  "86BD04"="FXStreet_Economic_Calendar",
                                  "CBEE83"="FXStreet_News",
                                  "ED68DC"="Alliance_News",
                                  "C98333"="Ticker_Report",
                                  "CAF988"="SleekMoney",
                                  "ALL"="AllPremiumSources")
  gics_sectors_libelle <- list("1"="Energy",
                               "2"="Materials",
                               "3"="Industrials",
                               "4"="ConsumerDiscretionary",
                               "5"="ConsumerStaples",
                               "6"="HealthCare",
                               "7"="Financials",
                               "8"="IT",
                               "9"="Telecommunication",
                               "10"="Utiliites",
                               "-1"="AllSectors"
  )
  
  
  pathToSave <- NULL
  pathToSaveClean <- NULL
  if(product_criteria == "DJ-EQ"){
    filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sectors_libelle[[as.character(gics_sector)]],"_",my_event,"_log_return_minute_signif_event_plot_best_prof")
    pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHERBEST/",filename)
    pathToSaveClean <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHERBEST/")
  }
  
  if(product_criteria == "WE-EQ"){
    filename <- paste0(product_criteria,"_",premium_sources_libelle[[localSource]],"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sectors_libelle[[as.character(gics_sector)]],"_",my_event,"_log_return_minute_signif_event_plot_best_prof")
    filename <- gsub("-","",filename)
    pathToSaveClean <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHERBEST/")
    pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/ALLTOGETHERBEST/",filename)
  }
  significance_threshold <- 1 - 0.05
  dataframestats$ABNORMAL_THRESHOLD <- significance_threshold
  
  dataframestats[is.na(dataframestats)] <- 0
  g1 <- tryCatch(
    RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
    , error = function(e) {NULL})
  
  
  g2 <- tryCatch(
    RP_PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = NULL)
    , error = function(e) {NULL})
  
  g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = filename,outputDataPath = pathToSaveClean, cols = 1, width = 10, height = 15)
  
  
  
  
  if(is.null(g)){
    print("Trouble printing : ")
    print(pathToSave)
  }
  
  
  
  if (plotInArborescence){
    pathToSave <- NULL
    filename <- paste0(product_criteria,"_",aggregate_criteria,"_",sentiment_criteria,"_",similarity_gap_filter,"_",ens_filter,"_",event_number_event_filtering,"_",gics_sector,"_",my_event,"_log_return_minute_signif_event_plot")
    if(product_criteria == "DJ-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
    }
    
    if(product_criteria == "WE-EQ"){
      pathToSave <- paste0(outputDataPath,"EVENT_PICTURES/",Russell_version,"/BETA_ALL/",product_criteria,"/",premium_sources_libelle[[localSource]],"/",aggregate_criteria,"/",sentiment_criteria,"/","similarity_gap_",similarity_gap_filter,"/","ens_filter_",ens_filter,"/",filename)
    }
    
    g <- tryCatch(
      RP_PlotDataFrame(dataFrame,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = pathToSave)
      , error = function(e) {NULL})
    
    if(is.null(g)){
      print("Trouble printing : ")
      print(pathToSave)
    }
    
  }
  
  
}


computeCorradoAbnormalRets <- function(TIMESTAMP_UTC_MIN_NUM,MINUTE,HOUR,DAY,HIGH_PRICE,LOW_PRICE,LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN,CLOSE_PRICE,RP_ENTITY_ID_EVENT,SOURCE,TOTAL_VOLUME){
  minute_range <- 180
  stored_corrado_abnormalrets_results_tot <- NULL
  event_indexes <- which((!is.na(RP_ENTITY_ID_EVENT)))
  
  for (event_index in event_indexes){
    lower_timestamp <- TIMESTAMP_UTC_MIN_NUM[event_index] - minute_range*60
    upper_timestamp <- TIMESTAMP_UTC_MIN_NUM[event_index] + minute_range*60
    stored_corrado_abnormalrets_results <- data.frame(event_window_timestamps = seq(lower_timestamp,upper_timestamp,60))
    event_window_timestamps_indices <- match(stored_corrado_abnormalrets_results$event_window_timestamps,TIMESTAMP_UTC_MIN_NUM,  nomatch = NA)
    estimation_window_indices <- which((TIMESTAMP_UTC_MIN_NUM <= lower_timestamp) & (!is.na(LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN)) & (!is.na(CLOSE_PRICE)))
    if (length(estimation_window_indices) >0){
      #############
      #############
      ############# abnormal volatility
      sameMinutePastVolatility <- function(indice,MINUTE,HOUR,DAY,HIGH_PRICE,LOW_PRICE,CLOSE_PRICE){
        day_minute_mean_volatility <- mean(((HIGH_PRICE[indice] - LOW_PRICE[indice])/CLOSE_PRICE[indice])^2,na.rm=TRUE )
        if(is.na(day_minute_mean_volatility)){
          return(NA)
        }
        day_seasonality_index <- (MINUTE == MINUTE[indice]) & (HOUR == HOUR[indice] & (DAY != DAY[indice]))
        day_minute_mean_past_volatility <- mean(((HIGH_PRICE[day_seasonality_index] - LOW_PRICE[day_seasonality_index])/CLOSE_PRICE[day_seasonality_index])^2,na.rm=TRUE )
        
        if (is.na(day_minute_mean_past_volatility) || day_minute_mean_past_volatility == 0){
          return(NA)
        }
        return(day_minute_mean_volatility/day_minute_mean_past_volatility)
      }
      
      toPass <- function(indice){sameMinutePastVolatility(indice,MINUTE,HOUR,DAY,HIGH_PRICE,LOW_PRICE,CLOSE_PRICE)}
      
      abnormmal.vol.event <- apply(X=as.data.frame(event_window_timestamps_indices),MARGIN = 1,FUN=toPass)
      
      #############
      #############
      ############# abnormal volume
      sameMinutePastVolume <- function(indice,MINUTE,HOUR,DAY,TOTAL_VOLUME){
        day_minute_mean_volume <- TOTAL_VOLUME[indice]
        if(is.na(day_minute_mean_volume)){
          return(NA)
        }
        day_seasonality_index <- (MINUTE == MINUTE[indice]) & (HOUR == HOUR[indice] & (DAY != DAY[indice]))
        day_minute_mean_past_volume <- mean(TOTAL_VOLUME[day_seasonality_index],na.rm=TRUE )
        
        if (is.na(day_minute_mean_past_volume) || day_minute_mean_past_volume == 0){
          return(NA)
        }
        return(day_minute_mean_volume/day_minute_mean_past_volume)
      }
      
      toPass <- function(indice){sameMinutePastVolume(indice,MINUTE,HOUR,DAY,TOTAL_VOLUME)}
      
      abnormmal.volu.event <- apply(X=as.data.frame(event_window_timestamps_indices),MARGIN = 1,FUN=toPass)
      
      #############
      #############
      ############# abnormal returns and volume
      
      
      
      
      
      market.model.minute <- RP_CalibrateMarketModel(LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN[estimation_window_indices], CLOSE_PRICE[estimation_window_indices])
      
      dailyBeta <- 1
      
      market.model.day <- list(betam=dailyBeta, gamma=0, sigma=sd(CLOSE_PRICE[estimation_window_indices]-dailyBeta*LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN[estimation_window_indices],na.rm=TRUE))
      sigma_one=sd(CLOSE_PRICE[estimation_window_indices]-LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN[estimation_window_indices],na.rm=TRUE)
      #### Corrado rank
      AB_RET_ONE <- log(CLOSE_PRICE/mlag(CLOSE_PRICE,1))-LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN
      AB_RET_DAY <- log(CLOSE_PRICE/mlag(CLOSE_PRICE,1))-market.model.day$betam*LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN     
      AB_RET_MIN <- log(CLOSE_PRICE/mlag(CLOSE_PRICE,1))-market.model.minute$betam*LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN-market.model.minute$gamma*mlag(LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN,1)
      TOT_RET <- log(CLOSE_PRICE/mlag(CLOSE_PRICE,1))
      
      RANK_BETA_ONE <- AB_RET_ONE
      RANK_BETA_DAY <- AB_RET_DAY
      RANK_BETA_MINUTE <- AB_RET_MIN
      
      RANK_BETA_ONE[!is.na(RANK_BETA_ONE)] <- rank(AB_RET_ONE[!is.na(AB_RET_ONE)])
      RANK_BETA_DAY[!is.na(RANK_BETA_DAY)] <- rank(AB_RET_DAY[!is.na(AB_RET_ONE)])
      RANK_BETA_MINUTE[!is.na(RANK_BETA_MINUTE)] <- rank(AB_RET_MIN[!is.na(AB_RET_ONE)])
      
      #         ####
      #         clo <- CLOSE_PRICE[event_window_timestamps_indices]
      #         laggedclo <- mlag(clo,1)
      #         event_log_minute_return <- log(clo/laggedclo)
      #         indret <- LOG_CAP_WEIGHTED_LOG_MINUTE_RETURN[event_window_timestamps_indices]
      # 
      #         stored_corrado_abnormalrets_results$betaoneabnormalresults <- event_log_minute_return - indret
      #         stored_corrado_abnormalrets_results$betadailyabnormalresults <- event_log_minute_return - market.model.day$betam*indret
      #         stored_corrado_abnormalrets_results$betamarketabnormalresults <- event_log_minute_return - market.model.minute$betam*indret
      
      stored_corrado_abnormalrets_results$betaoneabnormalresults <- AB_RET_ONE[event_window_timestamps_indices]
      stored_corrado_abnormalrets_results$betadailyabnormalresults <- AB_RET_DAY[event_window_timestamps_indices]
      stored_corrado_abnormalrets_results$betamarketabnormalresults <- AB_RET_MIN[event_window_timestamps_indices]
      stored_corrado_abnormalrets_results$betamarketabsoluteresults <- TOT_RET[event_window_timestamps_indices]
      
      
      stored_corrado_abnormalrets_results$betaonerank <- RANK_BETA_ONE[event_window_timestamps_indices]/sum(!is.na(RANK_BETA_ONE))
      stored_corrado_abnormalrets_results$betadailyrank <- RANK_BETA_DAY[event_window_timestamps_indices]/sum(!is.na(RANK_BETA_DAY))
      stored_corrado_abnormalrets_results$betamarketrank <- RANK_BETA_MINUTE[event_window_timestamps_indices]/sum(!is.na(RANK_BETA_MINUTE))
      
      stored_corrado_abnormalrets_results$abnormalvol <- abnormmal.vol.event
      
      ### old standard way
      # stored_corrado_abnormalrets_results$volume <- TOTAL_VOLUME[event_window_timestamps_indices]
      stored_corrado_abnormalrets_results$volume <- abnormmal.volu.event
      
      stored_corrado_abnormalrets_results$event_window_timestamps <- NULL
      stored_corrado_abnormalrets_results <- as.data.frame(t(stored_corrado_abnormalrets_results))
      colnames(stored_corrado_abnormalrets_results) <- seq(1,2*minute_range+1)
      stored_corrado_abnormalrets_results$TIMESTAMP_UTC_MIN_NUM <- TIMESTAMP_UTC_MIN_NUM[event_index] 
      stored_corrado_abnormalrets_results$RP_ENTITY_ID <- RP_ENTITY_ID_EVENT[event_index] 
      stored_corrado_abnormalrets_results$SOURCE <- SOURCE[event_index] 
      
      stored_corrado_abnormalrets_results$SIGMA <- c(sigma_one, market.model.day$sigma, market.model.minute$sigma,sigma_one, sigma_one, market.model.day$sigma, market.model.minute$sigma,market.model.minute$sigma,market.model.minute$sigma) 
      
      stored_corrado_abnormalrets_results$BETA_METHODO <- c("BETA_ONE","BETA_DAY","BETA_MINUTE","ABSOLUTE","CORRADO_ONE","CORRADO_DAY","CORRADO_MINUTE","VOLATILITY","VOLUME")
      stored_corrado_abnormalrets_results_tot <- rbind(stored_corrado_abnormalrets_results_tot, stored_corrado_abnormalrets_results)
    }
  }
  return(stored_corrado_abnormalrets_results_tot)  
}  

