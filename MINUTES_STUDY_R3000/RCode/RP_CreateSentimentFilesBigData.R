RP_CreateSentimentFilesBigData <- function ( region,  EventOnly = TRUE, Entities,startDate, endDate, dbCon, dataPath, fileName) 
{
#   shift <- RP_TimeZone_Offset(region, snapshotReference, offset)
  sentimentFile = gsub("( )", "", paste(dataPath, fileName,".RData", sep = ""))
  startRP = startDate
  endRP = endDate
#   if (edition == "FL-EQ") {
#     EditionText <- ""
#   }
#   else {
#     EditionText <- paste("PRODUCT_KEY='", edition, "'", 
#                          sep = "")
#   }
  if (EventOnly) {
    # EventText <- c("RELEVANCE=100 AND ESS!=50 ")
    EventText <- c("RELEVANCE=100 ")
    Column <- ""
  }
  else {
    EventText <- c("RELEVANCE>=70 ")
    Column <- "RELEVANCE,"
  }
  
  EntitiesList <- paste("'",Entities,"'",collapse=",",sep='')
  EntitiesList <- paste('(',EntitiesList,')',sep="")
  
  ##### new
  #   'select timestamp_utc , RP_ENTITY_ID, "GROUP", CATEGORY, relevance, event_sentiment_score, event_relevance, event_similarity_days, rp_story_id,
  #   RP_STORY_EVENT_COUNT, PRODUCT_KEY from analytics.rbda_100_analytics_rpa_191 limit 1000'

  ##### old
  #   RPDataQuery = paste("Select timestamp_utc , RP_ENTITY_ID,", 
  #                       Column, RP_GetSelectClause_STR(RavenPackTableData), ",          
  #                       convert_timezone('UTC','",shift$timeZone, "',timestamp_utc) TIMESTAMP_REGION,
  #                       trunc(convert_timezone('UTC','",shift$timeZone, "',dateadd(mins,", shift$offset, ",Timestamp_utc))) \"DATE\"
  #                       FROM analytics.rpna_400_eqt     
  
  RPDataQuery = paste('select timestamp_utc , RP_ENTITY_ID, "GROUP", CATEGORY, relevance, event_sentiment_score, event_relevance, 
                      event_similarity_days, rp_story_id, rp_source_id, RP_STORY_EVENT_COUNT, PRODUCT_KEY from analytics.rbda_100_analytics_rpa_191 ',
                      "WHERE ", EventText,"AND TIMESTAMP_UTC>='", startDate,"'AND TIMESTAMP_UTC<='", endDate,
                      "' AND rp_entity_ID in ", EntitiesList," ORDER BY timestamp_utc limit 1000000 ", sep = "")
  
  
  start = Sys.time()
  CompanyRPData = dbGetQuery(dbCon, RPDataQuery)
  elapsedTime = Sys.time() - start
  colnames(CompanyRPData) <- toupper(colnames(CompanyRPData))
  # save(CompanyRPData, file = sentimentFile)
  log_message = paste("Sentiment file for region ", region, " created. Elapsed time: ", as.numeric(elapsedTime, 
                                                                                    units = "mins"), " mins")
  print(log_message)
  return(CompanyRPData)
}
