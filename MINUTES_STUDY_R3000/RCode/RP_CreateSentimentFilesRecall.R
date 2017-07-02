RP_CreateSentimentFilesRecall <- function (edition, region, RavenPackTableData, EventOnly = TRUE, Entities,startDate, endDate, dbCon, dataPath, fileName) 
{
  shift <- RP_TimeZone_Offset(region, snapshotReference, offset)
  sentimentFile = gsub("( )", "", paste(dataPath, fileName,".RData", sep = ""))
  startRP = startDate
  endRP = endDate
  if (edition == "FL-EQ") {
    EditionText <- ""
  }
  else {
    EditionText <- paste("PRODUCT_KEY='", edition, "'", 
                         sep = "")
  }
  if (EventOnly == TRUE) {
    EventText <- c("RELEVANCE=100 AND ESS!=50 ")
    Column <- ""
  }
  else {
    EventText <- c("RELEVANCE>=70 ")
    Column <- "RELEVANCE,"
  }
  
  EntitiesList <- paste("'",Entities,"'",collapse=",",sep='')
  EntitiesList <- paste('(',EntitiesList,')',sep="")
  

  RPDataQuery = paste("Select timestamp_utc , RP_ENTITY_ID,", 
                      Column, RP_GetSelectClause_STR(RavenPackTableData), ",          
                      convert_timezone('UTC','",shift$timeZone, "',timestamp_utc) TIMESTAMP_REGION,
                      trunc(convert_timezone('UTC','",shift$timeZone, "',dateadd(mins,", shift$offset, ",Timestamp_utc))) \"DATE\"
                      FROM analytics.rpna_400_eqt       
                      WHERE ", EventText,"AND RP_STORY_EVENT_COUNT>1 
                      AND TIMESTAMP_UTC>='", startDate,"'AND TIMESTAMP_UTC<='", endDate,
                      "' AND rp_entity_ID in ", EntitiesList,
                      " AND RP_STORY_ID IN (Select DISTINCT RP_STORY_ID FROM analytics.rpna_400_eqt 
                        WHERE RP_STORY_EVENT_COUNT>1 
                        AND ",EventText,")
                      ORDER BY timestamp_utc", sep = "")
  
  
  start = Sys.time()
  CompanyRPData = dbGetQuery(dbCon, RPDataQuery)
  elapsedTime = Sys.time() - start
  colnames(CompanyRPData) <- toupper(colnames(CompanyRPData))
  save(CompanyRPData, file = sentimentFile)
  log_message = paste("Sentiment file for edition ", edition, 
                      "and region ", region, " created. Elapsed time: ", as.numeric(elapsedTime, 
                                                                                    units = "mins"), " mins")
  print(log_message)
  CompanyRPData
}
