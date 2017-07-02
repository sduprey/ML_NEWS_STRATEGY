# 
# compute_bidirectional_strategy_west_first <- function(inputDataPath, outputDataPath, first_country, second_country, backtesting_starting_date, backtesting_ending_date, spread_amplification_factor, algorithm_used, europe_as_third_country, zscore, depth){
#   print(backtesting_starting_date)
#   print(first_country)  
#   print(second_country)  
#   # getting Ravenpack macro news data since 2000
#   whole_global_macroRPData <- readRDS(paste(inputDataPath,"rp_global_macro_data_dj_full.rds",sep=""))
#   sophisticated_average_daily_event_RPData <- readRDS(paste(inputDataPath, "european_country_code_list.rds",sep=""))
#   
#   if (first_country == "EU" | second_country == "EU"){
#     my_countries <- c(first_country,second_country)
#   }else{
#     if (first_country %in% sophisticated_average_daily_event_RPData$country_code | second_country %in% sophisticated_average_daily_event_RPData$country_code){
#       sophisticated_average_daily_event_RPData <- sophisticated_average_daily_event_RPData[-which(sophisticated_average_daily_event_RPData$country_code==first_country | sophisticated_average_daily_event_RPData$country_code==second_country| sophisticated_average_daily_event_RPData$country_code=="EU"),]
#       if (europe_as_third_country){
#         whole_global_macroRPData[whole_global_macroRPData$country_code %in% sophisticated_average_daily_event_RPData,"country_code"] <- "PAN_EU"
#         my_countries <- c("EU","PAN_EU",first_country,second_country)
#       }else{
#         my_countries <- c("EU",first_country,second_country)
#       }
#     } else {
#       my_countries <- c(first_country,second_country)
#     }
#   }
#   # for macro data
#   my_total_group_df <- NULL
#   for (my_country in my_countries){
#     global_macroRPData <- whole_global_macroRPData[whole_global_macroRPData$country_code==my_country,]
#     
#     # we cut to NY close to make our decision we always trade first in the US
#     global_macroRPData <- global_macroRPData[!is.na(global_macroRPData$timestamp_utc),]
#     # if (my_country == "US"| my_country %in% sophisticated_average_daily_event_RPData){
#     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "America/New_York")
#     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
#     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= us_news_cutting_off_hour
#     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
#     # }
#     #   ## aggregating the news per day for macro
#     #   if (my_country == "US"){
#     #     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "America/New_York")
#     #     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
#     #     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= us_news_cutting_off_hour
#     #     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
#     #   }
#     #   if (my_country == "EU" | my_country == "GB" | my_country == "DE"| my_country == "FR"){
#     #     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "Europe/London")
#     #     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
#     #     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= eu_news_cutting_off_hour
#     #     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
#     #   }
#     #   if (my_country == "JP"){
#     #     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "Asia/Tokyo")
#     #     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
#     #     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= jp_news_cutting_off_hour
#     #     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
#     #   }
#     #   
#     ## looping over topics for macro
#     global_macroRPData$topic <- as.factor(global_macroRPData$topic)
#     
#     ## looping over groups for macro
#     global_macroRPData$group <- as.factor(global_macroRPData$group)
#     
#     # ## data preprocessing : building a mapping group to topics
#     unique_macro_groups <- levels(global_macroRPData$group)
#     my_macro_topics <- as.vector(global_macroRPData[ match(unique_macro_groups, global_macroRPData[,'group']), 'topic'])
#     
#     # we don t do that here because we only limit ourself to Dow Jones : no change of regime
#     # ### Filtering before RP news change of regime
#     # global_macroRPData <- global_macroRPData[global_macroRPData$DATE >= "2007-01-01",]
#     # global_corporateRPData <- global_corporateRPData[global_corporateRPData$DATE >= "2007-01-01",]
#     
#     # stat <- function(x) c(min = min(x), max = max(x), mean = mean(x), count=length(x))
#     stat <- function(x) c(mean = mean(x))
#     
#     # we are going down to the Ravenpack sub type level
#     if (depth == 2){
#       global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),sep="_")
#       global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
#     }
#     if (depth == 3){
#       global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),gsub("-", "_", global_macroRPData$type),sep="_")
#       global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
#     }
#     if (depth == 4){
#       global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),gsub("-", "_", global_macroRPData$type),gsub("-", "_", global_macroRPData$sub_type),sep="_")
#       global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
#     }
#     
#     # aggregating per day our macro news
#     print("Aggregating Ravenpack taxonomy up to the sub type level")
#     daily_global_macroRPData <- aggregate(global_macroRPData$ess, by=list(global_macroRPData$DATE,global_macroRPData$macro_key), stat)
#     daily_global_macroRPData<-dcast(daily_global_macroRPData, "Group.1 ~ Group.2")
#     colnames(daily_global_macroRPData)[1] <- c("DATE")
#     
#     # z scoring our macro news up to 2013-01-01
#     if (zscore){
#       scaling_sample_size <- sum(daily_global_macroRPData$DATE <= backtesting_starting_date)
#       # it assumes here that our data frame is sorted by date with time increasing from bottom to top
#       my_scaled_parameters <-lapply(daily_global_macroRPData[,-1], function(x) {(x - mean(x[1:scaling_sample_size],na.rm=TRUE))/sd(x[1:scaling_sample_size],na.rm=TRUE)} )
#       daily_global_macroRPData[,-1] <- as.data.frame(t(do.call(rbind,my_scaled_parameters)))
#     }
#     ##### Cleaning up after z scoring (dropping empty news columns and zeroing the no news days)
#     # # when no news happened on that day we set our score to zero the neutral score after z scoring
#     daily_global_macroRPData[is.na(daily_global_macroRPData)] <- 0
#     my_columns_to_keep <- as.logical(colSums(daily_global_macroRPData[,-1]) != 0 & colSums(daily_global_macroRPData[,-1]) != Inf)
#     my_columns_to_keep[is.na(my_columns_to_keep)] <- FALSE
#     # we always keep the DATE column
#     daily_global_macroRPData<-daily_global_macroRPData[,c(TRUE,my_columns_to_keep)]
#     
#     if(sum(my_columns_to_keep)){
#       # plotting the macro group sentiment
#       #       daily_global_macroRPData_toplot_df <-
#       #         melt(daily_global_macroRPData,"DATE")
#       #       my_title <-
#       #         "Daily average macro news at the group level"
#       #       g<-ggplot(
#       #         daily_global_macroRPData_toplot_df,aes(
#       #           x = DATE,y = value,group = variable,color = variable
#       #         )
#       #       ) +
#       #         geom_line() +
#       #         scale_x_date() +
#       #         ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
#       #         theme(title = element_text(size = 12, face = 'bold')) +
#       #         theme(legend.position = c(0.2,0.2), legend.box = "vertical") +
#       #         theme(legend.background = element_rect(fill = "gray90")) +
#       #         theme(legend.key.size = unit(0.7, "cm"))
#       #       print(g)
#       
#       daily_global_macroRPData <- daily_global_macroRPData[order(daily_global_macroRPData$DATE),]
#       colnames(daily_global_macroRPData) <- c("DATE",paste(my_country,"_",colnames(daily_global_macroRPData[,-1]),sep=""))
#       if (is.null(my_total_group_df)){
#         my_total_group_df <- daily_global_macroRPData
#       } else {
#         # we keep all dates even if one has no sentiment
#         my_total_group_df <- merge(my_total_group_df,daily_global_macroRPData,by="DATE",all=T)
#       }
#       # make sure it is ordered to build our momentum signals
#       my_total_group_df <- my_total_group_df[order(my_total_group_df$DATE),]
#     }
#   }
#   
#   # we here just make sure that we have each day
#   all_days_df <- data.frame(DATE=seq(as.Date(backtesting_starting_date), as.Date(backtesting_ending_date), by="days"))
#   my_total_group_df <- merge(my_total_group_df,all_days_df,by="DATE", all=T)
#   my_predicting_columns <- colnames(my_total_group_df[,-1])
#   
#   # saving the per group daily macro/corporate aggregated sentiment data 
#   my_official_countries = c(first_country, second_country)
#   for (my_bond_future_country in my_official_countries){
#     filename <- my_bondfutures_flatfile[my_bond_future_country]
#     if (!is.null(filename)|(filename == "NULL")){
#       US_TN_Rolled_Futures <- readRDS(paste(outputDataPath,filename, sep = ""))
#       US_TN_Rolled_Futures <- US_TN_Rolled_Futures[order(US_TN_Rolled_Futures$DATE),]
#       US_TN_Rolled_Futures <- US_TN_Rolled_Futures[US_TN_Rolled_Futures$Date >= "2000-01-01",]
#       ####### to make those computations we have to make sure that our data frame begins with the last date
#       ####### to make those computations we have to make sure that our data frame begins with the last date
#       US_TN_Rolled_Futures <- US_TN_Rolled_Futures[rev(order(US_TN_Rolled_Futures$DATE)),]
#       ####### to make those computations we have to make sure that our data frame begins with the last date
#       ####### to make those computations we have to make sure that our data frame begins with the last date
#       # colnames(US_TN_Rolled_Futures)
#       
#       US_TN_Rolled_Futures$NextSettle <- lagpad(US_TN_Rolled_Futures$Settle,1)
#       US_TN_Rolled_Futures$LastSettle <- c(tail(US_TN_Rolled_Futures$Settle,-1),NA)
#       ## Open returns
#       US_TN_Rolled_Futures$NextOpen <- lagpad(US_TN_Rolled_Futures$Open,1)
#       US_TN_Rolled_Futures$OpenReturn <- log(US_TN_Rolled_Futures$NextOpen/US_TN_Rolled_Futures$Open)
#       US_TN_Rolled_Futures$NextOpenReturn <- lagpad(US_TN_Rolled_Futures$OpenReturn,1)
#       US_TN_Rolled_Futures$NextNextOpenReturn <- lagpad(US_TN_Rolled_Futures$NextOpenReturn,1)
#       US_TN_Rolled_Futures$LastOpenReturn <- c(tail(US_TN_Rolled_Futures$OpenReturn,-1),NA)
#       # Close to close returns
#       # Overnight returns
#       US_TN_Rolled_Futures$Return <- log(US_TN_Rolled_Futures$Settle/US_TN_Rolled_Futures$LastSettle)
#       US_TN_Rolled_Futures$ReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$Return >= 0)
#       
#       US_TN_Rolled_Futures$OvernightReturn <- log(US_TN_Rolled_Futures$Open/US_TN_Rolled_Futures$LastSettle)
#       US_TN_Rolled_Futures$NextReturn <- log(US_TN_Rolled_Futures$NextSettle/US_TN_Rolled_Futures$Settle)
#       
#       US_TN_Rolled_Futures$OvernightReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$OvernightReturn >= 0)
#       
#       US_TN_Rolled_Futures$LastOvernightReturn <- c(tail(US_TN_Rolled_Futures$OvernightReturn,-1),NA)
#       
#       US_TN_Rolled_Futures$NextOvernightReturn <- lagpad(US_TN_Rolled_Futures$OvernightReturn,1)
#       
#       
#       US_TN_Rolled_Futures$LastNextReturn <- c(tail(US_TN_Rolled_Futures$NextReturn,-1),NA)
#       
#       US_TN_Rolled_Futures$NextNextReturn <- lagpad(US_TN_Rolled_Futures$NextReturn,1)
#       US_TN_Rolled_Futures$NextReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$NextReturn >= 0)
#       # Volume
#       US_TN_Rolled_Futures$LastVolume <- c(tail(US_TN_Rolled_Futures$Volume,-1),NA)
#       
#       # we leave every dates with no trading
#       US_TN_Rolled_Futures <- US_TN_Rolled_Futures[complete.cases(US_TN_Rolled_Futures),]
#       # Ordering our dataset : a must do as we use EMA function where past is before in the vector
#       #       US_TN_Rolled_Futures <- US_TN_Rolled_Futures[order(US_TN_Rolled_Futures$Date),]
#       #       
#       # Computing momentum indicator here 
#       # this indicator are computed up to the last day close price or even today s open
#       
#       # Building a reversal momentum indicator optimized till 2013-01-01
#       n1<- 1
#       n2<- 2
#       
#       # we here take the lagged the momentum as we enter the trade from settle to settle and cannot know the current settle data
#       
#       US_TN_Rolled_Futures$Lastleading <- EMA(US_TN_Rolled_Futures$LastSettle, n1)
#       US_TN_Rolled_Futures$Lastlagging <- EMA(US_TN_Rolled_Futures$LastSettle, n2)
#       US_TN_Rolled_Futures$LastMomentum <- 2*((US_TN_Rolled_Futures$Lastleading -US_TN_Rolled_Futures$Lastlagging)>=0)-1
#       US_TN_Rolled_Futures$LastMomentum[1:n2] <- 0 
#       
#       US_TN_Rolled_Futures$Openleading <- EMA(US_TN_Rolled_Futures$Open, n1)
#       US_TN_Rolled_Futures$Openlagging <- EMA(US_TN_Rolled_Futures$Open, n2)
#       US_TN_Rolled_Futures$OpenMomentum <- 2*((US_TN_Rolled_Futures$Openleading -US_TN_Rolled_Futures$Openlagging)>=0)-1
#       US_TN_Rolled_Futures$OpenMomentum[1:n2] <- 0 
#       
#       US_TN_Rolled_Futures$leading <- EMA(US_TN_Rolled_Futures$Settle, n1)
#       US_TN_Rolled_Futures$lagging <- EMA(US_TN_Rolled_Futures$Settle, n2)
#       US_TN_Rolled_Futures$Momentum <- 2*((US_TN_Rolled_Futures$leading -US_TN_Rolled_Futures$lagging)>=0)-1
#       US_TN_Rolled_Futures$Momentum[1:n2] <- 0 
#       
#       # yesterday momentum indicators
#       US_TN_Rolled_Futures$LastRSI3 <- RSI(US_TN_Rolled_Futures$LastSettle, 3)
#       US_TN_Rolled_Futures$LastDEMA <- DEMA(US_TN_Rolled_Futures$LastSettle, 10)
#       US_TN_Rolled_Futures$LastDirection <- US_TN_Rolled_Futures$LastSettle - US_TN_Rolled_Futures$LastOpen
#       
#       US_TN_Rolled_Futures$OpenRSI3 <- RSI(US_TN_Rolled_Futures$Open, 3)
#       US_TN_Rolled_Futures$OpenDEMA <- DEMA(US_TN_Rolled_Futures$Open, 10)
#       bbOpen <- BBands(US_TN_Rolled_Futures$Open, sd=2.0)
#       US_TN_Rolled_Futures$OpenBB20 <- bbOpen[,"pctB"]
#       
#       # today momentum indicators
#       US_TN_Rolled_Futures$RSI3<- RSI(US_TN_Rolled_Futures$Settle, 3)
#       US_TN_Rolled_Futures$Trend <- US_TN_Rolled_Futures$Open - SMA(US_TN_Rolled_Futures$Open, 50)
#       US_TN_Rolled_Futures$DEMA <- DEMA(US_TN_Rolled_Futures$Settle, 10)
#       US_TN_Rolled_Futures$Direction <-  US_TN_Rolled_Futures$Settle - US_TN_Rolled_Futures$Open
#       bb <- BBands(US_TN_Rolled_Futures$Settle, sd=2.0)
#       US_TN_Rolled_Futures$BB20 <- bb[,"pctB"]
#       
#       ##### Building a trading signal
#       
#       colnames(US_TN_Rolled_Futures) <- paste(my_bond_future_country, colnames(US_TN_Rolled_Futures), sep="")
#       colnames(US_TN_Rolled_Futures)[1] <- "DATE"
#       
#       # we keep every date (because we alreay removed all the trading dates)
#       my_total_group_df <- merge(my_total_group_df,US_TN_Rolled_Futures,by="DATE")
#     }
#   }
#   
#   my_total_group_df[is.na(my_total_group_df)] <- 0
#   
#   # adding price momentum signals to improve our prediction
#   # first country is wester : 
#   my_predicting_columns_first_country <-  my_predicting_columns[grepl(paste("^",first_country,"*",sep=""),my_predicting_columns)]
#   my_predicting_columns_second_country <-  my_predicting_columns[grepl(paste("^",second_country,"*",sep=""),my_predicting_columns)]
#   if (europe_as_third_country){
#     sophisticated_average_daily_event_RPData <- readRDS(paste(inputDataPath, "european_country_code_list.rds",sep=""))
#     if (first_country %in% sophisticated_average_daily_event_RPData$country_code){
#       eu_columns<-  my_predicting_columns[grepl(paste("^","EU","*",sep=""),my_predicting_columns)]
#       my_predicting_columns_second_country <- c(my_predicting_columns_second_country,eu_columns)
#     }
#     sophisticated_average_daily_event_RPData <- readRDS(paste(inputDataPath, "european_country_code_list.rds",sep=""))
#     if (second_country %in% sophisticated_average_daily_event_RPData$country_code){
#       eu_columns<-  my_predicting_columns[grepl(paste("^","EU","*",sep=""),my_predicting_columns)]
#       my_predicting_columns_second_country <- c(my_predicting_columns_second_country,eu_columns)
#     }
#   }
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,paste(first_country, "Trend", sep=""))
#   my_predicting_columns_second_country <- c(my_predicting_columns_second_country,paste(second_country, "Trend", sep=""))
#   
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,paste(first_country, "OvernightReturn", sep=""))
#   my_predicting_columns_second_country <- c(my_predicting_columns_second_country,paste(second_country, "OvernightReturn", sep=""))
#   # we can go up to the close of the very day for the second country as we trade only at the next day open
#   # my_predicting_columns <- c(my_predicting_columns,paste(first_country, "LastMomentum", sep=""))
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,paste(first_country, "LastVolume", sep=""))
#   my_predicting_columns_second_country <- c(my_predicting_columns_second_country,paste(second_country, "Volume", sep=""))
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,paste(first_country, "LastMomentum", sep=""))
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,paste(first_country, "OpenMomentum", sep=""))
#   my_predicting_columns_second_country <- c(my_predicting_columns_second_country,paste(second_country, "Momentum", sep=""))
#   # my_predicting_columns <- c(my_predicting_columns,paste(first_country, "LastRSI3", sep=""))
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,paste(first_country, "OpenRSI3", sep=""))
#   my_predicting_columns_second_country <- c(my_predicting_columns_second_country,paste(second_country, "RSI3", sep=""))
#   # my_predicting_columns <- c(my_predicting_columns,  paste(first_country, "LastDEMA", sep=""))
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,  paste(first_country, "OpenDEMA", sep=""))
#   my_predicting_columns_second_country <- c(my_predicting_columns_second_country,paste(second_country, "DEMA", sep=""))
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,  paste(first_country, "OpenBB20", sep=""))
#   my_predicting_columns_second_country <- c(my_predicting_columns_second_country,paste(second_country, "BB20", sep=""))
#   my_predicting_columns_first_country <- c(my_predicting_columns_first_country,paste(first_country, "LastDirection", sep=""))  
#   my_predicting_columns_second_country <- c(my_predicting_columns_second_country,paste(second_country, "Direction", sep=""))
#   
#   # my_predicting_all_columns <- c(my_predicting_columns,paste(first_country, "Trend", sep=""))
#   
#   ### Backtesting our model with an expanding window and predicting out of sample next overnight returns each day according to sentiment up to the close
#   start_date <- backtesting_starting_date
#   end_date <-  backtesting_ending_date
#   
#   backtesting_dates_logical_index <-
#     (my_total_group_df$DATE >= start_date &
#        my_total_group_df$DATE <= end_date)
#   
#   backtesting_dates_index <-
#     which(backtesting_dates_logical_index == TRUE)
#   
#   nb_iteration <- sum(backtesting_dates_logical_index)
#   
#   backtesting_predictions_first <-
#     matrix(0,nrow = nb_iteration-1, ncol = 1)
#   backtesting_predictions_second <-
#     matrix(0,nrow = nb_iteration-1, ncol = 1)
#   backtesting_predictions_third <-
#     matrix(0,nrow = nb_iteration-1, ncol = 1)
#   backtesting_index <-
#     matrix(0,nrow = nb_iteration-1, ncol = 1)
#   
#   
#   for (i in 1:(nb_iteration-1)) {
#     print("Predicting iteration number ")
#     print(i)
#     print("Over")
#     print(nb_iteration)
#     # we use all the past available data to train our xgboost model
#     # my_model_columns <- c(my_predicting_columns, paste(my_official_countries, "NextReturn", sep=""), paste(my_official_countries, "NextOpenReturn", sep=""))
#     
#     output_one_column <-  paste(first_country, "NextReturn", sep="")
#     output_two_column <- paste(second_country, "NextOpenReturn", sep="")
#     
#     my_model_columns_first_country <- c(my_predicting_columns_first_country, output_one_column)
#     my_model_columns_second_country <- c(my_predicting_columns_second_country, output_two_column)
#     
#     my_iteration_data_first <- my_total_group_df[1:backtesting_dates_index[i],my_model_columns_first_country]
#     my_iteration_data_second <- my_total_group_df[1:backtesting_dates_index[i],my_model_columns_second_country]
#     #     
#     #     model_calibrating_data_first <- my_iteration_data_first[,my_predicting_columns_first_country]
#     #     model_calibrating_data_second <- my_iteration_data_second[,my_model_columns_second_country]
#     #     # we try to predict the next day spread by predicting the two directions
#     #     # the first country always has to be the united states
#     #     y_first <- my_iteration_data_first[,paste(first_country,"NextReturn",sep="")]
#     #     y_second <- my_iteration_data_second[,paste(second_country,"NextOpenReturn",sep="")]
#     #     # trading the spread between countries
#     #     # y_third <- my_iteration_data[,paste(first_country,"NextReturn",sep="")]-my_iteration_data[,paste(second_country,"NextOpenReturn",sep="")]
#     #     # y_second <- my_iteration_data[,paste(second_country,"NextDayReturn",sep="")]
#     
#     print("Using the past data from")
#     print(my_total_group_df$DATE[1])
#     print("up to ")
#     print(my_total_group_df$DATE[backtesting_dates_index[i]])
#     # we predict the next date
#     prediction_index <- backtesting_dates_index[i]+1
#     my_prediction_date <- my_total_group_df$DATE[prediction_index]
#     print("Predicting for the next day : ")
#     print(my_prediction_date)
#     
#     model_to_predict_first <- my_total_group_df[prediction_index,my_predicting_columns_first_country]
#     model_to_predict_second <- my_total_group_df[prediction_index,my_predicting_columns_second_country]
#     
#     #     model_to_predict <- my_total_group_df[prediction_index,]
#     #     
#     #     train_X_first <- as.matrix(model_calibrating_data_first)
#     #     train_X_second <- as.matrix(model_calibrating_data_second)
#     #     
#     #     train_Y_first <- as.matrix(y_first)
#     #     train_Y_second <- as.matrix(y_second)
#     #     # train_Y_third <- as.matrix(y_third)
#     #     
#     #     test_X_first <- as.matrix(model_to_predict[,my_predicting_columns_first_country])
#     #     test_X_second <- as.matrix(model_to_predict[,my_predicting_columns_second_country])
#     #     
#     #     # we train two different trees with the same training set
#     #     bst_first <- xgboost(data = train_X_first, label = train_Y_first, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
#     #     # we train two different trees with the same training set
#     #     bst_second <- xgboost(data = train_X_second, label = train_Y_second, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
#     #     # we train two different trees with the same training set
#     #     # bst_third <- xgboost(data = train_X, label = train_Y_third, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
#     #     
#     #     my_prediction_first <- predict(bst_first,test_X_first)
#     #     my_prediction_second <- predict(bst_second,test_X_second)
#     #     # my_prediction_third <- predict(bst_third,test_X)
#     #     
#     #     print("My first prediction")
#     #     print(my_prediction_first)
#     #     print("My second prediction")
#     #     print(my_prediction_second)
#     #     print("My third prediction")
#     #     print(my_prediction_third)
#     
#     if(algorithm_used == "xgboost"){
#       my_iteration_results <- predict_next_day_bidirectional_xgboost(my_predicting_columns_first_country , my_predicting_columns_second_country , output_one_column, output_two_column, my_iteration_data_first, my_iteration_data_second, model_to_predict_first, model_to_predict_second)
#     }
#     
#     if(algorithm_used == "rpart"){
#       my_iteration_results <- predict_next_day_bidirectional_rpart(my_predicting_columns_first_country , my_predicting_columns_second_country , output_one_column, output_two_column, my_iteration_data_first, my_iteration_data_second, model_to_predict_first, model_to_predict_second)
#     }
#     
#     if(algorithm_used == "rpart_unpruned"){
#       my_iteration_results <- predict_next_day_bidirectional_rpart_unpruned(my_predicting_columns_first_country , my_predicting_columns_second_country , output_one_column, output_two_column, my_iteration_data_first, my_iteration_data_second, model_to_predict_first, model_to_predict_second)
#     }
#     
#     
#     # backtesting_predictions[i,] <- my_prediction_first
#     backtesting_predictions_first[i,] <- my_iteration_results$first
#     backtesting_predictions_second[i,] <- my_iteration_results$second
#     # backtesting_predictions_third[i,] <- my_prediction_third
#     
#     backtesting_index[i,] <- prediction_index
#   }
#   
#   my_total_group_df$NextDaySpread_RegTree_first <- NA
#   my_total_group_df$NextDayWeigths_first <- NA
#   my_total_group_df$NextDaySpread_RegTree_second <- NA
#   my_total_group_df$NextDayWeigths_second <- NA
#   
#   weight_one <- backtesting_predictions_first/(backtesting_predictions_first + backtesting_predictions_second)
#   weight_two <- backtesting_predictions_second/(backtesting_predictions_first + backtesting_predictions_second)
#   
#   my_total_group_df$NextDaySpread_RegTree_first[backtesting_index] <- backtesting_predictions_first
#   my_total_group_df$NextDaySpread_RegTree_second[backtesting_index] <- backtesting_predictions_second
#   
#   
#   for (l in 1:length(backtesting_predictions_first)){
#     if ((backtesting_predictions_first[l] <0)&&(backtesting_predictions_second[l]<0)){
#       # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#       # my_total_group_df$NextDayWeigths_first[backtesting_index]
#       weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#       # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#       weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#       
#     } 
#     
#     #     if ((backtesting_predictions_first[l] >0)&&(backtesting_predictions_second[l] >0)){
#     #       # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
#     #       my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
#     #       my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
#     #     } 
#     #     
#     ## this is the case where we must set a limit on the traded quantities
#     ## as they can become huge and still summing to one because of their sign difference
#     if ((backtesting_predictions_first[l] <0)&&(backtesting_predictions_second[l] >0)){
#       if (abs(backtesting_predictions_first[l])<backtesting_predictions_second[l]){
#         # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
#         #         my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
#         #         my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
#         # we here limit the positive weight : the second one
#         if (abs(weight_two[l]) > spread_amplification_factor){
#           # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#           weight_two[l] <- spread_amplification_factor
#           # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#           weight_one[l] <- 1 - weight_two[l]# my_total_group_df$NextDayWeigths_second[backtesting_index]
#         }
#       } else {
#         # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#         # my_total_group_df$NextDayWeigths_first[backtesting_index]
#         weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#         # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#         weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#         # we here limit the positive weight : the second one
#         if (abs(weight_two[l]) > spread_amplification_factor){
#           # my_total_group_df$NextDayWeigths_second[backtesting_index]
#           weight_two[l]<- spread_amplification_factor
#           # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#           weight_one[l] <- 1 - weight_two[l] # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#         }
#       }
#     } 
#     ## this is the case where we must set a limit on the traded quantities
#     ## as they can become huge and still summing to one because of their sign difference
#     if ((backtesting_predictions_first[l] >0)&&(backtesting_predictions_second[l] <0)){
#       if (abs(backtesting_predictions_second[l])<backtesting_predictions_first[l]){
#         # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
#         #         my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
#         #         my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
#         # we here limit the positive weight : the first one
#         if (abs(weight_one[l]) > spread_amplification_factor){
#           # my_total_group_df$NextDayWeigths_first[backtesting_index]
#           weight_one[l] <- spread_amplification_factor
#           # my_total_group_df$NextDayWeigths_second[backtesting_index]
#           weight_two[l] <- 1 - weight_one[l] #my_total_group_df$NextDayWeigths_first[backtesting_index]
#         }
#       } else {
#         # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#         #         my_total_group_df$NextDayWeigths_first[backtesting_index] <- -weight_one
#         #         my_total_group_df$NextDayWeigths_second[backtesting_index] <- -weight_two
#         # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#         # my_total_group_df$NextDayWeigths_first[backtesting_index]
#         weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#         # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#         weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#         # we here limit the positive weight : the first one
#         if (abs(weight_one[l]) > spread_amplification_factor){
#           # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#           weight_one[l] <- spread_amplification_factor
#           # my_total_group_df$NextDayWeigths_second[backtesting_index]
#           weight_two[l] <- 1 - weight_one[l] #my_total_group_df$NextDayWeigths_first[backtesting_index]
#         }
#       }
#     } 
#   }
#   
#   my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
#   my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
#   
#   # my_total_group_df$NextDaySpread_RegTree_third[backtesting_index] <- backtesting_predictions_third
#   
#   ## we can Short/long the very liquid futures
#   #   technical_signal_first <- 2*as.numeric(my_total_group_df$NextDaySpread_RegTree_first[backtesting_index] > 0)-1
#   #   technical_signal_second <- 2*as.numeric(my_total_group_df$NextDaySpread_RegTree_second[backtesting_index] > 0)-1
#   #   #   coincidence <- (technical_signal_first == technical_signal_second)
#   #   #   technical_signal_first[coincidence] <- 0.5 * technical_signal_first[coincidence]
#   #   #   technical_signal_second[coincidence] <- 0.5 * technical_signal_second[coincidence]
#   #   technical_signal_first <- technical_signal_first * my_total_group_df$NextDayWeigths_first[backtesting_index] 
#   #   technical_signal_first <- technical_signal_second * my_total_group_df$NextDayWeigths_second[backtesting_index] 
#   technical_signal_first <-  my_total_group_df$NextDayWeigths_first[backtesting_index] 
#   technical_signal_second <-  my_total_group_df$NextDayWeigths_second[backtesting_index] 
#   
#   # technical spread signal
#   # technical_signal_third <- 2*as.numeric(my_total_group_df$NextDaySpread_RegTree_third[backtesting_index] > 0)-1
#   
#   # if positive we go long otherwise we short
#   # first country has always to be the US  
#   
#   first_leg_traded <- paste(first_country,"NextReturn",sep="")
#   first_leg_lasttraded <- paste(first_country,"Return",sep="")
#   first_leg_nexttraded <- paste(first_country,"NextNextReturn",sep="")
#   second_leg_traded <- paste(first_country,"",sep="NextOpenReturn")
#   second_leg_lasttraded <- paste(first_country,"OpenReturn",sep="")
#   second_leg_nexttraded <- paste(first_country,"NextNextOpenReturn",sep="")
#   
#   second_leg <- paste(second_country,"NextOpenReturn",sep="")
#   
#   traded_return <- my_total_group_df[backtesting_index,first_leg_traded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_traded] * technical_signal_second
#   traded_return_last <- my_total_group_df[backtesting_index,first_leg_lasttraded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_lasttraded] * technical_signal_second
#   traded_return_next <- my_total_group_df[backtesting_index,first_leg_nexttraded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_nexttraded] * technical_signal_second
#   
#   #   traded_return_spread <- my_total_group_df[backtesting_index,first_leg_traded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_traded] * technical_signal_third
#   #   traded_return_spread_last <- my_total_group_df[backtesting_index,first_leg_lasttraded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_lasttraded] * technical_signal_third
#   #   traded_return_spread_next <- my_total_group_df[backtesting_index,first_leg_nexttraded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_nexttraded] * technical_signal_third
#   #   
#   #   ############
#   # we make the output generic
#   first_bond_returns <- paste(first_country,"Return",sep="")
#   second_bond_returns <- paste(second_country,"Return",sep="")
#   results <- data.frame(DATES = my_total_group_df$DATE[backtesting_index], 
#                         STRATEGY_RETURN = traded_return, STRATEGY_YESTERDAY = CumFromRetToPricesStart(traded_return_last), STRATEGY_TODAY = CumFromRetToPricesStart(traded_return),STRATEGY_TOMORROW = CumFromRetToPricesStart(traded_return_next), 
#                         # SPREAD_STRATEGY_RETURN = traded_return_spread, SPREAD_STRATEGY_YESTERDAY = CumFromRetToPricesStart(traded_return_spread_last), SPREAD_STRATEGY_TODAY = CumFromRetToPricesStart(traded_return_spread), SPREAD_STRATEGY_TOMORROW = CumFromRetToPricesStart(traded_return_spread_next), 
#                         SECOND_BOND =   CumFromRetToPricesStart(my_total_group_df[backtesting_index,second_bond_returns]), FIRST_BOND =   CumFromRetToPricesStart(my_total_group_df[backtesting_index,first_bond_returns]))
#   ##
#   return(results)
# }


# 
# compute_spread_strategy_west_first <- function(inputDataPath, outputDataPath, first_country, second_country, backtesting_starting_date, backtesting_ending_date, spread_amplification_factor, algorithm_used, europe_as_third_country, zscore, depth){
#   print(backtesting_starting_date)
#   print(first_country)  
#   print(second_country)  
#   # getting Ravenpack macro news data since 2000
#   whole_global_macroRPData <- readRDS(paste(inputDataPath,"rp_global_macro_data_dj_full.rds",sep=""))
#   sophisticated_average_daily_event_RPData <- readRDS(paste(inputDataPath, "european_country_code_list.rds",sep=""))
#   my_countries <- c(first_country,second_country)
#   # if one of the country is in the european zone we add Europe to watch over
#   if (first_country == "EU" | second_country == "EU"){
#     my_countries <- c(first_country,second_country)
#   }else{
#     if (first_country %in% sophisticated_average_daily_event_RPData$country_code | second_country %in% sophisticated_average_daily_event_RPData$country_code){
#       sophisticated_average_daily_event_RPData <- sophisticated_average_daily_event_RPData[-which(sophisticated_average_daily_event_RPData$country_code==first_country | sophisticated_average_daily_event_RPData$country_code==second_country| sophisticated_average_daily_event_RPData$country_code=="EU"),]
#       if (europe_as_third_country){
#         whole_global_macroRPData[whole_global_macroRPData$country_code %in% sophisticated_average_daily_event_RPData,"country_code"] <- "PAN_EU"
#         my_countries <- c("EU","PAN_EU",first_country,second_country)
#       }else{
#         my_countries <- c("EU",first_country,second_country)
#       }
#     } else {
#       my_countries <- c(first_country,second_country)
#     }
#   }
#   
#   # for macro data
#   my_total_group_df <- NULL
#   for (my_country in my_countries){
#     global_macroRPData <- whole_global_macroRPData[whole_global_macroRPData$country_code==my_country,]
#     
#     # we cut to NY close to make our decision we always trade first in the US
#     global_macroRPData <- global_macroRPData[!is.na(global_macroRPData$timestamp_utc),]
#     # if (my_country == "US"| my_country %in% sophisticated_average_daily_event_RPData){
#     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "America/New_York")
#     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
#     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= us_news_cutting_off_hour
#     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
#     # }
#     #   ## aggregating the news per day for macro
#     #   if (my_country == "US"){
#     #     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "America/New_York")
#     #     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
#     #     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= us_news_cutting_off_hour
#     #     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
#     #   }
#     #   if (my_country == "EU" | my_country == "GB" | my_country == "DE"| my_country == "FR"){
#     #     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "Europe/London")
#     #     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
#     #     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= eu_news_cutting_off_hour
#     #     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
#     #   }
#     #   if (my_country == "JP"){
#     #     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "Asia/Tokyo")
#     #     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
#     #     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= jp_news_cutting_off_hour
#     #     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
#     #   }
#     #   
#     ## looping over topics for macro
#     global_macroRPData$topic <- as.factor(global_macroRPData$topic)
#     
#     ## looping over groups for macro
#     global_macroRPData$group <- as.factor(global_macroRPData$group)
#     
#     # ## data preprocessing : building a mapping group to topics
#     unique_macro_groups <- levels(global_macroRPData$group)
#     my_macro_topics <- as.vector(global_macroRPData[ match(unique_macro_groups, global_macroRPData[,'group']), 'topic'])
#     
#     # we don t do that here because we only limit ourself to Dow Jones : no change of regime
#     # ### Filtering before RP news change of regime
#     # global_macroRPData <- global_macroRPData[global_macroRPData$DATE >= "2007-01-01",]
#     # global_corporateRPData <- global_corporateRPData[global_corporateRPData$DATE >= "2007-01-01",]
#     
#     # stat <- function(x) c(min = min(x), max = max(x), mean = mean(x), count=length(x))
#     stat <- function(x) c(mean = mean(x))
#     
#     # we are going down to the Ravenpack sub type level
#     if (depth == 2){
#       global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),sep="_")
#       global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
#     }
#     if (depth == 3){
#       global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),gsub("-", "_", global_macroRPData$type),sep="_")
#       global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
#     }
#     if (depth == 4){
#       global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),gsub("-", "_", global_macroRPData$type),gsub("-", "_", global_macroRPData$sub_type),sep="_")
#       global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
#     }
#     
#     
#     
#     # aggregating per day our macro news
#     print("Aggregating Ravenpack taxonomy up to the sub type level")
#     daily_global_macroRPData <- aggregate(global_macroRPData$ess, by=list(global_macroRPData$DATE,global_macroRPData$macro_key), stat)
#     daily_global_macroRPData<-dcast(daily_global_macroRPData, "Group.1 ~ Group.2")
#     colnames(daily_global_macroRPData)[1] <- c("DATE")
#     if (zscore){
#       # z scoring our macro news up to 2013-01-01
#       scaling_sample_size <- sum(daily_global_macroRPData$DATE <= backtesting_starting_date)
#       # it assumes here that our data frame is sorted by date with time increasing from bottom to top
#       my_scaled_parameters <-lapply(daily_global_macroRPData[,-1], function(x) {(x - mean(x[1:scaling_sample_size],na.rm=TRUE))/sd(x[1:scaling_sample_size],na.rm=TRUE)} )
#       daily_global_macroRPData[,-1] <- as.data.frame(t(do.call(rbind,my_scaled_parameters)))
#     }
#     ##### Cleaning up after z scoring (dropping empty news columns and zeroing the no news days)
#     # # when no news happened on that day we set our score to zero the neutral score after z scoring
#     daily_global_macroRPData[is.na(daily_global_macroRPData)] <- 0
#     my_columns_to_keep <- as.logical(colSums(daily_global_macroRPData[,-1]) != 0 & colSums(daily_global_macroRPData[,-1]) != Inf)
#     my_columns_to_keep[is.na(my_columns_to_keep)] <- FALSE
#     # we always keep the DATE column
#     daily_global_macroRPData<-daily_global_macroRPData[,c(TRUE,my_columns_to_keep)]
#     
#     if(sum(my_columns_to_keep)){
#       # plotting the macro group sentiment
#       #       daily_global_macroRPData_toplot_df <-
#       #         melt(daily_global_macroRPData,"DATE")
#       #       my_title <-
#       #         "Daily average macro news at the group level"
#       #       g<-ggplot(
#       #         daily_global_macroRPData_toplot_df,aes(
#       #           x = DATE,y = value,group = variable,color = variable
#       #         )
#       #       ) +
#       #         geom_line() +
#       #         scale_x_date() +
#       #         ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
#       #         theme(title = element_text(size = 12, face = 'bold')) +
#       #         theme(legend.position = c(0.2,0.2), legend.box = "vertical") +
#       #         theme(legend.background = element_rect(fill = "gray90")) +
#       #         theme(legend.key.size = unit(0.7, "cm"))
#       #       print(g)
#       
#       daily_global_macroRPData <- daily_global_macroRPData[order(daily_global_macroRPData$DATE),]
#       colnames(daily_global_macroRPData) <- c("DATE",paste(my_country,"_",colnames(daily_global_macroRPData[,-1]),sep=""))
#       if (is.null(my_total_group_df)){
#         my_total_group_df <- daily_global_macroRPData
#       } else {
#         # we keep all dates even if one has no sentiment
#         my_total_group_df <- merge(my_total_group_df,daily_global_macroRPData,by="DATE",all=T)
#       }
#       # make sure it is ordered to build our momentum signals
#       my_total_group_df <- my_total_group_df[order(my_total_group_df$DATE),]
#     }
#   }
#   
#   # we here just make sure that we have each day
#   all_days_df <- data.frame(DATE=seq(as.Date(backtesting_starting_date), as.Date(backtesting_ending_date), by="days"))
#   my_total_group_df <- merge(my_total_group_df,all_days_df,by="DATE", all=T)
#   
#   my_predicting_columns <- colnames(my_total_group_df[,-1])
#   # saving the per group daily macro/corporate aggregated sentiment data 
#   my_official_countries = c(first_country, second_country)
#   for (my_bond_future_country in my_official_countries){
#     filename <- my_bondfutures_flatfile[my_bond_future_country]
#     if (!is.null(filename)|(filename == "NULL")){
#       US_TN_Rolled_Futures <- readRDS(paste(outputDataPath,filename, sep = ""))
#       US_TN_Rolled_Futures <- US_TN_Rolled_Futures[US_TN_Rolled_Futures$Date >= "2000-01-01" & US_TN_Rolled_Futures$Date <= "2015-10-01",]
#       # colnames(US_TN_Rolled_Futures)
#       US_TN_Rolled_Futures$NextOpen <- lagpad(US_TN_Rolled_Futures$Open,1)
#       US_TN_Rolled_Futures$NextSettle <- lagpad(US_TN_Rolled_Futures$Settle,1)
#       US_TN_Rolled_Futures$LastSettle <- c(tail(US_TN_Rolled_Futures$Settle,-1),NA)
#       ## Open returns
#       US_TN_Rolled_Futures$NexOpen <- lagpad(US_TN_Rolled_Futures$Open,1)
#       US_TN_Rolled_Futures$OpenReturn <- log(US_TN_Rolled_Futures$NextOpen/US_TN_Rolled_Futures$Open)
#       US_TN_Rolled_Futures$NextOpenReturn <- lagpad(US_TN_Rolled_Futures$OpenReturn,1)
#       US_TN_Rolled_Futures$NextNextOpenReturn <- lagpad(US_TN_Rolled_Futures$NextOpenReturn,1)
#       US_TN_Rolled_Futures$LastOpenReturn <- c(tail(US_TN_Rolled_Futures$OpenReturn,-1),NA)
#       # Close to close returns
#       # Overnight returns
#       US_TN_Rolled_Futures$Return <- log(US_TN_Rolled_Futures$Settle/US_TN_Rolled_Futures$LastSettle)
#       US_TN_Rolled_Futures$ReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$Return >= 0)
#       
#       US_TN_Rolled_Futures$OvernightReturn <- log(US_TN_Rolled_Futures$Open/US_TN_Rolled_Futures$LastSettle)
#       US_TN_Rolled_Futures$NextReturn <- log(US_TN_Rolled_Futures$NextSettle/US_TN_Rolled_Futures$Settle)
#       
#       US_TN_Rolled_Futures$OvernightReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$OvernightReturn >= 0)
#       
#       US_TN_Rolled_Futures$LastOvernightReturn <- c(tail(US_TN_Rolled_Futures$OvernightReturn,-1),NA)
#       
#       US_TN_Rolled_Futures$NextOvernightReturn <- lagpad(US_TN_Rolled_Futures$OvernightReturn,1)
#       
#       
#       US_TN_Rolled_Futures$LastNextReturn <- c(tail(US_TN_Rolled_Futures$NextReturn,-1),NA)
#       
#       US_TN_Rolled_Futures$NextNextReturn <- lagpad(US_TN_Rolled_Futures$NextReturn,1)
#       
#       US_TN_Rolled_Futures$NextReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$NextReturn >= 0)
#       
#       # Volume
#       US_TN_Rolled_Futures$LastVolume <- c(tail(US_TN_Rolled_Futures$Volume,-1),NA)
#       
#       # we leave every dates with no trading
#       US_TN_Rolled_Futures <- US_TN_Rolled_Futures[complete.cases(US_TN_Rolled_Futures),]
#       # Ordering our dataset : a must do as we use EMA function where past is before in the vector
#       US_TN_Rolled_Futures <- US_TN_Rolled_Futures[order(US_TN_Rolled_Futures$Date),]
#       
#       # Computing momentum indicator here 
#       # this indicator are computed up to the last day close price or even today s open
#       
#       # Building a reversal momentum indicator optimized till 2013-01-01
#       n1<- 1
#       n2<- 2
#       
#       # we here take the lagged the momentum as we enter the trade from settle to settle and cannot know the current settle data
#       
#       US_TN_Rolled_Futures$Lastleading <- EMA(US_TN_Rolled_Futures$LastSettle, n1)
#       US_TN_Rolled_Futures$Lastlagging <- EMA(US_TN_Rolled_Futures$LastSettle, n2)
#       US_TN_Rolled_Futures$LastMomentum <- 2*((US_TN_Rolled_Futures$Lastleading -US_TN_Rolled_Futures$Lastlagging)>=0)-1
#       US_TN_Rolled_Futures$LastMomentum[1:n2] <- 0 
#       
#       US_TN_Rolled_Futures$Openleading <- EMA(US_TN_Rolled_Futures$Open, n1)
#       US_TN_Rolled_Futures$Openlagging <- EMA(US_TN_Rolled_Futures$Open, n2)
#       US_TN_Rolled_Futures$OpenMomentum <- 2*((US_TN_Rolled_Futures$Openleading -US_TN_Rolled_Futures$Openlagging)>=0)-1
#       US_TN_Rolled_Futures$OpenMomentum[1:n2] <- 0 
#       
#       US_TN_Rolled_Futures$leading <- EMA(US_TN_Rolled_Futures$Settle, n1)
#       US_TN_Rolled_Futures$lagging <- EMA(US_TN_Rolled_Futures$Settle, n2)
#       US_TN_Rolled_Futures$Momentum <- 2*((US_TN_Rolled_Futures$leading -US_TN_Rolled_Futures$lagging)>=0)-1
#       US_TN_Rolled_Futures$Momentum[1:n2] <- 0 
#       
#       # yesterday momentum indicators
#       US_TN_Rolled_Futures$LastRSI3 <- RSI(US_TN_Rolled_Futures$LastSettle, 3)
#       US_TN_Rolled_Futures$LastDEMA <- DEMA(US_TN_Rolled_Futures$LastSettle, 10)
#       US_TN_Rolled_Futures$LastDirection <- US_TN_Rolled_Futures$LastSettle - US_TN_Rolled_Futures$LastOpen
#       
#       US_TN_Rolled_Futures$OpenRSI3 <- RSI(US_TN_Rolled_Futures$Open, 3)
#       US_TN_Rolled_Futures$OpenDEMA <- DEMA(US_TN_Rolled_Futures$Open, 10)
#       bbOpen <- BBands(US_TN_Rolled_Futures$Open, sd=2.0)
#       US_TN_Rolled_Futures$OpenBB20 <- bbOpen[,"pctB"]
#       
#       # today momentum indicators
#       US_TN_Rolled_Futures$RSI3<- RSI(US_TN_Rolled_Futures$Settle, 3)
#       US_TN_Rolled_Futures$Trend <- US_TN_Rolled_Futures$Open - SMA(US_TN_Rolled_Futures$Open, 50)
#       US_TN_Rolled_Futures$DEMA <- DEMA(US_TN_Rolled_Futures$Settle, 10)
#       US_TN_Rolled_Futures$Direction <-  US_TN_Rolled_Futures$Settle - US_TN_Rolled_Futures$Open
#       bb <- BBands(US_TN_Rolled_Futures$Settle, sd=2.0)
#       US_TN_Rolled_Futures$BB20 <- bb[,"pctB"]
#       
#       ##### Building a trading signal
#       
#       colnames(US_TN_Rolled_Futures) <- paste(my_bond_future_country, colnames(US_TN_Rolled_Futures), sep="")
#       colnames(US_TN_Rolled_Futures)[1] <- "DATE"
#       
#       # we keep every date (because we alreay removed all the trading dates)
#       my_total_group_df <- merge(my_total_group_df,US_TN_Rolled_Futures,by="DATE")
#     }
#   }
#   
#   my_total_group_df[is.na(my_total_group_df)] <- 0
#   
#   # adding price momentum signals to improve our prediction
#   # first country is wester : 
#   
#   my_predicting_columns <- c(my_predicting_columns,paste(my_official_countries, "Trend", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(my_official_countries, "OvernightReturn", sep=""))
#   # we can go up to the close of the very day for the second country as we trade only at the next day open
#   my_predicting_columns <- c(my_predicting_columns,paste(first_country, "LastMomentum", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(first_country, "OpenMomentum", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(second_country, "Momentum", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(first_country, "LastVolume", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(second_country, "Volume", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(first_country, "LastRSI3", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(first_country, "OpenRSI3", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(second_country, "RSI3", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,  paste(first_country, "LastDEMA", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,  paste(first_country, "OpenDEMA", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(second_country, "DEMA", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,  paste(first_country, "OpenBB20", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(second_country, "BB20", sep=""))
#   my_predicting_columns <- c(my_predicting_columns,paste(first_country, "LastDirection", sep=""))  
#   my_predicting_columns <- c(my_predicting_columns,paste(second_country, "Direction", sep=""))
#   
#   
#   ### Backtesting our model with an expanding window and predicting out of sample next overnight returns each day according to sentiment up to the close
#   start_date <- backtesting_starting_date
#   end_date <-  backtesting_ending_date
#   
#   backtesting_dates_logical_index <-
#     (my_total_group_df$DATE >= start_date &
#        my_total_group_df$DATE <= end_date)
#   
#   backtesting_dates_index <-
#     which(backtesting_dates_logical_index == TRUE)
#   
#   nb_iteration <- sum(backtesting_dates_logical_index)
#   
#   backtesting_predictions_first <-
#     matrix(0,nrow = nb_iteration-1, ncol = 1)
#   backtesting_predictions_second <-
#     matrix(0,nrow = nb_iteration-1, ncol = 1)
#   backtesting_predictions_third <-
#     matrix(0,nrow = nb_iteration-1, ncol = 1)
#   backtesting_index <-
#     matrix(0,nrow = nb_iteration-1, ncol = 1)
#   
#   
#   for (i in 1:(nb_iteration-1)) {
#     print("Predicting iteration number ")
#     print(i)
#     print("Over")
#     print(nb_iteration)
#     # we use all the past available data to train our xgboost model
#     
#     output_one_column <-  paste(first_country, "NextReturn", sep="")
#     output_two_column <- paste(second_country, "NextOpenReturn", sep="")
#     my_model_columns <- c(my_predicting_columns, output_one_column, output_two_column)
#     my_iteration_data <- my_total_group_df[1:backtesting_dates_index[i],my_model_columns]
#     
#     
#     print("Using the past data from")
#     print(my_total_group_df$DATE[1])
#     print("up to ")
#     print(my_total_group_df$DATE[backtesting_dates_index[i]])
#     # we predict the next date
#     prediction_index <- backtesting_dates_index[i]+1
#     my_prediction_date <- my_total_group_df$DATE[prediction_index]
#     print("Predicting for the next day : ")
#     print(my_prediction_date)
#     
#     
#     model_to_predict <- my_total_group_df[prediction_index,]
#     
#     
#     
#     if(algorithm_used == "xgboost"){
#       my_iteration_results <- predict_next_day_bidirectional_spread_xgboost(my_predicting_columns , output_one_column, output_two_column, my_iteration_data, model_to_predict )
#     }
#     
#     if(algorithm_used == "rpart"){
#       my_iteration_results <- predict_next_day_bidirectional_spread_rpart(my_predicting_columns , output_one_column, output_two_column, my_iteration_data, model_to_predict )
#     }
#     
#     
#     if(algorithm_used == "rpart_unpruned"){
#       my_iteration_results <- predict_next_day_bidirectional_spread_rpart_unpruned(my_predicting_columns , output_one_column, output_two_column, my_iteration_data, model_to_predict )
#     }
#     
#     
#     # backtesting_predictions[i,] <- my_prediction_first
#     backtesting_predictions_first[i,] <- my_iteration_results$first
#     backtesting_predictions_second[i,] <- my_iteration_results$second
#     backtesting_predictions_third[i,] <- my_iteration_results$spread
#     
#     backtesting_index[i,] <- prediction_index
#   }
#   
#   my_total_group_df$NextDaySpread_RegTree_first <- NA
#   my_total_group_df$NextDayWeigths_first <- NA
#   my_total_group_df$NextDaySpread_RegTree_second <- NA
#   my_total_group_df$NextDayWeigths_second <- NA
#   my_total_group_df$NextDaySpread_RegTree_third <- NA
#   my_total_group_df$NextDayWeigths_third <- NA
#   
#   
#   
#   weight_one <- backtesting_predictions_first/(backtesting_predictions_first + backtesting_predictions_second)
#   weight_two <- backtesting_predictions_second/(backtesting_predictions_first + backtesting_predictions_second)
#   
#   my_total_group_df$NextDaySpread_RegTree_first[backtesting_index] <- backtesting_predictions_first
#   my_total_group_df$NextDaySpread_RegTree_second[backtesting_index] <- backtesting_predictions_second
#   
#   
#   for (l in 1:length(backtesting_predictions_first)){
#     if ((backtesting_predictions_first[l] <0)&&(backtesting_predictions_second[l]<0)){
#       # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#       # my_total_group_df$NextDayWeigths_first[backtesting_index]
#       weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#       # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#       weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#       
#     } 
#     
#     #     if ((backtesting_predictions_first[l] >0)&&(backtesting_predictions_second[l] >0)){
#     #       # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
#     #       my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
#     #       my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
#     #     } 
#     #     
#     ## this is the case where we must set a limit on the traded quantities
#     ## as they can become huge and still summing to one because of their sign difference
#     if ((backtesting_predictions_first[l] <0)&&(backtesting_predictions_second[l] >0)){
#       if (abs(backtesting_predictions_first[l])<backtesting_predictions_second[l]){
#         # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
#         #         my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
#         #         my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
#         # we here limit the positive weight : the second one
#         if (abs(weight_two[l]) > spread_amplification_factor){
#           # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#           weight_two[l] <- spread_amplification_factor
#           # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#           weight_one[l] <- 1 - weight_two[l]# my_total_group_df$NextDayWeigths_second[backtesting_index]
#         }
#       } else {
#         # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#         # my_total_group_df$NextDayWeigths_first[backtesting_index]
#         weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#         # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#         weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#         # we here limit the positive weight : the second one
#         if (abs(weight_two[l]) > spread_amplification_factor){
#           # my_total_group_df$NextDayWeigths_second[backtesting_index]
#           weight_two[l]<- spread_amplification_factor
#           # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#           weight_one[l] <- 1 - weight_two[l] # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#         }
#       }
#     } 
#     ## this is the case where we must set a limit on the traded quantities
#     ## as they can become huge and still summing to one because of their sign difference
#     if ((backtesting_predictions_first[l] >0)&&(backtesting_predictions_second[l] <0)){
#       if (abs(backtesting_predictions_second[l])<backtesting_predictions_first[l]){
#         # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
#         #         my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
#         #         my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
#         # we here limit the positive weight : the first one
#         if (abs(weight_one[l]) > spread_amplification_factor){
#           # my_total_group_df$NextDayWeigths_first[backtesting_index]
#           weight_one[l] <- spread_amplification_factor
#           # my_total_group_df$NextDayWeigths_second[backtesting_index]
#           weight_two[l] <- 1 - weight_one[l] #my_total_group_df$NextDayWeigths_first[backtesting_index]
#         }
#       } else {
#         # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#         #         my_total_group_df$NextDayWeigths_first[backtesting_index] <- -weight_one
#         #         my_total_group_df$NextDayWeigths_second[backtesting_index] <- -weight_two
#         # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#         # my_total_group_df$NextDayWeigths_first[backtesting_index]
#         weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#         # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#         weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#         # we here limit the positive weight : the first one
#         if (abs(weight_one[l]) > spread_amplification_factor){
#           # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#           weight_one[l] <- spread_amplification_factor
#           # my_total_group_df$NextDayWeigths_second[backtesting_index]
#           weight_two[l] <- 1 - weight_one[l] #my_total_group_df$NextDayWeigths_first[backtesting_index]
#         }
#       }
#     } 
#   }
#   
#   my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
#   my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
#   
#   
#   my_total_group_df$NextDaySpread_RegTree_third[backtesting_index] <- backtesting_predictions_third
#   my_total_group_df$NextDayWeigths_third[backtesting_index] <- backtesting_predictions_third*spread_amplification_factor
#   
#   ## we can Short/long the very liquid futures
#   #   technical_signal_first <- 2*as.numeric(my_total_group_df$NextDaySpread_RegTree_first[backtesting_index] > 0)-1
#   #   technical_signal_second <- 2*as.numeric(my_total_group_df$NextDaySpread_RegTree_second[backtesting_index] > 0)-1
#   #   #   coincidence <- (technical_signal_first == technical_signal_second)
#   #   #   technical_signal_first[coincidence] <- 0.5 * technical_signal_first[coincidence]
#   #   #   technical_signal_second[coincidence] <- 0.5 * technical_signal_second[coincidence]
#   #   technical_signal_first <- technical_signal_first * my_total_group_df$NextDayWeigths_first[backtesting_index] 
#   #   technical_signal_first <- technical_signal_second * my_total_group_df$NextDayWeigths_second[backtesting_index] 
#   technical_signal_first <-  my_total_group_df$NextDayWeigths_first[backtesting_index] 
#   technical_signal_second <-  my_total_group_df$NextDayWeigths_second[backtesting_index] 
#   
#   
#   
#   technical_signal_third <- my_total_group_df$NextDayWeigths_third[backtesting_index] 
#   
#   #   modulation
#   #   technical_signal_third <- c(tail(technical_signal_third,-1),NA)-technical_signal_third
#   #   
#   #   technical_signal_first <-  -c(tail(technical_signal_first,-1),NA)+technical_signal_first
#   #   technical_signal_second <-  -c(tail(technical_signal_second,-1),NA)+technical_signal_second
#   
#   # if positive we go long otherwise we short
#   # first country has always to be the US  
#   
#   first_leg_traded <- paste(first_country,"NextReturn",sep="")
#   first_leg_lasttraded <- paste(first_country,"Return",sep="")
#   first_leg_nexttraded <- paste(first_country,"NextNextReturn",sep="")
#   second_leg_traded <- paste(first_country,"",sep="NextOpenReturn")
#   second_leg_lasttraded <- paste(first_country,"OpenReturn",sep="")
#   second_leg_nexttraded <- paste(first_country,"NextNextOpenReturn",sep="")
#   
#   second_leg <- paste(second_country,"NextOpenReturn",sep="")
#   
#   traded_return <- my_total_group_df[backtesting_index,first_leg_traded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_traded] * technical_signal_second
#   traded_return_last <- my_total_group_df[backtesting_index,first_leg_lasttraded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_lasttraded] * technical_signal_second
#   traded_return_next <- my_total_group_df[backtesting_index,first_leg_nexttraded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_nexttraded] * technical_signal_second
#   
#   traded_return_spread <- my_total_group_df[backtesting_index,first_leg_traded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_traded] * technical_signal_third
#   traded_return_spread_last <- my_total_group_df[backtesting_index,first_leg_lasttraded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_lasttraded] * technical_signal_third
#   traded_return_spread_next <- my_total_group_df[backtesting_index,first_leg_nexttraded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_nexttraded] * technical_signal_third
#   
#   ############
#   # we make the output generic
#   first_bond_returns <- paste(first_country,"Return",sep="")
#   second_bond_returns <- paste(second_country,"Return",sep="")
#   to_return <- list()
#   to_return$results <- data.frame(DATES = my_total_group_df$DATE[backtesting_index], 
#                                   STRATEGY_RETURN = traded_return, STRATEGY_YESTERDAY = CumFromRetToPricesStart(traded_return_last), STRATEGY_TODAY = CumFromRetToPricesStart(traded_return),STRATEGY_TOMORROW = CumFromRetToPricesStart(traded_return_next), 
#                                   SPREAD_STRATEGY_RETURN = traded_return_spread, SPREAD_STRATEGY_YESTERDAY = CumFromRetToPricesStart(traded_return_spread_last), SPREAD_STRATEGY_TODAY = CumFromRetToPricesStart(traded_return_spread), SPREAD_STRATEGY_TOMORROW = CumFromRetToPricesStart(traded_return_spread_next), 
#                                   SECOND_BOND =   CumFromRetToPricesStart(my_total_group_df[backtesting_index,second_bond_returns]), FIRST_BOND =   CumFromRetToPricesStart(my_total_group_df[backtesting_index,first_bond_returns]),
#                                   FIRST_WEIGHT = my_total_group_df$NextDayWeigths_first[backtesting_index] ,  SECOND_WEIGHT = my_total_group_df$NextDayWeigths_second[backtesting_index]  )
#   to_return$sentiments <-my_total_group_df[backtesting_index,]
#   return(to_return)  
# }
