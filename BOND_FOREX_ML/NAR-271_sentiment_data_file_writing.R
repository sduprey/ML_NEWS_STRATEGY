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
source("./RCode/RP_Dates_Utils.R")
source("./RCode/RP_Df_Utils.R")
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

my_countries <- c("US","GB","DE","JP","CA","EU")
whole_global_macroRPData <- readRDS(paste(inputDataPath,"rp_global_macro_data_dj_full_advanced.rds",sep=""))
sophisticated_average_daily_event_RPData <- readRDS(paste(inputDataPath, "european_country_code_list.rds",sep=""))

filter_economy <- TRUE
depth <- 4
us_news_cutting_off_hour <- 16
eu_news_cutting_off_hour <- 17
jp_news_cutting_off_hour <- 15

include_rp_ids <- TRUE



# for macro data sentiment aggregating dataset
my_total_group_df <- NULL
for (my_country in my_countries){
  print(paste0("Aggregating sentiment data for ",my_country))
  # we filter by country
  global_macroRPData <- whole_global_macroRPData[whole_global_macroRPData$country_code==my_country,]
  ### filtering to only get economic topic
  if (filter_economy){
    # we keep only the economy topic
    print("Filtering economy topic only")
    global_macroRPData <- global_macroRPData[global_macroRPData$topic == "economy",]
    # we keep only the central bank of our countries
    #       central_bank_index <- (global_macroRPData$rp_entity_id == "F38FC4") | ( global_macroRPData$rp_entity_id =="3A9CFF") |( global_macroRPData$rp_entity_id =="BF7AA0") | (global_macroRPData$rp_entity_id =="9742B6" | (global_macroRPData$rp_entity_id =="0425F5"))
    #       global_macroRPData <- global_macroRPData[central_bank_index,]
  }
  
  # we cut to NY close to make our decision we always trade first in the US and the day after at the open
  
  #   if (my_country == "US"| my_country %in% sophisticated_average_daily_event_RPData){
  #     global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "America/New_York")
  #     global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
  #     my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= us_news_cutting_off_hour
  #     # news that come after the close of the US market are used for the day after
  #     global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
  #   }
  ## aggregating the news per day for macro
  if (my_country == "US" | my_country == "CA"){
    global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "America/New_York")
    global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
    my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= us_news_cutting_off_hour
    global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
  }
  if (my_country == "EU" | my_country == "GB" | my_country == "DE"| my_country == "FR"){
    global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "Europe/London")
    global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
    my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= eu_news_cutting_off_hour
    global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
  }
  if (my_country == "JP"){
    global_macroRPData$localmarkettime <- as.POSIXlt(global_macroRPData$timestamp_utc, "Asia/Tokyo")
    global_macroRPData$DATE <- as.Date(global_macroRPData$localmarkettime)
    my_macro_market_close_filtering_index <- hour(global_macroRPData$localmarkettime) >= jp_news_cutting_off_hour
    global_macroRPData$DATE[my_macro_market_close_filtering_index] <- global_macroRPData$DATE[my_macro_market_close_filtering_index]+1
  }
  
  ## looping over topics for macro
  global_macroRPData$topic <- as.factor(global_macroRPData$topic)
  
  ## looping over groups for macro
  global_macroRPData$group <- as.factor(global_macroRPData$group)
  
  # ## data preprocessing : building a mapping group to topics
  unique_macro_groups <- levels(global_macroRPData$group)
  my_macro_topics <- as.vector(global_macroRPData[ match(unique_macro_groups, global_macroRPData[,'group']), 'topic'])
  
  # we don t do that here because we only limit ourself to Dow Jones : no change of regime
  # ### Filtering before RP news change of regime
  # global_macroRPData <- global_macroRPData[global_macroRPData$DATE >= "2007-01-01",]
  # global_corporateRPData <- global_corporateRPData[global_corporateRPData$DATE >= "2007-01-01",]
  
  # we are going down to the Ravenpack sub type level
  if (filter_economy){
    if (depth == 2){
      global_macroRPData$macro_key <- global_macroRPData$group
      global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
    }
    if (depth == 3){
      global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$group),gsub("-", "_", global_macroRPData$type),sep="_")
      global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
    }
    if (depth == 4){
      global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$group),gsub("-", "_", global_macroRPData$type),gsub("-", "_", global_macroRPData$sub_type),sep="_")
      global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
    }
  } else {
    if (depth == 2){
      global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),sep="_")
      global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
    }
    if (depth == 3){
      global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),gsub("-", "_", global_macroRPData$type),sep="_")
      global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
    }
    if (depth == 4){
      global_macroRPData$macro_key <- paste(gsub("-", "_", global_macroRPData$topic),gsub("-", "_", global_macroRPData$group),gsub("-", "_", global_macroRPData$type),gsub("-", "_", global_macroRPData$sub_type),sep="_")
      global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
    } 
  }
  
  
  
  
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
  
  
  # aggregating per day our macro news
  print("Aggregating Ravenpack taxonomy up to the sub type level on a daily basis")
  # global_macroRPData <- global_macroRPData[,c("rp_entity_id","country_code","DATE","macro_key","ess","ens","ens_similarity_gap","news_type")]
  # for the moment I just drop the news type as predictor : to be continued
  
  if (include_rp_ids){
    my_relevant_columns <- c("DATE","macro_key","rp_entity_id","ess","ens","ens_similarity_gap", "aes", "aev")#,"type","sub_type","news_type")
    my_to_average_columns <- c("ess","ens","ens_similarity_gap", "aes", "aev")#,"type","news_type")
    global_macroRPData <- global_macroRPData[,my_relevant_columns]
    # global_macroRPData$rp_entity_id <- as.factor(global_macroRPData$rp_entity_id)
    # global_macroRPData$country_code <- as.factor(global_macroRPData$country_code)
    global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
    daily_global_macroRPData <- aggregate(global_macroRPData[,my_to_average_columns], by=list(global_macroRPData$DATE,global_macroRPData$macro_key,global_macroRPData$rp_entity_id),FUN=function(x){ mean(x,na.rm=TRUE) })
    colnames(daily_global_macroRPData) <- my_relevant_columns
    daily_global_macroRPData <- daily_global_macroRPData[order(daily_global_macroRPData$DATE),]
    #       print("Aggregating by weighted ESS")
    #       daily_global_macroRPData$weighted_ess <- daily_global_macroRPData$ess*daily_global_macroRPData$ens_similarity_gap
    #       daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key+rp_entity_id",value.var =  "weighted_ess")
    print("Aggregating by ESS")
    ess_daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "ess",function(x){ mean(x,na.rm=TRUE) })
    colnames(ess_daily_global_macroRPData) <- c("DATE",paste0("ess_",colnames(ess_daily_global_macroRPData[,-1])))
    print("Aggregating by ENS similarity gap")
    ens_similarity_gap_daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "ens_similarity_gap",function(x){ mean(x,na.rm=TRUE) })
    colnames(ens_similarity_gap_daily_global_macroRPData) <- paste0("esg_",colnames(ens_similarity_gap_daily_global_macroRPData))
    print("Aggregating by AES")
    aes_daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "aes",function(x){ mean(x,na.rm=TRUE) })
    colnames(aes_daily_global_macroRPData) <- paste0("aes_",colnames(aes_daily_global_macroRPData))
    print("Aggregating by AEV")
    aev_daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "aev",function(x){ mean(x,na.rm=TRUE) })
    colnames(aev_daily_global_macroRPData) <- paste0("aev_",colnames(aev_daily_global_macroRPData))
    
    print("Merging all dataset")
    
    daily_global_macroRPData <- cbind(ess_daily_global_macroRPData,ens_similarity_gap_daily_global_macroRPData[,-1],aes_daily_global_macroRPData[,-1],aev_daily_global_macroRPData[,-1])
    
  } else {
    my_relevant_columns <- c("DATE","macro_key","ess","ens","ens_similarity_gap", "aes", "aev")#,"type","sub_type","news_type")
    my_to_average_columns <- c("ess","ens","ens_similarity_gap", "aes", "aev")#,"type","news_type")
    global_macroRPData <- global_macroRPData[,my_relevant_columns]
    # global_macroRPData$rp_entity_id <- as.factor(global_macroRPData$rp_entity_id)
    # global_macroRPData$country_code <- as.factor(global_macroRPData$country_code)
    global_macroRPData$macro_key <- as.factor(global_macroRPData$macro_key)
    daily_global_macroRPData <- aggregate(global_macroRPData[,my_to_average_columns], by=list(global_macroRPData$DATE,global_macroRPData$macro_key),FUN=function(x){ mean(x,na.rm=TRUE) })
    colnames(daily_global_macroRPData) <- my_relevant_columns
    daily_global_macroRPData <- daily_global_macroRPData[order(daily_global_macroRPData$DATE),]
    #       print("Aggregating by weighted ESS")
    #       daily_global_macroRPData$weighted_ess <- daily_global_macroRPData$ess*daily_global_macroRPData$ens_similarity_gap
    #       daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "weighted_ess")
    
    print("Aggregating by ESS")
    ess_daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "ess",function(x){ mean(x,na.rm=TRUE) })
    colnames(ess_daily_global_macroRPData) <- c("DATE",paste0("ess_",colnames(ess_daily_global_macroRPData[,-1])))
    print("Aggregating by ENS similarity gap")
    ens_similarity_gap_daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "ens_similarity_gap",function(x){ mean(x,na.rm=TRUE) })
    colnames(ens_similarity_gap_daily_global_macroRPData) <- paste0("esg_",colnames(ens_similarity_gap_daily_global_macroRPData))
    print("Aggregating by AES")
    aes_daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "aes",function(x){ mean(x,na.rm=TRUE) })
    colnames(aes_daily_global_macroRPData) <- paste0("aes_",colnames(aes_daily_global_macroRPData))
    print("Aggregating by AEV")
    aev_daily_global_macroRPData<-dcast(daily_global_macroRPData, "DATE ~ macro_key",value.var =  "aev",function(x){ mean(x,na.rm=TRUE) })
    colnames(aev_daily_global_macroRPData) <- paste0("aev_",colnames(aev_daily_global_macroRPData))
    
    print("Merging all dataset")
    
    daily_global_macroRPData <- cbind(ess_daily_global_macroRPData,ens_similarity_gap_daily_global_macroRPData[,-1],aes_daily_global_macroRPData[,-1],aev_daily_global_macroRPData[,-1])
  }
  
  
  
  
  ##### Cleaning up NA value to afterwards compute other metrics
  # # when no news happened on that day we set our score to zero the neutral score after z scoring
  daily_global_macroRPData[is.na(daily_global_macroRPData)] <- 0
  daily_global_macroRPData[is.nan.data.frame(daily_global_macroRPData)] <- 0
  daily_global_macroRPData[is.infinite.data.frame(daily_global_macroRPData)] <- 0
  
  ##### Keeping only the columns where we have values
  my_columns_to_keep <- as.logical(colSums(daily_global_macroRPData[,-1]) != 0)
  #     my_columns_to_keep <- as.logical(colSums(daily_global_macroRPData[,-1]) != 0 & colSums(daily_global_macroRPData[,-1]) != Inf)
  #     my_columns_to_keep[is.na(my_columns_to_keep)] <- FALSE
  # we always keep the DATE column
  daily_global_macroRPData<-daily_global_macroRPData[,c(TRUE,my_columns_to_keep)]
  
  if(sum(my_columns_to_keep)){
    # plotting the macro group sentiment
    #       daily_global_macroRPData_toplot_df <-
    #         melt(daily_global_macroRPData,"DATE")
    #       my_title <-
    #         "Daily average macro news at the group level"
    #       g<-ggplot(
    #         daily_global_macroRPData_toplot_df,aes(
    #           x = DATE,y = value,group = variable,color = variable
    #         )
    #       ) +
    #         geom_line() +
    #         scale_x_date() +
    #         ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
    #         theme(title = element_text(size = 12, face = 'bold')) +
    #         theme(legend.position = c(0.2,0.2), legend.box = "vertical") +
    #         theme(legend.background = element_rect(fill = "gray90")) +
    #         theme(legend.key.size = unit(0.7, "cm"))
    #       print(g)
    
    daily_global_macroRPData <- daily_global_macroRPData[order(daily_global_macroRPData$DATE),]
    colnames(daily_global_macroRPData) <- c("DATE",paste(my_country,"_",colnames(daily_global_macroRPData[,-1]),sep=""))
    if (is.null(my_total_group_df)){
      my_total_group_df <- daily_global_macroRPData
    } else {
      # we keep all dates even if one has no sentiment
      my_total_group_df <- merge(my_total_group_df,daily_global_macroRPData,by="DATE",all=T)
    }
    # make sure it is ordered to build our momentum signals
    my_total_group_df <- my_total_group_df[order(my_total_group_df$DATE),]
  }
}
# we keep as much data as we can
backtesting_starting_date <- min(my_total_group_df$DATE)

backtesting_ending_date <- max(my_total_group_df$DATE)


# we here just make sure that we have each day
all_days_df <- data.frame(DATE=seq(as.Date(backtesting_starting_date), as.Date(backtesting_ending_date), by="days"))
my_total_group_df <- merge(my_total_group_df,all_days_df,by="DATE", all=T)
# the only NaNs remaining are when there are no sentiment, we just put that to zero
my_total_group_df[is.na(my_total_group_df)] <- 0


# non numeric columns
non_numeric_columns <- unlist(sapply(my_total_group_df, class) != "numeric")
# getting the columns which contain ess

# we do that because of business
my_ess_columns <- grepl("EU_ess", colnames(my_total_group_df)) |  grepl("US_ess", colnames(my_total_group_df)) | grepl("GB_ess", colnames(my_total_group_df)) |  grepl("DE_ess", colnames(my_total_group_df)) | grepl("JP_ess", colnames(my_total_group_df)) | grepl("CA_ess", colnames(my_total_group_df))
my_esg_columns <- grepl("EU_esg", colnames(my_total_group_df)) |  grepl("US_esg", colnames(my_total_group_df)) | grepl("GB_esg", colnames(my_total_group_df)) |  grepl("DE_esg", colnames(my_total_group_df)) | grepl("JP_esg", colnames(my_total_group_df)) | grepl("CA_esg", colnames(my_total_group_df))
my_ma_columns <- my_ess_columns |  my_esg_columns
print("Computing delayed MA for ESS ENS similarity gap columns")
all_ma_ess_esg_df <- FitMAModelEachColumn(my_total_group_df[,my_ma_columns])

my_total_group_df <- cbind(my_total_group_df,all_ma_ess_esg_df)

SaveDataFrame(my_total_group_df,inputDataPath,"all_daily_aggregated_sentiment_df")
my_total_group_df <- readRDS(paste(inputDataPath,"all_daily_aggregated_sentiment_df.rds", sep = ""))





# to throw up to throw up to throw up 
# # my_ma_total_group_df <- my_total_group_df[,my_ma_columns]
# # test <- FitMAModelEachColumn(my_ma_total_group_df[,1:10])
# 
# 
# # Computing moving averages and rate of change predictors
# ### Roll  apply function to sum up sentiment to the training period
# my_lagged_df <- NULL
# if (investment_horizon != 1){
#   print(" Computing the time period MA for ESS and ENS similarity gap")
#   my_total_group_df[,-1] <-   rollapply(my_total_group_df[,-1],investment_horizon, sum, na.pad = TRUE, na.rm = TRUE)
#   my_basis_columns <- colnames(my_total_group_df[,-1])
#   diff_columns <- paste0(my_basis_columns,"_diff")
#   print("Adding the rate of change")
#   my_lagged_df <- diff(my_total_group_df[,-1])
#   my_lagged_df <- rbind(my_lagged_df[1,],my_lagged_df)
#   my_lagged_df[1,] <- NA
#   colnames(my_lagged_df) <- diff_columns
#   # head(my_lagged_df[,c("DE_economy_domestic_product_gross_domestic_product_E96159")])
# }
# 
# ## Adding sentiment MA predictors
# if (investment_horizon == 21){
#   my_basis_columns <- colnames(my_total_group_df[,-1])
#   print(" Computing the 5 days MA")
#   five_days_columns <- paste0(my_basis_columns,"_5days")
#   five_days_MA <-   rollapply(my_total_group_df[,-1],investment_horizon, sum, na.pad = TRUE, na.rm = TRUE) 
#   colnames(five_days_MA) <- five_days_columns
#   print(" Computing the 10 days MA")
#   ten_days_columns <- paste0(my_basis_columns,"_10days")
#   ten_days_MA <-   rollapply(my_total_group_df[,-1],investment_horizon, sum, na.pad = TRUE, na.rm = TRUE) 
#   colnames(ten_days_MA) <- ten_days_columns
#   print(" Computing the 15 days MA")
#   fifteen_days_columns <- paste0(my_basis_columns,"_15days")
#   fifteen_days_MA <-   rollapply(my_total_group_df[,-1],investment_horizon, sum, na.pad = TRUE, na.rm = TRUE)  
#   colnames(fifteen_days_MA) <- fifteen_days_columns
#   
#   my_total_group_df <- cbind(my_total_group_df, five_days_MA)
#   my_total_group_df <- cbind(my_total_group_df, ten_days_MA)
#   my_total_group_df <- cbind(my_total_group_df, fifteen_days_MA)
# }
# 
# if (!is.null(my_lagged_df)){
#   my_total_group_df <- cbind(my_total_group_df, my_lagged_df)
# }
# 
# # the only NaNs remaining are when there are no sentiment, we just put that to zero
# my_total_group_df[is.na(my_total_group_df)] <- 0
# 
# my_predicting_columns <- colnames(my_total_group_df[,-1])
# my_sentiment_predicting_columns <- my_predicting_columns
# # saving the per group daily macro/corporate aggregated sentiment data 
# my_official_countries = c(first_country, second_country)
# my_total_bond_group_df <- NULL
# for (my_bond_future_country in my_official_countries){
#   filename <- my_bondfutures_flatfile[my_bond_future_country]
#   if (!is.null(filename)|(filename == "NULL")){
#     US_TN_Rolled_Futures <- readRDS(paste(outputDataPath,filename, sep = ""))
#     print(paste0("Having read ",filename))
#     print("Beginning at : ")
#     print(min(US_TN_Rolled_Futures$Date))
#     
#     
#     # US_TN_Rolled_Futures <- US_TN_Rolled_Futures[US_TN_Rolled_Futures$Date >= "2000-01-01" & US_TN_Rolled_Futures$Date <= "2015-10-01",]
#     US_TN_Rolled_Futures <- US_TN_Rolled_Futures[US_TN_Rolled_Futures$Date >= "2000-01-01",]
#     ####### to make those computations we have to make sure that our data frame begins with the last date
#     ####### to make those computations we have to make sure that our data frame begins with the last date
#     US_TN_Rolled_Futures <- US_TN_Rolled_Futures[rev(order(US_TN_Rolled_Futures$Date)),]
#     ####### to make those computations we have to make sure that our data frame begins with the last date
#     ####### to make those computations we have to make sure that our data frame begins with the last date
#     # we here must keep the open of the very next day as we will use to en
#     US_TN_Rolled_Futures$TDayOpen <- lagpad(US_TN_Rolled_Futures$Open,1)
#     # we leave every dates with no trading
#     # US_TN_Rolled_Futures <- US_TN_Rolled_Futures[complete.cases(US_TN_Rolled_Futures),]
#     # Ordering our dataset : a must do as we use EMA function where past is before in the vector
#     # US_TN_Rolled_Futures <- US_TN_Rolled_Futures[order(US_TN_Rolled_Futures$Date),]
#     
#     colnames(US_TN_Rolled_Futures) <- paste(my_bond_future_country, colnames(US_TN_Rolled_Futures), sep="")
#     colnames(US_TN_Rolled_Futures)[1] <- "DATE"
#     
#     if (is.null(my_total_bond_group_df)){
#       my_total_bond_group_df <- US_TN_Rolled_Futures
#     } else {
#       # we keep all dates even if one has no sentiment
#       my_total_bond_group_df <- merge(my_total_bond_group_df,US_TN_Rolled_Futures,by="DATE",all=T)
#     }
#   }
# }
# # we compute the common trading dates
# if (investment_horizon != 1){
#   my_index <- dateroll_compute( my_total_bond_group_df[ , c("DATE", paste(first_country, "Settle", sep=""), paste(second_country, "Settle", sep=""))], investment_horizon)
#   # we now restrain ourselves to the specified trading dates
#   my_total_bond_group_df <- my_total_bond_group_df[my_index,]
# }
# 
# my_total_bond_group_df <- my_total_bond_group_df[rev(order(my_total_bond_group_df$DATE)),]
# ## we now must compute our momentum trading signal according to the trading period
# for (my_bond_future_country in my_official_countries){
#   
#   US_TN_Rolled_Futures <- my_total_bond_group_df[,c(TRUE, grep(my_bond_future_country,colnames(my_total_bond_group_df)))]
#   colnames(US_TN_Rolled_Futures) <- gsub(my_bond_future_country,"",colnames(US_TN_Rolled_Futures))
#   
#   ####### to make those computations we have to make sure that our data frame begins with the last date
#   ####### to make those computations we have to make sure that our data frame begins with the last date
#   US_TN_Rolled_Futures <- US_TN_Rolled_Futures[rev(order(US_TN_Rolled_Futures$DATE)),]
#   ####### to make those computations we have to make sure that our data frame begins with the last date
#   ####### to make those computations we have to make sure that our data frame begins with the last date
#   
#   US_TN_Rolled_Futures$NextSettle <- lagpad(US_TN_Rolled_Futures$Settle,1)
#   US_TN_Rolled_Futures$LastSettle <- c(tail(US_TN_Rolled_Futures$Settle,-1),NA)
#   ## Open returns
#   # this part is the tricky part : if daily we predict the next open to open return
#   # if weekly we predict the TuesDay open to Tuesday open return
#   if (investment_horizon == 1){
#     US_TN_Rolled_Futures$NextOpen <- lagpad(US_TN_Rolled_Futures$Open,1)
#     # today open to next day open when trading daily can not be traded because morning open already passed
#     US_TN_Rolled_Futures$OpenReturn <- log(US_TN_Rolled_Futures$NextOpen/US_TN_Rolled_Futures$Open)
#     US_TN_Rolled_Futures$NextOpenReturn <- lagpad(US_TN_Rolled_Futures$OpenReturn,1)
#     US_TN_Rolled_Futures$NextNextOpenReturn <- lagpad(US_TN_Rolled_Futures$NextOpenReturn,1)
#     US_TN_Rolled_Futures$LastOpenReturn <- c(tail(US_TN_Rolled_Futures$OpenReturn,-1),NA)
#   } else {
#     US_TN_Rolled_Futures$NextTDayOpen <- lagpad(US_TN_Rolled_Futures$TDayOpen,1)
#     US_TN_Rolled_Futures$NextOpenReturn <- log(US_TN_Rolled_Futures$NextTDayOpen/US_TN_Rolled_Futures$TDayOpen)
#     # US_TN_Rolled_Futures$NextOpenReturn <- lagpad(US_TN_Rolled_Futures$OpenReturn,1)
#     US_TN_Rolled_Futures$NextNextOpenReturn <- lagpad(US_TN_Rolled_Futures$NextOpenReturn,1)
#     US_TN_Rolled_Futures$OpenReturn <- c(tail(US_TN_Rolled_Futures$NextOpenReturn,-1),NA)
#     US_TN_Rolled_Futures$LastOpenReturn <- c(tail(US_TN_Rolled_Futures$OpenReturn,-1),NA)
#   }
#   
#   # Close to close returns
#   # Overnight returns
#   US_TN_Rolled_Futures$Return <- log(US_TN_Rolled_Futures$Settle/US_TN_Rolled_Futures$LastSettle)
#   US_TN_Rolled_Futures$ReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$Return >= 0)
#   
#   US_TN_Rolled_Futures$OvernightReturn <- log(US_TN_Rolled_Futures$Open/US_TN_Rolled_Futures$LastSettle)
#   US_TN_Rolled_Futures$NextReturn <- log(US_TN_Rolled_Futures$NextSettle/US_TN_Rolled_Futures$Settle)
#   
#   US_TN_Rolled_Futures$OvernightReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$OvernightReturn >= 0)
#   
#   US_TN_Rolled_Futures$LastOvernightReturn <- c(tail(US_TN_Rolled_Futures$OvernightReturn,-1),NA)
#   
#   US_TN_Rolled_Futures$NextOvernightReturn <- lagpad(US_TN_Rolled_Futures$OvernightReturn,1)
#   
#   
#   US_TN_Rolled_Futures$LastNextReturn <- c(tail(US_TN_Rolled_Futures$NextReturn,-1),NA)
#   
#   US_TN_Rolled_Futures$NextNextReturn <- lagpad(US_TN_Rolled_Futures$NextReturn,1)
#   
#   US_TN_Rolled_Futures$NextReturnBullishOrBearish <- as.numeric(US_TN_Rolled_Futures$NextReturn >= 0)
#   # Volume
#   US_TN_Rolled_Futures$LastVolume <- c(tail(US_TN_Rolled_Futures$Volume,-1),NA)
#   
#   US_TN_Rolled_Futures <- US_TN_Rolled_Futures[complete.cases(US_TN_Rolled_Futures),]
#   # Computing momentum indicator here 
#   # this indicator are computed up to the last day close price or even today s open
#   
#   # Building a reversal momentum indicator optimized till 2013-01-01
#   
#   ### Before making those EMA TTR computations, we have to make sure that our data frame is ordered from the smallest to the greatest date
#   ### Before making those EMA TTR computations, we have to make sure that our data frame is ordered from the smallest to the greatest date
#   ### Before making those EMA TTR computations, we have to make sure that our data frame is ordered from the smallest to the greatest date
#   ### Before making those EMA TTR computations, we have to make sure that our data frame is ordered from the smallest to the greatest date
#   US_TN_Rolled_Futures <- US_TN_Rolled_Futures[order(US_TN_Rolled_Futures$DATE),]
#   ### Before making those EMA TTR computations, we have to make sure that our data frame is ordered from the smallest to the greatest date
#   ### Before making those EMA TTR computations, we have to make sure that our data frame is ordered from the smallest to the greatest date
#   ### Before making those EMA TTR computations, we have to make sure that our data frame is ordered from the smallest to the greatest date
#   ### Before making those EMA TTR computations, we have to make sure that our data frame is ordered from the smallest to the greatest date
#   n1<- 1
#   n2<- 2
#   my_rsi <- 1
#   my_dema <- 10
#   my_trend <- 5
#   my_sd_bollinger <- 1.0
#   my_ema_bollinger <- 10
#   if (investment_horizon == 5){
#     ## parameters for weekly price momentum : would have to be optimized prior to 2013 for the best results
#     my_rsi <- 1
#     my_dema <- 3
#     my_trend <- 1
#     my_sd_bollinger <- 1.8
#     my_ema_bollinger <- 2
#   } else if (investment_horizon == 1) {
#     ## parameters for daily price momentum : would have to be optimized prior to 2013 for the best results
#     my_rsi <- 1
#     my_dema <- 3
#     my_trend <- 1
#     my_sd_bollinger <- 1.8
#     my_ema_bollinger <- 2
#   } else if (investment_horizon == 21) {
#     ## parameters for monthly price momentum : would have to be optimized prior to 2013 for the best results
#     my_rsi <- 2
#     my_dema <- 2
#     my_trend <- 1
#     my_sd_bollinger <- 1.0
#     my_ema_bollinger <- 1
#   }
#   
#   US_TN_Rolled_Futures$Lastleading <- EMA(US_TN_Rolled_Futures$LastSettle, n1)
#   US_TN_Rolled_Futures$Lastlagging <- EMA(US_TN_Rolled_Futures$LastSettle, n2)
#   US_TN_Rolled_Futures$LastMomentum <- 2*((US_TN_Rolled_Futures$Lastleading -US_TN_Rolled_Futures$Lastlagging)>=0)-1
#   US_TN_Rolled_Futures$LastMomentum[1:n2] <- 0 
#   
#   US_TN_Rolled_Futures$Openleading <- EMA(US_TN_Rolled_Futures$Open, n1)
#   US_TN_Rolled_Futures$Openlagging <- EMA(US_TN_Rolled_Futures$Open, n2)
#   US_TN_Rolled_Futures$OpenMomentum <- 2*((US_TN_Rolled_Futures$Openleading -US_TN_Rolled_Futures$Openlagging)>=0)-1
#   US_TN_Rolled_Futures$OpenMomentum[1:n2] <- 0 
#   
#   US_TN_Rolled_Futures$leading <- EMA(US_TN_Rolled_Futures$Settle, n1)
#   US_TN_Rolled_Futures$lagging <- EMA(US_TN_Rolled_Futures$Settle, n2)
#   US_TN_Rolled_Futures$Momentum <- 2*((US_TN_Rolled_Futures$leading -US_TN_Rolled_Futures$lagging)>=0)-1
#   US_TN_Rolled_Futures$Momentum[1:n2] <- 0 
#   
#   # yesterday momentum indicators
#   US_TN_Rolled_Futures$LastRSI3 <- RSI(US_TN_Rolled_Futures$LastSettle, my_rsi)
#   US_TN_Rolled_Futures$LastDEMA <- DEMA(US_TN_Rolled_Futures$LastSettle, my_dema)
#   US_TN_Rolled_Futures$LastDirection <- US_TN_Rolled_Futures$LastSettle - US_TN_Rolled_Futures$LastOpen
#   
#   US_TN_Rolled_Futures$OpenRSI3 <- RSI(US_TN_Rolled_Futures$Open, my_rsi)
#   US_TN_Rolled_Futures$OpenDEMA <- DEMA(US_TN_Rolled_Futures$Open, my_dema)
#   bbOpen <- BBands(US_TN_Rolled_Futures$Open,n=my_ema_bollinger, sd=my_sd_bollinger)
#   US_TN_Rolled_Futures$OpenBB20 <- bbOpen[,"pctB"]
#   
#   # today momentum indicators
#   US_TN_Rolled_Futures$RSI3<- RSI(US_TN_Rolled_Futures$Settle, my_rsi)
#   US_TN_Rolled_Futures$Trend <- US_TN_Rolled_Futures$Open - SMA(US_TN_Rolled_Futures$Open, my_trend)
#   US_TN_Rolled_Futures$DEMA <- DEMA(US_TN_Rolled_Futures$Settle, my_dema)
#   US_TN_Rolled_Futures$Direction <-  US_TN_Rolled_Futures$Settle - US_TN_Rolled_Futures$Open
#   bb <- BBands(US_TN_Rolled_Futures$Settle,n=my_ema_bollinger, sd=my_sd_bollinger)
#   US_TN_Rolled_Futures$BB20 <- bb[,"pctB"]
#   # we keep every date (because we alreay removed all the trading dates)
#   
#   # adding country to keep track in the big data frame
#   colnames(US_TN_Rolled_Futures) <- paste(my_bond_future_country, colnames(US_TN_Rolled_Futures), sep="")
#   colnames(US_TN_Rolled_Futures)[1] <- "DATE"
#   
#   # we here don t keep all : we stick to our time period trading
#   my_total_group_df <- merge(my_total_group_df,US_TN_Rolled_Futures,by="DATE")
# }
# 
# # adding price momentum signals to improve our prediction
# # first country is wester : 
# my_price_predicting_columns <- NULL
# if (investment_horizon == 1){
#   my_price_predicting_columns <- c(paste(my_official_countries, "Trend", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(my_official_countries, "OvernightReturn", sep=""))
#   # we can go up to the close of the very day for the second country as we trade only at the next day open
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "LastMomentum", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "OpenMomentum", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "Momentum", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "LastVolume", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "Volume", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "LastRSI3", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "OpenRSI3", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "RSI3", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,  paste(first_country, "LastDEMA", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,  paste(first_country, "OpenDEMA", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "DEMA", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,  paste(first_country, "OpenBB20", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "BB20", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "LastDirection", sep=""))  
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "Direction", sep=""))
#   
#   my_predicting_columns <- c(my_sentiment_predicting_columns, my_price_predicting_columns)
# } else {
#   # on a weekly or monthly basis we cannot put the week volume or momentum not like on a daily basis
#   my_price_predicting_columns <- c(paste(my_official_countries, "Trend", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(my_official_countries, "OvernightReturn", sep=""))
#   # we can go up to the close of the very day for the second country as we trade only at the next day open
#   # not true for week or month 
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "LastMomentum", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "OpenMomentum", sep=""))
#   
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "LastMomentum", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "OpenMomentum", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "LastVolume", sep=""))
#   
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "LastVolume", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "LastRSI3", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "OpenRSI3", sep=""))
#   
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "LastRSI3", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "OpenRSI3", sep=""))
#   
#   
#   my_price_predicting_columns <- c(my_price_predicting_columns,  paste(first_country, "LastDEMA", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,  paste(first_country, "OpenDEMA", sep=""))
#   
#   my_price_predicting_columns <- c(my_price_predicting_columns,  paste(second_country, "LastDEMA", sep=""))
#   my_price_predicting_columns <- c(my_price_predicting_columns,  paste(second_country, "OpenDEMA", sep=""))
#   
#   #     my_price_predicting_columns <- c(my_price_predicting_columns,  paste(first_country, "OpenBB20", sep=""))
#   #     my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "OpenBB20", sep=""))
#   
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(first_country, "LastDirection", sep=""))  
#   my_price_predicting_columns <- c(my_price_predicting_columns,paste(second_country, "LastDirection", sep=""))
#   
#   my_predicting_columns <- c(my_sentiment_predicting_columns, my_price_predicting_columns)
# }
# 
# if (investment_horizon == 5){
#   nfold <- 10
# } else if (investment_horizon == 21) {
#   nfold <- 10
# }
# 
# ### Adjusting the rolling window according to the investment horizon
# if (investment_horizon == 5){
#   # 52 trading weeks in one year
#   rolling_window <- rolling_window*52
# } else if (investment_horizon == 1) {
#   # 252 trading days in one year
#   rolling_window <- rolling_window*252
# } else if (investment_horizon == 21) {
#   # 12 trading months in one year
#   rolling_window <- rolling_window*12
# }
# 
# ### Backtesting our model with an expanding window and predicting out of sample next overnight returns each day according to sentiment up to the close
# start_date <- backtesting_starting_date
# end_date <-  backtesting_ending_date
# 
# 
# backtesting_dates_logical_index <-
#   (my_total_group_df$DATE >= start_date &
#      my_total_group_df$DATE <= end_date)
# 
# backtesting_dates_index <-
#   which(backtesting_dates_logical_index == TRUE)
# 
# nb_iteration <- sum(backtesting_dates_logical_index)
# 
# backtesting_predictions_first <-
#   matrix(0,nrow = nb_iteration-1, ncol = 1)
# backtesting_predictions_second <-
#   matrix(0,nrow = nb_iteration-1, ncol = 1)
# backtesting_predictions_third <-
#   matrix(0,nrow = nb_iteration-1, ncol = 1)
# backtesting_index <-
#   matrix(0,nrow = nb_iteration-1, ncol = 1)
# 
# my_last_model_first_leg <- NULL
# my_last_model_second_leg <- NULL
# 
# my_calibration_counter <- 0
# nb_tot_calibration <- 1
# list_calibration_parameters <- list()
# my_calibration_parameters <- NULL
# 
# nb_tot_trees <- 1
# list_first_directionality <- list()
# list_second_directionality <- list()
# first_directionality <- NULL
# second_directionality <- NULL
# for (i in 1:(nb_iteration-1)) {
#   print("Predicting iteration number ")
#   print(i)
#   print("Over")
#   print(nb_iteration)
#   # we use all the past available data to train our xgboost model
#   
#   output_one_column <-  paste(first_country, "NextReturn", sep="")
#   output_two_column <- paste(second_country, "NextOpenReturn", sep="")
#   my_model_columns <- c(my_predicting_columns, output_one_column, output_two_column)
#   starting_index <- 1
#   if(rolling_window > 0){
#     starting_index <- max((backtesting_dates_index[i]-rolling_window),1)
#   } 
#   my_iteration_data <- my_total_group_df[starting_index:backtesting_dates_index[i],my_model_columns]
#   
#   if(length(my_model_columns) != dim(my_iteration_data)[2]){
#     print("WarningWarningWarningWarningWarningWarning : Predictors dropped")
#   }
#   
#   print("Using the past data from")
#   print(my_total_group_df$DATE[starting_index])
#   print("up to ")
#   print(my_total_group_df$DATE[backtesting_dates_index[i]])
#   # we predict the next date
#   prediction_index <- backtesting_dates_index[i]+1
#   my_prediction_date <- my_total_group_df$DATE[prediction_index]
#   print("Predicting for the next day : ")
#   print(my_prediction_date)
#   
#   model_to_predict <- my_total_group_df[prediction_index,]
#   
#   # we use the same set of predictors to predict the bidirectionnality of the spread : different methodologies have been tested here
#   if (algorithm_used == "svm" ){
#     first_directionality <- tryCatch(RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_one_column, my_iteration_data, model_to_predict,nfold),
#                                      error = function(e) {NULL})
#     second_directionality <- tryCatch(RP_PredictNextReturn(algorithm_used,my_predicting_columns , output_two_column, my_iteration_data, model_to_predict,nfold),
#                                       error = function(e) {NULL})
#     
#     if (is.null(first_directionality)){
#       # no data for calibration
#       first_directionality <- list()
#       first_directionality$prediction <- 0.
#       first_directionality$model <- NULL
#     } 
#     if (is.null(second_directionality)){
#       # no data for calibration
#       second_directionality <- list()
#       second_directionality$prediction <- 0.
#       second_directionality$model <- NULL
#     } 
#     
#   } else  if (algorithm_used == "xgboost_cv" ){
#     # 0 for the first time we arrive and recalibration_frequency for the next times according to frequency
#     if (my_calibration_counter == recalibration_frequency || my_calibration_counter == 0){
#       my_calibration_parameters <- RP_CalibrateNextReturn(algorithm_used,my_predicting_columns , output_one_column, my_iteration_data, model_to_predict,nfold)
#       # we reset the counter
#       my_calibration_counter <- 1
#       list_calibration_parameters[[nb_tot_calibration]] <-  my_calibration_parameters
#       nb_tot_calibration <- nb_tot_calibration + 1
#     }
#     first_directionality <- RP_AlCaPredictNextReturn(my_calibration_parameters,algorithm_used,my_predicting_columns , output_one_column, my_iteration_data, model_to_predict,nfold)
#     
#     second_directionality <- RP_AlCaPredictNextReturn(my_calibration_parameters,algorithm_used,my_predicting_columns , output_two_column, my_iteration_data, model_to_predict,nfold)
#     # if we just recalibrated our model we store its results
#     
#     list_first_directionality[[(nb_tot_trees)]] <- first_directionality$model
#     list_second_directionality[[(nb_tot_trees)]] <- second_directionality$model
#     nb_tot_trees <- nb_tot_trees+1
#     
#   } else  if (algorithm_used == "xgboost_caret_cv" ){
#     # 0 for the first time we arrive and recalibration_frequency for the next times according to frequency
#     if (my_calibration_counter == recalibration_frequency || my_calibration_counter == 0){
#       my_calibration_parameters <- RP_CaretCalibrateNextReturn(algorithm_used,my_predicting_columns , output_one_column, my_iteration_data, model_to_predict,nfold)
#       # we reset the counter
#       my_calibration_counter <- 1
#       list_calibration_parameters[[nb_tot_calibration]] <-  my_calibration_parameters
#       nb_tot_calibration <- nb_tot_calibration + 1
#     }
#     first_directionality <- RP_AlCaCaretPredictNextReturn(my_calibration_parameters,algorithm_used,my_predicting_columns , output_one_column, my_iteration_data, model_to_predict,nfold)
#     
#     second_directionality <- RP_AlCaCaretPredictNextReturn(my_calibration_parameters,algorithm_used,my_predicting_columns , output_two_column, my_iteration_data, model_to_predict,nfold)
#     # if we just recalibrated our model we store its results
#     
#     list_first_directionality[[(nb_tot_trees)]] <- first_directionality$model
#     list_second_directionality[[(nb_tot_trees)]] <- second_directionality$model
#     nb_tot_trees <- nb_tot_trees+1
#     
#   }
#   
#   backtesting_predictions_first[i,] <- first_directionality$prediction
#   backtesting_predictions_second[i,] <- second_directionality$prediction
#   
#   
#   my_last_model_first_leg <- first_directionality$model
#   my_last_model_second_leg <- second_directionality$model
#   
#   backtesting_index[i,] <- prediction_index
#   
#   my_calibration_counter <- my_calibration_counter+1
#   
#   #     if (i == (nb_iteration-1)){
#   #       print("End of backtesting")
#   #       to_store = list (algo=algorithm_used, predicting_columns=my_predicting_columns ,  output=output_one_column, calib= my_iteration_data, predic = model_to_predict)
#   #       SaveDataFrame(to_store, outputDataPathMonth,"resubstitution_df")
#   #     }
#   
# }
# 
# my_total_group_df$NextDaySpread_RegTree_first <- NA
# my_total_group_df$NextDayWeigths_first <- NA
# my_total_group_df$NextDaySpread_RegTree_second <- NA
# my_total_group_df$NextDayWeigths_second <- NA
# 
# my_total_group_df$NextDayPrediction_first <- NA
# my_total_group_df$NextDayPrediction_second <- NA
# 
# 
# ##### my_total_group_df$NextDaySpread_RegTree_third <- NA
# ##### my_total_group_df$NextDayWeigths_third <- NA
# 
# 
# weight_one <- backtesting_predictions_first/(backtesting_predictions_first + backtesting_predictions_second)
# weight_two <- backtesting_predictions_second/(backtesting_predictions_first + backtesting_predictions_second)
# 
# my_total_group_df$NextDaySpread_RegTree_first[backtesting_index] <- backtesting_predictions_first
# my_total_group_df$NextDaySpread_RegTree_second[backtesting_index] <- backtesting_predictions_second
# 
# for (l in 1:length(backtesting_predictions_first)){
#   if ((backtesting_predictions_first[l] <0)&&(backtesting_predictions_second[l]<0)){
#     # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#     # my_total_group_df$NextDayWeigths_first[backtesting_index]
#     weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#     # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#     weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#   } 
#   ## this is the case where we must set a limit on the traded quantities
#   ## as they can become huge and still summing to one because of their sign difference
#   if ((backtesting_predictions_first[l] <0)&&(backtesting_predictions_second[l] >0)){
#     if (abs(backtesting_predictions_first[l])<backtesting_predictions_second[l]){
#       ## first case : we are more confident in x2
#       # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
#       # we here limit the positive weight : the second one
#       if (abs(weight_two[l]) > (spread_amplification_factor+1)){
#         # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#         weight_two[l] <- (spread_amplification_factor+1)
#         # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#         weight_one[l] <- 1 - weight_two[l]# my_total_group_df$NextDayWeigths_second[backtesting_index]
#       }
#     } else {
#       # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#       # my_total_group_df$NextDayWeigths_first[backtesting_index]
#       weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#       # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#       weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#       # we here limit the positive weight : the second one
#       if (abs(weight_two[l]) > spread_amplification_factor){
#         # my_total_group_df$NextDayWeigths_second[backtesting_index]
#         weight_two[l]<- spread_amplification_factor
#         # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#         weight_one[l] <- -1 - weight_two[l] # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#       }
#     }
#   } 
#   ## this is the case where we must set a limit on the traded quantities
#   ## as they can become huge and still summing to one because of their sign difference
#   if ((backtesting_predictions_first[l] >0)&&(backtesting_predictions_second[l] <0)){
#     if (abs(backtesting_predictions_second[l])<backtesting_predictions_first[l]){
#       # first case : we are more confident in x1
#       # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
#       # we here limit the positive weight : the first one
#       if (abs(weight_one[l]) > (spread_amplification_factor+1)){
#         # my_total_group_df$NextDayWeigths_first[backtesting_index]
#         weight_one[l] <- (spread_amplification_factor+1)
#         # my_total_group_df$NextDayWeigths_second[backtesting_index]
#         weight_two[l] <- 1 - weight_one[l] #my_total_group_df$NextDayWeigths_first[backtesting_index]
#       }
#     } else {
#       # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
#       # my_total_group_df$NextDayWeigths_first[backtesting_index]
#       weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
#       # my_total_group_df$NextDayWeigths_second[backtesting_index] 
#       weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
#       # we here limit the positive weight : the first one
#       if (abs(weight_one[l]) > spread_amplification_factor){
#         # my_total_group_df$NextDayWeigths_first[backtesting_index] 
#         weight_one[l] <- spread_amplification_factor
#         # my_total_group_df$NextDayWeigths_second[backtesting_index]
#         weight_two[l] <- -1 - weight_one[l] #my_total_group_df$NextDayWeigths_first[backtesting_index]
#       }
#     }
#   } 
# }
# 
# my_total_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
# my_total_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
# my_total_group_df$NextDayPrediction_first[backtesting_index] <- backtesting_predictions_first
# my_total_group_df$NextDayPrediction_second[backtesting_index] <- backtesting_predictions_second
# 
# 
# ##### my_total_group_df$NextDaySpread_RegTree_third[backtesting_index] <- backtesting_predictions_third
# ##### my_total_group_df$NextDayWeigths_third[backtesting_index] <- backtesting_predictions_third*spread_amplification_factor
# 
# 
# technical_signal_first <-  my_total_group_df$NextDayWeigths_first[backtesting_index] 
# technical_signal_second <-  my_total_group_df$NextDayWeigths_second[backtesting_index] 
# 
# 
# 
# ##### technical_signal_third <- my_total_group_df$NextDayWeigths_third[backtesting_index] 
# 
# #   modulation
# 
# # if positive we go long otherwise we short
# # first country has always to be the US  
# 
# first_leg_traded <- paste(first_country,"NextReturn",sep="")
# first_leg_lasttraded <- paste(first_country,"Return",sep="")
# first_leg_nexttraded <- paste(first_country,"NextNextReturn",sep="")
# 
# second_leg_traded <- paste(second_country,"",sep="NextOpenReturn")
# second_leg_lasttraded <- paste(second_country,"OpenReturn",sep="")
# second_leg_nexttraded <- paste(second_country,"NextNextOpenReturn",sep="")
# 
# traded_return <- my_total_group_df[backtesting_index,first_leg_traded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_traded] * technical_signal_second
# traded_return_last <- my_total_group_df[backtesting_index,first_leg_lasttraded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_lasttraded] * technical_signal_second
# traded_return_next <- my_total_group_df[backtesting_index,first_leg_nexttraded] * technical_signal_first + my_total_group_df[backtesting_index,second_leg_nexttraded] * technical_signal_second
# 
# ##### traded_return_spread <- my_total_group_df[backtesting_index,first_leg_traded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_traded] * technical_signal_third
# ##### traded_return_spread_last <- my_total_group_df[backtesting_index,first_leg_lasttraded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_lasttraded] * technical_signal_third
# ##### traded_return_spread_next <- my_total_group_df[backtesting_index,first_leg_nexttraded] * technical_signal_third - my_total_group_df[backtesting_index,second_leg_nexttraded] * technical_signal_third
# 
# ############
# # we make the output generic
# first_bond_returns <- paste(first_country,"Return",sep="")
# second_bond_returns <- paste(second_country,"Return",sep="")
# 
# to_return <- list()
# to_return$results <- data.frame(DATES = my_total_group_df$DATE[backtesting_index], 
#                                 STRATEGY_RETURN = traded_return, STRATEGY_YESTERDAY = CumFromRetToPricesStart(traded_return_last), STRATEGY_TODAY = CumFromRetToPricesStart(traded_return),STRATEGY_TOMORROW = CumFromRetToPricesStart(traded_return_next), 
#                                 #####   SPREAD_STRATEGY_RETURN = traded_return_spread, SPREAD_STRATEGY_YESTERDAY = CumFromRetToPricesStart(traded_return_spread_last), SPREAD_STRATEGY_TODAY = CumFromRetToPricesStart(traded_return_spread), SPREAD_STRATEGY_TOMORROW = CumFromRetToPricesStart(traded_return_spread_next), 
#                                 SECOND_BOND =   CumFromRetToPricesStart(my_total_group_df[backtesting_index,second_bond_returns]), FIRST_BOND =   CumFromRetToPricesStart(my_total_group_df[backtesting_index,first_bond_returns]),
#                                 FIRST_WEIGHT = my_total_group_df$NextDayWeigths_first[backtesting_index] ,  SECOND_WEIGHT = my_total_group_df$NextDayWeigths_second[backtesting_index],
#                                 FIRST_PREDICTION = my_total_group_df$NextDayPrediction_first[backtesting_index], SECOND_PREDICTION = my_total_group_df$NextDayPrediction_second[backtesting_index], 
#                                 FIRST_BOND_RETURN = my_total_group_df[backtesting_index,first_leg_lasttraded],  SECOND_BOND_OPEN_RETURN = my_total_group_df[backtesting_index,second_leg_lasttraded], 
#                                 FIRST_BOND_NEXT_RETURN = my_total_group_df[backtesting_index,first_leg_traded], SECOND_BOND_NEXT_OPEN_RETURN = my_total_group_df[backtesting_index,second_leg_traded],
#                                 FIRST_BOND_NEXT_NEXT_RETURN = my_total_group_df[backtesting_index,first_leg_nexttraded], SECOND_BOND_NEXT_NEXT_OPEN_RETURN = my_total_group_df[backtesting_index,second_leg_nexttraded])
# 
# to_return$sentiments <- my_total_group_df[backtesting_index,my_sentiment_predicting_columns]
# to_return$first_leg <- list_first_directionality
# to_return$second_leg <- list_second_directionality
# to_return$predicting_columns <- my_predicting_columns
# to_return$calibration_parameters <- list_calibration_parameters
# to_return$features_names <- my_predicting_columns
# #   return(to_return)  
# # }
