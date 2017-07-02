### Launching all pair spread computations
### Trying to forecast the spread between ?/? bonds futures
# library("SIT")
# library("RPQuantUtils")
# library("RPToolsDB")
require(ggplot2)
require("ppcor")
require(graphics)
require("TTR")
require(plyr)
# server fails if we not require this package loading
require(labeling)
# require(reshape)
require(reshape2)
require(RColorBrewer)
require(stats)
require(Rsolnp)
require(zoo)
require(xts)
require(vars)
# require(Quandl)
## for the modeling
require(rpart)
require(randomForest)
require(xgboost)
## require caret (for dummy variables)
# require(caret)
## require Metrics to compute error
require(Metrics)


# source("./RCode/RP_Plotting_Utils.R")
# source("./RCode/RP_Macro_Monthly_Utils.R")
# source("./RCode/RP_Spread_Utils.R")
# source("./RCode/RP_Dates_Utils.R")
# source("./RCode/RP_Df_Utils.R")


user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
# repoPath = RP_GetSharedPath(user)
# # Input Data Path
# inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# # Output Data Path
# outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")
# 
# outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")


inputDataPath  <- "C://My_RP_Data/"

backtesting_starting_date <- "2002-01-01"
backtesting_ending_date <- "2015-10-01"


## Getting all the previously aggregated sentiment and prices
my_total_group_df <- readRDS(paste(inputDataPath,"all_daily_aggregated_sentiment_df.rds", sep = ""))
max_sent_date <- max(my_total_group_df$DATE)
min_sent_date <- min(my_total_group_df$DATE)

all_data_df <- readRDS(paste(inputDataPath,"all_data_df.rds", sep = ""))
all_predictors_df <- readRDS(paste(inputDataPath,"all_predictors_df.rds", sep = ""))
all_output_df <- readRDS(paste(inputDataPath,"all_output_df.rds", sep = ""))
max_price_date <- max(all_predictors_df$DATE)
min_price_date <- min(all_predictors_df$DATE)

# harmonizing dates
min_date <- max(min_sent_date,min_price_date)
max_date <- min(max_sent_date,max_price_date)



backtesting_starting_date <- "2000-01-01"
backtesting_ending_date <- "2015-10-01"


min_date <- backtesting_starting_date
max_date <- backtesting_ending_date
  
# we here just make sure that we have each day
all_days_df <- data.frame(DATE=seq(as.Date(min_date), as.Date(max_date), by="days"))

all_data_df <- all_data_df[all_data_df$DATE >= min_date & all_data_df$DATE <= max_date,]
all_predictors_df <- all_predictors_df[all_predictors_df$DATE >= min_date & all_predictors_df$DATE <= max_date,]
all_output_df <- all_output_df[all_output_df$DATE >= min_date & all_output_df$DATE <= max_date,]
my_total_group_df <- my_total_group_df[my_total_group_df$DATE >= min_date & my_total_group_df$DATE <= max_date,]


all_data_df <- all_data_df[order(all_data_df$DATE),]
all_predictors_df <- all_predictors_df[order(all_predictors_df$DATE),]
all_output_df <- all_output_df[order(all_output_df$DATE),]
my_total_group_df <- my_total_group_df[order(my_total_group_df$DATE),]

print(dim(all_days_df))
print(dim(all_data_df))
print(dim(all_predictors_df))
print(dim(all_output_df))
print(dim(my_total_group_df))


# Filtering week ends and pertinent predictors
# complete.cases(all_predictors_df[,-1])

my_relevant_predictor_index <- (colSums(all_predictors_df[,-1], na.rm = TRUE) != 0)
# just for rows where you do not have values
my_week_end_index <- (rowSums(all_predictors_df[,-1], na.rm = TRUE) != 0)
my_na_ratio <- rowSums(is.na(all_predictors_df[,-1])) /dim(all_predictors_df[,-1])[2]
my_week_end_index <- my_na_ratio <= 0.5

all_predictors_df <- all_predictors_df[my_week_end_index,]
all_predictors_df <- all_predictors_df[,c(TRUE,my_relevant_predictor_index)]

all_output_df <- all_output_df[my_week_end_index,]

print(dim(all_predictors_df)) 
print(dim(my_total_group_df)) 
print(dim(all_output_df)) 

my_total_predictors_df <- merge(my_total_group_df,all_predictors_df,by="DATE")
my_total_outputs_df <- all_output_df

print(dim(my_total_predictors_df)) 
print(dim(my_total_outputs_df)) 

SaveDataFrame(my_total_predictors_df,inputDataPath,"my_total_predictors_df")
SaveDataFrame(my_total_outputs_df,inputDataPath,"my_total_outputs_df")

