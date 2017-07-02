require(vars)
require(plyr)
### Utility function for spread prevision
# needed libraries
# library("SIT")
library("RPQuantUtils")
library("RPToolsDB")
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
# require(Quandl)
require(rpart)
require(randomForest)
# require(rpart.plot)
# require(rattle)
# install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
# require(caret)
require(xgboost)

source("./RCode/RP_Dates_Utils.R")
source("./RCode/RP_Prediction_Utils.R")
source("./RCode/RP_Macro_Monthly_Utils.R")

us_news_cutting_off_hour <- 16
eu_news_cutting_off_hour <- 17
jp_news_cutting_off_hour <- 15

my_bondfutures_flatfile <- list(GB ="UK_10Y_GILT_Rolled_Futures.rds", DE ="GER_10Y_BUND_Rolled_Futures.rds",
                                FR = "Fr_10Y_OAT_Rolled_Futures.rds" , US="US_10Y_TN_Rolled_Futures.rds", 
                                JP="JP_10Y_YEN_Rolled_Futures.rds" , EU="EU_10Y_BTP_Rolled_Futures.rds")


all.subsets <- function(set) {
  n <- length(set)
  bin <- expand.grid(rlply(n, c(F, T)))
  mlply(bin, function(...) { set[c(...)] })
}

get.modelvariables <- function(vars) {
  vars.form <- "MACRO"
  if (length(vars) != 0) {
    vars.form <- paste("MACRO+", paste(vars, collapse = "+"),sep="")
  } 
  return(vars.form)
}

get.onelist <- function(vars) {
  out <- F
  if (length(vars) == 1) {
    out <- T
  } 
  return(out)
}


compute_backtest_spread_strategy_west_first_horizon_price_momentum <- function(my_predicting_columns, my_total_bond_group_df,first_country,second_country,backtesting_starting_date, backtesting_ending_date, spread_amplification_factor, algorithm_used, investment_horizon,leading_one=1,lagging_one=2,rsi_depth=2,dema_depth=3,trend_depth=5, bolllinger_sd=2.0){
  if (length(my_predicting_columns) != 0){
    print(my_predicting_columns)
    ### Backtesting our model with an expanding window and predicting out of sample next overnight returns each day according to sentiment up to the close
    start_date <- backtesting_starting_date
    end_date <-  backtesting_ending_date
    
    
    backtesting_dates_logical_index <-
      (my_total_bond_group_df$DATE >= start_date &
         my_total_bond_group_df$DATE <= end_date)
    
    backtesting_dates_index <-
      which(backtesting_dates_logical_index == TRUE)
    
    nb_iteration <- sum(backtesting_dates_logical_index)
    
    backtesting_predictions_first <-
      matrix(0,nrow = nb_iteration-1, ncol = 1)
    backtesting_predictions_second <-
      matrix(0,nrow = nb_iteration-1, ncol = 1)
    backtesting_predictions_third <-
      matrix(0,nrow = nb_iteration-1, ncol = 1)
    backtesting_index <-
      matrix(0,nrow = nb_iteration-1, ncol = 1)
    
    output_one_column <-  paste(first_country, "NextReturn", sep="")
    output_two_column <- paste(second_country, "NextOpenReturn", sep="")
    my_US_predicting_columns <-  paste(first_country, my_predicting_columns, sep="")
    my_DE_predicting_columns <-  paste(second_country, my_predicting_columns, sep="")
    my_predicting_columns <- c(my_US_predicting_columns,my_DE_predicting_columns)
    
    for (i in 1:(nb_iteration-1)) {
      print("Predicting iteration number ")
      print(i)
      print("Over")
      print(nb_iteration)
      # we use all the past available data to train our xgboost model
      

      my_model_columns <- c(my_predicting_columns, output_one_column, output_two_column)
      my_iteration_data <- my_total_bond_group_df[1:backtesting_dates_index[i],my_model_columns]
      if(length(my_model_columns) != dim(my_iteration_data)[2]){
        print("WarningWarningWarningWarningWarningWarning : Predictors dropped")
      }
      
      print("Using the past data from")
      print(my_total_bond_group_df$DATE[1])
      print("up to ")
      print(my_total_bond_group_df$DATE[backtesting_dates_index[i]])
      # we predict the next date
      prediction_index <- backtesting_dates_index[i]+1
      my_prediction_date <- my_total_bond_group_df$DATE[prediction_index]
      print("Predicting for the next day : ")
      print(my_prediction_date)
      
      model_to_predict <- my_total_bond_group_df[prediction_index,]
      
      
      
      if(algorithm_used == "xgboost"){
        my_iteration_results <- predict_next_day_bidirectional_spread_xgboost(my_predicting_columns , output_one_column, output_two_column, my_iteration_data, model_to_predict )
      }
      
      if(algorithm_used == "rpart"){
        my_iteration_results <- predict_next_day_bidirectional_spread_rpart(my_predicting_columns , output_one_column, output_two_column, my_iteration_data, model_to_predict )
      }
      
      
      if(algorithm_used == "rpart_unpruned"){
        my_iteration_results <- predict_next_day_bidirectional_spread_rpart_unpruned(my_predicting_columns , output_one_column, output_two_column, my_iteration_data, model_to_predict )
      }
      
      
      # backtesting_predictions[i,] <- my_prediction_first
      backtesting_predictions_first[i,] <- my_iteration_results$first
      backtesting_predictions_second[i,] <- my_iteration_results$second
      #########backtesting_predictions_third[i,] <- my_iteration_results$spread
      
      backtesting_index[i,] <- prediction_index
    }
    
    my_total_bond_group_df$NextDaySpread_RegTree_first <- NA
    my_total_bond_group_df$NextDayWeigths_first <- NA
    my_total_bond_group_df$NextDaySpread_RegTree_second <- NA
    my_total_bond_group_df$NextDayWeigths_second <- NA
    #########my_total_bond_group_df$NextDaySpread_RegTree_third <- NA
    #########my_total_bond_group_df$NextDayWeigths_third <- NA
    
    
    weight_one <- backtesting_predictions_first/(backtesting_predictions_first + backtesting_predictions_second)
    weight_two <- backtesting_predictions_second/(backtesting_predictions_first + backtesting_predictions_second)
    
    my_total_bond_group_df$NextDaySpread_RegTree_first[backtesting_index] <- backtesting_predictions_first
    my_total_bond_group_df$NextDaySpread_RegTree_second[backtesting_index] <- backtesting_predictions_second
    
    for (l in 1:length(backtesting_predictions_first)){
      if ((backtesting_predictions_first[l] <0)&&(backtesting_predictions_second[l]<0)){
        # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
        # my_total_group_df$NextDayWeigths_first[backtesting_index]
        weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
        # my_total_group_df$NextDayWeigths_second[backtesting_index] 
        weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
      } 
      ## this is the case where we must set a limit on the traded quantities
      ## as they can become huge and still summing to one because of their sign difference
      if ((backtesting_predictions_first[l] <0)&&(backtesting_predictions_second[l] >0)){
        if (abs(backtesting_predictions_first[l])<backtesting_predictions_second[l]){
          ## first case : we are more confident in x2
          # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
          # we here limit the positive weight : the second one
          if (abs(weight_two[l]) > (spread_amplification_factor+1)){
            # my_total_group_df$NextDayWeigths_second[backtesting_index] 
            weight_two[l] <- (spread_amplification_factor+1)
            # my_total_group_df$NextDayWeigths_first[backtesting_index] 
            weight_one[l] <- 1 - weight_two[l]# my_total_group_df$NextDayWeigths_second[backtesting_index]
          }
        } else {
          # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
          # my_total_group_df$NextDayWeigths_first[backtesting_index]
          weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
          # my_total_group_df$NextDayWeigths_second[backtesting_index] 
          weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
          # we here limit the positive weight : the second one
          if (abs(weight_two[l]) > spread_amplification_factor){
            # my_total_group_df$NextDayWeigths_second[backtesting_index]
            weight_two[l]<- spread_amplification_factor
            # my_total_group_df$NextDayWeigths_first[backtesting_index] 
            weight_one[l] <- -1 - weight_two[l] # my_total_group_df$NextDayWeigths_second[backtesting_index] 
          }
        }
      } 
      ## this is the case where we must set a limit on the traded quantities
      ## as they can become huge and still summing to one because of their sign difference
      if ((backtesting_predictions_first[l] >0)&&(backtesting_predictions_second[l] <0)){
        if (abs(backtesting_predictions_second[l])<backtesting_predictions_first[l]){
          # first case : we are more confident in x1
          # x1/(x1+x2)   x2/(x1+x2)  they sum to plus 1 
          # we here limit the positive weight : the first one
          if (abs(weight_one[l]) > (spread_amplification_factor+1)){
            # my_total_group_df$NextDayWeigths_first[backtesting_index]
            weight_one[l] <- (spread_amplification_factor+1)
            # my_total_group_df$NextDayWeigths_second[backtesting_index]
            weight_two[l] <- 1 - weight_one[l] #my_total_group_df$NextDayWeigths_first[backtesting_index]
          }
        } else {
          # -x1/(x1+x2)   -x2/(x1+x2)  they sum to minus 1 
          # my_total_group_df$NextDayWeigths_first[backtesting_index]
          weight_one[l]<- -backtesting_predictions_first[l]/(backtesting_predictions_first[l]+ backtesting_predictions_second[l])
          # my_total_group_df$NextDayWeigths_second[backtesting_index] 
          weight_two[l] <- -backtesting_predictions_second[l]/(backtesting_predictions_first[l] + backtesting_predictions_second[l])
          # we here limit the positive weight : the first one
          if (abs(weight_one[l]) > spread_amplification_factor){
            # my_total_group_df$NextDayWeigths_first[backtesting_index] 
            weight_one[l] <- spread_amplification_factor
            # my_total_group_df$NextDayWeigths_second[backtesting_index]
            weight_two[l] <- -1 - weight_one[l] #my_total_group_df$NextDayWeigths_first[backtesting_index]
          }
        }
      } 
    }
    
    my_total_bond_group_df$NextDayWeigths_first[backtesting_index] <- weight_one
    my_total_bond_group_df$NextDayWeigths_second[backtesting_index] <- weight_two
    
    
    #########my_total_bond_group_df$NextDaySpread_RegTree_third[backtesting_index] <- backtesting_predictions_third
    #########my_total_bond_group_df$NextDayWeigths_third[backtesting_index] <- backtesting_predictions_third*spread_amplification_factor
    
    
    technical_signal_first <-  my_total_bond_group_df$NextDayWeigths_first[backtesting_index] 
    technical_signal_second <-  my_total_bond_group_df$NextDayWeigths_second[backtesting_index] 
    
    
    
    #########technical_signal_third <- my_total_bond_group_df$NextDayWeigths_third[backtesting_index] 
    
    #   modulation
    
    # if positive we go long otherwise we short
    # first country has always to be the US  
    
    first_leg_traded <- paste(first_country,"NextReturn",sep="")
    second_leg_traded <- paste(second_country,"",sep="NextOpenReturn")
    
    traded_return <- my_total_bond_group_df[backtesting_index,first_leg_traded] * technical_signal_first + my_total_bond_group_df[backtesting_index,second_leg_traded] * technical_signal_second
    ############
    # we make the output generic
    first_bond_returns <- paste(first_country,"Return",sep="")
    second_bond_returns <- paste(second_country,"Return",sep="")
    
    results <- data.frame(DATES = my_total_bond_group_df$DATE[backtesting_index], 
                          STRATEGY_RETURN = traded_return)
    
    return(computeIR(traded_return,investment_horizon))  
  } else {
    return(0)
  }
    
}





