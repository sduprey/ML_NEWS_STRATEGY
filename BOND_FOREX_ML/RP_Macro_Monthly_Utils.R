computeIR <- function(x, horizon =1){
  if (horizon == 1){
    IR <- mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)*sqrt(252)
  }
  if (horizon == 5){
    IR <- mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)*sqrt(52)
  }
  if (horizon == 21){
    IR <- mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)*sqrt(12)
  }
  if (horizon == 252){
    IR <- mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)
  }
  
  return(IR)
}

computeIRfromPrice <- function(prices, horizon =1){
#   if (min(prices) <0 ){
#     prices <- prices-min(prices)+10^(-6)
#   }
   # x<- fromPricesToRet(prices)
   x<- CumfromPricesToRet(prices)
  if (horizon == 1){
    IR <- (mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))*sqrt(252)
  }
  if (horizon == 5){
    IR <- (mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))*sqrt(52)
  }
  if (horizon == 21){
    IR <- (mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))*sqrt(12)
  }
  if (horizon == 252){
    IR <- mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)
  }
  return(IR)
}


lagpad <- function(x, k) {
  c(rep(NA, k), x)[1 : length(x)] 
}

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  x$hour <- 0
  x$sec <- 0
  x$min <- 0
  as.Date(x)
}

ifna <- function (x, y) 
{
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

iif <-function (cond, truepart, falsepart) 
{
  if (length(cond) == 1) {
    if (cond) 
      truepart
    else falsepart
  }
  else {
    if (length(falsepart) == 1) {
      temp = falsepart
      falsepart = cond
      falsepart[] = temp
    }
    if (length(truepart) == 1) 
      falsepart[cond] = truepart
    else {
      cond = ifna(cond, F)
      if (is.xts(truepart)) 
        falsepart[cond] = coredata(truepart)[cond]
      else falsepart[cond] = truepart[cond]
    }
    return(falsepart)
  }
}

ifna_prev <- function(y)
{
  y1 = !is.na(y)
  y1[1]=T
  return( y[cummax( (1:length(y)) * y1 )]	)
}


ifzero_prev <- function(y)
{
  y1 = !(y==0)
  y1[1]=T
  return( y[cummax( (1:length(y)) * y1 )]	)
}

fromRetToPrices<-function(logReturns){
  my_prices <- c(1,cumprod(exp(ifna(logReturns,0))))
  return (my_prices)
}

fromRetToPricesStart<-function(logReturns){
  my_prices <- c(cumprod(exp(ifna(logReturns,0))))
  return (my_prices)
}

CumFromRetToPricesStart<-function(logReturns){
  my_prices <- c(cumsum(ifna(logReturns,0)))
  return (my_prices)
}

CumfromPricesToRet<-function(prices){
  my_returns <-  c(diff(prices))
  return (my_returns)
}


fromPricesToRet<-function(prices){
  my_returns <-  c(0,diff(log(prices)))
  return (my_returns)
}


vec_square_diff_rmse <- function(data){
  c(RMSE<-with(data, ((rUNEMP-rUNEMP_PRED)^2+(rCONS-rCONS_PRED)^2+(rCPI-rCPI_PRED)^2+(rDEF-rDEF_PRED)^2+(rGCE-rGCE_PRED)^2+(rGDP-rGDP_PRED)^2+(rHOURS-rHOURS_PRED)^2+(rINV-rINV_PRED)^2+(rM1-rM1_PRED)^2+(rM2-rM2_PRED)^2+(rWAGES-rWAGES_PRED)^2+(rFED-rFED_PRED)^2+(rG10-rG10_PRED)^2+(rTB3-rTB3_PRED)^2)))
}

vec_norm_rmse <- function(data){
  c(RMSE<-with(data,(rUNEMP^2+rCONS^2+rCPI^2+rDEF^2+rGCE^2+rGDP^2+rHOURS^2+rINV^2+rM1^2+rM2^2+rWAGES^2+rFED^2+rG10^2+rTB3^2)))
}



vec_diff_rmse <- function(data){
  c(ERROR<-with(data, (abs(rUNEMP-rUNEMP_PRED)+abs(rCONS-rCONS_PRED)+abs(rCPI-rCPI_PRED)+abs(rDEF-rDEF_PRED)+abs(rGCE-rGCE_PRED)+abs(rGDP-rGDP_PRED)+abs(rHOURS-rHOURS_PRED)+abs(rINV-rINV_PRED)+abs(rM1-rM1_PRED)+abs(rM2-rM2_PRED)+abs(rWAGES-rWAGES_PRED)+abs(rFED-rFED_PRED)+abs(rG10-rG10_PRED)+abs(rTB3-rTB3_PRED))))
}


vec_l1_norm_rmse <- function(data){
  c(RMSE<-with(data,(abs(rUNEMP)+abs(rCONS)+abs(rCPI)+abs(rDEF)+abs(rGCE)+abs(rGDP)+abs(rHOURS)+abs(rINV)+abs(rM1)+abs(rM2)+abs(rWAGES)+abs(rFED)+abs(rG10)+abs(rTB3))))
}


fillNAByPreviousData <- function(column) {
  # At first we find out which columns contain NAs
  navals <- which(is.na(column))
  # and which columns are filled with data.
  filledvals <- which(! is.na(column))
  
  # If there would be no NAs following each other, navals-1 would give the
  # entries we need. In our case, however, we have to find the last column filled for
  # each value of NA. We may do this using the following sapply trick:
  fillup <- sapply(navals, function(x) max(filledvals[filledvals < x]))
  
  # And finally replace the NAs with our data.
  column[navals] <- column[fillup]
  column
}


calcpc <- function(variables,loadings)
{
  # find the number of samples in the data set
  as.data.frame(variables)
  numsamples <- dim(variables)[1]
  # make a vector to store the component
  pc <- numeric(numsamples)
  # find the number of variables
  numvariables <-  dim(variables)[2]
  # calculate the value of the component for each sample
  for (i in 1:numsamples)
  {
    valuei <- 0
    for (j in 1:numvariables)
    {
      valueij <- variables[i,j]
      loadingj <- loadings[j]
      valuei <- valuei + (valueij * loadingj)
    }
    pc[i] <- valuei
  }
  return(pc)
}


shuffle_weights <- function(my_weights){
  
  pert <- rnorm(length(my_weights), mean = 0, sd = 0.1)  
  pert <- my_weights + pert
  pert <- pert /sum(pert)
  
}


computeGroupsSentimentGlobalRMSE <- function(my_us_macro_df, daily_global_macroRPData, weights,  start_date="2010-01-01", end_date="2015-07-01") {
  
  group_matrix <- as.matrix(daily_global_macroRPData[,-1])
  
  # daily_global_macroRPData$agg_macro_sentiment <- (group_matrix %*% weights)/rowSums(abs(group_matrix))
  daily_global_macroRPData$agg_macro_sentiment <- (group_matrix %*% weights)
  
  #### we now transform our daily averaged data to monthly data
  sentiment_serie <-
    xts(daily_global_macroRPData[c("agg_macro_sentiment")],order.by = daily_global_macroRPData$DATE)
  
  sentiment_m_serie <-
    apply.monthly(sentiment_serie,sum)
  
  # From xts time series to data frame
  data_matrix <-
    matrix(sentiment_m_serie,dim(sentiment_m_serie)[1],dim(sentiment_m_serie)[2],dimnames =
             list(
               time(sentiment_m_serie),c(
                 "sentiment"
               )
             ))
  my_mdf <- as.data.frame(data_matrix)
  my_mdf$DATES <- as.Date(index(sentiment_m_serie))
  my_monthly_sentiment_macro_df <-
    my_mdf[c("DATES","sentiment")]
  colnames(my_monthly_sentiment_macro_df) <-
    c("DATES", "rSENT")
  
  my_monthly_sentiment_macro_df$DATES <-
    my_monthly_sentiment_macro_df$DATES + 1
  my_total_df <-
    merge(my_monthly_sentiment_macro_df, my_us_macro_df, by = 'DATES')
  
  ## cleaning the dataset from NaN values and non predicting variables
  my_variables <- c("rSENT","rFED","rG10","rTB3","rUNEMP","rPPI","rCPI","rCPIFE","rGDP","rDEF","rPCE","rHOURS","rIP","rRS","rIPI","rSP500","rM1","rM2","rCONS","rPMI","rWAGES")
  my_total_df <- my_total_df[c("DATES",my_variables)]    
  
  my_total_df[,-1] <- apply(my_total_df[,-1] , 2, ifna_prev)
  my_total_df <- my_total_df[complete.cases(my_total_df),]
  
  ## restricting our data to the only important value
  my_model_data <- my_total_df[,-1]
  
  backtesting_dates_logical_index <-
    (my_total_df$DATES >= start_date &
       my_total_df$DATES <= end_date)
  backtesting_dates_index <-
    which(backtesting_dates_logical_index == TRUE)
  d.vselect <-
    VARselect(my_model_data, lag.max = 2, type = 'const')$selection[1]
  nb_iteration <- sum(backtesting_dates_logical_index)
  
  backtesting_predictions <-
    matrix(0,nrow = nb_iteration, ncol = length(my_variables))
  backtesting_lowerbounds <-
    matrix(0,nrow = nb_iteration, ncol = length(my_variables))
  backtesting_upperbounds <-
    matrix(0,nrow = nb_iteration, ncol = length(my_variables))
  
  for (i in 1:nb_iteration) {
    model_calibrating_data <-
      my_total_df[1:backtesting_dates_index[i],my_variables]
    d.var <-
      VAR(model_calibrating_data, p = d.vselect, type = 'const')
    my_quarter_prediction <- predict(d.var, n.ahead = 1)
    backtesting_predictions[i,] <-
      unlist(lapply(my_variables, function(x) {
        my_quarter_prediction$fcst[[x]][,1]
      }))
    backtesting_lowerbounds[i,] <-
      unlist(lapply(my_variables, function(x) {
        my_quarter_prediction$fcst[[x]][,2]
      }))
    backtesting_upperbounds[i,] <-
      unlist(lapply(my_variables, function(x) {
        my_quarter_prediction$fcst[[x]][,3]
      }))
  }
  
  #### Plotting the prediction of our sentiment_enhanced_model
  
  my_happened_df <- my_total_df[backtesting_dates_logical_index,]
  
  colnames(backtesting_predictions) <-
    paste(my_variables, "_PRED",sep = "")
  rownames(backtesting_predictions) <- my_happened_df$DATES
  backtesting_predictions <- as.data.frame(backtesting_predictions)
  
  
  colnames(backtesting_upperbounds) <- paste(my_variables, "_UP",sep = "")
  rownames(backtesting_upperbounds) <- my_happened_df$DATES
  backtesting_upperbounds <- as.data.frame(backtesting_upperbounds)
  
  colnames(backtesting_lowerbounds) <-
    paste(my_variables, "_DOWN",sep = "")
  rownames(backtesting_lowerbounds) <- my_happened_df$DATES
  backtesting_lowerbounds <- as.data.frame(backtesting_lowerbounds)
  
  mbacktesting_results_df <-
    cbind(
      my_happened_df,backtesting_predictions,backtesting_lowerbounds,backtesting_upperbounds
    )
  ############ Computing the observation averaged RMSE for some significant macro variables
  #### Getting the same unpercentaged value
  mbacktesting_results_df$rUNEMP_RMSE <-
    (mbacktesting_results_df$rUNEMP - mbacktesting_results_df$rUNEMP_PRED) ^
    2
  mbacktesting_results_df$rCONS_RMSE <-
    (mbacktesting_results_df$rCONS - mbacktesting_results_df$rCONS_PRED) ^
    2
  mbacktesting_results_df$rCPI_RMSE <-
    (mbacktesting_results_df$rCPI - mbacktesting_results_df$rCPI_PRED) ^ 2
  mbacktesting_results_df$rDEF_RMSE <-
    (mbacktesting_results_df$rDEF - mbacktesting_results_df$rDEF_PRED) ^ 2
  mbacktesting_results_df$rGCE_RMSE <-
    (mbacktesting_results_df$rGCE - mbacktesting_results_df$rGCE_PRED) ^ 2
  mbacktesting_results_df$rGDP_RMSE <-
    (mbacktesting_results_df$rGDP - mbacktesting_results_df$rGDP_PRED) ^ 2
  mbacktesting_results_df$rHOURS_RMSE <-
    (mbacktesting_results_df$rHOURS - mbacktesting_results_df$rHOURS_PRED) ^
    2
  mbacktesting_results_df$rINV_RMSE <-
    (mbacktesting_results_df$rINV - mbacktesting_results_df$rINV_PRED) ^ 2
  mbacktesting_results_df$rM1_RMSE <-
    (mbacktesting_results_df$rM1 - mbacktesting_results_df$rM1_PRED) ^ 2
  mbacktesting_results_df$rM2_RMSE <-
    (mbacktesting_results_df$rM2 - mbacktesting_results_df$rM2_PRED) ^ 2
  mbacktesting_results_df$rWAGES_RMSE <-
    (mbacktesting_results_df$rWAGES - mbacktesting_results_df$rWAGES_PRED) ^
    2
  mbacktesting_results_df$rFED_RMSE <-
    (mbacktesting_results_df$rFED - mbacktesting_results_df$rFED_PRED) ^ 2
  mbacktesting_results_df$rG10_RMSE <-
    (mbacktesting_results_df$rG10 - mbacktesting_results_df$rG10_PRED) ^ 2
  mbacktesting_results_df$rTB3_RMSE <-
    (mbacktesting_results_df$rTB3 - mbacktesting_results_df$rTB3_PRED) ^ 2
  mbacktesting_results_df$RMSE <-
    vec_square_diff_rmse(mbacktesting_results_df)
  
  mbacktesting_results_df$rUNEMP_NORM_RMSE <-
    (mbacktesting_results_df$rUNEMP) ^ 2
  mbacktesting_results_df$rCONS_NORM_RMSE <-
    (mbacktesting_results_df$rCONS) ^ 2
  mbacktesting_results_df$rCPI_NORM_RMSE <-
    (mbacktesting_results_df$rCPI) ^ 2
  mbacktesting_results_df$rDEF_NORM_RMSE <-
    (mbacktesting_results_df$rDEF) ^ 2
  mbacktesting_results_df$rGCE_NORM_RMSE <-
    (mbacktesting_results_df$rGCE) ^ 2
  mbacktesting_results_df$rGDP_NORM_RMSE <-
    (mbacktesting_results_df$rGDP) ^ 2
  mbacktesting_results_df$rHOURS_NORM_RMSE <-
    (mbacktesting_results_df$rHOURS) ^ 2
  mbacktesting_results_df$rINV_NORM_RMSE <-
    (mbacktesting_results_df$rINV) ^ 2
  mbacktesting_results_df$rM1_NORM_RMSE <-
    (mbacktesting_results_df$rM1) ^ 2
  mbacktesting_results_df$rM2_NORM_RMSE <-
    (mbacktesting_results_df$rM2) ^ 2
  mbacktesting_results_df$rWAGES_NORM_RMSE <-
    (mbacktesting_results_df$rWAGES) ^ 2
  mbacktesting_results_df$rFED_NORM_RMSE <-
    (mbacktesting_results_df$rFED) ^ 2
  mbacktesting_results_df$rG10_NORM_RMSE <-
    (mbacktesting_results_df$rG10) ^ 2
  mbacktesting_results_df$rTB3_NORM_RMSE <-
    (mbacktesting_results_df$rTB3) ^ 2
  mbacktesting_results_df$NORM_RMSE <-
    vec_norm_rmse(mbacktesting_results_df)
  
  
  mbacktesting_results_df$L1_NORM <-
    vec_l1_norm_rmse(mbacktesting_results_df)
  mbacktesting_results_df$ERROR <-
    vec_diff_rmse(mbacktesting_results_df)
  mbacktesting_results_df$GLOBAL_PERCENT_ERROR <-
    mbacktesting_results_df$ERROR / mbacktesting_results_df$L1_NORM
  
  mbacktesting_results_df$L1_NORM_GDP <-
    abs(mbacktesting_results_df$rGDP)
  mbacktesting_results_df$ERROR_GDP <-
    abs(mbacktesting_results_df$rGDP - mbacktesting_results_df$rGDP_PRED)
  mbacktesting_results_df$GDP_PERCENT_ERROR <-
    mbacktesting_results_df$ERROR_GDP / mbacktesting_results_df$L1_NORM_GDP
  
  ##### Displaying the computed averaged RMSE
  my_rmse_perc <-
    sqrt(sum(mbacktesting_results_df$RMSE,na.rm = T)) / (sqrt(sum(
      mbacktesting_results_df$NORM_RMSE,na.rm = T
    )) * sum(!is.nan(mbacktesting_results_df$RMSE)))
  if (my_rmse_perc == 0){
    return (Inf)
  }
  
  return (my_rmse_perc)
}




# default dates are set for the Web edition backtesting with at least 3 years of training
computeGroupsSentimentGDPRMSE <- function(my_us_macro_df, daily_global_macroRPData, weights,  start_date="2012-01-01", end_date="2015-07-01") {
  
  group_matrix <- as.matrix(daily_global_macroRPData[,-1])
  
  # daily_global_macroRPData$agg_macro_sentiment <- (group_matrix %*% weights)/rowSums(abs(group_matrix))
  daily_global_macroRPData$agg_macro_sentiment <- (group_matrix %*% weights)
  
  
  
  #### we now transform our daily averaged data to monthly data
  sentiment_serie <-
    xts(daily_global_macroRPData[c("agg_macro_sentiment")],order.by = daily_global_macroRPData$DATE)
  
  sentiment_m_serie <-
    apply.monthly(sentiment_serie,sum)
  
  # From xts time series to data frame
  data_matrix <-
    matrix(sentiment_m_serie,dim(sentiment_m_serie)[1],dim(sentiment_m_serie)[2],dimnames =
             list(
               time(sentiment_m_serie),c(
                 "sentiment"
               )
             ))
  my_qdf <- as.data.frame(data_matrix)
  my_qdf$DATES <- as.Date(index(sentiment_m_serie))
  my_quarter_sentiment_macro_df <-
    my_qdf[c("DATES","sentiment")]
  colnames(my_quarter_sentiment_macro_df) <-
    c("DATES", "rSENT")
  
  my_quarter_sentiment_macro_df$DATES <-
    my_quarter_sentiment_macro_df$DATES + 1
  my_total_df <-
    merge(my_quarter_sentiment_macro_df, my_us_macro_df, by = 'DATES')
  
  ## cleaning the dataset from NaN values and non predicting variables
  # my_variables <- c("rSENT","rFED","rG10","rTB3","rUNEMP","rPPI","rCPI","rCPIFE","rGDP","rDEF","rPCE","rHOURS","rIP","rRS","rIPI","rSP500","rM1","rM2","rCONS","rPMI","rWAGES")
  my_variables <- c("rSENT","rFED","rG10","rTB3","rUNEMP","rGDP","rSP500","rCPIFE","rPCE","rHOURS","rIP", "rRS","rM2","rPMI","rWAGES")
  my_total_df <- my_total_df[c("DATES",my_variables)]    
  
  my_total_df[,-1] <- apply(my_total_df[,-1] , 2, ifna_prev)
  my_total_df <- my_total_df[complete.cases(my_total_df),]
  
  ## restricting our data to the only important value
  my_model_data <- my_total_df[,-1]
  #   
  #   my_variables <-
  #     c(
  #       "rSENT","rFED","rG10","rTB3","rUNEMP","rPPI","rCPI","rCPIFE","rGDP","rGDPI","rPCE","rAHE","rIP","rRS","rIPI","rSP500","rM1SL","rM2SL","rCONS","rPMI","rWAGE"
  #     )
  #   
  #   
  #   my_model_data <- my_total_df[my_variables]
  
  
  backtesting_dates_logical_index <-
    (my_total_df$DATES >= start_date &
       my_total_df$DATES <= end_date)
  backtesting_dates_index <-
    which(backtesting_dates_logical_index == TRUE)
  d.vselect <-
    VARselect(my_model_data, lag.max = 2, type = 'const')$selection[1]
  nb_iteration <- sum(backtesting_dates_logical_index)
  
  backtesting_predictions <-
    matrix(0,nrow = nb_iteration, ncol = length(my_variables))
  backtesting_lowerbounds <-
    matrix(0,nrow = nb_iteration, ncol = length(my_variables))
  backtesting_upperbounds <-
    matrix(0,nrow = nb_iteration, ncol = length(my_variables))
  
  for (i in 1:nb_iteration) {
    model_calibrating_data <-
      my_total_df[1:backtesting_dates_index[i],my_variables]
    d.var <-
      VAR(model_calibrating_data, p = d.vselect, type = 'const')
    my_quarter_prediction <- predict(d.var, n.ahead = 1)
    backtesting_predictions[i,] <-
      unlist(lapply(my_variables, function(x) {
        my_quarter_prediction$fcst[[x]][,1]
      }))
    backtesting_lowerbounds[i,] <-
      unlist(lapply(my_variables, function(x) {
        my_quarter_prediction$fcst[[x]][,2]
      }))
    backtesting_upperbounds[i,] <-
      unlist(lapply(my_variables, function(x) {
        my_quarter_prediction$fcst[[x]][,3]
      }))
  }
  
  #### Plotting the prediction of our sentiment_enhanced_model
  
  my_happened_df <- my_total_df[backtesting_dates_logical_index,]
  
  colnames(backtesting_predictions) <-
    paste(my_variables, "_PRED",sep = "")
  rownames(backtesting_predictions) <- my_happened_df$DATES
  backtesting_predictions <- as.data.frame(backtesting_predictions)
  
  
  colnames(backtesting_upperbounds) <- paste(my_variables, "_UP",sep = "")
  rownames(backtesting_upperbounds) <- my_happened_df$DATES
  backtesting_upperbounds <- as.data.frame(backtesting_upperbounds)
  
  colnames(backtesting_lowerbounds) <-
    paste(my_variables, "_DOWN",sep = "")
  rownames(backtesting_lowerbounds) <- my_happened_df$DATES
  backtesting_lowerbounds <- as.data.frame(backtesting_lowerbounds)
  
  mbacktesting_results_df <-
    cbind(
      my_happened_df,backtesting_predictions,backtesting_lowerbounds,backtesting_upperbounds
    )
  ############ Computing the observation averaged RMSE for some significant macro variables
  #### Getting the same unpercentaged value
  #   mbacktesting_results_df$rUNEMP_RMSE <-
  #     (mbacktesting_results_df$rUNEMP - mbacktesting_results_df$rUNEMP_PRED) ^
  #     2
  #   mbacktesting_results_df$rCONS_RMSE <-
  #     (mbacktesting_results_df$rCONS - mbacktesting_results_df$rCONS_PRED) ^
  #     2
  #   mbacktesting_results_df$rCPI_RMSE <-
  #     (mbacktesting_results_df$rCPI - mbacktesting_results_df$rCPI_PRED) ^ 2
  #   mbacktesting_results_df$rDEF_RMSE <-
  #     (mbacktesting_results_df$rDEF - mbacktesting_results_df$rDEF_PRED) ^ 2
  #   mbacktesting_results_df$rGCE_RMSE <-
  #     (mbacktesting_results_df$rGCE - mbacktesting_results_df$rGCE_PRED) ^ 2
  mbacktesting_results_df$rGDP_RMSE <-
    (mbacktesting_results_df$rGDP - mbacktesting_results_df$rGDP_PRED) ^ 2
  #   mbacktesting_results_df$rHOURS_RMSE <-
  #     (mbacktesting_results_df$rHOURS - mbacktesting_results_df$rHOURS_PRED) ^
  #     2
  #   mbacktesting_results_df$rINV_RMSE <-
  #     (mbacktesting_results_df$rINV - mbacktesting_results_df$rINV_PRED) ^ 2
  #   mbacktesting_results_df$rM1_RMSE <-
  #     (mbacktesting_results_df$rM1 - mbacktesting_results_df$rM1_PRED) ^ 2
  #   mbacktesting_results_df$rM2_RMSE <-
  #     (mbacktesting_results_df$rM2 - mbacktesting_results_df$rM2_PRED) ^ 2
  #   mbacktesting_results_df$rWAGES_RMSE <-
  #     (mbacktesting_results_df$rWAGES - mbacktesting_results_df$rWAGES_PRED) ^
  #     2
  #   mbacktesting_results_df$rFED_RMSE <-
  #     (mbacktesting_results_df$rFED - mbacktesting_results_df$rFED_PRED) ^ 2
  #   mbacktesting_results_df$rG10_RMSE <-
  #     (mbacktesting_results_df$rG10 - mbacktesting_results_df$rG10_PRED) ^ 2
  #   mbacktesting_results_df$rTB3_RMSE <-
  #     (mbacktesting_results_df$rTB3 - mbacktesting_results_df$rTB3_PRED) ^ 2
  #   mbacktesting_results_df$RMSE <-
  #     vec_square_diff_rmse(mbacktesting_results_df)
  
  #   mbacktesting_results_df$rUNEMP_NORM_RMSE <-
  #     (mbacktesting_results_df$rUNEMP) ^ 2
  #   mbacktesting_results_df$rCONS_NORM_RMSE <-
  #     (mbacktesting_results_df$rCONS) ^ 2
  #   mbacktesting_results_df$rCPI_NORM_RMSE <-
  #     (mbacktesting_results_df$rCPI) ^ 2
  #   mbacktesting_results_df$rDEF_NORM_RMSE <-
  #     (mbacktesting_results_df$rDEF) ^ 2
  #   mbacktesting_results_df$rGCE_NORM_RMSE <-
  #     (mbacktesting_results_df$rGCE) ^ 2
  mbacktesting_results_df$rGDP_NORM_RMSE <-
    (mbacktesting_results_df$rGDP) ^ 2
  #   mbacktesting_results_df$rHOURS_NORM_RMSE <-
  #     (mbacktesting_results_df$rHOURS) ^ 2
  #   mbacktesting_results_df$rINV_NORM_RMSE <-
  #     (mbacktesting_results_df$rINV) ^ 2
  #   mbacktesting_results_df$rM1_NORM_RMSE <-
  #     (mbacktesting_results_df$rM1) ^ 2
  #   mbacktesting_results_df$rM2_NORM_RMSE <-
  #     (mbacktesting_results_df$rM2) ^ 2
  #   mbacktesting_results_df$rWAGES_NORM_RMSE <-
  #     (mbacktesting_results_df$rWAGES) ^ 2
  #   mbacktesting_results_df$rFED_NORM_RMSE <-
  #     (mbacktesting_results_df$rFED) ^ 2
  #   mbacktesting_results_df$rG10_NORM_RMSE <-
  #     (mbacktesting_results_df$rG10) ^ 2
  #   mbacktesting_results_df$rTB3_NORM_RMSE <-
  #     (mbacktesting_results_df$rTB3) ^ 2
  #   mbacktesting_results_df$NORM_RMSE <-
  #     vec_norm_rmse(mbacktesting_results_df)
  #   
  
  #   mbacktesting_results_df$L1_NORM <-
  #     vec_l1_norm_rmse(mbacktesting_results_df)
  #   mbacktesting_results_df$ERROR <-
  #     vec_diff_rmse(mbacktesting_results_df)
  #   mbacktesting_results_df$GLOBAL_PERCENT_ERROR <-
  #     mbacktesting_results_df$ERROR / mbacktesting_results_df$L1_NORM
  #   
  #   mbacktesting_results_df$L1_NORM_GDP <-
  #     abs(mbacktesting_results_df$rGDP)
  #   mbacktesting_results_df$ERROR_GDP <-
  #     abs(mbacktesting_results_df$rGDP - mbacktesting_results_df$rGDP_PRED)
  #   mbacktesting_results_df$GDP_PERCENT_ERROR <-
  #     mbacktesting_results_df$ERROR_GDP / mbacktesting_results_df$L1_NORM_GDP
  
  ##### Displaying the computed averaged RMSE
  my_gdp_rmse <- sqrt(sum(mbacktesting_results_df$rGDP_RMSE,na.rm = T ))/sum(!is.nan(abs(mbacktesting_results_df$rGDP_RMSE)))
  
  if (my_gdp_rmse == 0){
    return (Inf)
  }
  
  return (my_gdp_rmse)
}



# default dates are set for the Web edition backtesting with at least 3 years of training
computeMLGDPRMSE <- function(my_monthly_df, my_daily_sentiment_df, weights,   start_date="2012-01-01", end_date="2015-10-01") {
  group_matrix <- as.matrix(my_daily_sentiment_df[,-1])
  
  # daily_global_macroRPData$agg_macro_sentiment <- (group_matrix %*% weights)/rowSums(abs(group_matrix))
  my_daily_sentiment_df$agg_macro_sentiment <- (group_matrix %*% weights)
  
  
  
  #### we now transform our daily averaged data to monthly data
  sentiment_serie <-
    xts(my_daily_sentiment_df[c("agg_macro_sentiment")],order.by = my_daily_sentiment_df$DATE)
  
  sentiment_m_serie <-
    apply.monthly(sentiment_serie,sum)
  
  # From xts time series to data frame
  data_matrix <-
    matrix(sentiment_m_serie,dim(sentiment_m_serie)[1],dim(sentiment_m_serie)[2],dimnames =
             list(
               time(sentiment_m_serie),c(
                 "sentiment"
               )
             ))
  my_qdf <- as.data.frame(data_matrix)
  my_qdf$DATES <- as.Date(index(sentiment_m_serie))
  my_quarter_sentiment_macro_df <-
    my_qdf[c("DATES","sentiment")]
  colnames(my_quarter_sentiment_macro_df) <-
    c("DATES", "rSENT")
  
  my_quarter_sentiment_macro_df$DATES <-
    my_quarter_sentiment_macro_df$DATES + 1
  
  my_total_df <-
    merge(my_quarter_sentiment_macro_df, my_monthly_df, by = 'DATES')
  
  
  my_df <- my_total_df[c("DATES","rSENT","rGDP","rGDP_CONS")]
  # my_df <- my_total_df[c("rSENT","rGDP","rGDP_CONS")]
  
  my_df <- my_df[complete.cases(my_df),]
  
  ##  Backtesting our model over time and assessing its predictabillity
  my_df <- my_total_df[c("DATES","rSENT","rGDP","rGDP_CONS")]
  my_df <- my_df[complete.cases(my_df),]
  
  
  backtesting_dates_logical_index <-
    (my_df$DATES >= start_date &
       my_df$DATES <= end_date)
  
  backtesting_dates_index <-
    which(backtesting_dates_logical_index == TRUE)
  
  nb_iteration <- sum(backtesting_dates_logical_index)
  
  backtesting_predictions <-
    matrix(0,nrow = nb_iteration-1, ncol = 1)
  backtesting_index <-
    matrix(0,nrow = nb_iteration-1, ncol = 1)
  
  
  for (i in 1:(nb_iteration-1)) {
    model_calibrating_data <- my_df[1:backtesting_dates_index[i],]
    prediction_index <- backtesting_dates_index[i]+1
    model_to_predict  <- my_df[prediction_index,]
    gdp.rf <- randomForest(rGDP ~ rGDP_CONS + rSENT, data=model_calibrating_data)
    my_prediction <- predict(gdp.rf,model_to_predict)
    backtesting_predictions[i,] <- my_prediction
    backtesting_index[i,] <- prediction_index
  }
  
  
  my_df$rGDP_PRED_FOREST <- NA
  my_df$rGDP_PRED_FOREST[backtesting_index] <- backtesting_predictions
  
  # length(my_df$rGDP_PRED_FOREST[backtesting_index])
  # length(backtesting_predictions)
  # dim(backtesting_predictions)
  # length(my_df$rGDP_PRED_FOREST)
  # dim(my_df)
  
  
  for (i in 1:(nb_iteration-1)) {
    model_calibrating_data <- my_df[1:backtesting_dates_index[i],]
    prediction_index <- backtesting_dates_index[i]+1
    model_to_predict  <- my_df[prediction_index,]
    #     my_tree <- rpart(rGDP ~ rGDP_CONS + rSENT, data=model_calibrating_data)
    #     my_prediction <- predict(my_tree,model_to_predict)
    my_tree <- rpart(rGDP ~ rGDP_CONS + rSENT, method="class", data=model_calibrating_data)
    my_prediction <- predict(my_tree,model_to_predict)
    backtesting_predictions[i,] <- rowSums(my_prediction * matrix(as.numeric(colnames(my_prediction)), byrow=TRUE, nrow = dim(my_prediction)[1], ncol= dim(my_prediction)[2]) )
    backtesting_index[i,] <- prediction_index
  }
  
  
  my_df$rGDP_PRED_TREE <- NA
  my_df$rGDP_PRED_TREE[backtesting_index] <- backtesting_predictions
  
  #   my_toplot_df <- melt(my_df[my_df$DATES >= "2012-01-01" ,],"DATES")
  #   my_title <-
  #     "Daily 10 days lagging moving average Macro topics sentiment"
  #   ggplot(
  #     my_toplot_df,aes(
  #       x = DATES,y = value,group = variable,color = variable
  #     )
  #   ) +
  #     geom_line() +
  #     scale_x_date() +
  #     ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  #     theme(title = element_text(size = 12, face = 'bold')) +
  #     theme(legend.position = c(0.2,0.2), legend.box = "vertical") +
  #     theme(legend.background = element_rect(fill = "gray90")) +
  #     theme(legend.key.size = unit(0.7, "cm"))
  #   
  my_df$rGDP_NORM_RMSE <-
    (my_df$rGDP) ^ 2
  
  my_df$rGDP_TREE_RMSE <-
    (my_df$rGDP - my_df$rGDP_PRED_TREE) ^ 2
  
  my_df$rGDP_FOREST_RMSE <-
    (my_df$rGDP - my_df$rGDP_PRED_FOREST) ^ 2
  
  
  my_tree_gdp_rmse <- sqrt(sum(my_df$rGDP_TREE_RMSE,na.rm = T ))/sum(!is.nan(abs(my_df$rGDP_TREE_RMSE)))
  my_forest_gdp_rmse <- sqrt(sum(my_df$rGDP_FOREST_RMSE,na.rm = T ))/sum(!is.nan(abs(my_df$rGDP_FOREST_RMSE)))
  
  temp <- min(my_forest_gdp_rmse,my_tree_gdp_rmse)
  
  if (my_forest_gdp_rmse == 0){
    return (Inf)
  }
  
  return (my_forest_gdp_rmse)
}

