### Computing all price data for our bond futures
require("zoo")

computeAllPriceData <- function(OHLC_df){
  # beware : your dataset has to end with 
  # first of all : we make sure our dataset is ordered by dates descending 
  OHLC_df <- OHLC_df[rev(order(OHLC_df$DATE)),]
  ## Trading day daily data
  
  #### Computing for each day for each country
  # daily we do open to open and close to close
  # the current daily return
  # the next daily return
  # the next next daily return
  
  # close to close
  OHLC_df$NEXT_CLOSE <- lagpad(OHLC_df$CLOSE,1)
  OHLC_df$LAST_CLOSE <- c(tail(OHLC_df$CLOSE,-1),NA)
  OHLC_df$CLOSE_RETURN <- log(OHLC_df$CLOSE/OHLC_df$LAST_CLOSE)
  OHLC_df$NEXT_CLOSE_RETURN <- lagpad(OHLC_df$CLOSE_RETURN,1)
  OHLC_df$NEXT_NEXT_CLOSE_RETURN <- lagpad(OHLC_df$NEXT_CLOSE_RETURN,1)
  OHLC_df$LAST_CLOSE_RETURN <- c(tail(OHLC_df$CLOSE_RETURN,-1),NA)
  
  # open to open
  OHLC_df$NEXT_OPEN <- lagpad(OHLC_df$OPEN,1)
  OHLC_df$LAST_OPEN <- c(tail(OHLC_df$OPEN,-1),NA)
  # today open to next day open when trading daily can not be traded because morning open already passed
  OHLC_df$OPEN_RETURN <- log(OHLC_df$NEXT_OPEN/OHLC_df$OPEN)
  OHLC_df$NEXT_OPEN_RETURN <- lagpad(OHLC_df$OPEN_RETURN,1)
  OHLC_df$NEXT_NEXT_OPEN_RETURN <- lagpad(OHLC_df$NEXT_OPEN_RETURN,1)
  OHLC_df$LAST_OPEN_RETURN <- c(tail(OHLC_df$OPEN_RETURN,-1),NA)
  
  # overnight return
  OHLC_df$OVERNIGHT_RETURN <- log(OHLC_df$OPEN/OHLC_df$LAST_CLOSE)
  OHLC_df$NEXT_OVERNIGHT_RETURN <- lagpad(OHLC_df$OVERNIGHT_RETURN,1)
  OHLC_df$NEXT_NEXT_OVERNIGHT_RETURN <- lagpad(OHLC_df$NEXT_OVERNIGHT_RETURN,1)
  OHLC_df$LAST_OVERNIGHT_RETURN <- c(tail(OHLC_df$OVERNIGHT_RETURN,-1),NA)
  
  # Volumes
  OHLC_df$NEXT_VOLUME <- lagpad(OHLC_df$VOLUME,1)
  OHLC_df$NEXT_NEXT_VOLUME <- lagpad(OHLC_df$NEXT_VOLUME,1)
  OHLC_df$LAST_VOLUME <- c(tail(OHLC_df$VOLUME,-1),NA)
  
  
  # monthly
  # the current month return (close of day to next month close US/CA, next day open to next month open other currency) 
  # the next month return
  # the next next month return
  
  
  # weekly
  # the current month return (close to close US/CA, next day open to open other currency) 
  # the next month return
  # the next next month return
  # for weekly and monthly and yearly returns
  
  ### NEXT WEEK OPEN for other than US
  # we enter the market at the open of the day just after till one week after the day just after
  week_period_size <- 5
  OHLC_df$LAST_WEEK_OPEN <- c(tail(OHLC_df$OPEN,-week_period_size),rep(NA, week_period_size))
  OHLC_df$LAST_WEEK_OPEN_RETURN <- log(OHLC_df$OPEN/OHLC_df$LAST_WEEK_OPEN)
  
  OHLC_df$NEXT_WEEK_OPEN <- lagpad(OHLC_df$NEXT_OPEN,week_period_size)
  # from the open of the next day to the open of next day+5 days
  OHLC_df$NEXT_WEEK_OPEN_RETURN <-  log(OHLC_df$NEXT_WEEK_OPEN/OHLC_df$NEXT_OPEN)
  ######### to change because only the next day week returns
  #   OHLC_df$NEXT_NEXT_WEEK_OPEN_RETURN <- lagpad(OHLC_df$NEXT_WEEK_OPEN_RETURN,1)
  #   OHLC_df$WEEK_OPEN_RETURN <- c(tail(OHLC_df$NEXT_WEEK_OPEN_RETURN,-1),NA)
  #   OHLC_df$LAST_WEEK_OPEN_RETURN <- c(tail(OHLC_df$WEEK_OPEN_RETURN,-1),NA)
  #   
  ### NEXT WEEK CLOSE for US
  # we enter the market at the close of the currebt day just after till the close of the day one week after 
  week_period_size <- 5
  # from the last close - 5 days to the last close
  OHLC_df$LAST_WEEK_CLOSE <- c(tail(OHLC_df$LAST_CLOSE,-week_period_size),rep(NA, week_period_size))
  OHLC_df$LAST_WEEK_CLOSE_RETURN <- log(OHLC_df$LAST_CLOSE/OHLC_df$LAST_WEEK_CLOSE)
  
  OHLC_df$NEXT_WEEK_CLOSE <- lagpad(OHLC_df$NEXT_CLOSE,week_period_size)
  OHLC_df$NEXT_WEEK_CLOSE_RETURN <-  log(OHLC_df$NEXT_WEEK_CLOSE/OHLC_df$NEXT_CLOSE)
  
  ######### to change because only the next day week returns
  #   OHLC_df$NEXT_NEXT_WEEK_CLOSE_RETURN <- lagpad(OHLC_df$NEXT_WEEK_CLOSE_RETURN,1)
  #   OHLC_df$WEEK_CLOSE_RETURN <- c(tail(OHLC_df$NEXT_WEEK_CLOSE_RETURN,-1),NA)
  #   OHLC_df$LAST_WEEK_CLOSE_RETURN <- c(tail(OHLC_df$WEEK_CLOSE_RETURN,-1),NA)
  #   
  
  ### NEXT MONTH OPEN for other than US
  # we enter the market at the open of the day just after till one month after the day just after
  month_period_size <- 21
  OHLC_df$LAST_MONTH_OPEN <- c(tail(OHLC_df$OPEN,-month_period_size),rep(NA, month_period_size))
  OHLC_df$LAST_MONTH_OPEN_RETURN <- log(OHLC_df$OPEN/OHLC_df$LAST_MONTH_OPEN)
  
  OHLC_df$NEXT_MONTH_OPEN <- lagpad(OHLC_df$NEXT_OPEN,month_period_size)
  OHLC_df$NEXT_MONTH_OPEN_RETURN <-  log(OHLC_df$NEXT_MONTH_OPEN/OHLC_df$NEXT_OPEN)
  ######### to change because only the next day week returns
  #   OHLC_df$NEXT_NEXT_WEEK_OPEN_RETURN <- lagpad(OHLC_df$NEXT_WEEK_OPEN_RETURN,1)
  #   OHLC_df$WEEK_OPEN_RETURN <- c(tail(OHLC_df$NEXT_WEEK_OPEN_RETURN,-1),NA)
  #   OHLC_df$LAST_WEEK_OPEN_RETURN <- c(tail(OHLC_df$WEEK_OPEN_RETURN,-1),NA)
  #   
  ### NEXT WEEK CLOSE for US
  # we enter the market at the close of the currebt day just after till the close of the day one week after 
  month_period_size <- 21
  OHLC_df$LAST_MONTH_CLOSE <- c(tail(OHLC_df$LAST_CLOSE,-month_period_size),rep(NA, month_period_size))
  OHLC_df$LAST_MONTH_CLOSE_RETURN <- log(OHLC_df$LAST_CLOSE/OHLC_df$LAST_MONTH_CLOSE)
  
  OHLC_df$NEXT_MONTH_CLOSE <- lagpad(OHLC_df$CLOSE,month_period_size)
  OHLC_df$NEXT_MONTH_CLOSE_RETURN <-  log(OHLC_df$NEXT_MONTH_CLOSE/OHLC_df$CLOSE)
  
  print("Daily, weekly, monthly price computation done")
  
  results <- list()
  results$all <- OHLC_df
  # what am i trying to predict
  # my_output_to_predict <- c("DATE","NEXT_CLOSE_RETURN", "NEXT_WEEK_CLOSE_RETURN", "NEXT_WEEK_OPEN_RETURN","NEXT_MONTH_CLOSE_RETURN", "NEXT_MONTH_OPEN_RETURN")
  my_output_to_predict <- c("DATE", "NEXT_WEEK_CLOSE_RETURN", "NEXT_MONTH_CLOSE_RETURN")
  
  results$output <- OHLC_df[,my_output_to_predict]
  
  return(results)
}

# computeMAPriceMomentumData <- function(OHLC_df){
#   # beware : your dataset has to end with 
#   # first of all : we make sure our dataset is ordered by dates ascending for the TTR package
#   OHLC_df <- OHLC_df[order(OHLC_df$DATE),]
#   ## Trading day daily data
#   ## MA momentum on the last close return
#   
#   #   # for Europe you might add "OPEN","OPEN_RETURN","CLOSE","CLOSE_RETURN" (you might already know your close when taking the decision before the close of the market)
#   #   # for US you might add "OPEN","OPEN_RETURN",   
#   #   # for JAPAN you might remove "OPEN","OPEN_RETURN" (Japan not even open when taking the decision before the US close )
#   #   
#   #   known <- c( "DATE","LAST_CLOSE","LAST_CLOSE_RETURN","LAST_OPEN",         
#   #               "LAST_OPEN_RETURN", "OVERNIGHT_RETURN","LAST_OVERNIGHT_RETURN","LAST_VOLUME","LAST_WEEK_OPEN_RETURN","LAST_VOLUME","LAST_WEEK_OPEN",
#   #               "LAST_WEEK_CLOSE", "LAST_WEEK_CLOSE_RETURN","LAST_MONTH_CLOSE","LAST_MONTH_OPEN","LAST_MONTH_OPEN_RETURN","LAST_MONTH_CLOSE_RETURN")
#   #   # in Europe you might already know the close
#   #   # in Japan you might still not know the open
#   #   daily_known_prices  <- c( "LAST_OPEN","LAST_CLOSE", "LAST_VOLUME")
#   #   daily_known_returns  <- c( "LAST_OPEN_RETURN","LAST_CLOSE_RETURN")
#   #   
#   #   weekly_known_prices <- c( "LAST_WEEK_CLOSE","LAST_WEEK_OPEN")
#   #   weekly_known_returns <- c( "LAST_WEEK_CLOSE_RETURN","LAST_WEEK_OPEN_RETURN")
#   #   
#   #   monthly_known_prices <- c( "LAST_MONTH_CLOSE","LAST_MONTH_OPEN")
#   #   monthly_known_returns <- c( "LAST_MONTH_CLOSE_RETURN","LAST_MONTH_OPEN_RETURN")
#   
#   
#   # Fitting RSI indicators to our LAST_CLOSE
#   
#   MA_Columns <- FitMAModel(OHLC_df)
#   
#   
#   
#   
#   colnames(RSI_Columns) <- paste0("RSI_",c(3,5,10,20,30,40,50,60,70,80,90))
#   colnames(DEMA_Columns) <- paste0("DEMA_",c(3,5,10,20,30,40,50,60,70,80,90))
#   colnames(TREND_Columns) <- paste0("TREND_",c(3,5,10,20,30,40,50,60,70,80,90))
#   colnames(VOL_MA_Columns) <- paste0("MA_",c(3,5,10,20,30,40,50,60,70,80,90))
#   
#   ma_params <- paste0("BOLLINGER_",c(3,5,10,15,20,30,40,50,60,70,80,90))
#   sd_params <- c(1.0,1.5,2.0)
#   tot_params <- c()
#   for (sd in sd_params){
#     print(sd)
#     tot_params <- c(tot_params, paste0(ma_params,sd))
#   }
#   
#   colnames(Bollinger_Columns) <- tot_params
#   
#   
#   kept_as_predictors <- c( "DATE","LAST_CLOSE","LAST_CLOSE_RETURN","LAST_VOLUME","LAST_WEEK_CLOSE_RETURN","LAST_MONTH_CLOSE_RETURN")
#   OHLC_df <- OHLC_df[,kept_as_predictors]
#   
#   OHLC_df <- cbind(OHLC_df, RSI_Columns)
#   OHLC_df <- cbind(OHLC_df, DEMA_Columns)
#   OHLC_df <- cbind(OHLC_df, Bollinger_Columns)
#   OHLC_df <- cbind(OHLC_df, TREND_Columns)
#   OHLC_df <- cbind(OHLC_df, VOL_MA_Columns)
#   
#   # unknown_OHLC_df <- OHLC_df[,!((colnames(OHLC_df) %in% known))]
#   
#   # we only do our computation on known variables on the trading day
#   print("Momentum price computation done")
#   return(OHLC_df)
# }


computeAllPriceMomentumData <- function(OHLC_df){
  # beware : your dataset has to end with 
  # first of all : we make sure our dataset is ordered by dates ascending for the TTR package
  OHLC_df <- OHLC_df[order(OHLC_df$DATE),]
  ## Trading day daily data
  ## MA momentum on the last close return
  
  #   # for Europe you might add "OPEN","OPEN_RETURN","CLOSE","CLOSE_RETURN" (you might already know your close when taking the decision before the close of the market)
  #   # for US you might add "OPEN","OPEN_RETURN",   
  #   # for JAPAN you might remove "OPEN","OPEN_RETURN" (Japan not even open when taking the decision before the US close )
  #   
  #   known <- c( "DATE","LAST_CLOSE","LAST_CLOSE_RETURN","LAST_OPEN",         
  #               "LAST_OPEN_RETURN", "OVERNIGHT_RETURN","LAST_OVERNIGHT_RETURN","LAST_VOLUME","LAST_WEEK_OPEN_RETURN","LAST_VOLUME","LAST_WEEK_OPEN",
  #               "LAST_WEEK_CLOSE", "LAST_WEEK_CLOSE_RETURN","LAST_MONTH_CLOSE","LAST_MONTH_OPEN","LAST_MONTH_OPEN_RETURN","LAST_MONTH_CLOSE_RETURN")
  #   # in Europe you might already know the close
  #   # in Japan you might still not know the open
  #   daily_known_prices  <- c( "LAST_OPEN","LAST_CLOSE", "LAST_VOLUME")
  #   daily_known_returns  <- c( "LAST_OPEN_RETURN","LAST_CLOSE_RETURN")
  #   
  #   weekly_known_prices <- c( "LAST_WEEK_CLOSE","LAST_WEEK_OPEN")
  #   weekly_known_returns <- c( "LAST_WEEK_CLOSE_RETURN","LAST_WEEK_OPEN_RETURN")
  #   
  #   monthly_known_prices <- c( "LAST_MONTH_CLOSE","LAST_MONTH_OPEN")
  #   monthly_known_returns <- c( "LAST_MONTH_CLOSE_RETURN","LAST_MONTH_OPEN_RETURN")
  
  
  # Fitting RSI indicators to our LAST_CLOSE
  RSI_Columns <- FitRSIModel(OHLC_df[,c("LAST_CLOSE")])
  DEMA_Columns <- FitDEMAModel(OHLC_df[,c("LAST_CLOSE")])
  Bollinger_Columns <- FitBollingerModel(OHLC_df[,c("LAST_CLOSE")])
  TREND_Columns <- FitTRENDModel(OHLC_df[,c("LAST_CLOSE")])
  VOL_MA_Columns <- FitMAModel(OHLC_df[,c("LAST_VOLUME")])
  
  
  
  
  colnames(RSI_Columns) <- paste0("RSI_",c(3,5,10,20,30,40,50,60,70,80,90))
  colnames(DEMA_Columns) <- paste0("DEMA_",c(3,5,10,20,30,40,50,60,70,80,90))
  colnames(TREND_Columns) <- paste0("TREND_",c(3,5,10,20,30,40,50,60,70,80,90))
  colnames(VOL_MA_Columns) <- paste0("VOL_MA_",c(3,5,10,20,30,40,50,60,70,80,90))
  
  ma_params <- paste0("BOLLINGER_",c(3,5,10,15,20,30,40,50,60,70,80,90))
  sd_params <- c(1.0,1.5,2.0)
  tot_params <- c()
  for (sd in sd_params){
    print(sd)
    tot_params <- c(tot_params, paste0(ma_params,sd))
  }
  
  colnames(Bollinger_Columns) <- tot_params
  
  
  kept_as_predictors <- c( "DATE","LAST_CLOSE","LAST_CLOSE_RETURN","LAST_VOLUME","LAST_WEEK_CLOSE_RETURN","LAST_MONTH_CLOSE_RETURN")
  OHLC_df <- OHLC_df[,kept_as_predictors]
  
  OHLC_df <- cbind(OHLC_df, RSI_Columns)
  OHLC_df <- cbind(OHLC_df, DEMA_Columns)
  OHLC_df <- cbind(OHLC_df, Bollinger_Columns)
  OHLC_df <- cbind(OHLC_df, TREND_Columns)
  OHLC_df <- cbind(OHLC_df, VOL_MA_Columns)
  
  # unknown_OHLC_df <- OHLC_df[,!((colnames(OHLC_df) %in% known))]
  
  # we only do our computation on known variables on the trading day
  print("Momentum price computation done")
  return(OHLC_df)
}


FitDEMAModelEachColumn <- function(myDf){
  myDf <- apply(myDf, 2, function(x) FitDEMAModel(x) )
  return(t(myDf))
}

FitBollingerModelEachColumn <- function(myDf){
  myDf <- apply(myDf, 2, function(x) FitBollingerModel(x) )
  return(t(myDf))
}

FitTRENDModelEachColumn <- function(myDf){
  myDf <- apply(myDf, 2, function(x) FitTRENDModel(x) )
  return(t(myDf))
}

FitRSIModelEachColumn <- function(myDf){
  myDf <- apply(myDf, 2, function(x) FitRSIModel(x) )
  return(t(myDf))
}


FitRSIModel <- function(myRow){
  #   myRowZoo <- zoo(myRow)
  #   ## use underlying time scale for interpolation
  #   myRowZoo <- na.approx(myRowZoo) 
#   RSI1 <- RSI(myRow,1)
#   RSI2 <- RSI(myRow,2)
  RSI3 <- RSI(myRow,3)
  RSI5 <- RSI(myRow,5)
  RSI10 <- RSI(myRow,10)
  RSI20 <- RSI(myRow,20)
  RSI30 <- RSI(myRow,30)
  RSI40 <- RSI(myRow,40)
  RSI50 <- RSI(myRow,50)
  RSI60 <- RSI(myRow,60)
  RSI70 <- RSI(myRow,70)
  RSI80 <- RSI(myRow,80)
  RSI90 <- RSI(myRow,90)
  # RSI100 <- RSI(myRowZoo,100)
  # we keep just the very last minute signal
  return (data.frame(RSI3=RSI3, RSI5=RSI5, RSI10=RSI10, RSI20=RSI20, RSI30=RSI30, RSI40=RSI40, RSI50=RSI50, RSI60=RSI60, RSI70=RSI70, RSI80=RSI80,RSI90=RSI90))
  # return(c(RSI1[length(RSI1)],RSI2[length(RSI2)],RSI3[length(RSI3)],RSI5[length(RSI5)],RSI10[length(RSI10)],RSI20[length(RSI20)],RSI30[length(RSI30)],RSI40[length(RSI40)],RSI50[length(RSI50)],RSI60[length(RSI60)],RSI70[length(RSI70)],RSI80[length(RSI80)],RSI90[length(RSI90)]))
}

FitDEMAModel <- function(myRow){
  #   myRowZoo <- zoo(myRow)
  #   ## use underlying time scale for interpolation
  #   myRowZoo <- na.approx(myRowZoo) 
#   DEMA1 <- DEMA(myRow,1)
#   DEMA2 <- DEMA(myRow,2)
  DEMA3 <- DEMA(myRow,3)
  DEMA5 <- DEMA(myRow,5)
  DEMA10 <- DEMA(myRow,10)
  # DEMA15 <- DEMA(myRowZoo,15)
  DEMA20 <- DEMA(myRow,20)
  DEMA30 <- DEMA(myRow,30)
  DEMA40 <- DEMA(myRow,40)
  DEMA50 <- DEMA(myRow,50)
  DEMA60 <- DEMA(myRow,60)
  DEMA70 <- DEMA(myRow,70)
  DEMA80 <- DEMA(myRow,80)
  DEMA90 <- DEMA(myRow,90)
  DEMA100 <- DEMA(myRow,100)
  return (data.frame(DEMA3=DEMA3, DEMA5=DEMA5, DEMA10=DEMA10, DEMA20=DEMA20, DEMA30=DEMA30, DEMA40=DEMA40,DEMA50=DEMA50, DEMA60=DEMA60, DEMA70=DEMA70, DEMA80=DEMA80,  DEMA90=DEMA90))
  # return(c(DEMA1[length(DEMA1)],DEMA2[length(DEMA2)],DEMA3[length(DEMA3)],DEMA5[length(DEMA5)],DEMA10[length(DEMA10)],DEMA20[length(DEMA20)],DEMA30[length(DEMA30)],DEMA40[length(DEMA40)]))
}

FitBollingerModel <- function(myRow){
  # myRowZoo <- zoo(myRow)
  ## use underlying time scale for interpolation
  # myRowZoo <- na.approx(myRowZoo) 
  bollinger1sd1 <- BBands(myRow,n=1, sd=1.0)
  my_length <- dim(bollinger1sd1)[1]
  # my_length <- dim(bollinger1sd1)[1]
  # bollinger2sd1 <- BBands(myRow,n=2, sd=1.0)
  bollinger3sd1 <- BBands(myRow,n=3, sd=1.0)
  bollinger5sd1 <- BBands(myRow,n=5, sd=1.0)
  bollinger10sd1 <- BBands(myRow,n=10, sd=1.0)
  bollinger15sd1 <- BBands(myRow,n=15, sd=1.0)
  bollinger20sd1 <- BBands(myRow,n=20, sd=1.0)
  bollinger30sd1 <- BBands(myRow,n=30, sd=1.0)
  bollinger40sd1 <- BBands(myRow,n=40, sd=1.0)
  bollinger50sd1 <- BBands(myRow,n=50, sd=1.0)
  bollinger60sd1 <- BBands(myRow,n=60, sd=1.0)
  bollinger70sd1 <- BBands(myRow,n=70, sd=1.0)
  bollinger80sd1 <- BBands(myRow,n=80, sd=1.0)
  bollinger90sd1 <- BBands(myRow,n=90, sd=1.0)
  # bollinger100sd1 <- BBands(myRowZoo,n=100, sd=1.0)
  
#   bollinger1sd15 <- BBands(myRow,n=1, sd=1.5)
#   bollinger2sd15 <- BBands(myRow,n=2, sd=1.5)
  bollinger3sd15 <- BBands(myRow,n=3, sd=1.5)
  bollinger5sd15 <- BBands(myRow,n=5, sd=1.5)
  bollinger10sd15 <- BBands(myRow,n=10, sd=1.5)
  bollinger15sd15 <- BBands(myRow,n=15, sd=1.5)
  bollinger20sd15 <- BBands(myRow,n=20, sd=1.5)
  bollinger30sd15 <- BBands(myRow,n=30, sd=1.5)
  bollinger40sd15 <- BBands(myRow,n=40, sd=1.5)
  bollinger50sd15 <- BBands(myRow,n=50, sd=1.5)
  bollinger60sd15 <- BBands(myRow,n=60, sd=1.5)
  bollinger70sd15 <- BBands(myRow,n=70, sd=1.5)
  bollinger80sd15 <- BBands(myRow,n=80, sd=1.5)
  bollinger90sd15 <- BBands(myRow,n=90, sd=1.5)
  # bollinger100sd15 <- BBands(myRowZoo,n=100, sd=1.5)
  
#   bollinger1sd2 <- BBands(myRow,n=1, sd=2.0)
#   bollinger2sd2 <- BBands(myRow,n=2, sd=2.0)
  bollinger3sd2 <- BBands(myRow,n=3, sd=2.0)
  bollinger5sd2 <- BBands(myRow,n=5, sd=2.0)
  bollinger10sd2 <- BBands(myRow,n=10, sd=2.0)
  bollinger15sd2 <- BBands(myRow,n=15, sd=2.0)
  bollinger20sd2 <- BBands(myRow,n=20, sd=2.0)
  bollinger30sd2 <- BBands(myRow,n=30, sd=2.0)
  bollinger40sd2 <- BBands(myRow,n=40, sd=2.0)
  bollinger50sd2 <- BBands(myRow,n=50, sd=2.0)
  bollinger60sd2 <- BBands(myRow,n=60, sd=2.0)
  bollinger70sd2 <- BBands(myRow,n=70, sd=2.0)
  bollinger80sd2 <- BBands(myRow,n=80, sd=2.0)
  bollinger90sd2 <- BBands(myRow,n=90, sd=2.0)
  # bollinger100sd2 <- BBands(myRowZoo,n=100, sd=2.0)
  
  results <- data.frame(bollinger3sd1=bollinger3sd1[,4], bollinger5sd1=bollinger5sd1[,4] ,bollinger10sd1=bollinger10sd1[,4],bollinger15sd1=bollinger15sd1[,4] , bollinger20sd1=bollinger20sd1[,4],bollinger30sd1=bollinger30sd1[,4] ,bollinger40sd1=bollinger40sd1[,4] ,bollinger50sd1=bollinger50sd1[,4] ,bollinger60sd1=bollinger60sd1[,4] ,bollinger70sd1=bollinger70sd1[,4] ,bollinger80sd1=bollinger80sd1[,4] ,bollinger90sd1=bollinger90sd1[,4], #bollinger100sd1[,4] ,
                        bollinger3sd15=bollinger3sd15[,4], bollinger5sd15=bollinger5sd15[,4],bollinger10sd15=bollinger10sd15[,4],bollinger15sd15=bollinger15sd15[,4], bollinger20sd15=bollinger20sd15[,4],bollinger30sd15=bollinger30sd15[,4],bollinger40sd15=bollinger40sd15[,4],bollinger50sd15=bollinger50sd15[,4],bollinger60sd15=bollinger60sd15[,4],bollinger70sd15=bollinger70sd15[,4],bollinger80sd15=bollinger80sd15[,4],bollinger90sd15=bollinger90sd15[,4],#bollinger100sd15[,4],
                        bollinger3sd2=bollinger3sd2[,4], bollinger5sd2=bollinger5sd2[,4] ,bollinger10sd2=bollinger10sd2[,4] ,bollinger15sd2=bollinger15sd2[,4] , bollinger20sd2=bollinger20sd2[,4] ,bollinger30sd2=bollinger30sd2[,4] ,bollinger40sd2=bollinger40sd2[,4] ,bollinger50sd2=bollinger50sd2[,4],bollinger60sd2=bollinger60sd2[,4],bollinger70sd2=bollinger70sd2[,4] ,bollinger80sd2=bollinger80sd2[,4],bollinger90sd2=bollinger90sd2[,4] )#,#bollinger100sd2[my_length,4] )
  
  return(results)
}


FitTRENDModel <- function(myRow){
  # myRowZoo <- zoo(myRow)
  ## use underlying time scale for interpolation
  #   myRowZoo <- na.approx(myRowZoo) 
  #   TREND2 <- myRow[length(myRow)]-SMA(myRowZoo,2)
  #   my_length <- length(TREND2)
  TREND2 <- myRow-SMA(myRow,2)
  TREND3 <- myRow-SMA(myRow,3)
  TREND5 <- myRow-SMA(myRow,5)
  TREND10 <- myRow-SMA(myRow,10)
  TREND20 <- myRow-SMA(myRow,20)
  TREND30 <- myRow-SMA(myRow,30)
  TREND40 <- myRow-SMA(myRow,40)
  TREND50 <- myRow-SMA(myRow,50)
  TREND60 <- myRow-SMA(myRow,60)
  TREND70 <- myRow-SMA(myRow,70)
  TREND80 <- myRow-SMA(myRow,80)
  TREND90 <- myRow-SMA(myRow,90)
  # TREND100 <- myRow[length(myRow)]-SMA(myRowZoo,100)
  return(data.frame(TREND2=TREND2,TREND3=TREND3,TREND5=TREND5,TREND10=TREND10,TREND20=TREND20,TREND30=TREND30,TREND40=TREND40,TREND50=TREND50,TREND60=TREND60,TREND70=TREND70,TREND80=TREND80,TREND90=TREND90))
}

FitMAModel <- function(myRow){
  myRowZoo <- zoo(myRow)
  ## use underlying time scale for interpolation
  myRowZoo <- na.approx(myRowZoo,na.rm = FALSE) 
  myRow <- myRowZoo
  #   TREND2 <- myRow[length(myRow)]-SMA(myRowZoo,2)
  #   my_length <- length(TREND2)
  TREND2 <- SMA(myRow,2)
  TREND3 <- SMA(myRow,3)
  TREND5 <- SMA(myRow,5)
  TREND10 <- SMA(myRow,10)
  TREND20 <- SMA(myRow,20)
  TREND30 <- SMA(myRow,30)
  TREND40 <- SMA(myRow,40)
  TREND50 <- SMA(myRow,50)
  TREND60 <- SMA(myRow,60)
  TREND70 <- SMA(myRow,70)
  TREND80 <- SMA(myRow,80)
  TREND90 <- SMA(myRow,90)
  # TREND100 <- myRow[length(myRow)]-SMA(myRowZoo,100)
  return(data.frame(TREND2=TREND2,TREND3=TREND3,TREND5=TREND5,TREND10=TREND10,TREND20=TREND20,TREND30=TREND30,TREND40=TREND40,TREND50=TREND50,TREND60=TREND60,TREND70=TREND70,TREND80=TREND80,TREND90=TREND90))
}




FitMAModelEachColumn <- function(myDf){
  my_names <- colnames(myDf)
  print("Beginning to compute the delayed MA")
  myResultsDf <- apply(myDf, 2, function(x) FitMAModel(x) )
  # aggregating each dataframe results with the proper columns names
  
  if (length(my_names) != length(myResultsDf)){
    print("Trouble")
  }
  print("Beginning to aggregate")
  aggregated_df <- NULL
  for (i in 1:length(my_names)){
    my_computed_element <- myResultsDf[[i]]
    my_name <- my_names[i]
    colnames(my_computed_element) <- paste0(my_name,"_MA_",c(2,3,5,10,20,30,40,50,60,70,80,90))
    if (is.null(aggregated_df)){
      aggregated_df <- my_computed_element
    } else {
      aggregated_df <- cbind(aggregated_df,my_computed_element)
    }
  }

  return(aggregated_df)
}
