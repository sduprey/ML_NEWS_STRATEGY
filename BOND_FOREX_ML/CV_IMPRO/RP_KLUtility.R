### submission writing file function
require(xts)
require(zoo)
require(TTR)



##### my function to export my plot to a file
ExportPlot <- function(gplot, outputDataPath, filename, width=10, height=6) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  #ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  #postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  #print(gplot)
  #dev.off()
  png(file = paste(outputDataPath, filename, '.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}

SaveDataFrame <- function(dataframe, outputDataPath, filename) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  #ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  #postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  #print(gplot)
  #dev.off()
  saveRDS(dataframe, file=paste(outputDataPath, filename, '.rds', sep=""))
}

CumFromRetToPricesStart<-function(logReturns){
  my_prices <- c(cumsum(ifna(logReturns,0)))
  return (my_prices)
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

diff.data.frame <- function(obj){
  sapply(obj,FUN = function(x) diff(x))
}

is.column.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) is.finite(x))
}

is.column.infinite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.infinite(x)))
}

is.infinite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) is.infinite(x))
}

is.column.nan.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.nan(x)))
}

is.nan.data.frame <- function(obj){
  sapply(obj,FUN = function(x) is.nan(x))
}

Fit4ModelEachRow <- function(myDf){
  myDf <- apply(myDf, 1, function(x) Fit4Model(x) )
  return(t(myDf))
}

Fit4ModelWithIntercept <- function(myRow){
  x <- 1:length(myRow)
  y <- CumFromRetToPricesStart(t(myRow))
  #   plot(x,y)
  #   fit  <- lm(y~x)
  #   #second degree
  #   fit2 <- lm(y~poly(x,2,raw=TRUE))
  #   #third degree
  #   fit3 <- lm(y~poly(x,3,raw=TRUE))
  #fourth degree
  fit4 <- lm(y~poly(x,4,raw=TRUE))
  #generate range of 50 numbers starting from 30 and ending at 160
  coefs4 <- coef(fit4)
  # my_4_prediction <- coefs4[5]*x^4+coefs4[4]*x^3+coefs4[3]*x^2+coefs4[2]*x^1+coefs4[1]*x^0
  return(coefs4)
}

Fit4Model <- function(myRow){
  x <- 1:length(myRow)
  y <- CumFromRetToPricesStart(t(myRow))
  x <- c(0,x)
  y <- c(0,y) 
#   plot(x,y)
#   fit  <- lm(y~x)
#   #second degree
#   fit2 <- lm(y~poly(x,2,raw=TRUE))
#   #third degree
#   fit3 <- lm(y~poly(x,3,raw=TRUE))
  #fourth degree
  fit4 <- lm(y~poly(x,4,raw=TRUE)-1)
  #generate range of 50 numbers starting from 30 and ending at 160
  coefs4 <- coef(fit4)
  # my_4_prediction <- coefs4[5]*x^4+coefs4[4]*x^3+coefs4[3]*x^2+coefs4[2]*x^1+coefs4[1]*x^0
  return(coefs4)
}

FitRSIModelEachRow <- function(myDf){
  myDf <- apply(myDf, 1, function(x) FitRSIModel(x) )
  return(t(myDf))
}


FitDEMAModelEachRow <- function(myDf){
  myDf <- apply(myDf, 1, function(x) FitDEMAModel(x) )
  return(t(myDf))
}

FitBollingerModelEachRow <- function(myDf){
  myDf <- apply(myDf, 1, function(x) FitBollingerModel(x) )
  return(t(myDf))
}

FitTRENDModelEachRow <- function(myDf){
  myDf <- apply(myDf, 1, function(x) FitTRENDModel(x) )
  return(t(myDf))
}


Fit1ModelEachRow <- function(myDf){
  myDf <- apply(myDf, 1, function(x) Fit1Model(x) )
  return(t(myDf))
}

Fit1ModelWithIntercept <- function(myRow){
  x <- 1:length(myRow)
  y <- CumFromRetToPricesStart(t(myRow))
  x <- c(0,x)
  y <- c(0,y) 
  #   plot(x,y)
  #   fit  <- lm(y~x)
  #   #second degree
  #   fit2 <- lm(y~poly(x,2,raw=TRUE))
  #   #third degree
  #   fit3 <- lm(y~poly(x,3,raw=TRUE))
  #fourth degree
  fit1 <- lm(y~poly(x,1,raw=TRUE))
  #generate range of 50 numbers starting from 30 and ending at 160
  coefs1 <- coef(fit1)
  # my_4_prediction <- coefs4[5]*x^4+coefs4[4]*x^3+coefs4[3]*x^2+coefs4[2]*x^1+coefs4[1]*x^0
  return(coefs1)
}

FitRSIModel <- function(myRow){
  myRowZoo <- zoo(myRow)
  ## use underlying time scale for interpolation
  myRowZoo <- na.approx(myRowZoo) 
  RSI1 <- RSI(myRowZoo,1)
  RSI2 <- RSI(myRowZoo,2)
  RSI3 <- RSI(myRowZoo,3)
  RSI5 <- RSI(myRowZoo,5)
  RSI10 <- RSI(myRowZoo,10)
  RSI15 <- RSI(myRowZoo,15)
  RSI20 <- RSI(myRowZoo,20)
  RSI30 <- RSI(myRowZoo,30)
  RSI40 <- RSI(myRowZoo,40)
  RSI50 <- RSI(myRowZoo,50)
  RSI60 <- RSI(myRowZoo,60)
  RSI70 <- RSI(myRowZoo,70)
  RSI80 <- RSI(myRowZoo,80)
  RSI90 <- RSI(myRowZoo,90)
  RSI100 <- RSI(myRowZoo,100)
  return(c(RSI1,RSI2,RSI3,RSI5,RSI10,RSI20,RSI30,RSI40,RSI50,RSI60,RSI70,RSI80,RSI90,RSI100))
}

FitDEMAModel <- function(myRow){
  myRowZoo <- zoo(myRow)
  ## use underlying time scale for interpolation
  myRowZoo <- na.approx(myRowZoo) 
  DEMA1 <- DEMA(myRowZoo,1)
  DEMA2 <- DEMA(myRowZoo,2)
  DEMA3 <- DEMA(myRowZoo,3)
  DEMA5 <- DEMA(myRowZoo,5)
  DEMA10 <- DEMA(myRowZoo,10)
  DEMA15 <- DEMA(myRowZoo,15)
  DEMA20 <- DEMA(myRowZoo,20)
  DEMA30 <- DEMA(myRowZoo,30)
  DEMA40 <- DEMA(myRowZoo,40)
  DEMA50 <- DEMA(myRowZoo,50)
  # DEMA60 <- DEMA(myRowZoo,60)
#   DEMA70 <- DEMA(myRowZoo,70)
#   DEMA80 <- DEMA(myRowZoo,80)
#   DEMA90 <- DEMA(myRowZoo,90)
#   DEMA100 <- DEMA(myRowZoo,100)
  return(c(DEMA1,DEMA2,DEMA3,DEMA5,DEMA10,DEMA20,DEMA30,DEMA40,DEMA50))
}

FitBollingerModel <- function(myRow){
  myRowZoo <- zoo(myRow)
  ## use underlying time scale for interpolation
  myRowZoo <- na.approx(myRowZoo) 
  bollinger1sd1 <- BBands(myRowZoo,n=1, sd=1.0)
  bollinger2sd1 <- BBands(myRowZoo,n=2, sd=1.0)
  bollinger3sd1 <- BBands(myRowZoo,n=3, sd=1.0)
  bollinger5sd1 <- BBands(myRowZoo,n=5, sd=1.0)
  bollinger10sd1 <- BBands(myRowZoo,n=10, sd=1.0)
  bollinger15sd1 <- BBands(myRowZoo,n=15, sd=1.0)
  bollinger20sd1 <- BBands(myRowZoo,n=20, sd=1.0)
  bollinger30sd1 <- BBands(myRowZoo,n=30, sd=1.0)
  bollinger40sd1 <- BBands(myRowZoo,n=40, sd=1.0)
  bollinger50sd1 <- BBands(myRowZoo,n=50, sd=1.0)
  bollinger60sd1 <- BBands(myRowZoo,n=60, sd=1.0)
  bollinger70sd1 <- BBands(myRowZoo,n=70, sd=1.0)
  bollinger80sd1 <- BBands(myRowZoo,n=80, sd=1.0)
  bollinger90sd1 <- BBands(myRowZoo,n=90, sd=1.0)
  bollinger100sd1 <- BBands(myRowZoo,n=100, sd=1.0)
  
  bollinger1sd15 <- BBands(myRowZoo,n=1, sd=1.5)
  bollinger2sd15 <- BBands(myRowZoo,n=2, sd=1.5)
  bollinger3sd15 <- BBands(myRowZoo,n=3, sd=1.5)
  bollinger5sd15 <- BBands(myRowZoo,n=5, sd=1.5)
  bollinger10sd15 <- BBands(myRowZoo,n=10, sd=1.5)
  bollinger15sd15 <- BBands(myRowZoo,n=15, sd=1.5)
  bollinger20sd15 <- BBands(myRowZoo,n=20, sd=1.5)
  bollinger30sd15 <- BBands(myRowZoo,n=30, sd=1.5)
  bollinger40sd15 <- BBands(myRowZoo,n=40, sd=1.5)
  bollinger50sd15 <- BBands(myRowZoo,n=50, sd=1.5)
  bollinger60sd15 <- BBands(myRowZoo,n=60, sd=1.5)
  bollinger70sd15 <- BBands(myRowZoo,n=70, sd=1.5)
  bollinger80sd15 <- BBands(myRowZoo,n=80, sd=1.5)
  bollinger90sd15 <- BBands(myRowZoo,n=90, sd=1.5)
  bollinger100sd15 <- BBands(myRowZoo,n=100, sd=1.5)
  
  bollinger1sd2 <- BBands(myRowZoo,n=1, sd=2.0)
  bollinger2sd2 <- BBands(myRowZoo,n=2, sd=2.0)
  bollinger3sd2 <- BBands(myRowZoo,n=3, sd=2.0)
  bollinger5sd2 <- BBands(myRowZoo,n=5, sd=2.0)
  bollinger10sd2 <- BBands(myRowZoo,n=10, sd=2.0)
  bollinger15sd2 <- BBands(myRowZoo,n=15, sd=2.0)
  bollinger20sd2 <- BBands(myRowZoo,n=20, sd=2.0)
  bollinger30sd2 <- BBands(myRowZoo,n=30, sd=2.0)
  bollinger40sd2 <- BBands(myRowZoo,n=40, sd=2.0)
  bollinger50sd2 <- BBands(myRowZoo,n=50, sd=2.0)
  bollinger60sd2 <- BBands(myRowZoo,n=60, sd=2.0)
  bollinger70sd2 <- BBands(myRowZoo,n=70, sd=2.0)
  bollinger80sd2 <- BBands(myRowZoo,n=80, sd=2.0)
  bollinger90sd2 <- BBands(myRowZoo,n=90, sd=2.0)
  bollinger100sd2 <- BBands(myRowZoo,n=100, sd=2.0)
  
  results <- c(bollinger1sd1 , bollinger2sd1 , bollinger3sd1 , bollinger5sd1 ,bollinger10sd1 ,bollinger15sd1 , bollinger20sd1 ,bollinger30sd1 ,bollinger40sd1 ,bollinger50sd1 ,bollinger60sd1 ,bollinger70sd1 ,bollinger80sd1 ,bollinger90sd1 ,bollinger100sd1 ,
               bollinger1sd15, bollinger2sd15, bollinger3sd15, bollinger5sd15,bollinger10sd15,bollinger15sd15, bollinger20sd15,bollinger30sd15,bollinger40sd15,bollinger50sd15,bollinger60sd15,bollinger70sd15,bollinger80sd15,bollinger90sd15,bollinger100sd15,
               bollinger1sd2 , bollinger2sd2 , bollinger3sd2 , bollinger5sd2 ,bollinger10sd2 ,bollinger15sd2 , bollinger20sd2 ,bollinger30sd2 ,bollinger40sd2 ,bollinger50sd2 ,bollinger60sd2 ,bollinger70sd2 ,bollinger80sd2 ,bollinger90sd2 ,bollinger100sd2 )

  return(results)
}


FitTRENDModel <- function(myRow){
  myRowZoo <- zoo(myRow)
  ## use underlying time scale for interpolation
  myRowZoo <- na.approx(myRowZoo) 
  TREND2 <- myRow[length(myRow)]-SMA(myRowZoo,2)
  TREND3 <- myRow[length(myRow)]-SMA(myRowZoo,3)
  TREND5 <- myRow[length(myRow)]-SMA(myRowZoo,5)
  TREND10 <- myRow[length(myRow)]-SMA(myRowZoo,10)
  TREND15 <- myRow[length(myRow)]-SMA(myRowZoo,15)
  TREND20 <- myRow[length(myRow)]-SMA(myRowZoo,20)
  TREND30 <- myRow[length(myRow)]-SMA(myRowZoo,30)
  TREND40 <- myRow[length(myRow)]-SMA(myRowZoo,40)
  TREND50 <- myRow[length(myRow)]-SMA(myRowZoo,50)
  TREND60 <- myRow[length(myRow)]-SMA(myRowZoo,60)
  TREND70 <- myRow[length(myRow)]-SMA(myRowZoo,70)
  TREND80 <- myRow[length(myRow)]-SMA(myRowZoo,80)
  TREND90 <- myRow[length(myRow)]-SMA(myRowZoo,90)
  TREND100 <- myRow[length(myRow)]-SMA(myRowZoo,100)
  return(c(TREND2,TREND3,TREND5,TREND10,TREND20,TREND30,TREND40,TREND50,TREND60,TREND70,TREND80,TREND90,TREND100))
}

Fit1Model <- function(myRow){

  x <- 1:length(myRow)
  y <- CumFromRetToPricesStart(t(myRow))
  x <- c(0,x)
  y <- c(0,y) 
  #   plot(x,y)
  #   fit  <- lm(y~x)
  #   #second degree
  #   fit2 <- lm(y~poly(x,2,raw=TRUE))
  #   #third degree
  #   fit3 <- lm(y~poly(x,3,raw=TRUE))
  #fourth degree
  fit1 <- lm(y~poly(x,1,raw=TRUE)-1)
  #generate range of 50 numbers starting from 30 and ending at 160
  coefs1 <- coef(fit1)
  # my_4_prediction <- coefs4[5]*x^4+coefs4[4]*x^3+coefs4[3]*x^2+coefs4[2]*x^1+coefs4[1]*x^0
  return(coefs1)
}
