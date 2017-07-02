# Data collection and flat file saving for all graphics
library("RPToolsDB")
library("abind")
library("RPPlotUtils")
library("RPostgreSQL")
library("sqldf")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")

# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

Sys.setenv(TZ = "UTC")


##### draft frequency
load(file =paste0(outputDataPath, "Fourier.RData"))

PlotRows <- function (DataFrame, XLab = "", YLab = "", Title = "", Legend = FALSE, AxisIncluded = FALSE, 
          byRows = FALSE, spread = FALSE, pointOnly = FALSE, lineOnly = FALSE, 
          FullExportingPath = NULL) 
{
  if (byRows) 
    DataFrame = t(DataFrame)
  if (is.null(colnames(DataFrame))) 
    colnames(DataFrame) <- paste0("COLUMN_", seq(1, dim(DataFrame)[2]))
  if (class(DataFrame) == "matrix") 
    DataFrame <- as.data.frame(DataFrame)
  if (AxisIncluded) {
    my_column_to_plot_against <- colnames(DataFrame)[1]
  }
  else {
    my_column_to_plot_against <- "TO_PLOT_AGAINST"
    DataFrame$TO_PLOT_AGAINST <- as.numeric(seq(1, dim(DataFrame)[1]))
  }
  ToPlotDataFrame <- melt(DataFrame, my_column_to_plot_against)
  g <- ggplot(ToPlotDataFrame, aes(x = ToPlotDataFrame[, c(my_column_to_plot_against)], 
                                   y = value, group = variable, color = variable, fill = variable))
  if (pointOnly) {
    g <- g + geom_point()
  }
  else if (lineOnly) {
    g <- g + geom_line()
  }
  else {
    g <- g + geom_line() + geom_point()
  }
  g <- g + xlab(XLab) + ylab(YLab) + ggtitle(Title) + theme(title = element_text(size = 16, 
                                                                                 face = "bold")) + theme(axis.text.x = element_text(size = 14)) 
  if(Legend){
    g <- g + theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
                      legend.text = element_text(size = 16)) + theme(legend.position = "bottom", 
                                                                     legend.title = element_blank())
  } else {
    g <- g + theme(legend.position = "none")
  }

  if (spread) 
    g <- g + facet_wrap(~variable)
  if ("DATE" == my_column_to_plot_against) 
    g <- g + scale_x_date()
  if (!is.null(FullExportingPath)) 
    RP_ExportPlot(g, FullExportingPath, "")
  return(g)
}

investigateTS <- function(indice, dates, features){

  to_compute <- as.vector(as.matrix(features[indice,1:12]))
  
  coefs <- ar(to_compute, method = "ols")
  coefs$ar[2][1][1]
  coefs$ar[1][1][1]

  library(stats)
  test <- fft(to_compute) 
  
  X.k <- fft(to_compute)  
  modules  <- cbind(0:(length(X.k)-1), Mod(X.k))
  modules[2:length(X.k),2] <- 2*modules[2:length(X.k),2] # find all harmonics with fft()
  
#   plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
#     plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
#     
#     # TODO: why this scaling is necessary?
#     plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
#     
#     plot(plot.data, t="h", lwd=2, main="", 
#          xlab="Frequency (Hz)", ylab="Strength", 
#          xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
#   }
  # plot.frequency.spectrum(X.k, xlimits=c(0,20))
  
  dates_indice <- which(!is.na(dates[indice,]))
  features_indice <- which(!is.na(features[indice,]))
  if (!identical(dates_indice,features_indice)){
    return (NA)
  } else {
    epsilon <- rep(0.001,length(dates[indice,dates_indice]))
    dates[indice,dates_indice] <- dates[indice,dates_indice]+cumsum(epsilon)
    plot_df <- data.frame(ABS=as.vector(as.matrix(dates[indice,dates_indice])) ,ORD= as.vector(as.matrix(features[indice,features_indice])))
    PlotRows(DataFrame = plot_df,AxisIncluded = TRUE)
    print("plot done")
  }
}

row_index <- data.frame(indices = 1:dim(com_dats)[1])
com_dats <- com_dats[,-c(1)]
com_nums <- com_nums[,-c(1)]
apply(X = row_index, MARGIN = 1, FUN = function(x){investigateTS(x, com_dats, com_nums)})
