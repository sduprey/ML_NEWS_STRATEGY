#####################
##################### outputing paper graphics
library("RPToolsDB")
library(shiny)
library(DT)
library("RPPlotUtils")
library(dplyr)

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")



source("./RCode/RP_BigData_EventStudy_Utilities.R")

################################
################################
################################
################################
################################
################################
################################ Plotting functions

RP_PlotCIInterval <- function(DataFrame, Title = "", FullExportingPath=NULL) {
  g <- ggplot(DataFrame, aes(MINUTES, MEAN))+
    geom_point()+
    geom_line(data=DataFrame,size=1.2)+
    geom_ribbon(data=DataFrame,aes(ymin=CI_LOW,ymax=CI_HIGH),alpha=0.3)+
    xlab("Minute Lags") + 
    ylab("BPS cumulated minute volume") +
    ggtitle(Title)+
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold")) +
    theme(plot.title = element_text(size = 25, face = "bold"))
  
  if (!is.null(FullExportingPath)){
    RP_ExportPlot(g,FullExportingPath,"")
  }
  return(g)
}

outputGraphicsBestProfileCI <- function( data,  Russell_version = "R1000", type = "RET"){
  toplotDF <- NULL
  MyTitle <- ""
  if (type == "VOL"){
    toplotDF<- data[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
    colnames(toplotDF) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    MyTitle <- "Abnormal volatility"
    
    toplotDF <- toplotDF+0.8
    toplotDF$MINUTES <- toplotDF$MINUTES - 0.8
  }
  
  if(type =="RET"){
    toplotDF<- data[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDF) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    MyTitle <- "Abnormal returns"
  }
  
  if(type =="VOLU"){
    toplotDF<- data[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
    colnames(toplotDF) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    MyTitle <- "Abnormal volume"
    
    toplotDF <- toplotDF+0.8
    toplotDF$MINUTES <- toplotDF$MINUTES - 0.8
  }
  
  if(type =="STATS"){
    toplotDF<- data[,c("MINUTES","SIGNIFICANCE_CI_LOW","SIGNIFICANCE","SIGNIFICANCE_CI_HIGH")]
    colnames(toplotDF) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    MyTitle <- "Statistical significance"
  }
  
  
  g2 <- RP_PlotCIInterval(DataFrame = toplotDF, Title = MyTitle, FullExportingPath =  NULL)
  return(g2)
}

RP_PlotProfile <- function(rowProfile){
  stats_sign <- colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))]
  rets <- paste0("RET",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  ord_stats_sign <- paste0("ORD",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  vol_stats_sign <- paste0("VOLU",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  vola_stats_sign <- paste0("VOLA",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  
  stats_sign <- rowProfile[,stats_sign]
  rets <- rowProfile[,rets]
  colnames(rets) <- colnames(stats_sign)
  ord_stats_sign <- rowProfile[,ord_stats_sign]
  colnames(ord_stats_sign) <- colnames(stats_sign)
  vol_stats_sign <- rowProfile[,vol_stats_sign]
  colnames(vol_stats_sign) <- colnames(stats_sign)
  vola_stats_sign <- rowProfile[,vola_stats_sign]
  colnames(vola_stats_sign) <- colnames(stats_sign)
  
  
  
  # dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  
  colnames(dataframe) <- c("SIGNIFICANCE(RANK)_CI_LOW","SIGNIFICANCE(RANK)","SIGNIFICANCE(RANK)_CI_HIGH","SIGNIFICANCE_CI_LOW","SIGNIFICANCE","SIGNIFICANCE_CI_HIGH","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframe$MINUTES <- as.numeric(colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  dataframe <- dataframe[,c("MINUTES","SIGNIFICANCE(RANK)_CI_LOW","SIGNIFICANCE(RANK)","SIGNIFICANCE(RANK)_CI_HIGH","SIGNIFICANCE_CI_LOW","SIGNIFICANCE","SIGNIFICANCE_CI_HIGH","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  gret <- outputGraphicsBestProfileCI(dataframe,Russell_version = "R1000", type = "RET")
  gvol <- outputGraphicsBestProfileCI(dataframe,Russell_version = "R1000", type = "VOL")
  gvolu <- outputGraphicsBestProfileCI(dataframe,Russell_version = "R1000", type = "VOLU")
  gstats <- outputGraphicsBestProfileCI(dataframe,Russell_version = "R1000", type = "STATS")
  
  
  return(list(gret=gret,gvol=gvol,gvolu=gvolu,gstats=gstats))
}

differentiateRets <- function(toDF){
  newToDF <- toDF
  for (i in 2:dim(toDF)[2]){
    newToDF[,i] <- toDF[,i] - toDF[,(i-1)] 
  }
  newToDF[,1] <- newToDF[,2]
  return(newToDF)
}

PlotDataFrame <- function (DataFrame, XLab = "", YLab = "", Title = "", AxisIncluded = FALSE, 
                           byRows = FALSE, spread = FALSE, pointOnly = FALSE, lineOnly = TRUE, percent=FALSE,
                           FullExportingPath = NULL, ylim = FALSE) 
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
  ToPlotDataFrame <- ToPlotDataFrame[(ToPlotDataFrame$variable != "ABNORMAL_THRESHOLD"),]
  ToPlotDataFrame <- ToPlotDataFrame[(ToPlotDataFrame$variable != "threshold"),]
  
  
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
  
  if ("threshold" %in% colnames(DataFrame)){
    g <- g + geom_hline(aes(yintercept=0.95),colour = 'black', size = 1.5,linetype="dashed")
  }
  
  if ("ABNORMAL_THRESHOLD" %in% colnames(DataFrame)){
    g <- g + geom_hline(aes(yintercept=0.95),colour = 'black', size = 1.5,linetype="dashed")
  }
  g <- g + xlab(XLab) + ylab(YLab) + ggtitle(Title) + theme(title = element_text(size = 16, 
                                                                                 face = "bold")) + theme(axis.text.x = element_text(size = 14)) + 
    theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
          legend.text = element_text(size = 16)) + theme(legend.position = "bottom", 
                                                         legend.title = element_blank())+theme(axis.text=element_text(size=14),
                                                                                               axis.title=element_text(size=16,face="bold"))
  if (spread) 
    g <- g + facet_wrap(~variable)
  
  if (percent){
    g <- g +   scale_y_continuous(labels = percent_format(),limits = c(-0, 1))  
  }
  
  if (percent & ylim){
    g <- g +   scale_y_continuous(labels = percent_format(),limits = c(0.5, 0.9)) 
  }
  
  
  
  if ("DATE" == my_column_to_plot_against) 
    g <- g + scale_x_date()
  if (!is.null(FullExportingPath)) 
    RP_ExportPlot(g, FullExportingPath, "")
  return(g)
}

outputGraphicsBestProfileStats <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE, ylim = FALSE){
  
  if(Together){
    dataframestats <- dataFrame[,c("MINUTES","RDBA_COR_STATS_SIGN","RDBA_ORD_STATS_SIGN","RPNA_COR_STATS_SIGN","RPNA_ORD_STATS_SIGN")]
    colnames(dataframestats) <- c("MINUTES","RDBA_SIGNIFICANCE(RANK)","RDBA_SIGNIFICANCE","RPNA_SIGNIFICANCE(RANK)","RPNA_SIGNIFICANCE")
  } else {
    dataframestats <- dataFrame[,c("MINUTES","COR_STATS_SIGN","ORD_STATS_SIGN")]
    colnames(dataframestats) <- c("MINUTES","SIGNIFICANCE(RANK)","SIGNIFICANCE")
  }
  
  
  
  significance_threshold <- 1 - 0.05
  dataframestats$ABNORMAL_THRESHOLD <- significance_threshold
  
  dataframestats[is.na(dataframestats)] <- 0
  
  g1 <- PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",percent= TRUE, Title = paste0(my_event," statistical significance"), FullExportingPath = NULL, ylim = ylim)
  # g2 <- RP_PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  return(g1)
}

outputGraphicsBestProfileRets <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
  if(Together){
    dataframerets <- dataFrame[,c("MINUTES","RDBA_RETS","RPNA_RETS")]
    colnames(dataframerets) <- c("MINUTES","RDBA_RETURNS","RPNA_RETURNS")
    # dataframerets$RDBA_RETURNS <- 10*dataframerets$RDBA_RETURNS
    # dataframerets$RPNA_RETURNS <- 10*dataframerets$RPNA_RETURNS
  } else {
    dataframerets <- dataFrame[,c("MINUTES","RETS")]
    colnames(dataframerets) <- c("MINUTES","RETURNS")
    # dataframerets$RETURNS <- 10*dataframerets$RETURNS
  }
  
  
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = paste0(my_event," returns"), FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}

outputGraphicsBestProfileVol <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
  if(Together){
    dataframevol<- dataFrame[,c("MINUTES","RDBA_VOLUME","RPNA_VOLUME")]
  } else {
    dataframevol<- dataFrame[,c("MINUTES","VOLUME")]
  }
  
  
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataframevol,AxisIncluded = T,XLab = "Minute Lags",YLab = "Volume in billion dollars ",Title = paste0(my_event," abnormal volume"), FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}

outputGraphicsBestProfileVola <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
  if(Together){
    dataframervola<- dataFrame[,c("MINUTES","RDBA_VOLATILITY","RPNA_VOLATILITY")]
  } else {
    dataframervola<- dataFrame[,c("MINUTES","VOLATILITY")]
  }
  
  
  g2 <- PlotDataFrame(dataframervola,AxisIncluded = T,XLab = "Minute Lags",YLab = "Abnormal volatility ratio",Title = paste0(my_event," abnormal volatility"), FullExportingPath = NULL)
  
  return(g2)
  
}

outputTogetherGraphicsBestProfileStats <- function(event_one, event_two,  dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
  if(Together){
    dataframestats <- dataFrame[,c("MINUTES",paste0(event_one,c("COR_STATS_SIGN_1","ORD_STATS_SIGN_1")),paste0(event_two,c("COR_STATS_SIGN_2","ORD_STATS_SIGN_2")))]
    colnames(dataframestats) <- c("MINUTES",paste0(event_one,c("_rank_1","_stat_1")),paste0(event_two,c("_rank_2","_stat_2")))
  } else {
    dataframestats <- dataFrame[,c("MINUTES","COR_STATS_SIGN","ORD_STATS_SIGN")]
    colnames(dataframestats) <- c("MINUTES","SIGNIFICANCE(RANK)","SIGNIFICANCE")
  }
  
  
  
  significance_threshold <- 1 - 0.05
  dataframestats$threshold <- significance_threshold
  
  dataframestats[is.na(dataframestats)] <- 0
  
  g1 <- PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",percent= TRUE, Title = "Statistical Significance", FullExportingPath = NULL)
  # g2 <- RP_PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  return(g1)
}

outputTogetherGraphicsBestProfileRets <- function(event_one, event_two,  dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
  if(Together){
    dataframerets <- dataFrame[,c("MINUTES",paste0(event_one,"RETS_1"),paste0(event_two,"RETS_2"))]
    colnames(dataframerets) <- c("MINUTES",paste0(event_one,"_return_1"),paste0(event_two,"_return_2"))
    # dataframerets[,c("MINUTES",paste0(event_one,"_return_1"),paste0(event_two,"_return_2"))] <- 10*dataframerets[,c("MINUTES",paste0(event_one,"_return_1"),paste0(event_two,"_return_2"))]
  } else {
    dataframerets <- dataFrame[,c("MINUTES","RETS")]
    colnames(dataframerets) <- c("MINUTES","RETURNS")
    # dataframerets$RETURNS <- 10*dataframerets$RETURNS
  }
  
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = "Abnormal Returns", FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}

outputTogetherGraphicsBestProfileVol <- function(event_one, event_two,  dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
  if(Together){
    dataframevol <- dataFrame[,c("MINUTES",paste0(event_one,"VOLUME_1"),paste0(event_two,"VOLUME_2"))]
    colnames(dataframevol) <- c("MINUTES",paste0(event_one,"_volume_1"),paste0(event_two,"_volume_2"))
  } else {
    dataframevol<- dataFrame[,c("MINUTES","VOLUME")]
  }
  
  
  
  
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataframevol,AxisIncluded = T,XLab = "Minute Lags",YLab = "Volume in billion dollars ",Title = "Abnormal Volume", FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}

outputTogetherGraphicsBestProfileVola <-  function(event_one, event_two,  dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  if(Together){
    dataframervola <- dataFrame[,c("MINUTES",paste0(event_one,"VOLATILITY_1"),paste0(event_two,"VOLATILITY_2"))]
    colnames(dataframervola) <- c("MINUTES",paste0(event_one,"_volatility_1"),paste0(event_two,"_volatility_2"))
  } else {
    dataframervola<- dataFrame[,c("MINUTES","VOLATILITY")]
  }
  
  g2 <- PlotDataFrame(dataframervola,AxisIncluded = T,XLab = "Minute Lags",YLab = "Abnormal volatility ratio",Title = "Abnormal Volatility", FullExportingPath = NULL)
  
  return(g2)
}


ExportMultiplot <- function (..., plotlist = NULL, filename, outputDataPath, cols = 1, 
                             width, height, layout = NULL) 
{
  filename <- paste(outputDataPath, filename, ".png", sep = "")
  png(file = filename, 
      width = width * 100, height = height * 100)
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  }
  else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                                      layout.pos.col = matchidx$col))
    }
  }
  dev.off()
  return(filename)
}


getPlotBestProfiles <- function(aggregate_criteria, data, all_group_events, all_category_events,suffix){
  plotLimit <- 6
  if (aggregate_criteria == "GROUP"){
    plotLimit <- 6
    all_events <- all_group_events
  } else {
    plotLimit <- 16
    all_events <- all_category_events
  }
  
  for (i in 1:min(plotLimit, dim(data)[1])){
    rowProfile <- data[i,]
    stats_sign <- colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))]
    rets <- paste0("RET",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
    ord_stats_sign <- paste0("ORD",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
    vol_stats_sign <- paste0("VOLU",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
    vola_stats_sign <- paste0("VOLA",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
    
    stats_sign <- rowProfile[,stats_sign]
    rets <- rowProfile[,rets]
    colnames(rets) <- colnames(stats_sign)
    ord_stats_sign <- rowProfile[,ord_stats_sign]
    colnames(ord_stats_sign) <- colnames(stats_sign)
    vol_stats_sign <- rowProfile[,vol_stats_sign]
    colnames(vol_stats_sign) <- colnames(stats_sign)
    vola_stats_sign <- rowProfile[,vola_stats_sign]
    colnames(vola_stats_sign) <- colnames(stats_sign)
    
    dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
    colnames(dataframe) <- c("COR_STATS_SIGN","ORD_STATS_SIGN","VOLUME","VOLATILITY","RETS")
    dataframe$MINUTES <- as.numeric(colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
    dataframe <- dataframe[,c("MINUTES","COR_STATS_SIGN","ORD_STATS_SIGN","VOLUME","VOLATILITY","RETS")]
    
    
    gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
    
    gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
    assign(paste0("gret", i), gret)
    assign(paste0("gvol", i), gvol)
  }
  width=20
  height=22.5
  gname <- NULL
  if (aggregate_criteria == "GROUP"){
    gname <- ExportMultiplot(gret1,gvol1,gret2,gvol2,gret3,gvol3,gret4,gvol4,gret5,gvol5,plotlist = NULL, filename = paste0("bestGroups",suffix), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  } else {
    gname <- ExportMultiplot(gret1,gvol1,
                             gret2,gvol2,
                             gret3,gvol3,
                             gret4,gvol4,
                             gret5,gvol5,
                             gret6,gvol6,
                             gret7,gvol7,
                             gret8,gvol8,
                             gret9,gvol9,
                             gret10,gvol10,
                             plotlist = NULL, filename = paste0("bestCategories",suffix), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  }
  # print("multiple plot done")
  return(gname)
  #   
  #   Counter <- 1
  #   Events <- NULL
  #   i <- 1
  #   fo
  #   
  #   while (Counter < plotLimit & i <= dim(data)[1]){
  #     rowProfile <- rpna_dataTotr1000[i,]
  #     stats_sign <- colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))]
  #     rets <- paste0("RET",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     ord_stats_sign <- paste0("ORD",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     vol_stats_sign <- paste0("VOLU",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     vola_stats_sign <- paste0("VOLA",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     
  #     stats_sign <- rowProfile[,stats_sign]
  #     rets <- rowProfile[,rets]
  #     colnames(rets) <- colnames(stats_sign)
  #     ord_stats_sign <- rowProfile[,ord_stats_sign]
  #     colnames(ord_stats_sign) <- colnames(stats_sign)
  #     vol_stats_sign <- rowProfile[,vol_stats_sign]
  #     colnames(vol_stats_sign) <- colnames(stats_sign)
  #     vola_stats_sign <- rowProfile[,vola_stats_sign]
  #     colnames(vola_stats_sign) <- colnames(stats_sign)
  #     
  #     dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  #     colnames(dataframe) <- c("COR_STATS_SIGN","ORD_STATS_SIGN","VOLUME","VOLATILITY","RETS")
  #     dataframe$MINUTES <- as.numeric(colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     dataframe <- dataframe[,c("MINUTES","COR_STATS_SIGN","ORD_STATS_SIGN","VOLUME","VOLATILITY","RETS")]
  #     
  #     
  #     gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
  #     
  #     gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
  #     
  #     # eval.parent(expr = paste0("g", i, " <- g"))
  #     
  #     if((rowProfile$EVENT %in% all_events) & Counter < plotLimit){
  #       if (Counter == 1){
  #         assign(paste0("gret", Counter), gret)
  #         assign(paste0("gvol", Counter), gvol)
  #         Counter <- Counter+1
  #         Events <- c(Events,rowProfile$EVENT)
  #       } else if (!(rowProfile$EVENT %in% Events)){
  #         assign(paste0("gret", Counter), gret)
  #         assign(paste0("gvol", Counter), gvol)
  #         Counter <- Counter+1
  #         Events <- c(Events,rowProfile$EVENT)
  #       }
  #     }
  #     
  #     print(Counter)
  #     print(i)
  #     #     width=15
  #     #     height=9
  #     #     gout <- RP_ExportMultiplePlot(g,g, plotlist = NULL, filename = "best", outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 2, width = width, height = height)
  #     #     print(gout)
  #     print("profile displayed")
  #     i <- i+1
  #     
  #   }
  #   
  #   print("multiple plot aggregating done")
  #   width=15
  #   height=9
  #   g <- NULL
  #   if (aggregate_criteria == "GROUP"){
  #     g <- ExportMultiplot(gret1,gvol1,gret2,gvol2,gret3,gvol3,gret4,gvol4,gret5,gvol5,plotlist = NULL, filename = paste0("bestGroups"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  #   } else {
  #     g <- ExportMultiplot(gret1,gvol1,
  #                          gret2,gvol2,
  #                          gret3,gvol3,
  #                          gret4,gvol4,
  #                          gret5,gvol5,
  #                          gret6,gvol6,
  #                          gret7,gvol7,
  #                          gret8,gvol8,
  #                          gret9,gvol9,
  #                          gret10,gvol10,
  #                          plotlist = NULL, filename = paste0("bestCategories"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  #   }
  #   print("multiple plot done")
  #   return(g)
}

################################
################################
################################
################################
################################
################################
################################ End of Plotting functions



library("boot")

data <- readRDS(file=paste0(outputDataPath,"metrics_clean_prod_spr_r1000_bigdata_abvol_abvol_corrado_df.rds"))

my_metrics <- 
  c(
    #     "card_post_return",
    #     "card_post_ranked_return",
    #     "post_return",
    "post_ranked_return"
    # "post_volatility",
    # "post_ranked_volatility"
    #     "card_pre_return",
    #     "card_pre_ranked_return",
    #     "pre_return",
    #     "pre_ranked_return",
    #     "pre_volatility",
    #     "pre_ranked_volatility",
    #     "volatility_correction",
    #     "ranked_volatility_correction",
    #     "return_correction",
    #     "ranked_return_correction",
    #     "card_return_correction",
    #     "card_ranked_return_correction"
  )

dataGroup <- data[data$aggregate_criteria =="GROUP",]
dataCategory <- data[data$aggregate_criteria =="CATEGORY",]

trimBest <- function(dfrow,metrics_to_use){
  trimmed_df <- dfrow[dfrow[,metrics_to_use] >= max(dfrow[,metrics_to_use]),]
  toReturn <- trimmed_df[1,]
  stats_prepost <- colnames(dfrow)[which(as.numeric(colnames(dfrow))>= -180)]
  ret_prepost <- paste0("RET",stats_prepost)
  if(toReturn$sentiment == "NEGATIVE"){
    toReturn[,ret_prepost] <- -toReturn[,ret_prepost]
  }
  return(toReturn)
}

computeCIbound <- function(my_return_vector){
  print("entering")
  my_return_vector <- my_return_vector[!is.na(my_return_vector)]
  if(!(sum(my_return_vector) ==0)){
    
    theta.boot.mean <- boot(my_return_vector, bootThetaMean, R=2000) 
    mean_ci <- boot.ci(theta.boot.mean, conf=0.9)
    return(c(mean_ci$normal[2],mean(my_return_vector),mean_ci$normal[3]))  
  } else {
    return(c(0,0,0))
  }
}

for(my_metric in my_metrics){
  print(my_metric)
  dataGroupBest  <- ddply(.data = dataGroup,.variables = "my_event",.fun = function(x){trimBest(x,my_metric)})
  print("after")
  print(dim(dataGroupBest))
  
  
  stats_prepost <- colnames(dataGroupBest)[which(as.numeric(colnames(data))>= -180)]
  
  ord_prepost <- paste0("ORD",stats_prepost)
  ret_prepost <- paste0("RET",stats_prepost)
  vol_prepost <- paste0("VOLA",stats_prepost)
  volu_prepost <- paste0("VOLU",stats_prepost)  
  
  
  allNumericColumns <- c(stats_prepost,ord_prepost,ret_prepost,vol_prepost,volu_prepost)
  allRetsColumns <- ret_prepost
  
  
  my_df <- dataGroupBest[,allNumericColumns]
  my_diff_df <- differentiateRets(dataGroupBest[,allRetsColumns])
  
  print("computing the CI interval")
  event_minutes_ci_matrix <- apply(my_df,2,FUN=computeCIbound)
  print("diff return CI")
  event_minutes_diff_ci_matrix <- apply(my_diff_df,2,FUN=computeCIbound)
  print("CI done")
  
  rowProfile <- dataGroupBest[1:3,,drop=FALSE]
  rowProfile[1,allNumericColumns] <- event_minutes_ci_matrix[1,]
  rowProfile[2,allNumericColumns] <- event_minutes_ci_matrix[2,]
  rowProfile[3,allNumericColumns] <- event_minutes_ci_matrix[3,]
  
  
  rowProfile[1,allRetsColumns] <-  event_minutes_ci_matrix[2,allRetsColumns] - abs(event_minutes_diff_ci_matrix[1,])
  # rowProfile[2,allRetsColumns] <- event_minutes_ci_matrix[2,]
  rowProfile[3,allRetsColumns] <- event_minutes_ci_matrix[2,allRetsColumns] + abs(event_minutes_diff_ci_matrix[3,])
  
  print("outputing an average graphic for our metric group")
  results <- RP_PlotProfile(rowProfile)
  RP_ExportPlot(results$gret,outputDataPath = outputDataPath,filename = "PAPER_PICTURES/bigdata_returns_group_average_profile_ci")
  print(results$gret)
  RP_ExportPlot(results$gvol,outputDataPath = outputDataPath,filename = "PAPER_PICTURES/bigdata_volatility_group_average_profile_ci")
  print(results$gvol)  
  RP_ExportPlot(results$gvolu,outputDataPath = outputDataPath,filename = "PAPER_PICTURES/bigdata_volume_group_average_profile_ci")
  print(results$gvolu)  
  RP_ExportPlot(results$gstats,outputDataPath = outputDataPath,filename = "PAPER_PICTURES/bigdata_significance_group_average_profile_ci")
  print(results$gstats)  
  
  
  
  ####### computing the average statistical conf
  print("Plotting the average statistical profile")
  ord_prepost <- paste0("ORD",stats_prepost)
  best_avg_stats_ORD_profile <- colMeans(dataGroupBest[,ord_prepost],na.rm=FALSE)
  best_avg_stats_profile  <- colMeans(dataGroupBest[,stats_prepost],na.rm=FALSE)
  
  rowProfileSTATS <- dataGroupBest[1,,drop=FALSE]
  rowProfileSTATS[1,stats_prepost] <- best_avg_stats_profile
  rowProfileSTATS[1,ord_prepost] <- best_avg_stats_ORD_profile
  
  ##########################
  ##########################
  ##########################
  stats_sign <- colnames(rowProfileSTATS)[which(!is.na(as.numeric(colnames(rowProfileSTATS))))]
  rets <- paste0("RET",colnames(rowProfileSTATS)[which(!is.na(as.numeric(colnames(rowProfileSTATS))))])
  ord_stats_sign <- paste0("ORD",colnames(rowProfileSTATS)[which(!is.na(as.numeric(colnames(rowProfileSTATS))))])
  vol_stats_sign <- paste0("VOLU",colnames(rowProfileSTATS)[which(!is.na(as.numeric(colnames(rowProfileSTATS))))])
  vola_stats_sign <- paste0("VOLA",colnames(rowProfileSTATS)[which(!is.na(as.numeric(colnames(rowProfileSTATS))))])
  
  stats_sign <- rowProfileSTATS[,stats_sign]
  rets <- rowProfileSTATS[,rets]
  colnames(rets) <- colnames(stats_sign)
  ord_stats_sign <- rowProfileSTATS[,ord_stats_sign]
  colnames(ord_stats_sign) <- colnames(stats_sign)
  vol_stats_sign <- rowProfileSTATS[,vol_stats_sign]
  colnames(vol_stats_sign) <- colnames(stats_sign)
  vola_stats_sign <- rowProfileSTATS[,vola_stats_sign]
  colnames(vola_stats_sign) <- colnames(stats_sign)
  
  dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  colnames(dataframe) <- c("COR_STATS_SIGN","ORD_STATS_SIGN","VOLUME","VOLATILITY","RETS")
  dataframe$MINUTES <- as.numeric(colnames(rowProfileSTATS)[which(!is.na(as.numeric(colnames(rowProfileSTATS))))])
  dataframe <- dataframe[,c("MINUTES","COR_STATS_SIGN","ORD_STATS_SIGN","VOLUME","VOLATILITY","RETS")]
  
  
  g <- outputGraphicsBestProfileStats(rowProfileSTATS$product_criteria,rowProfileSTATS$aggregate_criteria,rowProfileSTATS$sentiment_criteria,rowProfileSTATS$similarity_gap_filter,rowProfileSTATS$ens_filter,rowProfileSTATS$event_number_event_filtering, rowProfileSTATS$gics_sector, rowProfileSTATS$EVENT, rowProfileSTATS$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000",ylim = TRUE)
  print(g)
  # Render your graph
  RP_ExportPlot(gplot = g,outputDataPath = outputDataPath,filename = paste0("PAPER_PICTURES/bigdata_group_stat_",my_metric))   
  print("statistical plotting")
  
  
  
  dataCategoryBest  <- ddply(.data = dataCategory,.variables = "my_event",.fun = function(x){trimBest(x,my_metric)})
  dataCategoryBest$RANKING <- dataCategoryBest[,my_metric]
  
  
  
  
  #################### bootstrapping the mean
  print("bootstrapping the confidence interval")
  
  
  
  ############# problem in the code before
  # dataCategoryBest[,allRetsColumns] <- dataCategoryBest[,allRetsColumns]*10
  
  my_df <- dataCategoryBest[,allNumericColumns]
  my_diff_df <- differentiateRets(dataCategoryBest[,allRetsColumns])
  
  print("computing the CI interval")
  event_minutes_ci_matrix <- apply(my_df,2,FUN=computeCIbound)
  print("diff return CI")
  event_minutes_diff_ci_matrix <- apply(my_diff_df,2,FUN=computeCIbound)
  print("CI done")
  
  rowProfile <- dataCategoryBest[1:3,,drop=FALSE]
  rowProfile[1,allNumericColumns] <- event_minutes_ci_matrix[1,]
  rowProfile[2,allNumericColumns] <- event_minutes_ci_matrix[2,]
  rowProfile[3,allNumericColumns] <- event_minutes_ci_matrix[3,]
  
  rowProfile[1,allRetsColumns] <-  event_minutes_ci_matrix[2,allRetsColumns] - abs(event_minutes_diff_ci_matrix[1,])
  # rowProfile[2,allRetsColumns] <- event_minutes_ci_matrix[2,]
  rowProfile[3,allRetsColumns] <- event_minutes_ci_matrix[2,allRetsColumns] + abs(event_minutes_diff_ci_matrix[3,])
  
  print("outputing an average graphic for our metric category")
  results <- RP_PlotProfile(rowProfile)
  print("outputing an average graphic for our metric group")
  results <- RP_PlotProfile(rowProfile)
  RP_ExportPlot(results$gret,outputDataPath = outputDataPath,filename = "PAPER_PICTURES/bigdata_returns_category_average_profile_ci")
  print(results$gret)
  RP_ExportPlot(results$gvol,outputDataPath = outputDataPath,filename = "PAPER_PICTURES/bigdata_volatility_category_average_profile_ci")
  print(results$gvol)  
  RP_ExportPlot(results$gvolu,outputDataPath = outputDataPath,filename = "PAPER_PICTURES/bigdata_volume_category_average_profile_ci")
  print(results$gvolu)  
  RP_ExportPlot(results$gstats,outputDataPath = outputDataPath,filename = "PAPER_PICTURES/bigdata_significance_category_average_profile_ci")
  print(results$gstats) 
  ##########################
  ##########################
  ##########################
  ########################## plotting the average profile
  
}
###########
########### Fro groups




print("best profiles graphics computed")

