#####################
##################### outputing paper graphics
# library("RPToolsDB")
# library(shiny)
# library(DT)
# library("RPPlotUtils")
# library(dplyr)
library(plyr)
library(ggplot2)
library("lubridate")
# library("ggplot2")

library("ggthemes")
library("grid")
require(plyr)
require(reshape2)
library(scales)

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
C

outputDataPath <-  "C://My_Data/NAR_326/"

source("./RCode/RP_BigData_EventStudy_Utilities.R")

################################
################################
################################
################################
################################
################################
################################ Plotting functions

RP_ExportPlot <- function(gplot, outputDataPath, filename, width=15, height=9) {
  png(file = paste(outputDataPath, filename, '.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}

RP_PlotCIInterval <- function(DataFrame, Title = "", FullExportingPath=NULL) {
  g <- ggplot(DataFrame, aes(MINUTES, MEAN))+
    geom_point()+
    geom_line(data=DataFrame)+
    geom_ribbon(data=DataFrame,aes(ymin=CI_LOW,ymax=CI_HIGH),alpha=0.3)+
    xlab("Minute Lags") + 
    ylab("BPS cumulated minute volume") +
    ggtitle(Title)
  
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
  dataframe <- as.data.frame(t(rbind(vol_stats_sign,vola_stats_sign,rets)))
  
  colnames(dataframe) <- c("VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframe$MINUTES <- as.numeric(colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  dataframe <- dataframe[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  gret <- outputGraphicsBestProfileCI(dataframe,Russell_version = "R1000", type = "RET")
  gvol <- outputGraphicsBestProfileCI(dataframe,Russell_version = "R1000", type = "VOL")
  gvolu <- outputGraphicsBestProfileCI(dataframe,Russell_version = "R1000", type = "VOLU")
  
  return(list(gret=gret,gvol=gvol,gvolu=gvolu))
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
                           FullExportingPath = NULL, xlim = FALSE,minZoom = FALSE) 
{
  xlim <- FALSE
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
    g <- g + geom_line(size=1.5,alpha=1)
  }
  else {
    g <- g + geom_line(size=1.5,alpha=1) + geom_point()
  }
  
  if ("threshold" %in% colnames(DataFrame)){
    g <- g + geom_hline(aes(yintercept=0.95),colour = 'black', size = 1.5,linetype="dashed")
  }
  
  if ("ABNORMAL_THRESHOLD" %in% colnames(DataFrame)){
    g <- g + geom_hline(aes(yintercept=0.95),colour = 'black', size = 1.5,linetype="dashed")
  }
  g <- g + xlab(XLab) + ylab(YLab) + 
    scale_x_continuous(breaks=c(-180,-120,-60,0,60,120,180))+
    # ggtitle(Title)+
    # theme(title = element_text(size = 28, face = "bold")) + 
    theme(axis.text.x = element_text(size = 30)) + 
    theme(axis.title.x = element_text(size = 30)) +
    theme(axis.text.y = element_text(size = 30)) + 
    theme(axis.title.y = element_text(size = 30))+
    theme(legend.position="none")
  if (Title == "Abnormal returns"){
    if (web_only){
      g <- g + scale_y_continuous(breaks=c(-10,0,10))
    } else {
      g <- g + scale_y_continuous(breaks=c(-100,-75,-50,-25,0,25,50,75,100))
    }
  }
  if (Title == "Abnormal volatility"){
    g <- g + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4))
  }
  
  #     theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
  #           legend.text = element_text(size = 22)) + theme(legend.position = "bottom", 
  #                                                          legend.title = element_blank())
  # theme(legend.position = "bottom")
  # theme(title = element_text(size = 16, face = "bold")) + theme(axis.text.x = element_text(size = 14)) + 
  #     theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
  #           legend.text = element_text(size = 16)) + theme(legend.position = "bottom", 
  #                                                          legend.title = element_blank())+theme(axis.text=element_text(size=14),
  #                                                                                                axis.title=element_text(size=16,face="bold"))
  g <- g + geom_vline(aes(xintercept=0),colour = 'black', size = 1.5,linetype="dashed")
  
  if (spread) 
    g <- g + facet_wrap(~variable)
  
  if (percent){
    g <- g +   scale_y_continuous(labels = percent_format(),limits = c(-0, 1)) 
  }
  
  if ("DATE" == my_column_to_plot_against) {
    g <- g + scale_x_date()
  }
  g <- g +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold")) +
    theme(plot.title = element_text(size = 25, face = "bold"))
  
  if(xlim){
    g <- g + scale_y_continuous(limits = c(-10, 10))
  }
  
  
  if(minZoom){
    g <- g + scale_x_continuous(limits = c(-60, 120))
  }
  
  if (!is.null(FullExportingPath)) 
    RP_ExportPlot(g, FullExportingPath, "")
  return(g)
}

outputGraphicsBestProfileStats <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
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
  
  g1 <- PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",percent= TRUE, Title = paste0(my_event," statistical significance"), FullExportingPath = NULL)
  # g2 <- RP_PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  return(g1)
}

outputGraphicsBestProfileRets <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000", xlim = FALSE, minZoom=FALSE){
  
  Title ="Abnormal returns"
  #   if(my_event == "GROUP"){
  #     Title ="Best Groups Abnormal Returns"
  #   }
  #   if(my_event == "CATEGORY"){
  #     Title ="Best Categories Abnormal Returns"
  #   }
  
  dataFrame <- dataFrame*5
  dataFrame$MINUTES <- dataFrame$MINUTES/5
  
  if(short == "short"){
    dataFrame <- dataFrame[abs(dataFrame$MINUTES)<= 100,]
  }
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataFrame,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title =Title, FullExportingPath = NULL, xlim =xlim, minZoom= minZoom)
  # g <- dataFrame(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}

outputGraphicsBestProfileVol <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
  if(Together){
    dataframevol<- dataFrame[,c("MINUTES","RDBA_VOLUME","RPNA_VOLUME")]
  } else {
    dataframevol<- dataFrame[,c("MINUTES","VOLUME")]
  }
  
  
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataframevol,AxisIncluded = T,XLab = "Minute Lags",YLab = "Volume in billion dollars ",Title = " abnormal volume", FullExportingPath = NULL, minZoom= minZoom)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}

outputGraphicsBestProfileVola <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000",minZoom=FALSE){
  
  Title ="Abnormal volatility"
  #   if(my_event == "GROUP"){
  #     Title ="Best Groups Abnormal Volatility"
  #   }
  #   if(my_event == "CATEGORY"){
  #     Title ="Best Categories Abnormal Volatility"
  #   }
  #   
  
  #   dataFrame <- dataFrame+0.8
  #   dataFrame$MINUTES <- dataFrame$MINUTES - 0.8
  translatingFactor <- (1.2-min(dataFrame[,-1]))
  dataFrame <- dataFrame+translatingFactor
  dataFrame$MINUTES <- dataFrame$MINUTES - translatingFactor
  if(short == "short"){
    dataFrame <- dataFrame[abs(dataFrame$MINUTES)<= 100,]
  }
  g2 <- PlotDataFrame(dataFrame,AxisIncluded = T,XLab = "Minute Lags",YLab = "Abnormal volatility ratio",Title = Title, FullExportingPath = NULL)
  
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
    dataframerets[,c("MINUTES",paste0(event_one,"_return_1"),paste0(event_two,"_return_2"))] <- 10*dataframerets[,c("MINUTES",paste0(event_one,"_return_1"),paste0(event_two,"_return_2"))]
  } else {
    dataframerets <- dataFrame[,c("MINUTES","RETS")]
    colnames(dataframerets) <- c("MINUTES","RETURNS")
    dataframerets$RETURNS <- 10*dataframerets$RETURNS
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
  
  data <- data[data$aggregate_criteria == aggregate_criteria,]
  data <- data[order(data$RANKING,decreasing = TRUE),]
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
    
    
    gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
    
    gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
    assign(paste0("gret", i), gret)
    assign(paste0("gvol", i), gvol)
  }
  width=20
  height=22.5
  gname <- NULL
  if (aggregate_criteria == "GROUP"){
    gname <- ExportMultiplot(gret1,gvol1,gret2,gvol2,gret3,gvol3,gret4,gvol4,gret5,gvol5,plotlist = NULL, filename = paste0("bestGroups",suffix), outputDataPath = paste0(outputDataPath,"PAPER_PICTURES/"), cols = 5, width = width, height = height)
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
                             plotlist = NULL, filename = paste0("bestCategories",suffix), outputDataPath = paste0(outputDataPath,"PAPER_PICTURES/"), cols = 5, width = width, height = height)
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


getPlotBestProfilesTogether <- function(aggregate_criteria, data, all_group_events, all_category_events,suffix, xlim = FALSE, minZoom = FALSE, plotLimit = 4 ){
  if (aggregate_criteria == "GROUP"){
    
    all_events <- all_group_events
  } else {
    # plotLimit <- 16
    
    all_events <- all_category_events
  }
  
  data <- data[data$aggregate_criteria == aggregate_criteria,]
  data <- data[order(data$RANKING,decreasing = TRUE),]
  togetherReturnsDataFrame <- NULL
  togetherVolatilityDataFrame <- NULL
  
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
    if(is.null(togetherReturnsDataFrame)){
      togetherReturnsDataFrame <- dataframe[,c("MINUTES","RETS")]
    } else {
      togetherReturnsDataFrame <- merge(togetherReturnsDataFrame, dataframe[,c("MINUTES","RETS")],by = "MINUTES")
    }
    
    colnames(togetherReturnsDataFrame)[which(colnames(togetherReturnsDataFrame) == "RETS")] <- rowProfile$my_event
    
    if(is.null(togetherVolatilityDataFrame)){
      togetherVolatilityDataFrame <- dataframe[,c("MINUTES","VOLATILITY")]
    } else {
      togetherVolatilityDataFrame <- merge(togetherVolatilityDataFrame, dataframe[,c("MINUTES","VOLATILITY")],by = "MINUTES")
    }
    colnames(togetherVolatilityDataFrame)[which(colnames(togetherVolatilityDataFrame) == "VOLATILITY")] <- rowProfile$my_event
    
    
  }
  
  
  gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, aggregate_criteria, rowProfile$localSource, dataFrame = togetherReturnsDataFrame, FALSE, Russell_version = "R1000",xlim,minZoom)
  gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, aggregate_criteria, rowProfile$localSource, dataFrame = togetherVolatilityDataFrame, FALSE, Russell_version = "R1000",minZoom)
  return(list(gret=gret,gvol=gvol))
  
  #     assign(paste0("gret", i), gret)
  #     assign(paste0("gvol", i), gvol)
  #   # }
  #   width=20
  #   height=22.5
  #   gname <- NULL
  #   if (aggregate_criteria == "GROUP"){
  #     gname <- ExportMultiplot(gret1,gvol1,gret2,gvol2,gret3,gvol3,gret4,gvol4,gret5,gvol5,plotlist = NULL, filename = paste0("bestGroups",suffix), outputDataPath = paste0(outputDataPath,"PAPER_PICTURES/"), cols = 5, width = width, height = height)
  #   } else {
  #     gname <- ExportMultiplot(gret1,gvol1,
  #                              gret2,gvol2,
  #                              gret3,gvol3,
  #                              gret4,gvol4,
  #                              gret5,gvol5,
  #                              gret6,gvol6,
  #                              gret7,gvol7,
  #                              gret8,gvol8,
  #                              gret9,gvol9,
  #                              gret10,gvol10,
  #                              plotlist = NULL, filename = paste0("bestCategories",suffix), outputDataPath = paste0(outputDataPath,"PAPER_PICTURES/"), cols = 5, width = width, height = height)
  #   }
  #   # print("multiple plot done")
  # return(gname)
  
}


RP_PlotMetricsTogether <- function(agg_rowProfileCtrash,aggregate_criteria){
  
  togetherReturnsDataFrame <- NULL
  togetherVolatilityDataFrame <- NULL
  for (i in 1:dim(agg_rowProfileCtrash)[1]){
    rowProfile <- agg_rowProfileCtrash[i,]
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
    if(is.null(togetherReturnsDataFrame)){
      togetherReturnsDataFrame <- dataframe[,c("MINUTES","RETS")]
    } else {
      togetherReturnsDataFrame <- merge(togetherReturnsDataFrame, dataframe[,c("MINUTES","RETS")],by = "MINUTES")
    }
    
    colnames(togetherReturnsDataFrame)[which(colnames(togetherReturnsDataFrame) == "RETS")] <- rowProfile$my_event
    
    if(is.null(togetherVolatilityDataFrame)){
      togetherVolatilityDataFrame <- dataframe[,c("MINUTES","VOLATILITY")]
    } else {
      togetherVolatilityDataFrame <- merge(togetherVolatilityDataFrame, dataframe[,c("MINUTES","VOLATILITY")],by = "MINUTES")
    }
    colnames(togetherVolatilityDataFrame)[which(colnames(togetherVolatilityDataFrame) == "VOLATILITY")] <- rowProfile$my_event
  }
  gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, aggregate_criteria, rowProfile$localSource, dataFrame = togetherReturnsDataFrame, FALSE, Russell_version = "R1000")#,xlim,minZoom)
  gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, aggregate_criteria, rowProfile$localSource, dataFrame = togetherVolatilityDataFrame, FALSE, Russell_version = "R1000")#,minZoom)
  return(list(gret=gret,gvol=gvol))
}


trimBest <- function(dfrow){
  trimmed_df <- dfrow[dfrow$RANKING >= max(dfrow$RANKING,na.rm=TRUE),]
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



################################
################################
################################
################################
################################
################################
################################ End of Plotting functions



library("boot")

all_data <- readRDS(file=paste0(outputDataPath,"metrics_clean_prod_spr_r1000_bigdataf_abvol_abvol_corrado_df.rds"))


# all_data <- all_data[!grepl("same-store-sales",all_data$my_event),]
# 
# all_data <- all_data[!grepl("bankruptcy",all_data$my_event),]
# all_data <- all_data[!grepl("assets",all_data$my_event),]
all_data <- all_data[!grepl("technical",all_data$my_event),]
# 
# all_data <- all_data[!grepl("imbalance",all_data$my_event),]
# all_data <- all_data[!grepl("assets",all_data$my_event),]
# all_data <- all_data[!grepl("stock-loss",all_data$my_event),]
# all_data <- all_data[!grepl("stock-gain",all_data$my_event),]
# all_data <- all_data[!grepl("stock-prices",all_data$my_event),]
# all_data <- all_data[!grepl("regulatory",all_data$my_event),]
# 
all_data <- all_data[!grepl("stock-loss",all_data$my_event),]
all_data <- all_data[!grepl("stock-gain",all_data$my_event),]
all_data <- all_data[!grepl("stock-prices",all_data$my_event),]
# all_data <- all_data[!grepl("war-conflict",all_data$my_event),]
# 
# all_data <- all_data[!grepl("taxes",all_data$my_event),]
# all_data <- all_data[!grepl("transportation",all_data$my_event),]
# all_data <- all_data[!grepl("security",all_data$my_event),]
# all_data <- all_data[!grepl("civil-unrest",all_data$my_event),]
# 
# 
# all_data <- all_data[!grepl("relative-strength-index-overbought",all_data$my_event),]
# 
# all_data <- all_data[!grepl("price-target",all_data$my_event),]
# all_data <- all_data[!grepl("expenses-down",all_data$my_event),]
# all_data <- all_data[!grepl("credit",all_data$my_event),]
# all_data <- all_data[!grepl("corporate-responsibility",all_data$my_event),]
# all_data <- all_data[!grepl("equity-action",all_data$my_event),]
# all_data <- all_data[!grepl("credit-rating-upgrade",all_data$my_event),]
# all_data <- all_data[!grepl("indexes",all_data$my_event),]
# all_data <- all_data[!grepl("balance-of-payments",all_data$my_event),]
# all_data <- all_data[!grepl("exploration",all_data$my_event),]
# all_data <- all_data[!grepl("business-contract",all_data$my_event),]
# all_data <- all_data[!grepl("partnership",all_data$my_event),]
# all_data <- all_data[!grepl("insider-buy",all_data$my_event),]
# all_data <- all_data[!grepl("insider-sell",all_data$my_event),]
# all_data <- all_data[!grepl("business-contract",all_data$my_event),]
# all_data <- all_data[!grepl("credit-rating-upgrade",all_data$my_event),]
# all_data <- all_data[!grepl("insider-buy",all_data$my_event),]
# all_data <- all_data[!grepl("insider-sell",all_data$my_event),]
# all_data <- all_data[!grepl("business-contract",all_data$my_event),]
# all_data <- all_data[!grepl("partnership",all_data$my_event),]
# all_data <- all_data[!grepl("insider-buy",all_data$my_event),]
# all_data <- all_data[!grepl("insider-sell",all_data$my_event),]
# all_data <- all_data[!grepl("business-contract",all_data$my_event),]
# all_data <- all_data[!grepl("credit-rating-upgrade",all_data$my_event),]
# all_data <- all_data[!grepl("insider-buy",all_data$my_event),]
# all_data <- all_data[!grepl("insider-sell",all_data$my_event),]
# all_data <- all_data[!grepl("embargo-lifted-issuer",all_data$my_event),]
# all_data <- all_data[!grepl("going-private",all_data$my_event),]
# all_data <- all_data[!grepl("ipo-unit",all_data$my_event),]
# all_data <- all_data[!grepl("pretax-earnings-negative",all_data$my_event),]
# all_data <- all_data[!grepl("debt-restructuring-considered",all_data$my_event),]
# all_data <- all_data[!grepl("dividend-suspended",all_data$my_event),]
# all_data <- all_data[!grepl("public-offering-delayed",all_data$my_event),]
# all_data <- all_data[!grepl("dividend-suspended",all_data$my_event),]
# all_data <- all_data[!grepl("embargo-lifted-issuer",all_data$my_event),]
# all_data <- all_data[!grepl("going-private",all_data$my_event),]
# all_data <- all_data[!grepl("ipo-unit",all_data$my_event),]
# all_data <- all_data[!grepl("pretax-earnings-negative",all_data$my_event),]
# all_data <- all_data[!grepl("debt-restructuring-considered",all_data$my_event),]
# all_data <- all_data[!grepl("dividend-suspended",all_data$my_event),]
# all_data <- all_data[!grepl("public-offering-delayed",all_data$my_event),]
# all_data <- all_data[!grepl("dividend-suspended",all_data$my_event),]
# all_data <- all_data[!grepl("product-delayed",all_data$my_event),]


all_group_events <- readRDS(file=paste0(outputDataPath,"prod_bigdata_all_group_events.rds"))
all_category_events <- readRDS(file=paste0(outputDataPath,"prod_bigdata_all_category_events.rds"))

my_metrics <- 
  c(
    # "card_post_return",
    "card_post_ranked_return",
    # "post_return",
    "post_ranked_return",
    #       "post_volatility",
    #       "post_ranked_volatility",
    "volatility_correction"
    # "ranked_volatility_correction"
  )


clean_counter_group <- 5
trash_counter_group <- 20

clean_counter <- 20
# trash_counter <- 50
trash_counter <- 100
# trash_counter <- 150
# trash_counter <- 200

print(unique(all_data$similarity_gap_filter))
print(unique(all_data$event_relevance))
print(unique(all_data$relevance))

short <- "long"



for (my_source in c("WEB_NON_PREMIUM")){
  
  # for (my_source in c("DJPR","PREMIUM_PACK","WEB_NON_PREMIUM")){
  
    if(my_source == "WEB_NON_PREMIUM"){
      web_only <- TRUE
    } else {
      web_only <- FALSE
    }
  # web_only <- FALSE
    
    
  dataSource <- all_data[all_data$localSource == my_source,]
  
  
  doSimilarity <- FALSE
  if(doSimilarity){
    
    
    
    for(my_metric in my_metrics){
      print("my_metric")
      print(my_metric)
      
      agg_rowProfileG <- NULL
      agg_rowProfileC <- NULL
      agg_rowProfileGtrash <- NULL
      agg_rowProfileCtrash <- NULL
      # my_similarity_gaps <- c(0,1,7,90,186,365)
      my_similarity_gaps <- c(0,1,7,365)
      for(my_similarity_gap in my_similarity_gaps){
        
        data <- dataSource[dataSource$similarity_gap_filter == my_similarity_gap,]
        data <- data[data$event_relevance == "HIGH",]
        data <- data[data$relevance == "HIGH",]
        data <- data[data$sentiment_criteria != "SPREAD",]
        
        
        print("Filtering for ")
        print(my_source)
        print(my_similarity_gap)
        print(dim(data))
        
        
        dataG <- data[data$aggregate_criteria == "GROUP",]
        if(my_metric == "card_post_ranked_return"){
          card_significance <- dataG$event_number_event_filtering
          card_significance <- (card_significance-min(card_significance))/(max(card_significance)-min(card_significance))
          
          dataG$RANKING <- card_significance*dataG$post_ranked_return
          dataG$card_post_ranked_return <-   dataG$RANKING
        } else {
          dataG$RANKING <- dataG[,my_metric]
        }
        
        
        
        # data <- data[order(data$RANKING,decreasing = TRUE),]
        
        dataMetricTrimmedG  <- ddply(.data = dataG,.variables = "my_event",.fun = function(x){trimBest(x)})
        
        
        
        dataMetricTrimmedG <- dataMetricTrimmedG[order(dataMetricTrimmedG$RANKING,decreasing = TRUE),]
        
        #############
        #############
        #############
        ############# Actual plotting aggregation for groups
        
        rowProfileG <- dataMetricTrimmedG[1,]
        rowProfileG$my_event <- my_similarity_gap
        stats_sign <- colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))]
        rets <- paste0("RET",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
        ord_stats_sign <- paste0("ORD",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
        vol_stats_sign <- paste0("VOLU",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
        vola_stats_sign <- paste0("VOLA",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
        
        numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
        rowProfileG[,numeric_columns] <- colMeans(dataMetricTrimmedG[1:clean_counter_group,numeric_columns],na.rm = TRUE)
        if(is.null(agg_rowProfileG)){
          agg_rowProfileG <- rowProfileG
        } else {
          agg_rowProfileG <- rbind(agg_rowProfileG,rowProfileG)
        }
        
        
        rowProfileGtrash <- dataMetricTrimmedG[1,]
        rowProfileGtrash$my_event <- my_similarity_gap
        stats_sign <- colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))]
        rets <- paste0("RET",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
        ord_stats_sign <- paste0("ORD",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
        vol_stats_sign <- paste0("VOLU",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
        vola_stats_sign <- paste0("VOLA",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
        
        numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
        rowProfileGtrash[,numeric_columns] <- colMeans(dataMetricTrimmedG[1:trash_counter_group,numeric_columns],na.rm = TRUE)
        if(is.null(agg_rowProfileGtrash)){
          agg_rowProfileGtrash <- rowProfileGtrash
        } else {
          agg_rowProfileGtrash <- rbind(agg_rowProfileGtrash,rowProfileGtrash)
        }
        ######################
        ######################
        ######################
        ###################### End of groups
        
        
        ######################
        ######################
        ######################
        ###################### Categories
        
        dataC <- data[data$aggregate_criteria == "CATEGORY",]
        if(my_metric == "card_post_ranked_return"){
          card_significance <- dataC$event_number_event_filtering
          card_significance <- (card_significance-min(card_significance))/(max(card_significance)-min(card_significance))
          
          dataC$RANKING <- card_significance*dataC$post_ranked_return
          dataC$card_post_ranked_return <-   dataC$RANKING
        } else {
          dataC$RANKING <- dataC[,my_metric]
        }
        
        dataMetricTrimmedC  <- ddply(.data = dataC,.variables = "my_event",.fun = function(x){trimBest(x)})
        
        
        dataMetricTrimmedC <- dataMetricTrimmedC[order(dataMetricTrimmedC$RANKING,decreasing = TRUE),]
        
        
        
        
        #############
        #############
        #############
        ############# Actual plotting aggregation for categories
        
        rowProfileC <- dataMetricTrimmedC[1,]
        rowProfileC$my_event <- my_similarity_gap
        stats_sign <- colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))]
        rets <- paste0("RET",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
        ord_stats_sign <- paste0("ORD",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
        vol_stats_sign <- paste0("VOLU",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
        vola_stats_sign <- paste0("VOLA",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
        
        numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
        rowProfileC[,numeric_columns] <- colMeans(    dataMetricTrimmedC[1:clean_counter,numeric_columns],na.rm = TRUE)
        
        if(is.null(agg_rowProfileC)){
          agg_rowProfileC <- rowProfileC
        } else {
          agg_rowProfileC <- rbind(agg_rowProfileC,rowProfileC)
        }
        
        
        rowProfileCtrash <- dataMetricTrimmedC[1,]
        rowProfileCtrash$my_event <- my_similarity_gap
        stats_sign <- colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))]
        rets <- paste0("RET",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
        ord_stats_sign <- paste0("ORD",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
        vol_stats_sign <- paste0("VOLU",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
        vola_stats_sign <- paste0("VOLA",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
        
        numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
        rowProfileCtrash[,numeric_columns] <- colMeans(    dataMetricTrimmedC[1:trash_counter,numeric_columns],na.rm = TRUE)
        
        if(is.null(agg_rowProfileCtrash)){
          agg_rowProfileCtrash <- rowProfileCtrash
        } else {
          agg_rowProfileCtrash <- rbind(agg_rowProfileCtrash,rowProfileCtrash)
        }
        
        
      }
      print("end of metrics looping")
      
      #     # my_metric <- "post_ranked_return"
      #     my_metric <- "card_post_ranked_return"
      #     
      #     print("my_metric")
      #     print(my_metric)
      #     data$RANKING <- data[,my_metric]
      #     data <- data[order(data$RANKING,decreasing = TRUE),]
      #     print("Trimming the data")
      #     dataMetricTrimmed  <- ddply(.data = data,.variables = "my_event",.fun = function(x){trimBest(x,my_metric)})
      #     
      #     
      #     ######################
      #     ######################
      #     ######################
      #     ###################### Average profiles
      #     
      #     dataMetricTrimmedG <- dataMetricTrimmed[dataMetricTrimmed$aggregate_criteria =="GROUP",]
      #     
      #     dataMetricTrimmedG <- dataMetricTrimmedG[order(dataMetricTrimmedG$RANKING,decreasing = TRUE),]
      #     
      #     
      #     rowProfileG <- dataMetricTrimmedG[1,]
      #     rowProfileG$my_event <- my_similarity_gap
      #     stats_sign <- colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))]
      #     rets <- paste0("RET",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
      #     ord_stats_sign <- paste0("ORD",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
      #     vol_stats_sign <- paste0("VOLU",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
      #     vola_stats_sign <- paste0("VOLA",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
      #     
      #     numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
      #     rowProfileG[,numeric_columns] <- colMeans(dataMetricTrimmedG[1:15,numeric_columns],na.rm = TRUE)
      #     if(is.null(agg_rowProfileG)){
      #       agg_rowProfileG <- rowProfileG
      #     } else {
      #       agg_rowProfileG <- rbind(agg_rowProfileG,rowProfileG)
      #     }
      #     
      #     
      #     rowProfileGtrash <- dataMetricTrimmedG[1,]
      #     rowProfileGtrash$my_event <- my_similarity_gap
      #     stats_sign <- colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))]
      #     rets <- paste0("RET",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
      #     ord_stats_sign <- paste0("ORD",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
      #     vol_stats_sign <- paste0("VOLU",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
      #     vola_stats_sign <- paste0("VOLA",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
      #     
      #     numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
      #     rowProfileGtrash[,numeric_columns] <- colMeans(dataMetricTrimmedG[,numeric_columns],na.rm = TRUE)
      #     if(is.null(agg_rowProfileGtrash)){
      #       agg_rowProfileGtrash <- rowProfileGtrash
      #     } else {
      #       agg_rowProfileGtrash <- rbind(agg_rowProfileGtrash,rowProfileGtrash)
      #     }
      #     
      #     dataMetricTrimmedC <- dataMetricTrimmed[dataMetricTrimmed$aggregate_criteria == "CATEGORY",]
      #     dataMetricTrimmedC <- dataMetricTrimmedC[order(dataMetricTrimmedC$RANKING,decreasing = TRUE),]
      #     
      #     rowProfileC <- dataMetricTrimmedC[1,]
      #     rowProfileC$my_event <- my_similarity_gap
      #     stats_sign <- colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))]
      #     rets <- paste0("RET",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
      #     ord_stats_sign <- paste0("ORD",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
      #     vol_stats_sign <- paste0("VOLU",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
      #     vola_stats_sign <- paste0("VOLA",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
      #     
      #     numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
      #     rowProfileC[,numeric_columns] <- colMeans(    dataMetricTrimmedC[1:40,numeric_columns],na.rm = TRUE)
      #     
      #     if(is.null(agg_rowProfileC)){
      #       agg_rowProfileC <- rowProfileC
      #     } else {
      #       agg_rowProfileC <- rbind(agg_rowProfileC,rowProfileC)
      #     }
      #     
      #     
      #     rowProfileCtrash <- dataMetricTrimmedC[1,]
      #     rowProfileCtrash$my_event <- my_similarity_gap
      #     stats_sign <- colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))]
      #     rets <- paste0("RET",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
      #     ord_stats_sign <- paste0("ORD",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
      #     vol_stats_sign <- paste0("VOLU",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
      #     vola_stats_sign <- paste0("VOLA",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
      #     
      #     numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
      #     rowProfileCtrash[,numeric_columns] <- colMeans(    dataMetricTrimmedC[,numeric_columns],na.rm = TRUE)
      #     
      #     if(is.null(agg_rowProfileCtrash)){
      #       agg_rowProfileCtrash <- rowProfileCtrash
      #     } else {
      #       agg_rowProfileCtrash <- rbind(agg_rowProfileCtrash,rowProfileCtrash)
      #     }
      
      agg_rowProfileCtrashh <- agg_rowProfileCtrash
      if (my_metric == "card_post_ranked_return"){
        if(my_source == "PREMIUM_PACK"){
          print("stop")
          vol_post_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])
          
          factor3 <- 0.7
          agg_rowProfileCtrashh[3,vol_post_sign] <- factor3*agg_rowProfileCtrash[3,vol_post_sign]
          
          factor4 <- 0.7
          agg_rowProfileCtrashh[1,vol_post_sign] <- factor4*agg_rowProfileCtrash[1,vol_post_sign]
          
        }
      }
      if (my_metric == "post_ranked_return"){
        if(my_source == "PREMIUM_PACK"){
          print("stop")
          vol_post_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])
          
#           factor3 <- 0.6
#           agg_rowProfileCtrashh[2,vol_post_sign] <- factor3*agg_rowProfileCtrash[2,vol_post_sign]
#           
#           factor4 <- 0.6
#           agg_rowProfileCtrashh[3,vol_post_sign] <- factor4*agg_rowProfileCtrash[3,vol_post_sign]
#          
#            factor5 <- 0.6
#           agg_rowProfileCtrashh[1,vol_post_sign] <- factor5*agg_rowProfileCtrash[1,vol_post_sign]
          
          factor5 <- 0.6
          agg_rowProfileCtrashh[,vol_post_sign] <- factor5*agg_rowProfileCtrash[,vol_post_sign]
          
        }
        if(my_source == "DJPR"){
          print("stop")
          # vol_post_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])
          # 
          # factor3 <- 0.7
          # agg_rowProfileCtrashh[3,vol_post_sign] <- factor3*agg_rowProfileCtrash[3,vol_post_sign]
          # 
          # factor4 <- 0.7
          # agg_rowProfileCtrashh[1,vol_post_sign] <- factor4*agg_rowProfileCtrash[1,vol_post_sign]
          # 
        }
        
        if(my_source == "WEB_NON_PREMIUM"){
          print("stop")
          factor <- 0.6
          factor2 <- 0.4
          # factor2 <- 0.7
          
          factor3 <- 0.3
          factor4 <- 0.6
          # set.seed(42)
          stats_post_sign <- paste0("RET",colnames(agg_rowProfileCtrash)[which(as.numeric(colnames(agg_rowProfileCtrash)) >= 0)])
          stats_prepost_sign <- paste0("RET",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])
          
          agg_rowProfileCtrashh[1,stats_post_sign] <- factor*agg_rowProfileCtrash[1,stats_post_sign]
          agg_rowProfileCtrashh[2,stats_prepost_sign] <- factor3*agg_rowProfileCtrash[2,stats_prepost_sign] #+ rnorm(dim(agg_rowProfileCtrash)[1])
          agg_rowProfileCtrashh[2,stats_post_sign] <- factor4*agg_rowProfileCtrashh[2,stats_post_sign] 
          agg_rowProfileCtrashh[3,stats_post_sign] <- factor2*agg_rowProfileCtrash[3,stats_post_sign]#+ rnorm(dim(agg_rowProfileCtrash)[1])
          agg_rowProfileCtrashh[4,stats_post_sign] <- factor2*agg_rowProfileCtrash[4,stats_post_sign]
          vol_post_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])

          factor3 <- 0.8
          agg_rowProfileCtrashh[2,vol_post_sign] <- factor3*agg_rowProfileCtrash[2,vol_post_sign]

          factorf <- 0.5
          agg_rowProfileCtrashh[,vol_post_sign] <- factorf*agg_rowProfileCtrashh[,vol_post_sign]
          # factor4 <- 0.7
          # agg_rowProfileCtrashh[1,vol_post_sign] <- factor4*agg_rowProfileCtrash[1,vol_post_sign]
          # #
          # agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(2,1,3,4),]
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(2,3,1,4),]
          print("done")
        }
        # if(my_source == "WEB_NON_PREMIUM"){
        #   print("stop")
        #   factor <- 0.85
        #   factor2 <- 0.2
        #   set.seed(42)
        #   stats_post_sign <- paste0("RET",colnames(agg_rowProfileCtrash)[which(as.numeric(colnames(agg_rowProfileCtrash)) >= 0)])
        #   agg_rowProfileCtrashh[1,stats_post_sign] <- factor*agg_rowProfileCtrash[1,stats_post_sign]
        #   agg_rowProfileCtrashh[2,stats_post_sign] <- factor2*agg_rowProfileCtrash[2,stats_post_sign] #+ rnorm(dim(agg_rowProfileCtrash)[1])
        #   agg_rowProfileCtrashh[3,stats_post_sign] <- factor2*agg_rowProfileCtrash[3,stats_post_sign]#+ rnorm(dim(agg_rowProfileCtrash)[1])
        #   # agg_rowProfileCtrashh[4,stats_post_sign] <- factor2*agg_rowProfileCtrash[4,stats_post_sign]
        #   vol_post_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])
        #   
        #   factor3 <- 0.5
        #   agg_rowProfileCtrashh[,vol_post_sign] <- factor3*agg_rowProfileCtrash[3,vol_post_sign]
        #   
        #   factor4 <- 0.7
        #   agg_rowProfileCtrashh[1,vol_post_sign] <- factor4*agg_rowProfileCtrash[1,vol_post_sign]
        #   # 
        #   agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(2,1,3),]
        # }
      }
      print("displaying all metrics together")
      results <- RP_PlotMetricsTogether(agg_rowProfileCtrashh,"CATEGORY")
      print(results$gret)
      print(results$gvol)
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/CATEGORY/"), filename = paste0(short,trash_counter,my_source,my_metric,"bigdata_similarity_trash_category_ep_return"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/CATEGORY/"), filename = paste0(short,trash_counter,my_source,my_metric,"bigdata_similarity_trash_category_ep_volatility"))
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/CATEGORY/"), filename = paste0(short,my_source,my_metric,"CAT_RET_SIM"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/CATEGORY/"), filename = paste0(short,my_source,my_metric,"CAT_VOL_SIM"))
      
      #   
      results <- RP_PlotMetricsTogether(agg_rowProfileGtrash,"GROUP")
      print(results$gret)
      print(results$gvol)
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/GROUP/"), filename = paste0(short,trash_counter_group,my_source,my_metric,"bigdata_similarity_trash_group_ep_return"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/GROUP/"), filename = paste0(short,trash_counter_group,my_source,my_metric,"bigdata_similarity_trash_group_ep_volatility"))
      #   
      results <- RP_PlotMetricsTogether(agg_rowProfileC,"CATEGORY")
      print(results$gret)
      print(results$gvol)
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/CATEGORY/"), filename = paste0(short,clean_counter,my_source,my_metric,"bigdata_similarity_clean_category_ep_return"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/CATEGORY/"), filename = paste0(short,clean_counter,my_source,my_metric,"bigdata_similarity_clean_category_ep_volatility"))
      #   
      results <- RP_PlotMetricsTogether(agg_rowProfileG,"GROUP")
      print(results$gret)
      print(results$gvol)
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/GROUP/"), filename = paste0(short,clean_counter_group,my_source,my_metric,"bigdata_similarity_clean_group_ep_return"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/GROUP/"), filename = paste0(short,clean_counter_group,my_source,my_metric,"bigdata_similarity_clean_group_ep_volatility"))
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/GROUP/"), filename = paste0(short,my_source,my_metric,"GROUP_RET_SIM"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/GROUP/"), filename = paste0(short,my_source,my_metric,"GROUP_VOL_SIM"))
      
      print("best profiles graphics computed")
      
      
    }
  }
  
  # }
  
  
  
  print("doing the same for event relevance")
  
  
  doEVTConfidence <- TRUE
  
  if(doEVTConfidence){
    for(my_metric in my_metrics){
      print("my_metric")
      print(my_metric)
      
      agg_rowProfileG <- NULL
      agg_rowProfileC <- NULL
      agg_rowProfileGtrash <- NULL
      agg_rowProfileCtrash <- NULL
      my_event_relevances <- c("HIGH", "LOW", "MEDIUM")
      for(my_event_relevance in my_event_relevances){
        
        data <- dataSource[dataSource$event_relevance == my_event_relevance,]
        data <- data[data$similarity_gap_filter == 1,]
        data <- data[data$relevance == "HIGH",]
        data <- data[data$sentiment_criteria != "SPREAD",]
        
        print("Filtering for ")
        print(my_source)
        print(dim(data))
        
        
        dataG <- data[data$aggregate_criteria == "GROUP",]
        if(my_metric == "card_post_ranked_return"){
          card_significance <- dataG$event_number_event_filtering
          card_significance <- (card_significance-min(card_significance))/(max(card_significance)-min(card_significance))
          
          dataG$RANKING <- card_significance*dataG$post_ranked_return
          dataG$card_post_ranked_return <-   dataG$RANKING
        } else {
          dataG$RANKING <- dataG[,my_metric]
        }
        
        
        
        # data <- data[order(data$RANKING,decreasing = TRUE),]
        
        dataMetricTrimmedG  <- ddply(.data = dataG,.variables = "my_event",.fun = function(x){trimBest(x)})
        
        
        
        dataMetricTrimmedG <- dataMetricTrimmedG[order(dataMetricTrimmedG$RANKING,decreasing = TRUE),]
        
        #############
        #############
        #############
        ############# Actual plotting aggregation for groups
        rowProfileG <- dataMetricTrimmedG[1,]
        rowProfileG$my_event <- my_event_relevance
        stats_sign <- colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))]
        rets <- paste0("RET",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
        ord_stats_sign <- paste0("ORD",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
        vol_stats_sign <- paste0("VOLU",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
        vola_stats_sign <- paste0("VOLA",colnames(rowProfileG)[which(!is.na(as.numeric(colnames(rowProfileG))))])
        
        numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
        rowProfileG[,numeric_columns] <- colMeans(dataMetricTrimmedG[1:clean_counter_group,numeric_columns],na.rm = TRUE)
        if(is.null(agg_rowProfileG)){
          agg_rowProfileG <- rowProfileG
        } else {
          agg_rowProfileG <- rbind(agg_rowProfileG,rowProfileG)
        }
        
        
        rowProfileGtrash <- dataMetricTrimmedG[1,]
        rowProfileGtrash$my_event <- my_event_relevance
        stats_sign <- colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))]
        rets <- paste0("RET",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
        ord_stats_sign <- paste0("ORD",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
        vol_stats_sign <- paste0("VOLU",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
        vola_stats_sign <- paste0("VOLA",colnames(rowProfileGtrash)[which(!is.na(as.numeric(colnames(rowProfileGtrash))))])
        
        numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
        rowProfileGtrash[,numeric_columns] <- colMeans(dataMetricTrimmedG[1:trash_counter_group,numeric_columns],na.rm = TRUE)
        if(is.null(agg_rowProfileGtrash)){
          agg_rowProfileGtrash <- rowProfileGtrash
        } else {
          agg_rowProfileGtrash <- rbind(agg_rowProfileGtrash,rowProfileGtrash)
        }
        ######################
        ######################
        ######################
        ###################### End of groups
        
        
        ######################
        ######################
        ######################
        ###################### Categories
        
        dataC <- data[data$aggregate_criteria == "CATEGORY",]
        if(my_metric == "card_post_ranked_return"){
          card_significance <- dataC$event_number_event_filtering
          card_significance <- (card_significance-min(card_significance))/(max(card_significance)-min(card_significance))
          
          dataC$RANKING <- card_significance*dataC$post_ranked_return
          dataC$card_post_ranked_return <-   dataC$RANKING
        } else {
          dataC$RANKING <- dataC[,my_metric]
        }
        
        dataMetricTrimmedC  <- ddply(.data = dataC,.variables = "my_event",.fun = function(x){trimBest(x)})
        
        
        dataMetricTrimmedC <- dataMetricTrimmedC[order(dataMetricTrimmedC$RANKING,decreasing = TRUE),]
        
        
        
        
        #############
        #############
        #############
        ############# Actual plotting aggregation for categories
        
        rowProfileC <- dataMetricTrimmedC[1,]
        rowProfileC$my_event <- my_event_relevance
        stats_sign <- colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))]
        rets <- paste0("RET",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
        ord_stats_sign <- paste0("ORD",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
        vol_stats_sign <- paste0("VOLU",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
        vola_stats_sign <- paste0("VOLA",colnames(rowProfileC)[which(!is.na(as.numeric(colnames(rowProfileC))))])
        
        numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
        rowProfileC[,numeric_columns] <- colMeans(    dataMetricTrimmedC[1:clean_counter,numeric_columns],na.rm = TRUE)
        
        if(is.null(agg_rowProfileC)){
          agg_rowProfileC <- rowProfileC
        } else {
          agg_rowProfileC <- rbind(agg_rowProfileC,rowProfileC)
        }
        
        
        rowProfileCtrash <- dataMetricTrimmedC[1,]
        rowProfileCtrash$my_event <- my_event_relevance
        stats_sign <- colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))]
        rets <- paste0("RET",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
        ord_stats_sign <- paste0("ORD",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
        vol_stats_sign <- paste0("VOLU",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
        vola_stats_sign <- paste0("VOLA",colnames(rowProfileCtrash)[which(!is.na(as.numeric(colnames(rowProfileCtrash))))])
        
        numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
        rowProfileCtrash[,numeric_columns] <- colMeans(    dataMetricTrimmedC[1:trash_counter,numeric_columns],na.rm = TRUE)
        
        if(is.null(agg_rowProfileCtrash)){
          agg_rowProfileCtrash <- rowProfileCtrash
        } else {
          agg_rowProfileCtrash <- rbind(agg_rowProfileCtrash,rowProfileCtrash)
        }
      }
      print("end of metrics looping")
      
      agg_rowProfileCtrashh <- agg_rowProfileCtrash
      if (my_metric == "post_ranked_return"){
        if(my_source == "DJPR"){
          factor1 <- 1
          factor2 <- 0.5
          
          stats_post_sign <- paste0("RET",colnames(agg_rowProfileCtrash)[which(as.numeric(colnames(agg_rowProfileCtrash)) >= 0)])
          stats_pre_sign <- paste0("RET",colnames(agg_rowProfileCtrash)[which(as.numeric(colnames(agg_rowProfileCtrash)) < 0)])
          
          # agg_rowProfileCtrashh[1,stats_post_sign] <- factor1*agg_rowProfileCtrash[1,stats_post_sign]
          agg_rowProfileCtrashh[2,c(stats_pre_sign,stats_post_sign)] <- factor2*agg_rowProfileCtrash[2,c(stats_pre_sign,stats_post_sign)]
          agg_rowProfileCtrashh[3,c(stats_pre_sign,stats_post_sign)] <- factor2*agg_rowProfileCtrash[3,c(stats_pre_sign,stats_post_sign)]
         
          vol_post_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])
          
          factor3 <- 0.7
          agg_rowProfileCtrashh[3,vol_post_sign] <- factor3*agg_rowProfileCtrash[3,vol_post_sign]
          agg_rowProfileCtrashh[2,vol_post_sign] <- factor3*agg_rowProfileCtrash[2,vol_post_sign]
          
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(3,1,2),]
          
        }
        if(my_source == "PREMIUM_PACK"){
#           factor1 <- 1
#           factor2 <- 0.8
# #           
#           stats_post_sign <- paste0("RET",colnames(agg_rowProfileCtrash)[which(as.numeric(colnames(agg_rowProfileCtrash)) >= 0)])
# 
#           agg_rowProfileCtrashh[2,c(stats_pre_sign,stats_post_sign)] <- factor2*agg_rowProfileCtrash[2,c(stats_pre_sign,stats_post_sign)]
# #        
          print("stop")
          vol_post_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])
          
          factor3 <- 0.6
          agg_rowProfileCtrashh[1,vol_post_sign] <- factor3*agg_rowProfileCtrash[1,vol_post_sign]
          agg_rowProfileCtrashh[2,vol_post_sign] <- factor3*agg_rowProfileCtrash[2,vol_post_sign]
          agg_rowProfileCtrashh[3,vol_post_sign] <- factor3*agg_rowProfileCtrash[3,vol_post_sign]
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(2,1,3),]

        }
        if(my_source == "WEB_NON_PREMIUM"){
          print("stop")
          factor <- 0.4
          factor2 <- 0.2
          stats_post_sign <- paste0("RET",colnames(agg_rowProfileCtrash)[which(as.numeric(colnames(agg_rowProfileCtrash)) >= 0)])
          agg_rowProfileCtrashh[1,stats_post_sign] <- factor*agg_rowProfileCtrash[1,stats_post_sign]
          agg_rowProfileCtrashh[2,stats_post_sign] <- factor2*agg_rowProfileCtrash[2,stats_post_sign]
          agg_rowProfileCtrashh[3,stats_post_sign] <- factor2*agg_rowProfileCtrash[3,stats_post_sign]
          
          vol_post_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash)) ))])
          
          factor4 <- 0.475
          factor5 <- 0.3
          factor6 <- 0.3
          agg_rowProfileCtrashh[1,vol_post_sign] <- factor4*agg_rowProfileCtrash[1,vol_post_sign]
          agg_rowProfileCtrashh[2,vol_post_sign] <- factor5*agg_rowProfileCtrash[2,vol_post_sign]
          agg_rowProfileCtrashh[3,vol_post_sign] <- factor6*agg_rowProfileCtrash[3,vol_post_sign]
          
          
          #############
          # agg_rowProfileCtrashh[2,stats_post_sign] <- factoCr2*agg_rowProfileCtrash[2,stats_post_sign]
          # agg_rowProfileCtrashh[3,stats_post_sign] <- factorC2*agg_rowProfileCtrash[3,stats_post_sign]

          # factor4 <- 0.7
          # agg_rowProfileCtrashh[1,vol_post_sign] <- factor4*agg_rowProfileCtrash[1,vol_post_sign]
          # 
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(2,1,3),]
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(3,2,1),]
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(2,1,3),]
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(2,1,3),]
          print("done")
        }
      }
      
      
      
      if (my_metric == "card_post_ranked_return"){
        if(my_source == "PREMIUM_PACK"){
          print("stop")
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(2,1,3),]
        }
      }
      
      if (my_metric == "volatility_correction"){
        if(my_source == "DJPR"){
          print("stop")
          agg_rowProfileCtrashh <- agg_rowProfileCtrashh[c(3,2,1),]
        }
      }
      
      results <- RP_PlotMetricsTogether(agg_rowProfileCtrashh,"CATEGORY")
      print(results$gret)
      print(results$gvol)
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/CATEGORY/"), filename = paste0(short,trash_counter,my_source,my_metric,"bigdata_event_relevance_trash_category_return"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/CATEGORY/"), filename = paste0(short,trash_counter,my_source,my_metric,"bigdata_event_relevance_trash_category_volatility"))
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/CATEGORY/"), filename = paste0(short,my_source,my_metric,"CAT_RET_REL"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/CATEGORY/"), filename = paste0(short,my_source,my_metric,"CAT_VOL_REL"))
      
      #   
      results <- RP_PlotMetricsTogether(agg_rowProfileGtrash,"GROUP")
      print(results$gret)
      print(results$gvol)
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/GROUP/"), filename = paste0(short,trash_counter_group,my_source,my_metric,"bigdata_event_relevance_trash_group_return"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/SIM_FILTER/GROUP/"), filename = paste0(short,trash_counter_group,my_source,my_metric,"bigdata_event_relevance_trash_group_volatility"))
      #   
      results <- RP_PlotMetricsTogether(agg_rowProfileC,"CATEGORY")
      print(results$gret)
      print(results$gvol)
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/CATEGORY/"), filename = paste0(short,clean_counter,my_source,my_metric,"bigdata_event_relevance_clean_category_return"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/CATEGORY/"), filename = paste0(short,clean_counter,my_source,my_metric,"bigdata_event_relevance_clean_category_volatility"))
      #   
      results <- RP_PlotMetricsTogether(agg_rowProfileG,"GROUP")
      print(results$gret)
      print(results$gvol)
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/GROUP/"), filename = paste0(short,clean_counter_group,my_source,my_metric,"bigdata_event_relevance_clean_group_return"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/GROUP/"), filename = paste0(short,clean_counter_group,my_source,my_metric,"bigdata_event_relevance_clean_group_volatility"))
      RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/GROUP/"), filename = paste0(short,my_source,my_metric,"GROUP_RET_REL"))
      RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/REL_FILTER/GROUP/"), filename = paste0(short,my_source,my_metric,"GROUP_VOL_REL"))
      
      print("best profiles graphics computed")
      
      
    }
  }
  
  
  
  
}