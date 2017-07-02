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
################################ Plotting functions and others

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
    g <- g + geom_line(size=1.2)
  }
  else {
    g <- g + geom_line(size=1.2) + geom_point()
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
  
  Title ="Abnormal Returns"
  if(my_event == "GROUP"){
    Title ="Best Groups Abnormal Returns"
  }
  if(my_event == "CATEGORY"){
    Title ="Best Categories Abnormal Returns"
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
  
  Title ="Abnormal Volatility"
  if(my_event == "GROUP"){
    Title ="Best Groups Abnormal Volatility"
  }
  if(my_event == "CATEGORY"){
    Title ="Best Categories Abnormal Volatility"
  }
  
  #   dataFrame <- dataFrame+0.8
  #   dataFrame$MINUTES <- dataFrame$MINUTES - 0.8
  translatingFactor <- (1.2-min(dataFrame[,-1]))
  dataFrame <- dataFrame+translatingFactor
  dataFrame$MINUTES <- dataFrame$MINUTES - translatingFactor
  
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


RP_PlotMetricsTogether <- function(agg_rowProfileCtrash,my_metrics,aggregate_criteria){
  
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




RP_PlotCIIntervalSuperposed <- function(DataFrameOne, DataFrameTwo, DataFrameThree, Title = "", names, FullExportingPath=NULL, type = 'RET') {
  
  MyTitle <- ""
  if (type == "VOL"){
    toplotDFOne<- DataFrameOne[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
    colnames(toplotDFOne) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    toplotDFTwo<- DataFrameTwo[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
    colnames(toplotDFTwo) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    toplotDFThree<- DataFrameThree[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
    colnames(toplotDFThree) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    #     toplotDFOne <- toplotDFOne +0.8
    #     toplotDFOne$MINUTES <- toplotDFOne$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFOne[,-1]))
    toplotDFOne <- toplotDFOne+translatingFactor
    toplotDFOne$MINUTES <- toplotDFOne$MINUTES - translatingFactor
    
    #     
    #     toplotDFTwo <- toplotDFTwo +0.8
    #     toplotDFTwo$MINUTES <- toplotDFTwo$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFTwo[,-1]))
    toplotDFTwo <- toplotDFTwo+translatingFactor
    toplotDFTwo$MINUTES <- toplotDFTwo$MINUTES - translatingFactor
    
    #     toplotDFThree <- toplotDFThree +0.8
    #     toplotDFThree$MINUTES <- toplotDFThree$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFThree[,-1]))
    toplotDFThree <- toplotDFThree+translatingFactor
    toplotDFThree$MINUTES <- toplotDFThree$MINUTES - translatingFactor
    
    MyTitle <- "Abnormal volatility"
    MyYLabel <- "Abnormality ratio"
    
    toplotDFOne <- toplotDFOne[abs(toplotDFOne$MINUTES)<= 100,]
    toplotDFTwo <- toplotDFTwo[abs(toplotDFTwo$MINUTES)<= 100,]
    toplotDFThree <- toplotDFThree[abs(toplotDFThree$MINUTES)<= 100,]
  }
  
  if(type =="RET"){
    toplotDFOne<- DataFrameOne[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFOne) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFTwo<- DataFrameTwo[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFTwo) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFThree<- DataFrameThree[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFThree) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    MyTitle <- "Abnormal returns"
    MyYLabel <- "BPS cumulated abnormal return"
    
    toplotDFOne <- toplotDFOne[abs(toplotDFOne$MINUTES)<= 100,]
    toplotDFTwo <- toplotDFTwo[abs(toplotDFTwo$MINUTES)<= 100,]
    toplotDFThree <- toplotDFThree[abs(toplotDFThree$MINUTES)<= 100,]
  }
  
  if(type =="VOLU"){
    toplotDFOne<- DataFrameOne[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
    colnames(toplotDFOne) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFTwo<- DataFrameTwo[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
    colnames(toplotDFTwo) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFThree<- DataFrameThree[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
    colnames(toplotDFThree) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    #     toplotDFOne <- toplotDFOne +0.8
    #     toplotDFOne$MINUTES <- toplotDFOne$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFOne[,-1]))
    toplotDFOne <- toplotDFOne+translatingFactor
    toplotDFOne$MINUTES <- toplotDFOne$MINUTES - translatingFactor
    
    #     
    #     toplotDFTwo <- toplotDFTwo +0.8
    #     toplotDFTwo$MINUTES <- toplotDFTwo$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFTwo[,-1]))
    toplotDFTwo <- toplotDFTwo+translatingFactor
    toplotDFTwo$MINUTES <- toplotDFTwo$MINUTES - translatingFactor
    
    #     toplotDFThree <- toplotDFThree +0.8
    #     toplotDFThree$MINUTES <- toplotDFThree$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFThree[,-1]))
    toplotDFThree <- toplotDFThree+translatingFactor
    toplotDFThree$MINUTES <- toplotDFThree$MINUTES - translatingFactor
    
    
    MyTitle <- "Abnormal volume"
    MyYLabel <- "Abnormality ratio"
    
  }
  
  
  print("superposing") 
  colnames(toplotDFOne) <- paste0("FREQ_",colnames(toplotDFOne))
  colnames(toplotDFOne)[1] <- "MINUTES"
  
  colnames(toplotDFTwo) <- paste0("RET_",colnames(toplotDFTwo))
  colnames(toplotDFTwo)[1] <- "MINUTES"
  
  
  colnames(toplotDFThree) <- paste0("VOL_",colnames(toplotDFThree))
  colnames(toplotDFThree)[1] <- "MINUTES"
  
  
  
  DataFrame <- merge(toplotDFOne, toplotDFTwo, by="MINUTES")
  DataFrame <- merge(DataFrame, toplotDFThree, by="MINUTES")
  
  
  mav <- function(x){stats::filter(x,rep(1/3,3), sides=2)}
  DataFrame$FREQ_CI_LOW <- 3/4*( DataFrame$FREQ_CI_LOW) + 1/4*(mav(DataFrame$FREQ_CI_LOW))
  DataFrame$FREQ_CI_HIGH <- 3/4*(DataFrame$FREQ_CI_HIGH) + 1/4*mav(DataFrame$FREQ_CI_HIGH)
  DataFrame$FREQ_MEAN <- 3/4*(DataFrame$FREQ_MEAN) + 1/4*mav(DataFrame$FREQ_MEAN)
  DataFrame$RET_MEAN <- 3/4*(DataFrame$RET_MEAN) + 1/4*mav(DataFrame$RET_MEAN)
  DataFrame$RET_CI_LOW <-  3/4*(DataFrame$RET_CI_LOW)+ 1/4*mav(DataFrame$RET_CI_LOW)
  DataFrame$RET_CI_HIGH <- 3/4*(DataFrame$RET_CI_HIGH)+  1/4*mav(DataFrame$RET_CI_HIGH)
  DataFrame$VOL_MEAN <- 3/4*(DataFrame$VOL_MEAN) + 1/4*mav(DataFrame$VOL_MEAN)
  DataFrame$NEG_CI_LOW <-  3/4*(DataFrame$VOL_CI_LOW)+ 1/4*mav(DataFrame$NEG_CI_LOW)
  DataFrame$NEG_CI_HIGH <- 3/4*(DataFrame$VOL_CI_HIGH)+  1/4*mav(DataFrame$NEG_CI_HIGH)
  
  
  DataFrame <- DataFrame[complete.cases(DataFrame),]
  DataFrame <- DataFrame[abs(DataFrame$MINUTES) <=90,]
  
  
  
  g <- ggplot(DataFrame)+
#     geom_line(data=DataFrame, aes(x=MINUTES, y=FREQ_MEAN), size=1.5,alpha=1,show_guide = T)+#, colour="blue")+
#     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=FREQ_CI_LOW,ymax=FREQ_CI_HIGH),show_guide = F,alpha=0.25,colour="blue",fill="blue")+#, fill="steelblue1", color="steelblue1")+
#     geom_line(data=DataFrame, aes(x=MINUTES, y=RET_MEAN), size=1.5,alpha=1,show_guide = T)+#, colour="red")+
#     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=RET_CI_LOW,ymax=RET_CI_HIGH),show_guide = F,alpha=0.25,colour="green",fill="green")+#, fill="steelblue2", color="steelblue3")+
#     geom_line(data=DataFrame, aes(x=MINUTES, y=VOL_MEAN), size=1.5,alpha=1,show_guide = T)+#, colour="red")+
#     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=VOL_CI_LOW,ymax=VOL_CI_HIGH),show_guide = F,alpha=0.25,colour="red",fill="red")+#, fill="steelblue3", color="steelblue3")+
    geom_line(data=DataFrame, aes(x=MINUTES, y=FREQ_MEAN), size=1.5,alpha=1,show_guide = T, colour="#619CFF")+
    geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=FREQ_CI_LOW,ymax=FREQ_CI_HIGH),show_guide = F,alpha=0.25,colour="#619CFF",fill="#619CFF")+#, fill="steelblue1", color="steelblue1")+
    geom_line(data=DataFrame, aes(x=MINUTES, y=RET_MEAN), size=1.5,alpha=1,show_guide = T, colour="#00BA38")+
    geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=RET_CI_LOW,ymax=RET_CI_HIGH),show_guide = F,alpha=0.25,colour="#00BA38",fill="#00BA38")+#, fill="steelblue2", color="steelblue3")+
    geom_line(data=DataFrame, aes(x=MINUTES, y=VOL_MEAN), size=1.5,alpha=1,show_guide = T, colour="#F8766D")+
    geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=VOL_CI_LOW,ymax=VOL_CI_HIGH),show_guide = F,alpha=0.25,colour="#F8766D",fill="#F8766D")+#, fill="steelblue3", color="steelblue3")+    xlab("Minute Lags") + 
    
    xlab("Minute Lags") + 
    ylab(MyYLabel) +
    ggtitle(MyTitle)+
    theme(title = element_text(size = 28, face = "bold")) + 
    theme(axis.text.x = element_text(size = 24)) + 
    theme(axis.title.x = element_text(size = 24)) +
    theme(axis.text.y = element_text(size = 24)) + 
    theme(axis.title.y = element_text(size = 24)) +
    theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
          legend.text = element_text(size = 22)) + theme(legend.position = "bottom", 
                                                         legend.title = element_blank())
#     theme(legend.position = "bottom")+
#     theme(axis.text=element_text(size=16),
#           axis.title=element_text(size=18,face="bold")) +
#     theme(plot.title = element_text(size = 25, face = "bold"))
  g <- g + geom_vline(aes(xintercept=0),colour = 'black', size = 1.5,linetype="dashed")
  g <- g + scale_colour_brewer(palette = "Greens")
  if (!is.null(FullExportingPath)){
    RP_ExportPlot(g,FullExportingPath,"")
  }
  return(g)
}



RP_PlotCIIntervalSuperposedDashed <- function(DataFrameOne, DataFrameTwo, DataFrameThree, Title = "", names, FullExportingPath=NULL, type = 'RET') {
  
  MyTitle <- ""
  if (type == "VOL"){
    toplotDFOne<- DataFrameOne[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
    colnames(toplotDFOne) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    toplotDFTwo<- DataFrameTwo[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
    colnames(toplotDFTwo) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    toplotDFThree<- DataFrameThree[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
    colnames(toplotDFThree) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    #     toplotDFOne <- toplotDFOne +0.8
    #     toplotDFOne$MINUTES <- toplotDFOne$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFOne[,-1]))
    toplotDFOne <- toplotDFOne+translatingFactor
    toplotDFOne$MINUTES <- toplotDFOne$MINUTES - translatingFactor
    
    #     
    #     toplotDFTwo <- toplotDFTwo +0.8
    #     toplotDFTwo$MINUTES <- toplotDFTwo$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFTwo[,-1]))
    toplotDFTwo <- toplotDFTwo+translatingFactor
    toplotDFTwo$MINUTES <- toplotDFTwo$MINUTES - translatingFactor
    
    #     toplotDFThree <- toplotDFThree +0.8
    #     toplotDFThree$MINUTES <- toplotDFThree$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFThree[,-1]))
    toplotDFThree <- toplotDFThree+translatingFactor
    toplotDFThree$MINUTES <- toplotDFThree$MINUTES - translatingFactor
    
    MyTitle <- "Abnormal volatility"
    MyYLabel <- "Abnormality ratio"
    
    toplotDFOne <- toplotDFOne[abs(toplotDFOne$MINUTES)<= 120,]
    toplotDFTwo <- toplotDFTwo[abs(toplotDFTwo$MINUTES)<= 120,]
    toplotDFThree <- toplotDFThree[abs(toplotDFThree$MINUTES)<= 120,]
  }
  
  if(type =="RET"){
    toplotDFOne<- DataFrameOne[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFOne) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFTwo<- DataFrameTwo[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFTwo) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFThree<- DataFrameThree[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFThree) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    MyTitle <- "Abnormal returns"
    MyYLabel <- "BPS cumulated abnormal return"
    
    toplotDFOne <- toplotDFOne[abs(toplotDFOne$MINUTES)<= 120,]
    toplotDFTwo <- toplotDFTwo[abs(toplotDFTwo$MINUTES)<= 120,]
    toplotDFThree <- toplotDFThree[abs(toplotDFThree$MINUTES)<= 120,]
  }
  
  if(type =="VOLU"){
    toplotDFOne<- DataFrameOne[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
    colnames(toplotDFOne) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFTwo<- DataFrameTwo[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
    colnames(toplotDFTwo) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFThree<- DataFrameThree[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
    colnames(toplotDFThree) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    #     toplotDFOne <- toplotDFOne +0.8
    #     toplotDFOne$MINUTES <- toplotDFOne$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFOne[,-1]))
    toplotDFOne <- toplotDFOne+translatingFactor
    toplotDFOne$MINUTES <- toplotDFOne$MINUTES - translatingFactor
    
    #     
    #     toplotDFTwo <- toplotDFTwo +0.8
    #     toplotDFTwo$MINUTES <- toplotDFTwo$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFTwo[,-1]))
    toplotDFTwo <- toplotDFTwo+translatingFactor
    toplotDFTwo$MINUTES <- toplotDFTwo$MINUTES - translatingFactor
    
    #     toplotDFThree <- toplotDFThree +0.8
    #     toplotDFThree$MINUTES <- toplotDFThree$MINUTES - 0.8
    translatingFactor <- (1.2-min(toplotDFThree[,-1]))
    toplotDFThree <- toplotDFThree+translatingFactor
    toplotDFThree$MINUTES <- toplotDFThree$MINUTES - translatingFactor
    
    
    MyTitle <- "Abnormal volume"
    MyYLabel <- "Abnormality ratio"
    
  }
  
  
  print("superposing") 
  colnames(toplotDFOne) <- paste0("FREQ_",colnames(toplotDFOne))
  colnames(toplotDFOne) <- c("MINUTES","FREQUENCY DOWN","FREQUENCY","FREQUENCY UP")
  
  colnames(toplotDFOne)[1] <- "MINUTES"
  
  colnames(toplotDFTwo) <- paste0("RET_",colnames(toplotDFTwo))
  colnames(toplotDFTwo)[1] <- "MINUTES"
  colnames(toplotDFOne) <- c("MINUTES","RETURN DOWN","RETURN","RETURN UP")
  
  colnames(toplotDFThree) <- paste0("VOL_",colnames(toplotDFThree))
  colnames(toplotDFThree)[1] <- "MINUTES"
  colnames(toplotDFOne) <- c("MINUTES","VOLATILITY DOWN","VOLATILITY","VOLATILITY UP")
  
  
  DataFrame <- merge(toplotDFOne, toplotDFTwo, by="MINUTES")
  DataFrame <- merge(DataFrame, toplotDFThree, by="MINUTES")
  
  
  DataFrame <-  melt(DataFrame,"MINUTES")
  DataFrame$variable <- as.factor(DataFrame$variable)
  
  g <- ggplot(DataFrame,aes(x=MINUTES, y=value, fill=variable, group=variable, linetype = variable,color = variable))+
    # geom_line(aes(linetype=variable, color=variable), size=1.2,)+
    geom_line(size=1.2,alpha=0.75)+
    scale_linetype_manual(values=c("dashed","solid","dashed","dashed","solid","dashed","dashed","solid","dashed"))+
    scale_color_manual(values=c("blue","blue","blue","green","green","green","red","red","red"))+
    labs(color = "Metrics",linetype=  "Metrics")+
    labs(fill="",color = "Metrics",linetype=  "Metrics")+
    # scale_size(guide = "none")+
    # scale_fill_discrete(breaks=c("FREQ_MEAN","RET_MEAN","VOL_MEAN"))+
    # scale_fill_discrete(breaks=c("FREQ_MEAN","RET_MEAN","VOL_MEAN"), values=c("blue","blue","blue","green","green","green","red","red","red"))+
    # scale_fill_manual(breaks=c("FREQ_MEAN","RET_MEAN","VOL_MEAN"), values=c("blue","green","red"))+
    #     scale_fill_manual(breaks=c("FREQ_MEAN","RET_MEAN","VOL_MEAN"),
    #                      +
    #, colour="blue")+
    #     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=FREQ_CI_LOW,ymax=FREQ_CI_HIGH),show_guide = F,alpha=0.25,colour="blue",fill="blue")+#, fill="steelblue1", color="steelblue1")+
    #     geom_line(data=DataFrame, aes(x=MINUTES, y=RET_MEAN), size=1.2,show_guide = T)+#, colour="red")+
    #     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=RET_CI_LOW,ymax=RET_CI_HIGH),show_guide = F,alpha=0.25,colour="green",fill="green")+#, fill="steelblue2", color="steelblue3")+
    #     geom_line(data=DataFrame, aes(x=MINUTES, y=VOL_MEAN), size=1.2,show_guide = T)+#, colour="red")+
  #     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=VOL_CI_LOW,ymax=VOL_CI_HIGH),show_guide = F,alpha=0.25,colour="red",fill="red")+#, fill="steelblue3", color="steelblue3")+
  xlab("Minute Lags") + 
    ylab(MyYLabel) +
    ggtitle(MyTitle)+
    theme(title = element_text(size = 28, face = "bold")) + 
    theme(axis.text.x = element_text(size = 24)) + 
    theme(axis.title.x = element_text(size = 24)) +
    theme(axis.text.y = element_text(size = 24)) + 
    theme(axis.title.y = element_text(size = 24)) +
    theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
          legend.text = element_text(size = 22)) + theme(legend.position = "bottom", 
                                                         legend.title = element_blank())
  #     theme(legend.position = "bottom")+
  
    # scale_colour_manual(name="Legend", values = c("a" = "black", "b" = "red", "c" = "blue","a" = "black", "b" = "red", "c" = "blue","a" = "black", "b" = "red", "c" = "blue")) +
    # scale_linetype_manual(name="Legend", values = c("a" = "dashed", "b" = "dotted", "c" = "dotted")) +
    
    # scale_colour_manual(name="Legend", values = c("FREQ_MEAN" = "blue", "RET_MEAN" = "green", "VOL_MEAN" = "red")) +
    # scale_linetype_manual(name="Legend", values = c("FREQ_MEAN" = "dashed", "RET_MEAN" = "dotted", "VOL_MEAN" = "dotted")) +
    # adjust the colours to those you wanted
    # scale_colour_manual(values = c("black","red", "blue"))+
    # stick the legend on the bottom
    # guides(color=guide_legend("my title"))+
#     theme( legend.position = "bottom")+
#     theme(axis.text=element_text(size=16),
#           axis.title=element_text(size=18,face="bold")) +
#     theme(plot.title = element_text(size = 25, face = "bold"))
  g <- g + geom_vline(aes(xintercept=0),colour = 'black', size = 1.5,linetype="dashed")
  # g <- g + scale_colour_brewer(palette = "Greens")
  
  #   g <- ggplot(DataFrame)+
  #     geom_line(data=DataFrame, aes(x=MINUTES, y=FREQ_MEAN), size=1.2,show_guide = T)+#, colour="blue")+
  #     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=FREQ_CI_LOW,ymax=FREQ_CI_HIGH),show_guide = F,alpha=0.25,colour="blue",fill="blue")+#, fill="steelblue1", color="steelblue1")+
  #     geom_line(data=DataFrame, aes(x=MINUTES, y=RET_MEAN), size=1.2,show_guide = T)+#, colour="red")+
  #     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=RET_CI_LOW,ymax=RET_CI_HIGH),show_guide = F,alpha=0.25,colour="green",fill="green")+#, fill="steelblue2", color="steelblue3")+
  #     geom_line(data=DataFrame, aes(x=MINUTES, y=VOL_MEAN), size=1.2,show_guide = T)+#, colour="red")+
  #     geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=VOL_CI_LOW,ymax=VOL_CI_HIGH),show_guide = F,alpha=0.25,colour="red",fill="red")+#, fill="steelblue3", color="steelblue3")+
  #     xlab("Minute Lags") + 
  #     ylab(MyYLabel) +
  #     ggtitle(MyTitle)+
  #     scale_colour_manual(name="Legend", values = c("FREQ_MEAN" = "blue", "RET_MEAN" = "green", "VOL_MEAN" = "red")) +
  #     # scale_linetype_manual(name="Legend", values = c("FREQ_MEAN" = "dashed", "RET_MEAN" = "dotted", "VOL_MEAN" = "dotted")) +
  #     # adjust the colours to those you wanted
  #     scale_colour_manual(values = c("black","red", "blue"))+
  #   # stick the legend on the bottom
  #     # theme( legend.position = "bottom")+
  #     theme(axis.text=element_text(size=16),
  #           axis.title=element_text(size=18,face="bold")) +
  #     theme(plot.title = element_text(size = 25, face = "bold"))
  #   g <- g + geom_vline(aes(xintercept=0),colour = 'black', size = 1.5,linetype="dashed")
  #   g <- g + scale_colour_brewer(palette = "Greens")
  if (!is.null(FullExportingPath)){
    RP_ExportPlot(g,FullExportingPath,"")
  }
  return(g)
}

RP_PlotMetricsTogetherWithCI <- function(agg_rowProfileCtrash,my_metrics,aggregate_criteria, type){
  
  # for (i)
  stats_sign <- colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash))))]
  rets <- paste0("RET",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash))))])
  ord_stats_sign <- paste0("ORD",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash))))])
  vol_stats_sign <- paste0("VOLU",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash))))])
  vola_stats_sign <- paste0("VOLA",colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash))))])
  
  stats_sign <- agg_rowProfileCtrash[,stats_sign]
  rets <- agg_rowProfileCtrash[,rets]
  colnames(rets) <- colnames(stats_sign)
  ord_stats_sign <- agg_rowProfileCtrash[,ord_stats_sign]
  colnames(ord_stats_sign) <- colnames(stats_sign)
  vol_stats_sign <- agg_rowProfileCtrash[,vol_stats_sign]
  colnames(vol_stats_sign) <- colnames(stats_sign)
  vola_stats_sign <- agg_rowProfileCtrash[,vola_stats_sign]
  colnames(vola_stats_sign) <- colnames(stats_sign)
  
  
  vol_stats_sign_one   <- vol_stats_sign[1:3,]
  vol_stats_sign_two   <- vol_stats_sign[4:6,]
  vol_stats_sign_three <- vol_stats_sign[7:9,]
  
  vola_stats_sign_one   <- vola_stats_sign[1:3,]
  vola_stats_sign_two   <- vola_stats_sign[4:6,]
  vola_stats_sign_three <- vola_stats_sign[7:9,]
  
  rets_one   <- rets[1:3,]
  rets_two   <- rets[4:6,]
  rets_three <- rets[7:9,]
  
  dataframe_one <- as.data.frame(t(rbind(vol_stats_sign_one,vola_stats_sign_one,rets_one)))
  dataframe_two  <- as.data.frame(t(rbind(vol_stats_sign_two,vola_stats_sign_two,rets_two)))
  dataframe_three <- as.data.frame(t(rbind(vol_stats_sign_three,vola_stats_sign_three,rets_three)))
  
  colnames(dataframe_one) <- c("VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframe_one$MINUTES <- as.numeric(colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash))))])
  dataframe_one <- dataframe_one[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  colnames(dataframe_two) <- c("VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframe_two$MINUTES <- as.numeric(colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash))))])
  dataframe_two <- dataframe_two[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  
  colnames(dataframe_three) <- c("VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframe_three$MINUTES <- as.numeric(colnames(agg_rowProfileCtrash)[which(!is.na(as.numeric(colnames(agg_rowProfileCtrash))))])
  dataframe_three <- dataframe_three[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  if(dashed){
     g <- RP_PlotCIIntervalSuperposedDashed(DataFrameOne =  dataframe_one,DataFrameTwo =  dataframe_two, DataFrameThree = dataframe_three,Title = MyTitle, my_metrics, FullExportingPath =  NULL,type)
    
  } else {
    g <- RP_PlotCIIntervalSuperposed(DataFrameOne =  dataframe_one,DataFrameTwo =  dataframe_two, DataFrameThree = dataframe_three,Title = MyTitle, my_metrics, FullExportingPath =  NULL,type)
    
  }
  
  
  return(g)
}

computeCIIntervalBound <- function(data,counter,event){
  rowProfileToPlot <- data[1:3,]
  rowProfileToPlot$my_event <- event
  stats_sign <- colnames(rowProfileToPlot)[which(!is.na(as.numeric(colnames(rowProfileToPlot))))]
  rets <- paste0("RET",colnames(rowProfileToPlot)[which(!is.na(as.numeric(colnames(rowProfileToPlot))))])
  ord_stats_sign <- paste0("ORD",colnames(rowProfileToPlot)[which(!is.na(as.numeric(colnames(rowProfileToPlot))))])
  vol_stats_sign <- paste0("VOLU",colnames(rowProfileToPlot)[which(!is.na(as.numeric(colnames(rowProfileToPlot))))])
  vola_stats_sign <- paste0("VOLA",colnames(rowProfileToPlot)[which(!is.na(as.numeric(colnames(rowProfileToPlot))))])
  
  
  
  
  allNumericColumns <- c(stats_sign,rets,vola_stats_sign,vol_stats_sign)
  allRetsColumns <- rets
  
  #   print("problem in the code before : rescale")
  # dataGroupBest[,allRetsColumns] <- dataGroupBest[,allRetsColumns]*10
  
  
  my_df <- data[1:counter,allNumericColumns]
  my_diff_df <- differentiateRets(data[1:counter,allRetsColumns])
  
  print("computing the CI interval")
  event_minutes_ci_matrix <- apply(my_df,2,FUN=computeCIbound)
  print("diff return CI")
  event_minutes_diff_ci_matrix <- apply(my_diff_df,2,FUN=computeCIbound)
  print("CI done")
  
  rowProfileToPlot[1,allNumericColumns] <- event_minutes_ci_matrix[1,]
  # rowProfileToPlot[2,allNumericColumns] <- event_minutes_ci_matrix[2,]
  rowProfileToPlot[2,allNumericColumns] <- colMeans(data[1:counter,allNumericColumns],na.rm = TRUE)
  
  rowProfileToPlot[3,allNumericColumns] <- event_minutes_ci_matrix[3,]
  event_index <- (length(abs(event_minutes_diff_ci_matrix[1,]))-1)/2
  
  lowerbound <- abs(event_minutes_diff_ci_matrix[1,])
  upperbound <- abs(event_minutes_diff_ci_matrix[3,])
  
  
  upperbound[(event_index+1):length(upperbound)] <- 0.5*cumsum(upperbound)
  lowerbound[(event_index+1):length(lowerbound)] <- 0.5*cumsum(lowerbound)
  
  upperbound[event_index:1] <- 0.3*cumsum(upperbound[event_index:1])
  lowerbound[event_index:1] <- 0.3*cumsum(lowerbound[event_index:1])
  
  rowProfileToPlot[1,allRetsColumns] <-  event_minutes_ci_matrix[2,allRetsColumns] - lowerbound
  # rowProfile[2,allRetsColumns] <- event_minutes_ci_matrix[2,]
  rowProfileToPlot[3,allRetsColumns] <- event_minutes_ci_matrix[2,allRetsColumns] + upperbound
  
  numeric_columns <- c(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)
  
  
  return(rowProfileToPlot)
}



trimBest <- function(dfrow){
  trimmed_df <- dfrow[dfrow$RANKING >= max(dfrow$RANKING),]
  toReturn <- trimmed_df[1,]
  stats_prepost <- colnames(dfrow)[which(as.numeric(colnames(dfrow))>= -180)]
  ret_prepost <- paste0("RET",stats_prepost)
  if(toReturn$sentiment == "NEGATIVE"){
    toReturn[,ret_prepost] <- -toReturn[,ret_prepost]
  }
  
  toReturn$event_number_event_filtering <- max(dfrow[,c("event_number_event_filtering")])
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

five_stars <- function(col){
  my_unique_breaks <- unique(quantile(col, probs=seq(0,1, by=1/5)))
  my_cuts <- as.integer(cut(col,breaks=my_unique_breaks,include.lowest=TRUE))
  return(my_cuts)
}

################################
################################
################################
################################
################################
################################
################################ End of Plotting functions



library("boot")

# my dash selection
dashed <- FALSE

all_data <- readRDS(file=paste0(outputDataPath,"metrics_clean_prod_spr_r1000_bigdataf_abvol_abvol_corrado_df.rds"))

# all_data <- all_data[!grepl("same-store-sales",all_data$my_event),]
# 
# all_data <- all_data[!grepl("bankruptcy",all_data$my_event),]
# all_data <- all_data[!grepl("assets",all_data$my_event),]
# all_data <- all_data[!grepl("technical",all_data$my_event),]
# 
# all_data <- all_data[!grepl("imbalance",all_data$my_event),]
# all_data <- all_data[!grepl("assets",all_data$my_event),]
all_data <- all_data[!grepl("stock-loss",all_data$my_event),]
all_data <- all_data[!grepl("stock-gain",all_data$my_event),]
all_data <- all_data[!grepl("stock-prices",all_data$my_event),]
# all_data <- all_data[!grepl("regulatory",all_data$my_event),]
# 
# all_data <- all_data[!grepl("stock-loss",all_data$my_event),]
# all_data <- all_data[!grepl("stock-gain",all_data$my_event),]
# all_data <- all_data[!grepl("stock-prices",all_data$my_event),]
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



table_size_limit <- 20

all_group_events <- readRDS(file=paste0(outputDataPath,"prod_bigdata_all_group_events.rds"))
all_category_events <- readRDS(file=paste0(outputDataPath,"prod_bigdata_all_category_events.rds"))

# for (my_source in c("DJPR","PREMIUM_PACK","WEB_NON_PREMIUM")){
for (my_source in c("DJPR","PREMIUM_PACK","WEB_NON_PREMIUM","ALL")){
  
  if(!(my_source == "ALL")){
    data <- all_data[all_data$localSource == my_source,]
  } else {
    data <- all_data
  }
  
  data <- data[data$similarity_gap_filter == 1,]
  data <- data[data$event_relevance == "HIGH",]
  data <- data[data$relevance == "HIGH",]
  data <- data[data$sentiment_criteria != "SPREAD",]
  
  print(my_source)
  print(dim(data))
  
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
  
  dataGroup <- data[data$aggregate_criteria =="GROUP",]
  dataCategory <- data[data$aggregate_criteria =="CATEGORY",]

  
  agg_rowProfileG <- NULL
  agg_rowProfileC <- NULL
  agg_rowProfileGtrash <- NULL
  agg_rowProfileCtrash <- NULL
  
  #   clean_counter <- 10
  #   trash_counter <- 20
  
  clean_counter_group <- 5
  trash_counter_group <- 20
  
  clean_counter <- 20
  trash_counter <- 100

  
  trash_confidence_counter <- 20
  
  

  
  
  #   g <- RP_PlotMetricsTogetherWithCI(agg_rowProfileCtrash,my_metrics ,"CATEGORY","RET")
  #   
  #   print(g)
  
  for(my_metric in my_metrics){
    print("my_metric")
    print(my_metric)
    
    dataG <- data[data$aggregate_criteria == "GROUP",]
    if(my_metric == "card_post_ranked_return"){
      card_significance <- dataG$event_number_event_filtering
      card_significance <- (card_significance-min(card_significance))/(max(card_significance)-min(card_significance))
      
      dataG$RANKING <- card_significance*dataG$post_ranked_return
      dataG$card_post_ranked_return <-   dataG$RANKING
    } else {
      dataG$RANKING <- dataG[,my_metric]
    }
    

    dataMetricTrimmedG  <- ddply(.data = dataG,.variables = "my_event",.fun = function(x){trimBest(x)})
    
    
    ######################
    ######################
    ######################
    ###################### Average profiles
    
    dataMetricTrimmedG <- dataMetricTrimmedG[order(dataMetricTrimmedG$RANKING,decreasing = TRUE),]

    
    print("computing cnofidence bounds")
    rowProfileG <- computeCIIntervalBound(dataMetricTrimmedG,clean_counter_group,my_metric)
    
    if(is.null(agg_rowProfileG)){
      agg_rowProfileG <- rowProfileG
    } else {
      agg_rowProfileG <- rbind(agg_rowProfileG,rowProfileG)
    }
    

    print("computing cnofidence bounds")
    rowProfileGtrash <- computeCIIntervalBound(dataMetricTrimmedG,trash_counter_group,my_metric)
    if(is.null(agg_rowProfileGtrash)){
      agg_rowProfileGtrash <- rowProfileGtrash
    } else {
      agg_rowProfileGtrash <- rbind(agg_rowProfileGtrash,rowProfileGtrash)
    }
    
    
    
    
    
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
    
    

    
    rowProfileC <- dataMetricTrimmedC[1,]


    print("computing cnofidence bounds")
    rowProfileC <- computeCIIntervalBound(dataMetricTrimmedC,clean_counter,my_metric)
    print("end of computing cnofidence bounds")
    if(is.null(agg_rowProfileC)){
      agg_rowProfileC <- rowProfileC
    } else {
      agg_rowProfileC <- rbind(agg_rowProfileC,rowProfileC)
    }
    
    
    
    print("computing cnofidence bounds")
    rowProfileCtrash <- computeCIIntervalBound(dataMetricTrimmedC,trash_counter,my_metric)
    print("end of computing cnofidence bounds")
    
    #     results <- RP_PlotProfile(rowProfile)
    #     print(results$gret)
    #     
    
    if(is.null(agg_rowProfileCtrash)){
      agg_rowProfileCtrash <- rowProfileCtrash
    } else {
      agg_rowProfileCtrash <- rbind(agg_rowProfileCtrash,rowProfileCtrash)
    }
    
#     ######################
#     ######################
#     ######################
#     ###################### Together plot
#     results <- getPlotBestProfilesTogether("GROUP",dataMetricTrimmed,all_group_events,all_category_events,my_metric,xlim=limitations, plotLimit = 10 )
#     print(results$gret)
#     gretgp1 <- results$gret
#     print(results$gvol)
#     gvolgp1 <- results$gvol
#     
#     results <- getPlotBestProfilesTogether("CATEGORY",dataMetricTrimmed,all_group_events,all_category_events,my_metric,xlim=limitations, plotLimit = 20 )
#     print(results$gret)
#     gretcat1 <- results$gret
#     print(results$gvol)
#     gvolcat1 <- results$gvol
#     
#     
#     if (my_metric == "volatility_correction"){
#       width=20
#       height=22.5
#       gname <- ExportMultiplot(gvolgp1,gvolcat1,
#                                plotlist = NULL, filename = paste0("trash_bigdata_bests",my_source,my_metric), outputDataPath = paste0(outputDataPath,"PAPER_PICTURES/"), cols = 2, width = width, height = height)
#       
#     } else {
#       
#       width=20
#       height=22.5
#       gname <- ExportMultiplot(gretgp1,gvolgp1,
#                                gretcat1,gvolcat1,
#                                plotlist = NULL, filename = paste0("trash_bigdata_bests",my_source,my_metric), outputDataPath = paste0(outputDataPath,"PAPER_PICTURES/"), cols = 2, width = width, height = height)
#       
#       
#     }
#     
#     print("if you want real clean, drop some of the categories")
#     
#     results <- getPlotBestProfilesTogether("GROUP",dataMetricTrimmed,all_group_events,all_category_events,my_metric,xlim=limitations)
#     print(results$gret)
#     gretgp1 <- results$gret
#     print(results$gvol)
#     gvolgp1 <- results$gvol
#     
#     results <- getPlotBestProfilesTogether("CATEGORY",dataMetricTrimmed,all_group_events,all_category_events,my_metric,xlim=limitations)
#     print(results$gret)
#     gretcat1 <- results$gret
#     print(results$gvol)
#     gvolcat1 <- results$gvol
#     
#     
#     if (my_metric == "volatility_correction"){
#       width=20
#       height=22.5
#       gname <- ExportMultiplot(gvolgp1,gvolcat1,
#                                plotlist = NULL, filename = paste0("clean_bigdata_bests",my_source,my_metric), outputDataPath = paste0(outputDataPath,"PAPER_PICTURES/"), cols = 2, width = width, height = height)
#       
#     } else {
#       
#       width=20
#       height=22.5
#       gname <- ExportMultiplot(gretgp1,gvolgp1,
#                                gretcat1,gvolcat1,
#                                plotlist = NULL, filename = paste0("clean_bigdata_bests",my_source,my_metric), outputDataPath = paste0(outputDataPath,"PAPER_PICTURES/"), cols = 2, width = width, height = height)
#       
#       
#     }
    
  }
  
  print("displaying all metrics together")
  #   g <- RP_PlotMetricsTogetherWithCI(agg_rowProfileCtrash,my_metrics ,"CATEGORY","RET")
  #   
  #   print(g)
  gret <- RP_PlotMetricsTogetherWithCI(agg_rowProfileCtrash,my_metrics ,"CATEGORY",type="RET")
  gvol <- RP_PlotMetricsTogetherWithCI(agg_rowProfileCtrash,my_metrics ,"CATEGORY",type="VOL")
  print(gret)
  print(gvol)
  RP_ExportPlot(gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(trash_counter,my_source,dashed,"improved_trash_category_return"))
  RP_ExportPlot(gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(trash_counter,my_source,dashed,"improved_trash_category_volatility"))

  RP_ExportPlot(gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(my_source,dashed,"CAT_RET"))
  RP_ExportPlot(gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(my_source,dashed,"CAT_VOL"))
  
    
#   results <- RP_PlotMetricsTogether(agg_rowProfileC,my_metrics ,"CATEGORY")
#   
  gret <- RP_PlotMetricsTogetherWithCI(agg_rowProfileC,my_metrics ,"CATEGORY",type="RET")
  gvol <- RP_PlotMetricsTogetherWithCI(agg_rowProfileC,my_metrics ,"CATEGORY",type="VOL")
  print(gret)
  print(gvol)
  RP_ExportPlot(gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(clean_counter,my_source,dashed,"improved_clean_category_return"))
  RP_ExportPlot(gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(clean_counter,my_source,dashed,"improved_clean_category_volatility"))
  
  
  gret <- RP_PlotMetricsTogetherWithCI(agg_rowProfileGtrash,my_metrics ,"GROUP",type="RET")
  gvol <- RP_PlotMetricsTogetherWithCI(agg_rowProfileGtrash,my_metrics ,"GROUP",type="VOL")
  print(gret)
  print(gvol)
  RP_ExportPlot(gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(trash_counter,my_source,dashed,"improved_trash_group_return"))
  RP_ExportPlot(gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(trash_counter,my_source,dashed,"improved_trash_group_volatility"))
  RP_ExportPlot(gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(my_source,dashed,"GROUP_RET"))
  RP_ExportPlot(gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(my_source,dashed,"GROUP_VOL"))
  
  gret <- RP_PlotMetricsTogetherWithCI(agg_rowProfileG,my_metrics ,"GROUP",type="RET")
  gvol <- RP_PlotMetricsTogetherWithCI(agg_rowProfileG,my_metrics ,"GROUP",type="VOL")
  print(gret)
  print(gvol)
  RP_ExportPlot(gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(clean_counter,my_source,dashed,"improved_clean_group_return"))
  RP_ExportPlot(gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(clean_counter,my_source,dashed,"improved_clean_group_volatility"))
  
  
  
  #   
  #   
  #   results <- RP_PlotMetricsTogether(agg_rowProfileGtrash,my_metrics ,"GROUP")
  #   print(results$gret)
  #   print(results$gvol)
  #   RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(my_source,trash_counter,"improved_trash_group_return"))
  #   RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/CONFIDENCE/"), filename = paste0(my_source,trash_counter,"improved_trash_group_volatility"))
  #   
  #   
  #   results <- RP_PlotMetricsTogether(agg_rowProfileG,my_metrics ,"GROUP")
  #   print(results$gret)
  #   print(results$gvol)
  #   RP_ExportPlot(results$gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/"), filename = paste0(my_source,clean_counter,"improved_clean_group_return"))
  #   RP_ExportPlot(results$gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/"), filename = paste0(my_source,clean_counter,"improved_clean_group_volatility"))
  #   
  print("best profiles graphics computed")
  
}