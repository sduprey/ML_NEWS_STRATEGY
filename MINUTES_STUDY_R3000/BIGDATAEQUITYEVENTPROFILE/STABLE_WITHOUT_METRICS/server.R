library(shiny)
library(shinyjs)

library(DT)
library("RPPlotUtils")
library("RPToolsDB")

library("png")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")


##################
##################
##################
################## Big data file loading

bigdata_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"nnew_spr_r1000_bigdata_abvol_abvol_corrado_df.rds"))
bigdata_best_profile_group_ordered_r1000_corrado_df <- readRDS(file=paste0(outputDataPath,"new_bigdata_best_profile_group_ordered_r1000_corrado_df.rds"))
bigdata_best_profile_category_ordered_r1000_corrado_df <- readRDS(file=paste0(outputDataPath,"new_bigdata_best_profile_category_ordered_r1000_corrado_df.rds"))

# dataTotr2000 <- readRDS(file=paste0(outputDataPath,"spr_r2000_bigdata_abvol_abvol_corrado_df.rds"))
# best_profile_group_ordered_r2000_corrado_df <- readRDS(file=paste0(outputDataPath,"bigdata_best_profile_group_ordered_r2000_corrado_df.rds"))
# best_profile_category_ordered_r2000_corrado_df <- readRDS(file=paste0(outputDataPath,"bigdata_best_profile_category_ordered_r2000_corrado_df.rds"))


bigdata_all_group_events <- readRDS(file=paste0(outputDataPath,"new_bigdata_all_group_events.rds"))
bigdata_all_category_events <- readRDS(file=paste0(outputDataPath,"new_bigdata_all_category_events.rds"))

######################################
######################################
######################################
###################################### Computing the reference metric

bigdata_dataTotr1000$correcting_factor <- 2*(bigdata_dataTotr1000$sentiment_criteria == "POSITIVE"  | bigdata_dataTotr1000$sentiment_criteria == "SPREAD" )-1
bigdata_dataTotr1000$correcting_factor[bigdata_dataTotr1000$sentiment_criteria == "ALL"] <- 0
stats_post_sign <- colnames(bigdata_dataTotr1000)[which(as.numeric(colnames(bigdata_dataTotr1000)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)


infinity_return <- rowSums(bigdata_dataTotr1000[,rets_post]*bigdata_dataTotr1000$correcting_factor*bigdata_dataTotr1000[,stats_post_sign],na.rm = TRUE)

# bigdata_dataTotr1000$infinity_return <- rowSums(bigdata_dataTotr1000[,rets_post]*bigdata_dataTotr1000$correcting_factor*bigdata_dataTotr1000[,stats_post_sign],na.rm = TRUE)

infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
significance <- (bigdata_dataTotr1000$event_number_event_filtering-min(bigdata_dataTotr1000$event_number_event_filtering))/(max(bigdata_dataTotr1000$event_number_event_filtering)-min(bigdata_dataTotr1000$event_number_event_filtering))
infinity_return <- infinity_return*significance
bigdata_dataTotr1000$infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))



bigdata_dataTotr1000 <- bigdata_dataTotr1000[bigdata_dataTotr1000$event_number_event_filtering >= 50,]

bigdata_dataTotr1000$infinity_return <- (bigdata_dataTotr1000$infinity_return-min(bigdata_dataTotr1000$infinity_return))/(max(bigdata_dataTotr1000$infinity_return)-min(bigdata_dataTotr1000$infinity_return))


# bigdata_dataTotr2000$correcting_factor <- 2*(bigdata_dataTotr2000$sentiment_criteria == "POSITIVE"  | bigdata_dataTotr2000$sentiment_criteria == "SPREAD" )-1
# bigdata_dataTotr2000$correcting_factor[bigdata_dataTotr2000$sentiment_criteria == "ALL"] <- 0
# stats_post_sign <- colnames(bigdata_dataTotr2000)[which(as.numeric(colnames(bigdata_dataTotr2000)) >= 0)]
# rets_post <- paste0("RET",stats_post_sign)
# bigdata_dataTotr2000$infinity_return <- rowSums(bigdata_dataTotr2000[,rets_post]*bigdata_dataTotr2000$correcting_factor*bigdata_dataTotr2000[,stats_post_sign],na.rm = TRUE)
# bigdata_dataTotr2000 <- bigdata_dataTotr2000[bigdata_dataTotr2000$event_number_event_filtering >= 50,]
##################
##################
##################
################## End of big data file loading



##################
##################
##################
################## RPNA file loading
# rpna_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"nnew_spr_r1000_rpna_abvol_abvol_corrado_df.rds"))
rpna_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"clean_prod_spr_r1000_rpna_abvol_abvol_corrado_df.rds"))

rpna_best_profile_group_ordered_r1000_corrado_df <- readRDS(file=paste0(outputDataPath,"clean_prod_rpna_best_profile_group_ordered_r1000_corrado_df.rds"))
rpna_best_profile_category_ordered_r1000_corrado_df <- readRDS(file=paste0(outputDataPath,"clean_prod_rpna_best_profile_category_ordered_r1000_corrado_df.rds"))

# dataTotr2000 <- readRDS(file=paste0(outputDataPath,"spr_r2000_rpna_abvol_abvol_corrado_df.rds"))
# best_profile_group_ordered_r2000_corrado_df <- readRDS(file=paste0(outputDataPath,"rpna_best_profile_group_ordered_r2000_corrado_df.rds"))
# best_profile_category_ordered_r2000_corrado_df <- readRDS(file=paste0(outputDataPath,"rpna_best_profile_category_ordered_r2000_corrado_df.rds"))

rpna_all_group_events <- readRDS(file=paste0(outputDataPath,"prod_rpna_all_group_events.rds"))
rpna_all_category_events <- readRDS(file=paste0(outputDataPath,"prod_rpna_all_category_events.rds"))

# rpna_dataTotr1000$correcting_factor <- 2*(rpna_dataTotr1000$sentiment_criteria == "POSITIVE"  | rpna_dataTotr1000$sentiment_criteria == "SPREAD" )-1
# rpna_dataTotr1000$correcting_factor[rpna_dataTotr1000$sentiment_criteria == "ALL"] <- 0
# stats_post_sign <- colnames(rpna_dataTotr1000)[which(as.numeric(colnames(rpna_dataTotr1000)) >= 0)]
# rets_post <- paste0("RET",stats_post_sign)
# 
# infinity_return <- rowSums(rpna_dataTotr1000[,rets_post]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_post_sign],na.rm = TRUE)
# 
# infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
# significance <- (rpna_dataTotr1000$event_number_event_filtering-min(rpna_dataTotr1000$event_number_event_filtering))/(max(rpna_dataTotr1000$event_number_event_filtering)-min(rpna_dataTotr1000$event_number_event_filtering))
# infinity_return <- infinity_return*significance
# 
# rpna_dataTotr1000$infinity_return <- (infinity_return-min(infinity_return))/(max(infinity_return)-min(infinity_return))
# 
# 
# rpna_dataTotr1000 <- rpna_dataTotr1000[rpna_dataTotr1000$event_number_event_filtering >= 50,]
# 
# rpna_dataTotr1000$infinity_return <- (rpna_dataTotr1000$infinity_return-min(rpna_dataTotr1000$infinity_return))/(max(rpna_dataTotr1000$infinity_return)-min(rpna_dataTotr1000$infinity_return))
# 



# rpna_dataTotr2000$correcting_factor <- 2*(rpna_dataTotr2000$sentiment_criteria == "POSITIVE"  | rpna_dataTotr2000$sentiment_criteria == "SPREAD" )-1
# rpna_dataTotr2000$correcting_factor[rpna_dataTotr2000$sentiment_criteria == "ALL"] <- 0
# stats_post_sign <- colnames(rpna_dataTotr2000)[which(as.numeric(colnames(rpna_dataTotr2000)) >= 0)]
# rets_post <- paste0("RET",stats_post_sign)
# rpna_dataTotr2000$infinity_return <- rowSums(rpna_dataTotr2000[,rets_post]*rpna_dataTotr2000$correcting_factor*rpna_dataTotr2000[,stats_post_sign],na.rm = TRUE)
# rpna_dataTotr2000 <- rpna_dataTotr2000[rpna_dataTotr2000$event_number_event_filtering >= 50,]

##################
##################
##################
################## End of RPNA file loading

################################
################################
################################
################################
################################
################################
################################ Plotting functions

PlotDataFrame <- function (DataFrame, XLab = "", YLab = "", Title = "", AxisIncluded = FALSE, 
                           byRows = FALSE, spread = FALSE, pointOnly = FALSE, lineOnly = TRUE, percent=FALSE,
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
#   ToPlotDataFrame$DASHED_LINE <- (ToPlotDataFrame$variable == "ABNORMAL_THRESHOLD")
#   g <- ggplot(ToPlotDataFrame, aes(x = ToPlotDataFrame[, c(my_column_to_plot_against)], 
#                                    y = value, group = variable, color = variable, fill = variable,linetype = DASHED_LINE))
  
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
  
  if ("DATE" == my_column_to_plot_against) 
    g <- g + scale_x_date()
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

outputGraphicsBestProfileRets <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R1000", Together = FALSE){
  
  if(Together){
    dataframerets <- dataFrame[,c("MINUTES","RDBA_RETS","RPNA_RETS")]
    colnames(dataframerets) <- c("MINUTES","RDBA_RETURNS","RPNA_RETURNS")
  } else {
    dataframerets <- dataFrame[,c("MINUTES","RETS")]
    colnames(dataframerets) <- c("MINUTES","RETURNS")
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
  } else {
    dataframerets <- dataFrame[,c("MINUTES","RETS")]
    colnames(dataframerets) <- c("MINUTES","RETURNS")
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
    
    
    gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
    
    gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
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
  #     gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
  #     
  #     gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
  #     
  #     # eval.parent(expr = paste0("g", i, " <- g"))
  #     
  #     if((rowProfile$my_event %in% all_events) & Counter < plotLimit){
  #       if (Counter == 1){
  #         assign(paste0("gret", Counter), gret)
  #         assign(paste0("gvol", Counter), gvol)
  #         Counter <- Counter+1
  #         Events <- c(Events,rowProfile$my_event)
  #       } else if (!(rowProfile$my_event %in% Events)){
  #         assign(paste0("gret", Counter), gret)
  #         assign(paste0("gvol", Counter), gvol)
  #         Counter <- Counter+1
  #         Events <- c(Events,rowProfile$my_event)
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


function(input, output, session) {
  column_to_plot <- c("my_event",
                      "relevance",
                      "event_relevance",
                      "sentiment_criteria",
                      "similarity_gap_filter",
                      "event_number_event_filtering",
                      "localSource",
                      "infinity_return") 
  
  column_to_plot_name <-  c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SIMILARITY","SENTIMENT","COUNT","SOURCE", "RANKING")
  
  reorderedColumns <- c(column_to_plot, setdiff(colnames(rpna_dataTotr1000), column_to_plot))
  
  column_to_hide_indice <- which(!(reorderedColumns %in% column_to_plot))
  
  shinyjs::disable("sec_my_plot")
  
  shinyjs::disable("event_relevance")
  
  
  
  observe({
    # shinyjs::disable("relevance", selector = "#navbar li a[data-value=tab2]")
    
    # toggle(condition = input$comparative_mode, selector = "#navbar li a[data-value=tab2]")
#     print(paste0("You are viewing tab \"", input$mynavlist, "\""))
#     
    
    if(input$mynavlist == "Comparative Plots"){
      shinyjs::enable("sec_ravenpack_type")
      shinyjs::enable("sec_aggregate_criteria")
      shinyjs::enable("sec_ravenpack_type")
      shinyjs::enable("sec_aggregate_criteria")
      shinyjs::enable("sec_event_number_event_filtering", "Minimum number of event observations")
      shinyjs::enable("sec_my_event")
      shinyjs::enable("sec_my_plot")
      shinyjs::enable("sec_sort_profiles")
      shinyjs::enable("sec_sentiment_criteria")
      shinyjs::enable("sec_relevance")
      shinyjs::enable("sec_event_relevance")
      shinyjs::enable("sec_similarity_gap_filter")
      shinyjs::enable("sec_localSource")
      shinyjs::enable("sec_filtering_criteria")
    } else {
      shinyjs::disable("sec_ravenpack_type")
      shinyjs::disable("sec_aggregate_criteria")
      shinyjs::disable("sec_ravenpack_type")
      shinyjs::disable("sec_aggregate_criteria")
      shinyjs::disable("sec_event_number_event_filtering", "Minimum number of event observations")
      shinyjs::disable("sec_my_event")
      shinyjs::disable("sec_my_plot")
      shinyjs::disable("sec_sort_profiles")
      shinyjs::disable("sec_sentiment_criteria")
      shinyjs::disable("sec_relevance")
      shinyjs::disable("sec_event_relevance")
      shinyjs::disable("sec_similarity_gap_filter")
      shinyjs::disable("sec_localSource")
      shinyjs::disable("sec_filtering_criteria")
    }
    
#     if(input$mynavlist != "Comparative Plots"){
#       output$global_best_profiles_table <-   DT::renderDataTable(firstGlobalBestdata(),selection='single',server = TRUE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
#       
#     } 
    
  })
  
  observeEvent(input$ravenpack_type,{
    
    if (input$ravenpack_type == "RBDA"){
      if(input$similarity_gap_filter %in% c(0,1,7,30,90,186,365)){
        
        updateSelectInput(session, "similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90,186,365),
                          selected = input$similarity_gap_filter
        )
        
      } else {
        updateSelectInput(session, "similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90,186,365),
                          selected = 0
                          )
      }
    } else {
      if(input$similarity_gap_filter %in% c(0,1,7,30,90)){
        
        updateSelectInput(session, "similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90),
                          selected = input$similarity_gap_filter
        )
      } else {
        updateSelectInput(session, "similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90),
                          selected = 0
        )
      }
    }
    
    updatingList <- NULL
    dataTotr1000 <- NULL
    all_group_events <- NULL
    all_category_events <- NULL
    best_profile_group_ordered_r1000_corrado_df <- NULL
    best_profile_category_ordered_r1000_corrado_df <- NULL
    if (input$ravenpack_type == "RBDA"){
      dataTotr1000 <- bigdata_dataTotr1000
      # dataTotr2000 <- bigdata_dataTotr2000
      all_group_events <- bigdata_all_group_events
      all_category_events <- bigdata_all_category_events
      best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
      # best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
      best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
      # best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
    }
    
    if (input$ravenpack_type == "RPNA"){
      dataTotr1000 <- rpna_dataTotr1000
      # dataTotr2000 <- rpna_dataTotr2000
      all_group_events <- rpna_all_group_events
      all_category_events <- rpna_all_category_events
      best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
      # best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
      best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
      # best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
    }
    
    
    if(input$sort_profiles){
      if(input$aggregate_criteria == "GROUP"){
        updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
      }
      if(input$aggregate_criteria == "CATEGORY"){
        updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
      }
    } else {
      if(input$aggregate_criteria == "GROUP"){
        updatingList <- all_group_events
      }
      if(input$aggregate_criteria == "CATEGORY"){
        updatingList <- all_category_events
      }
    }
    
    updateSelectInput(session, "my_event",
                      label ="EVENT",
                      choices = updatingList,
                      selected = input$my_event
    )
  })  
        
        observeEvent(input$sec_ravenpack_type,{
          
          if (input$ravenpack_type == "RBDA"){
            if(input$similarity_gap_filter %in% c(0,1,7,30,90,186,365)){
              
              updateSelectInput(session, "similarity_gap_filter",
                                label ="SIMILARITY EVENT DAYS",
                                choices = c(0,1,7,30,90,186,365),
                                selected = input$similarity_gap_filter
              )
              
            } else {
              updateSelectInput(session, "similarity_gap_filter",
                                label ="SIMILARITY EVENT DAYS",
                                choices = c(0,1,7,30,90,186,365),
                                selected = 0
              )
            }
          } else {
            if(input$similarity_gap_filter %in% c(0,1,7,30,90)){
              
              updateSelectInput(session, "similarity_gap_filter",
                                label ="SIMILARITY EVENT DAYS",
                                choices = c(0,1,7,30,90),
                                selected = input$similarity_gap_filter
              )
            } else {
              updateSelectInput(session, "similarity_gap_filter",
                                label ="SIMILARITY EVENT DAYS",
                                choices = c(0,1,7,30,90),
                                selected = 0
              )
            }
          }
          
          updatingList <- NULL
          dataTotr1000 <- NULL
          all_group_events <- NULL
          all_category_events <- NULL
          best_profile_group_ordered_r1000_corrado_df <- NULL
          best_profile_category_ordered_r1000_corrado_df <- NULL
          if (input$sec_ravenpack_type == "RBDA"){
            dataTotr1000 <- bigdata_dataTotr1000
            # dataTotr2000 <- bigdata_dataTotr2000
            all_group_events <- bigdata_all_group_events
            all_category_events <- bigdata_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
          }
          
          if (input$sec_ravenpack_type == "RPNA"){
            dataTotr1000 <- rpna_dataTotr1000
            # dataTotr2000 <- rpna_dataTotr2000
            all_group_events <- rpna_all_group_events
            all_category_events <- rpna_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
          }
          
          
          if(input$sec_sort_profiles){
            if(input$sec_aggregate_criteria == "GROUP"){
              updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
            }
            if(input$sec_aggregate_criteria == "CATEGORY"){
              updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
            }
          } else {
            if(input$sec_aggregate_criteria == "GROUP"){
              updatingList <- all_group_events
            }
            if(input$sec_aggregate_criteria == "CATEGORY"){
              updatingList <- all_category_events
            }
          }
          
          updateSelectInput(session, "sec_my_event",
                            label ="EVENT",
                            choices = updatingList,
                            selected = input$sec_my_event
          )
        })  
        
        observeEvent(input$my_plot,{
          updateSelectInput(session, "sec_my_plot",
                            label ="PLOT TYPE",
                            selected = input$my_plot
          )
        })
        
        
        observeEvent(input$aggregate_criteria,{
          if(input$mynavlist != "Comparative Plots"){
            output$global_best_profiles_table <-   DT::renderDataTable(firstGlobalBestdata(),selection='single',server = FALSE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
          } 
        })
        observeEvent(input$my_event,{
          #######
          #######
          ####### Updating the sentimentn criteria
          
          if(input$mynavlist != "Comparative Plots"){
            # dataTableProxy('best_profiles_table') %>% selectRows(1)
            # dataTableProxy(firstBestdata()) %>% selectRows(1)
            output$best_profiles_table <-   DT::renderDataTable(firstBestdata(),selection='single',server = FALSE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
            # output$global_best_profiles_table <-   DT::renderDataTable(firstGlobalBestdata(),selection='single',server = TRUE,options=list(colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
            
            # dataTableProxy(output$best_profiles_table) %>% selectRows(1)
            # selectRows(dataTableProxy(output$best_profiles_table),1)
          }
          
          
          seconddata <- NULL
          
          if (input$ravenpack_type == "RBDA"){
            seconddata <- bigdata_dataTotr1000
          } else {
            seconddata <- rpna_dataTotr1000
          }
          
          seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
          seconddataf <- seconddata[seconddata$my_event == input$my_event,]
          ####################
          ####################
          #################### updating sentiment
          restricted_sentiment_universe <- unique(seconddataf$sentiment_criteria)
          toSelect <- NULL
          if(length(restricted_sentiment_universe) == 1){
            toSelect <- restricted_sentiment_universe
          }
          updateSelectInput(session, "sentiment_criteria",
                            label ="SENTIMENT",
                            choices = restricted_sentiment_universe,
                            selected = toSelect
          )
          
          ####################
          ####################
          #################### similarity gap
          restricted_similarity_universe <- unique(seconddataf$similarity_gap_filter)
          
          if(input$similarity_gap_filter %in% restricted_similarity_universe){
            updateSelectInput(session, "similarity_gap_filter",
                              label ="SIMILARITY EVENT DAYS",
                              choices = restricted_similarity_universe,
                              selected = input$similarity_gap_filter
            )
            
          } else {
            updateSelectInput(session, "similarity_gap_filter",
                              label ="SIMILARITY EVENT DAYS",
                              choices = restricted_similarity_universe,
                              selected = min(restricted_similarity_universe)
            )
          }
          
          
        })
        
        observeEvent(input$sec_my_event,{
          #######
          #######
          ####### Updating the sentimentn criteria
          seconddata <- NULL
          
          if (input$sec_ravenpack_type == "RBDA"){
            seconddata <- bigdata_dataTotr1000
          } else {
            seconddata <- rpna_dataTotr1000
          }
          
          seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$sec_event_number_event_filtering),]
          seconddataf <- seconddata[seconddata$my_event == input$sec_my_event,]
          
          restricted_sentiment_universe <- unique(seconddataf$sentiment_criteria)
          toSelect <- NULL
          if(length(restricted_sentiment_universe) == 1){
            toSelect <- restricted_sentiment_universe
          }
          updateSelectInput(session, "sec_sentiment_criteria",
                            label ="SENTIMENT",
                            choices = unique(seconddataf$sentiment_criteria),
                            selected = toSelect
          )
          
          ####################
          ####################
          #################### similarity gap
          restricted_similarity_universe <- unique(seconddataf$similarity_gap_filter)
          
          
          updateSelectInput(session, "sec_similarity_gap_filter",
                            label = "SIMILARITY EVENT DAYS",
                            choices = restricted_similarity_universe,
                            selected = input$sec_similarity_gap_filter
          )
          
        })
        
        #   observeEvent(input$sentiment_criteria, {
        #     if(input$sentiment_criteria == "BEST"){
        #       shinyjs::disable("similarity_gap_filter")
        #       shinyjs::disable("relevance")
        #       shinyjs::disable("event_relevance")
        #       shinyjs::disable("localSource")
        #     } else {
        #       shinyjs::enable("similarity_gap_filter")
        #       shinyjs::enable("relevance")
        #       shinyjs::enable("event_relevance")
        #       shinyjs::enable("localSource")
        #     }
        #     
        #   })
        #   
        
        #   observeEvent(input$sec_sentiment_criteria, {
        #     if(input$sec_sentiment_criteria == "BEST"){
        #       shinyjs::disable("sec_similarity_gap_filter")
        #       shinyjs::disable("sec_relevance")
        #       shinyjs::disable("sec_event_relevance")
        #       shinyjs::disable("sec_localSource")
        #     } else {
        #       shinyjs::enable("sec_similarity_gap_filter")
        #       shinyjs::enable("sec_relevance")
        #       shinyjs::enable("sec_event_relevance")
        #       shinyjs::enable("sec_localSource")
        #     }
        #   })
        
        observeEvent(input$aggregate_criteria,{
          updatingList <- NULL
          dataTotr1000 <- NULL
          all_group_events <- NULL
          all_category_events <- NULL
          best_profile_group_ordered_r1000_corrado_df <- NULL
          best_profile_category_ordered_r1000_corrado_df <- NULL
          if (input$ravenpack_type == "RBDA"){
            dataTotr1000 <- bigdata_dataTotr1000
            # dataTotr2000 <- bigdata_dataTotr2000
            all_group_events <- bigdata_all_group_events
            all_category_events <- bigdata_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
          }
          
          if (input$ravenpack_type == "RPNA"){
            dataTotr1000 <- rpna_dataTotr1000
            # dataTotr2000 <- rpna_dataTotr2000
            all_group_events <- rpna_all_group_events
            all_category_events <- rpna_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
          }
          
          
          if(input$sort_profiles){
            if(input$aggregate_criteria == "GROUP"){
              updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
            }
            if(input$aggregate_criteria == "CATEGORY"){
              updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
            }
          } else {
            if(input$aggregate_criteria == "GROUP"){
              updatingList <- all_group_events
            }
            if(input$aggregate_criteria == "CATEGORY"){
              updatingList <- all_category_events
            }
          }
          
          updateSelectInput(session, "my_event",
                            label ="EVENT",
                            choices = updatingList,
                            selected = input$my_event
          )
        })
        
        observeEvent(input$sort_profiles,{
          updatingList <- NULL
          dataTotr1000 <- NULL
          all_group_events <- NULL
          all_category_events <- NULL
          best_profile_group_ordered_r1000_corrado_df <- NULL
          best_profile_category_ordered_r1000_corrado_df <- NULL
          if (input$ravenpack_type == "RBDA"){
            dataTotr1000 <- bigdata_dataTotr1000
            # dataTotr2000 <- bigdata_dataTotr2000
            all_group_events <- bigdata_all_group_events
            all_category_events <- bigdata_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
          }
          
          if (input$ravenpack_type == "RPNA"){
            dataTotr1000 <- rpna_dataTotr1000
            # dataTotr2000 <- rpna_dataTotr2000
            all_group_events <- rpna_all_group_events
            all_category_events <- rpna_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
          }
          
          
          if(input$sort_profiles){
            if(input$aggregate_criteria == "GROUP"){
              updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
            }
            if(input$aggregate_criteria == "CATEGORY"){
              updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
            }
          } else {
            if(input$aggregate_criteria == "GROUP"){
              updatingList <- all_group_events
            }
            if(input$aggregate_criteria == "CATEGORY"){
              updatingList <- all_category_events
            }
          }
          
          updateSelectInput(session, "my_event",
                            label ="EVENT",
                            choices = updatingList,
                            selected = input$my_event
          )
        })
        
        observeEvent(input$sec_aggregate_criteria,{
          updatingList <- NULL
          dataTotr1000 <- NULL
          all_group_events <- NULL
          all_category_events <- NULL
          best_profile_group_ordered_r1000_corrado_df <- NULL
          best_profile_category_ordered_r1000_corrado_df <- NULL
          if (input$sec_ravenpack_type == "RBDA"){
            dataTotr1000 <- bigdata_dataTotr1000
            # dataTotr2000 <- bigdata_dataTotr2000
            all_group_events <- bigdata_all_group_events
            all_category_events <- bigdata_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
          }
          
          if (input$sec_ravenpack_type == "RPNA"){
            dataTotr1000 <- rpna_dataTotr1000
            # dataTotr2000 <- rpna_dataTotr2000
            all_group_events <- rpna_all_group_events
            all_category_events <- rpna_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
          }
          
          
          if(input$sec_sort_profiles){
            if(input$sec_aggregate_criteria == "GROUP"){
              updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
            }
            if(input$sec_aggregate_criteria == "CATEGORY"){
              updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
            }
          } else {
            if(input$sec_aggregate_criteria == "GROUP"){
              updatingList <- all_group_events
            }
            if(input$sec_aggregate_criteria == "CATEGORY"){
              updatingList <- all_category_events
            }
          }
          
          updateSelectInput(session, "sec_my_event",
                            label ="EVENT",
                            choices = updatingList,
                            selected = input$sec_my_event
          )
        })
        
        observeEvent(input$sec_sort_profiles,{
          updatingList <- NULL
          dataTotr1000 <- NULL
          all_group_events <- NULL
          all_category_events <- NULL
          best_profile_group_ordered_r1000_corrado_df <- NULL
          best_profile_category_ordered_r1000_corrado_df <- NULL
          if (input$sec_ravenpack_type == "RBDA"){
            dataTotr1000 <- bigdata_dataTotr1000
            # dataTotr2000 <- bigdata_dataTotr2000
            all_group_events <- bigdata_all_group_events
            all_category_events <- bigdata_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
          }
          
          if (input$sec_ravenpack_type == "RPNA"){
            dataTotr1000 <- rpna_dataTotr1000
            # dataTotr2000 <- rpna_dataTotr2000
            all_group_events <- rpna_all_group_events
            all_category_events <- rpna_all_category_events
            best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
            # best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
            best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
            # best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
          }
          
          
          if(input$sec_sort_profiles){
            if(input$sec_aggregate_criteria == "GROUP"){
              updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
            }
            if(input$sec_aggregate_criteria == "CATEGORY"){
              updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
            }
          } else {
            if(input$sec_aggregate_criteria == "GROUP"){
              updatingList <- all_group_events
            }
            if(input$sec_aggregate_criteria == "CATEGORY"){
              updatingList <- all_category_events
            }
          }
          
          updateSelectInput(session, "sec_my_event",
                            label ="EVENT",
                            choices = updatingList,
                            selected = input$sec_my_event
          )
        })
        
        
        
        
        
        ##################################################################################################
        ##################################################################################################
        ##################################################################################################
        ##################################################################################################
        ################################################################################################## Refreshing GUI
        output$sec_filtering_criteria <- renderText({ 
          ###################
          ###################
          ###################
          ################### Plotting together
          seconddata <- NULL
          
          if (input$sec_ravenpack_type == "RBDA"){
            seconddata <- bigdata_dataTotr1000
          } else {
            seconddata <- rpna_dataTotr1000
          }
          
          seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$sec_event_number_event_filtering),]
          seconddataf <- seconddata[seconddata$my_event == input$sec_my_event,]
          
          if (dim(seconddataf)[1]>0){
            seconddataf <- seconddataf[order(seconddataf$infinity_return,decreasing = TRUE),]
            rowProfile <- seconddataf[1,]    
            ############# Right text field    
            #       if(input$sec_sentiment_criteria == "BEST"){
            #         outputLyrics <- ""
            #         
            #         outputLyrics <- paste0("Source : ",rowProfile$localSource," ,Sector : ",rowProfile$gics_sector," ,Sentiment : ",rowProfile$sentiment_criteria," ,Similarity days : ",rowProfile$similarity_gap_filter,
            #                                " ,Relevance : ",rowProfile$relevance, " ,Event relevance : ",rowProfile$event_relevance, " ,Nb Events  : ",rowProfile$event_number_event_filtering)
            #         return(outputLyrics)
            #       } else {
            seconddataf <- seconddataf[seconddataf$localSource == input$sec_localSource,]
            seconddataf <- seconddataf[seconddataf$similarity_gap_filter == input$sec_similarity_gap_filter,]
            seconddataf <- seconddataf[seconddataf$sentiment_criteria == input$sec_sentiment_criteria,]
            seconddataf <- seconddataf[seconddataf$relevance == input$sec_relevance,]
            seconddataf <- seconddataf[seconddataf$event_relevance == input$sec_event_relevance,]
            
            return(paste0("Your selection applies ,Nb Events  : ",seconddataf$event_number_event_filtering))
            # }
          } 
          
        })
        
        
        #   observeEvent(input$best_profiles_table_rows_selected, {
        #     print("##############")
        #     print(head(input$best_profiles_table))
        #     print(input$best_profiles_table[input$best_profiles_table_rows_selected,])
        #     
        #     output$bestProfilePlot <- renderPlot({
        #       plot(1:10,1:10)
        #     })
        #       
        #     
        #   })
        
        output$globalBestProfilePlotRet <- renderPlot({
          selectedrowindex <- input$global_best_profiles_table_rows_selected
#           print("########")
#           print(selectedrowindex)
          if(!is.null(selectedrowindex)){
            currentTable <- as.data.frame(firstGlobalBestdata())
            
            rowProfile <- currentTable[selectedrowindex,]

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
#             print("##############")
#             print(dim(rowProfile))
#             print(rowProfile$EVENT)
            #       save(rowProfile,dataframe,file=paste0(outputDataPath,"bestPlotDebugBis.RData"))
            g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
            return(g)    
            
          }
        })
        
        output$bestProfilePlotRet <- renderPlot({
          selectedrowindex <- input$best_profiles_table_rows_selected
          if(!is.null(selectedrowindex)){
            currentTable <- as.data.frame(firstBestdata())
            
            rowProfile <- currentTable[selectedrowindex,]
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
            #       print("##############")
            #       print(rowProfile)
            #       save(rowProfile,dataframe,file=paste0(outputDataPath,"bestPlotDebugBis.RData"))
            g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
            return(g)    
            
          }
        })
        
        output$globalBestProfilePlotStat <- renderPlot({
          selectedrowindex <- input$global_best_profiles_table_rows_selected
          if(!is.null(selectedrowindex)){
            currentTable <- as.data.frame(firstGlobalBestdata())
            
            rowProfile <- currentTable[selectedrowindex,]
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
            g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
            return(g)    
            
          }
        })
        
        
        output$bestProfilePlotStat <- renderPlot({
          selectedrowindex <- input$best_profiles_table_rows_selected
          if(!is.null(selectedrowindex)){
            currentTable <- as.data.frame(firstBestdata())
            
            rowProfile <- currentTable[selectedrowindex,]
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
            g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
            return(g)    
            
          }
        })
        
        output$globalBestProfilePlotVola <- renderPlot({
          selectedrowindex <- input$global_best_profiles_table_rows_selected
          if(!is.null(selectedrowindex)){
            currentTable <- as.data.frame(firstGlobalBestdata())
            
            rowProfile <- currentTable[selectedrowindex,]
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
            g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
            return(g)    
            
          }
        })
        
        output$bestProfilePlotVola <- renderPlot({
          selectedrowindex <- input$best_profiles_table_rows_selected
          if(!is.null(selectedrowindex)){
            currentTable <- as.data.frame(firstBestdata())
            
            rowProfile <- currentTable[selectedrowindex,]
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
            g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
            return(g)    
            
          }
        })
        
        output$globalBestProfilePlotVolu <- renderPlot({
          selectedrowindex <- input$global_best_profiles_table_rows_selected
          if(!is.null(selectedrowindex)){
            currentTable <- as.data.frame(firstGlobalBestdata())
            
            rowProfile <- currentTable[selectedrowindex,]
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
            g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
            return(g)    
            
          }
        })
        
        output$bestProfilePlotVolu <- renderPlot({
          selectedrowindex <- input$best_profiles_table_rows_selected
          if(!is.null(selectedrowindex)){
            currentTable <- as.data.frame(firstBestdata())
            
            rowProfile <- currentTable[selectedrowindex,]
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
            g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
            return(g)    
            
          }
        })
        
        
  
        output$global_best_profiles_table <-   DT::renderDataTable(firstGlobalBestdata(),selection='single',server = FALSE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
        
        firstGlobalBestdata <- eventReactive(input$my_event, {
          firstGlobalBestdata <- NULL

          
          if(input$aggregate_criteria == "GROUP"){
            firstGlobalBestdata <- rpna_best_profile_group_ordered_r1000_corrado_df
            # print("@@@@@@@@@@@@@@@@@")
            # print(dim(firstGlobalBestdata))
          }
          
          if(input$aggregate_criteria == "CATEGORY"){
            firstGlobalBestdata <- rpna_best_profile_category_ordered_r1000_corrado_df
            # print("@@@@@@@@@@@@@@@@@")
            # print(dim(firstGlobalBestdata))
          }
          
          
#           firstGlobalBestdata  <- firstGlobalBestdata[firstGlobalBestdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
#           
#           firstGlobalBestdata <- firstGlobalBestdata[firstGlobalBestdata$aggregate_criteria == input$aggregate_criteria,]
#           
          #     
          #     suffix <- NULL
          #     if(input$fix_group){
          #       firstGlobalBestdata <- firstGlobalBestdata[firstGlobalBestdata$my_event == input$my_event,]
          #       suffix <- paste0("group",input$my_event)
          #     } else {
          #       firstGlobalBestdata <- firstGlobalBestdata[firstGlobalBestdata$localSource == input$localSource,]
          #       firstGlobalBestdata <- firstGlobalBestdata[firstGlobalBestdata$similarity_gap_filter == input$similarity_gap_filter,]
          #       firstGlobalBestdata <- firstGlobalBestdata[firstGlobalBestdata$sentiment_criteria == input$sentiment_criteria,]
          #       firstGlobalBestdata <- firstGlobalBestdata[firstGlobalBestdata$relevance == input$relevance,]
          #       firstGlobalBestdata <- firstGlobalBestdata[firstGlobalBestdata$event_relevance == input$event_relevance,]
          #       suffix <- paste0("_localSource_",input$localSource,"_gap_",input$similarity_gap_filter,"_sent_",input$sentiment_criteria,"_rel_",input$relevance,"_ev_rel_",input$event_relevance)
          #     }
          #     print("##############")
          
          ##################
          ##################
          ##################
          #     ################## plotting the best profiles
          #     all_group_events <- NULL
          #     all_category_events <- NULL
          #     if (input$ravenpack_type == "RBDA"){
          #       all_group_events <- bigdata_all_group_events
          #       all_category_events <- bigdata_all_category_events
          #     }
          #     
          #     if (input$ravenpack_type == "RPNA"){
          #       all_group_events <- rpna_all_group_events
          #       all_category_events <- rpna_all_category_events
          #     }
          #     if ((dim(firstGlobalBestdata)[1] >0)){
          #       ####
          #       firstGlobalBestdata <- firstGlobalBestdata[firstGlobalBestdata$aggregate_criteria == input$aggregate_criteria,]
          #       firstGlobalBestdata <- firstGlobalBestdata[order(firstGlobalBestdata$infinity_return, decreasing = TRUE),]
          #       #         print("###################")
          #       #         # print(colnames(firstGlobalBestdata))
          #       #         print(unique(firstGlobalBestdata$my_event))
          #       #         filename <- getPlotBestProfiles(input$aggregate_criteria,firstGlobalBestdata,all_group_events,all_category_events,suffix)
          #       return(firstGlobalBestdata)
          #     }
          #     print("##############")
          #     
          
          
          # data.frame(x=firstGlobalBestdata())
          
          # print(colnames(firstGlobalBestdata))
#           
#           print(colnames(firstGlobalBestdata))
          column_to_plot <- c("my_event",
                              "relevance",
                              "event_relevance",
                              "sentiment_criteria",
                              "similarity_gap_filter",
                              "event_number_event_filtering",
                              "localSource",
                              "infinity_return")                               
                              # "infinity_return_global") 

          
          my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
          colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="my_event")] <- "EVENT"
          colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="relevance")] <- "RELEVANCE"
          colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_relevance")] <- "EVENT_RELEVANCE"
          colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="sentiment_criteria")] <- "SENTIMENT"
          colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="similarity_gap_filter")] <- "SIMILARITY"
          colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_number_event_filtering")] <- "COUNT"
          colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="localSource")] <- "SOURCE"
          colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return")] <- "RANKING"
          # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return_global")] <- "RANKING"
          
          firstGlobalBestdata <- firstGlobalBestdata[,c(my_names,setdiff(colnames(firstGlobalBestdata),my_names))]
          # print(head(firstGlobalBestdata[,my_names],100))
          
          firstGlobalBestdata$RANKING <- as.integer(100  * (firstGlobalBestdata$RANKING - min(firstGlobalBestdata$RANKING))/(max(firstGlobalBestdata$RANKING) - min(firstGlobalBestdata$RANKING)))
          firstGlobalBestdata <- as.data.table(firstGlobalBestdata)
          firstGlobalBestdata <- firstGlobalBestdata[order(-RANKING),]
          # print(colnames(firstGlobalBestdata))
          return(firstGlobalBestdata)
        })
        
        output$best_profiles_table <-   DT::renderDataTable(firstBestdata(),selection='single',server = FALSE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
        
        firstBestdata <- eventReactive(input$my_event, {
          firstBestData <- NULL
          seconddata <- NULL
          if(input$ravenpack_type == "RPNA"){
            firstBestData <- rpna_dataTotr1000
          }
          if(input$ravenpack_type == "RBDA"){
            firstBestData <- bigdata_dataTotr1000
          }
          
          firstBestData  <- firstBestData[firstBestData$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
          #     
          #     suffix <- NULL
          #     if(input$fix_group){
          #       firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
          #       suffix <- paste0("group",input$my_event)
          #     } else {
          #       firstBestData <- firstBestData[firstBestData$localSource == input$localSource,]
          #       firstBestData <- firstBestData[firstBestData$similarity_gap_filter == input$similarity_gap_filter,]
          #       firstBestData <- firstBestData[firstBestData$sentiment_criteria == input$sentiment_criteria,]
          #       firstBestData <- firstBestData[firstBestData$relevance == input$relevance,]
          #       firstBestData <- firstBestData[firstBestData$event_relevance == input$event_relevance,]
          #       suffix <- paste0("_localSource_",input$localSource,"_gap_",input$similarity_gap_filter,"_sent_",input$sentiment_criteria,"_rel_",input$relevance,"_ev_rel_",input$event_relevance)
          #     }
          #     print("##############")
          
          ##################
          ##################
          ##################
          #     ################## plotting the best profiles
          #     all_group_events <- NULL
          #     all_category_events <- NULL
          #     if (input$ravenpack_type == "RBDA"){
          #       all_group_events <- bigdata_all_group_events
          #       all_category_events <- bigdata_all_category_events
          #     }
          #     
          #     if (input$ravenpack_type == "RPNA"){
          #       all_group_events <- rpna_all_group_events
          #       all_category_events <- rpna_all_category_events
          #     }
          #     if ((dim(firstBestData)[1] >0)){
          #       ####
          #       firstBestData <- firstBestData[firstBestData$aggregate_criteria == input$aggregate_criteria,]
          #       firstBestData <- firstBestData[order(firstBestData$infinity_return, decreasing = TRUE),]
          #       #         print("###################")
          #       #         # print(colnames(firstBestData))
          #       #         print(unique(firstBestData$my_event))
          #       #         filename <- getPlotBestProfiles(input$aggregate_criteria,firstBestData,all_group_events,all_category_events,suffix)
          #       return(firstBestData)
          #     }
          #     print("##############")
          #     
          
          
          # data.frame(x=firstBestdata())
          
          # print(colnames(firstBestData))
          column_to_plot <- c("my_event",
                              "relevance",
                              "event_relevance",
                              "sentiment_criteria",
                              "similarity_gap_filter",
                              "event_number_event_filtering",
                              "localSource",
                              "infinity_return") 
          firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
          
          my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
          colnames(firstBestData)[which(colnames(firstBestData)=="my_event")] <- "EVENT"
          colnames(firstBestData)[which(colnames(firstBestData)=="relevance")] <- "RELEVANCE"
          colnames(firstBestData)[which(colnames(firstBestData)=="event_relevance")] <- "EVENT_RELEVANCE"
          colnames(firstBestData)[which(colnames(firstBestData)=="sentiment_criteria")] <- "SENTIMENT"
          colnames(firstBestData)[which(colnames(firstBestData)=="similarity_gap_filter")] <- "SIMILARITY"
          colnames(firstBestData)[which(colnames(firstBestData)=="event_number_event_filtering")] <- "COUNT"
          colnames(firstBestData)[which(colnames(firstBestData)=="localSource")] <- "SOURCE"
          colnames(firstBestData)[which(colnames(firstBestData)=="infinity_return")] <- "RANKING"
          
          firstBestData <- firstBestData[,c(my_names,setdiff(colnames(firstBestData),my_names))]
          # print(head(firstBestData[,my_names],100))
          
          firstBestData$RANKING <- as.integer(100  * (firstBestData$RANKING - min(firstBestData$RANKING))/(max(firstBestData$RANKING) - min(firstBestData$RANKING)))
          firstBestData <- as.data.table(firstBestData)
          firstBestData <- firstBestData[order(-RANKING),]
          # print(colnames(firstBestData))
          return(firstBestData)
        })
        
        #   firstBestdata <- eventReactive(input$plot_best_profiles, {
        #     if(input$plot_best_profiles){
        #       firstBestData <- NULL
        #       seconddata <- NULL
        #       if(input$ravenpack_type == "RPNA"){
        #         firstBestData <- rpna_dataTotr1000
        #       }
        #       if(input$ravenpack_type == "RBDA"){
        #         firstBestData <- bigdata_dataTotr1000
        #       }
        #       
        #       firstBestData  <- firstBestData[firstBestData$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
        #       
        #       suffix <- NULL
        #       if(input$fix_group){
        #         firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
        #         suffix <- paste0("group",input$my_event)
        #       } else {
        #         firstBestData <- firstBestData[firstBestData$localSource == input$localSource,]
        #         firstBestData <- firstBestData[firstBestData$similarity_gap_filter == input$similarity_gap_filter,]
        #         firstBestData <- firstBestData[firstBestData$sentiment_criteria == input$sentiment_criteria,]
        #         firstBestData <- firstBestData[firstBestData$relevance == input$relevance,]
        #         firstBestData <- firstBestData[firstBestData$event_relevance == input$event_relevance,]
        #         suffix <- paste0("_localSource_",input$localSource,"_gap_",input$similarity_gap_filter,"_sent_",input$sentiment_criteria,"_rel_",input$relevance,"_ev_rel_",input$event_relevance)
        #       }
        #       
        #       ##################
        #       ##################
        #       ##################
        #       ################## plotting the best profiles
        #       all_group_events <- NULL
        #       all_category_events <- NULL
        #       if (input$ravenpack_type == "RBDA"){
        #         all_group_events <- bigdata_all_group_events
        #         all_category_events <- bigdata_all_category_events
        #       }
        #       
        #       if (input$ravenpack_type == "RPNA"){
        #         all_group_events <- rpna_all_group_events
        #         all_category_events <- rpna_all_category_events
        #       }
        #       if ((dim(firstBestData)[1] >0)){
        #         ####
        #         firstBestData <- firstBestData[firstBestData$aggregate_criteria == input$aggregate_criteria,]
        #         firstBestData <- firstBestData[order(firstBestData$infinity_return, decreasing = TRUE),]
        # #         print("###################")
        # #         # print(colnames(firstBestData))
        # #         print(unique(firstBestData$my_event))
        # #         filename <- getPlotBestProfiles(input$aggregate_criteria,firstBestData,all_group_events,all_category_events,suffix)
        #         return(firstBestData)
        #       }
        #       
        #     }
        #     
        #   })
        
        
        #   output$best_plot_path <- renderText({ 
        #     bestefilename()
        #   })
        
        output$filtering_criteria <- renderText({ 
          
          ###################
          ###################
          ###################
          ################### Plotting together
          
          firstdata <- NULL
          
          if (input$ravenpack_type == "RBDA"){
            firstdata <- bigdata_dataTotr1000
          } else {
            firstdata <- rpna_dataTotr1000
          }
          
          
          
          firstdata  <- firstdata[firstdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
          
          
          ############# Left text field
          firstdataf <- firstdata[firstdata$my_event == input$my_event,]
          outputLyrics <- ""
          if (dim(firstdataf)[1]>0){
            firstdataf <- firstdataf[order(firstdataf$infinity_return,decreasing = TRUE),]
            rowProfile <- firstdataf[1,]
            
            #       if(input$sentiment_criteria == "BEST"){
            #         
            #         
            #         
            #         outputLyrics <- paste0("Source : ",rowProfile$localSource," ,Sector : ",rowProfile$gics_sector," ,Sentiment : ",rowProfile$sentiment_criteria," ,Similarity days : ",rowProfile$similarity_gap_filter,
            #                                " ,Relevance : ",rowProfile$relevance, " ,Event relevance : ",rowProfile$event_relevance, " ,Nb Events  : ",rowProfile$event_number_event_filtering)
            #         return(outputLyrics)
            #       } else {
            firstdataf <- firstdataf[firstdataf$localSource == input$localSource,]
            firstdataf <- firstdataf[firstdataf$similarity_gap_filter == input$similarity_gap_filter,]
            firstdataf <- firstdataf[firstdataf$sentiment_criteria == input$sentiment_criteria,]
            firstdataf <- firstdataf[firstdataf$relevance == input$relevance,]
            firstdataf <- firstdataf[firstdataf$event_relevance == input$event_relevance,]
            
            return(paste0("Your selection applies ,Nb Events  : ",firstdataf$event_number_event_filtering))
            
            # }
          } 
          
        })
        ###################
        ###################
        ###################
        ################### First plot
        
        
        
        
        
        output$eventMinutesPlotHigh <- renderPlot({
          firstdata <- NULL
          seconddata <- NULL
          if(input$ravenpack_type == "RPNA"){
            firstdata <- rpna_dataTotr1000
          }
          if(input$ravenpack_type == "RBDA"){
            firstdata <- bigdata_dataTotr1000
          }
          if(input$sec_ravenpack_type == "RPNA"){
            seconddata <- rpna_dataTotr1000
          }
          if(input$sec_ravenpack_type == "RBDA"){
            seconddata <- bigdata_dataTotr1000
          }
          
          
          
          firstdata  <- firstdata[firstdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
          seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$sec_event_number_event_filtering),]
          
          
          
          # if(input$sentiment_criteria != "BEST"){
          firstdata <- firstdata[firstdata$localSource == input$localSource,]
          firstdata <- firstdata[firstdata$similarity_gap_filter == input$similarity_gap_filter,]
          firstdata <- firstdata[firstdata$sentiment_criteria == input$sentiment_criteria,]
          firstdata <- firstdata[firstdata$relevance == input$relevance,]
          firstdata <- firstdata[firstdata$event_relevance == input$event_relevance,]
          # }
          
          # if(input$sec_sentiment_criteria != "BEST"){
          seconddata <- seconddata[seconddata$localSource == input$sec_localSource,]
          seconddata <- seconddata[seconddata$similarity_gap_filter == input$sec_similarity_gap_filter,]
          seconddata <- seconddata[seconddata$sentiment_criteria == input$sec_sentiment_criteria,]
          seconddata <- seconddata[seconddata$relevance == input$sec_relevance,]
          seconddata <- seconddata[seconddata$event_relevance == input$sec_event_relevance,]
          
          # }
          
          
          
          
          
          firstdataf <- firstdata[firstdata$my_event == input$my_event,]
          seconddataf <- seconddata[seconddata$my_event == input$sec_my_event,]
          
          
          if ((dim(firstdataf)[1] == 0) & (dim(seconddataf)[1] == 0 ) ){
            pp <- readPNG("SORRY.PNG")
            rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
            return
          }
          
          
          if ((dim(firstdataf)[1] >0) & (dim(seconddataf)[1] >0 ) ){
            print("plotting them together")
            ####### first big data 
            
            firstdataf <- firstdataf[order(firstdataf$infinity_return,decreasing = TRUE),]
            
            rowProfileOne <- firstdataf[1,]
            
            stats_sign <- colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))]
            rets <- paste0("RET",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            ord_stats_sign <- paste0("ORD",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            vol_stats_sign <- paste0("VOLU",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            vola_stats_sign <- paste0("VOLA",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            
            stats_sign <- rowProfileOne[,stats_sign]
            rets <- rowProfileOne[,rets]
            colnames(rets) <- colnames(stats_sign)
            ord_stats_sign <- rowProfileOne[,ord_stats_sign]
            colnames(ord_stats_sign) <- colnames(stats_sign)
            vol_stats_sign <- rowProfileOne[,vol_stats_sign]
            colnames(vol_stats_sign) <- colnames(stats_sign)
            vola_stats_sign <- rowProfileOne[,vola_stats_sign]
            colnames(vola_stats_sign) <- colnames(stats_sign)
            
            firstdataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
            firstProperNames <- paste0(rowProfileOne$my_event,c("COR_STATS_SIGN_1","ORD_STATS_SIGN_1","VOLUME_1","VOLATILITY_1","RETS_1"))
            colnames(firstdataframe) <- firstProperNames
            firstdataframe$MINUTES <- as.numeric(colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            firstdataframe <- firstdataframe[,c("MINUTES",firstProperNames)]
            
            #### second rpna
            seconddataf <- seconddataf[order(seconddataf$infinity_return,decreasing = TRUE),]
            
            rowProfileTwo <- seconddataf[1,]
            
            stats_sign <- colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))]
            rets <- paste0("RET",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            ord_stats_sign <- paste0("ORD",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            vol_stats_sign <- paste0("VOLU",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            vola_stats_sign <- paste0("VOLA",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            
            stats_sign <- rowProfileTwo[,stats_sign]
            rets <- rowProfileTwo[,rets]
            colnames(rets) <- colnames(stats_sign)
            ord_stats_sign <- rowProfileTwo[,ord_stats_sign]
            colnames(ord_stats_sign) <- colnames(stats_sign)
            vol_stats_sign <- rowProfileTwo[,vol_stats_sign]
            colnames(vol_stats_sign) <- colnames(stats_sign)
            vola_stats_sign <- rowProfileTwo[,vola_stats_sign]
            colnames(vola_stats_sign) <- colnames(stats_sign)
            
            seconddataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
            secondProperNames <- paste0(rowProfileTwo$my_event,c("COR_STATS_SIGN_2","ORD_STATS_SIGN_2","VOLUME_2","VOLATILITY_2","RETS_2"))
            colnames(seconddataframe) <- secondProperNames
            seconddataframe$MINUTES <- as.numeric(colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            seconddataframe <- seconddataframe[,c("MINUTES",secondProperNames)]
            ### merging both together
            dataframe <- merge(seconddataframe,firstdataframe,by="MINUTES")
            #       
            #       print("saving for debug@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
            #       save(rowProfileOne, rowProfileTwo, dataframe,  file=paste0(outputDataPath,"entryTwo.RData"))
            #       print("saving for debug@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
            #### plotting them together
            if (input$my_plot == "RETURNS_SIGNIFICANCE"){
              g <- outputTogetherGraphicsBestProfileRets(rowProfileOne$my_event,rowProfileTwo$my_event,dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
              
              # Render your graph
              print(g)    
              
            } else {
              g <- outputTogetherGraphicsBestProfileVola(rowProfileOne$my_event,rowProfileTwo$my_event, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
              
              # Render your graph
              print(g)  
            }
          } else { 
            print("Not enough data for one")
            
            if ((dim(seconddataf)[1] >0 )){
              print("plotting second only")
              dataf <- seconddataf
              dataf <- dataf[order(dataf$infinity_return,decreasing = TRUE),]
              rowProfile <- dataf[1,]
              
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
              
              if (input$my_plot == "RETURNS_SIGNIFICANCE"){
                g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
                
                # Render your graph
                print(g)    
                
              } else {
                g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
                
                # Render your graph
                print(g)  
              }
            }
            
            
            
            if ((dim(firstdataf)[1] >0 )){
              print("plotting first only")
              dataf <- firstdataf
              dataf <- dataf[order(dataf$infinity_return,decreasing = TRUE),]
              
              rowProfile <- dataf[1,]
              
              
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
              
              if (input$my_plot == "RETURNS_SIGNIFICANCE"){
                g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
                # Render your graph
                print(g)    
                
              } else {
                g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
                # Render your graph
                print(g)  
              }
              
            } 
          }
          
        })
        
        
        output$eventMinutesPlotLow <- renderPlot({
          
          ###################
          ###################
          ###################
          ################### Plotting together
          
          
          
          firstdata <- NULL
          seconddata <- NULL
          if(input$ravenpack_type == "RPNA"){
            firstdata <- rpna_dataTotr1000
          }
          if(input$ravenpack_type == "RBDA"){
            firstdata <- bigdata_dataTotr1000
          }
          if(input$sec_ravenpack_type == "RPNA"){
            seconddata <- rpna_dataTotr1000
          }
          if(input$sec_ravenpack_type == "RBDA"){
            seconddata <- bigdata_dataTotr1000
          }
          
          
          
          firstdata  <- firstdata[firstdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
          seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$sec_event_number_event_filtering),]
          
          # if(input$sentiment_criteria != "BEST"){
          firstdata <- firstdata[firstdata$localSource == input$localSource,]
          firstdata <- firstdata[firstdata$similarity_gap_filter == input$similarity_gap_filter,]
          firstdata <- firstdata[firstdata$sentiment_criteria == input$sentiment_criteria,]
          firstdata <- firstdata[firstdata$relevance == input$relevance,]
          firstdata <- firstdata[firstdata$event_relevance == input$event_relevance,]
          # }
          
          # if(input$sec_sentiment_criteria != "BEST"){
          seconddata <- seconddata[seconddata$localSource == input$sec_localSource,]
          seconddata <- seconddata[seconddata$similarity_gap_filter == input$sec_similarity_gap_filter,]
          seconddata <- seconddata[seconddata$sentiment_criteria == input$sec_sentiment_criteria,]
          seconddata <- seconddata[seconddata$relevance == input$sec_relevance,]
          seconddata <- seconddata[seconddata$event_relevance == input$sec_event_relevance,]
          
          # }
          
          firstdataf <- firstdata[firstdata$my_event == input$my_event,]
          seconddataf <- seconddata[seconddata$my_event == input$sec_my_event,]
          
          if ((dim(firstdataf)[1] == 0) & (dim(seconddataf)[1] == 0 ) ){
            pp <- readPNG("SORRY.PNG")
            rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
            return
          }
          
          if ((dim(firstdataf)[1] >0) & (dim(seconddataf)[1] >0 ) ){
            print("plotting them together")
            ####### first big data 
            
            firstdataf <- firstdataf[order(firstdataf$infinity_return,decreasing = TRUE),]
            
            rowProfileOne <- firstdataf[1,]
            
            stats_sign <- colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))]
            rets <- paste0("RET",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            ord_stats_sign <- paste0("ORD",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            vol_stats_sign <- paste0("VOLU",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            vola_stats_sign <- paste0("VOLA",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            
            stats_sign <- rowProfileOne[,stats_sign]
            rets <- rowProfileOne[,rets]
            colnames(rets) <- colnames(stats_sign)
            ord_stats_sign <- rowProfileOne[,ord_stats_sign]
            colnames(ord_stats_sign) <- colnames(stats_sign)
            vol_stats_sign <- rowProfileOne[,vol_stats_sign]
            colnames(vol_stats_sign) <- colnames(stats_sign)
            vola_stats_sign <- rowProfileOne[,vola_stats_sign]
            colnames(vola_stats_sign) <- colnames(stats_sign)
            
            firstdataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
            firstProperNames <- paste0(rowProfileOne$my_event,c("COR_STATS_SIGN_1","ORD_STATS_SIGN_1","VOLUME_1","VOLATILITY_1","RETS_1"))
            colnames(firstdataframe) <- firstProperNames
            firstdataframe$MINUTES <- as.numeric(colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
            firstdataframe <- firstdataframe[,c("MINUTES",firstProperNames)]
            
            #### second rpna
            seconddataf <- seconddataf[order(seconddataf$infinity_return,decreasing = TRUE),]
            
            rowProfileTwo <- seconddataf[1,]
            
            stats_sign <- colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))]
            rets <- paste0("RET",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            ord_stats_sign <- paste0("ORD",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            vol_stats_sign <- paste0("VOLU",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            vola_stats_sign <- paste0("VOLA",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            
            stats_sign <- rowProfileTwo[,stats_sign]
            rets <- rowProfileTwo[,rets]
            colnames(rets) <- colnames(stats_sign)
            ord_stats_sign <- rowProfileTwo[,ord_stats_sign]
            colnames(ord_stats_sign) <- colnames(stats_sign)
            vol_stats_sign <- rowProfileTwo[,vol_stats_sign]
            colnames(vol_stats_sign) <- colnames(stats_sign)
            vola_stats_sign <- rowProfileTwo[,vola_stats_sign]
            colnames(vola_stats_sign) <- colnames(stats_sign)
            
            seconddataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
            secondProperNames <- paste0(rowProfileTwo$my_event,c("COR_STATS_SIGN_2","ORD_STATS_SIGN_2","VOLUME_2","VOLATILITY_2","RETS_2"))
            colnames(seconddataframe) <- secondProperNames
            seconddataframe$MINUTES <- as.numeric(colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
            seconddataframe <- seconddataframe[,c("MINUTES",secondProperNames)]
            
            ### merging both together
            dataframe <- merge(seconddataframe,firstdataframe,by="MINUTES")
            
            if (input$my_plot == "RETURNS_SIGNIFICANCE"){
              g <- outputTogetherGraphicsBestProfileStats(rowProfileOne$my_event,rowProfileTwo$my_event,dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
              # Render your graph
              print(g)    
            } else {
              g <- outputTogetherGraphicsBestProfileVol(rowProfileOne$my_event,rowProfileTwo$my_event,dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
              # Render your graph
              print(g)  
            }
          } else { 
            print("not enough data for one")
            
            if ((dim(seconddataf)[1] >0 )){
              print("plotting second only")
              dataf <- seconddataf
              dataf <- dataf[order(dataf$infinity_return,decreasing = TRUE),]
              
              rowProfile <- dataf[1,]
              
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
              
              if (input$my_plot == "RETURNS_SIGNIFICANCE"){
                g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
                # Render your graph
                print(g)    
              } else {
                g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
                # Render your graph
                print(g)  
              }
            }
            
            if ((dim(firstdataf)[1] >0 )){
              print("plotting first only")
              dataf <- firstdataf
              dataf <- dataf[order(dataf$infinity_return,decreasing = TRUE),]
              
              rowProfile <- dataf[1,]
              
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
              
              
              if (input$my_plot == "RETURNS_SIGNIFICANCE"){
                g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
                # Render your graph
                print(g)    
              } else {
                g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
                # Render your graph
                print(g)  
              }
            }
            
            
          }

          
        })
        
        
        ##################################################################################################
        ##################################################################################################
        ##################################################################################################
        ##################################################################################################
        ################################################################################################## Refreshing GUI
        
        
}
