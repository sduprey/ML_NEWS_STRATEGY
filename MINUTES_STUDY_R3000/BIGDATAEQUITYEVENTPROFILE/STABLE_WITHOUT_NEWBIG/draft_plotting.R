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

################################
################################
################################
################################
################################
################################
################################ End of Plotting functions


# load(file=paste0(outputDataPath,"entryOne.RData"))
# print(rowProfileOne)
# print(rowProfileTwo)
# 
# g <- outputGraphicsBestProfileStats(rowProfileOne$my_event, rowProfileTwo$my_event, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
# # Render your graph
# print(g)    
# 
# g <- outputGraphicsBestProfileVol(rowProfileOne$my_event, rowProfileTwo$my_event, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
# # rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
# # Render your graph
# print(g)  


load(file=paste0(outputDataPath,"entryTwo"))

g <- outputGraphicsBestProfileRets(rowProfileOne$my_event, rowProfileTwo$my_event, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)

# Render your graph
print(g)    


g <- outputGraphicsBestProfileVola(rowProfileOne$my_event, rowProfileTwo$my_event, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)

print(g)  


print(dim(bigdataf))
print(dim(rpnadataf))

####### first big data 

bigdataf <- bigdataf[order(bigdataf$infinity_return,decreasing = TRUE),]

rowProfile <- bigdataf[1,]

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

bigdatadataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
colnames(bigdatadataframe) <- c("RDBA_COR_STATS_SIGN","RDBA_ORD_STATS_SIGN","RDBA_VOLUME","RDBA_VOLATILITY","RDBA_RETS")
bigdatadataframe$MINUTES <- as.numeric(colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
bigdatadataframe <- bigdatadataframe[,c("MINUTES","RDBA_COR_STATS_SIGN","RDBA_ORD_STATS_SIGN","RDBA_VOLUME","RDBA_VOLATILITY","RDBA_RETS")]






#### second rpna
rpnadataf <- rpnadataf[order(rpnadataf$infinity_return,decreasing = TRUE),]

rowProfile <- rpnadataf[1,]

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

rpnadataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
colnames(rpnadataframe) <- c("RPNA_COR_STATS_SIGN","RPNA_ORD_STATS_SIGN","RPNA_VOLUME","RPNA_VOLATILITY","RPNA_RETS")
rpnadataframe$MINUTES <- as.numeric(colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
rpnadataframe <- rpnadataframe[,c("MINUTES","RPNA_COR_STATS_SIGN","RPNA_ORD_STATS_SIGN","RPNA_VOLUME","RPNA_VOLATILITY","RPNA_RETS")]

### merging both together
dataframe <- merge(rpnadataframe,bigdatadataframe,by="MINUTES")


g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
# Render your graph
print(g)    

g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
# Render your graph
print(g)  

# g <- outputGraphicsTogetherBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
# # Render your graph
# print(g)    
# 
# g <- outputGraphicsTogetherBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
# # Render your graph
# print(g)  
