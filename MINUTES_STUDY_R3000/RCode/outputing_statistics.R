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

options(stringAsFactors = FALSE)

##################
##################
##################
################## Big data file loading
rpna_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"nnew_spr_r1000_rpna_abvol_abvol_corrado_df.rds"))

############### Filtering to avoid  overfitting
############################## ############### ############### 
############################## ############### ############### 
############################## ############### ############### 
############################## ############### ############### 

rpna_dataTotr1000 <- rpna_dataTotr1000[rpna_dataTotr1000$event_number_event_filtering >= 50,]
rpna_dataTotr1000 <- rpna_dataTotr1000[rpna_dataTotr1000$localSource == "DJPR",]
rpna_dataTotr1000 <- rpna_dataTotr1000[rpna_dataTotr1000$similarity_gap_filter == 0,]
rpna_dataTotr1000 <- rpna_dataTotr1000[rpna_dataTotr1000$event_relevance == "HIGH",]
rpna_dataTotr1000 <- rpna_dataTotr1000[rpna_dataTotr1000$relevance == "HIGH",]


############### End of Filtering to avoid  overfitting
############################## ############### ############### 
############################## ############### ############### 
############################## ############### ############### 
############################## ############### ############### 

resultsDF <- rpna_dataTotr1000[,c("my_event","relevance","event_relevance","aggregate_criteria","sentiment_criteria","similarity_gap_filter","event_number_event_filtering")]

rpna_dataTotr1000$correcting_factor <- 2*(rpna_dataTotr1000$sentiment_criteria == "POSITIVE"  | rpna_dataTotr1000$sentiment_criteria == "SPREAD" )-1
rpna_dataTotr1000$correcting_factor[rpna_dataTotr1000$sentiment_criteria == "ALL"] <- 0
### we keep the 5 minutes of the event as the jump makes a great profile

#####################
#####################
##################### post event metrics
stats_post_sign <- colnames(rpna_dataTotr1000)[which(as.numeric(colnames(rpna_dataTotr1000)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)
vola_post <- paste0("VOLA",stats_post_sign)
volu_post <- paste0("VOLU",stats_post_sign)
stats_ord_post <- paste0("ORD",stats_post_sign)


resultsDF$rank_weighted_avg_post_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_post]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_post_sign],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
resultsDF$stat_weighted_avg_post_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_post]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_ord_post],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
resultsDF$avg_post_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_post]*rpna_dataTotr1000$correcting_factor,na.rm = TRUE))/dim(rpna_dataTotr1000)[2]

rpna_dataTotr1000$rank_weighted_avg_post_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_post]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_post_sign],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
rpna_dataTotr1000$stat_weighted_avg_post_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_post]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_ord_post],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
rpna_dataTotr1000$avg_post_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_post]*rpna_dataTotr1000$correcting_factor,na.rm = TRUE))/dim(rpna_dataTotr1000)[2]



resultsDF$rank_weighted_avg_post_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_post]*rpna_dataTotr1000[,stats_post_sign],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
resultsDF$stat_weighted_avg_post_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_post]*rpna_dataTotr1000[,stats_ord_post],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
resultsDF$avg_post_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_post],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]

rpna_dataTotr1000$rank_weighted_avg_post_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_post]*rpna_dataTotr1000[,stats_post_sign],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
rpna_dataTotr1000$stat_weighted_avg_post_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_post]*rpna_dataTotr1000[,stats_ord_post],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
rpna_dataTotr1000$avg_post_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_post],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]

#####################
#####################
##################### pre event metrics

stats_pre_sign <- colnames(rpna_dataTotr1000)[which(as.numeric(colnames(rpna_dataTotr1000)) <0)]
rets_pre <- paste0("RET",stats_pre_sign)
vola_pre <- paste0("VOLA",stats_pre_sign)
volu_pre <- paste0("VOLU",stats_pre_sign)
stats_ord_pre <- paste0("ORD",stats_pre_sign)


resultsDF$rank_weighted_avg_pre_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_pre]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_pre_sign],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
resultsDF$stat_weighted_avg_pre_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_pre]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_ord_pre],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
resultsDF$avg_pre_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_pre]*rpna_dataTotr1000$correcting_factor,na.rm = TRUE))/dim(rpna_dataTotr1000)[2]

rpna_dataTotr1000$rank_weighted_avg_pre_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_pre]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_pre_sign],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
rpna_dataTotr1000$stat_weighted_avg_pre_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_pre]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_ord_pre],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
rpna_dataTotr1000$avg_pre_event_ab_return <- (rowSums(rpna_dataTotr1000[,rets_pre]*rpna_dataTotr1000$correcting_factor,na.rm = TRUE))/dim(rpna_dataTotr1000)[2]



resultsDF$rank_weighted_avg_pre_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_pre]*rpna_dataTotr1000[,stats_pre_sign],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
resultsDF$stat_weighted_avg_pre_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_pre]*rpna_dataTotr1000[,stats_ord_pre],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
resultsDF$avg_pre_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_pre],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]

rpna_dataTotr1000$rank_weighted_avg_pre_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_pre]*rpna_dataTotr1000[,stats_pre_sign],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
rpna_dataTotr1000$stat_weighted_avg_pre_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_pre]*rpna_dataTotr1000[,stats_ord_pre],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]
rpna_dataTotr1000$avg_pre_event_ab_volatility <- (rowSums(rpna_dataTotr1000[,vola_pre],na.rm = TRUE))/dim(rpna_dataTotr1000)[2]


#####################
#####################
##################### pre post event metrics




# resultsDF$rank_weighted_avg_prepost_event_ab_return <- resultsDF$rank_weighted_avg_pre_event_ab_return + resultsDF$rank_weighted_avg_post_event_ab_return
# resultsDF$stat_weighted_avg_prepost_event_ab_return <-  resultsDF$stat_weighted_avg_pre_event_ab_return +  resultsDF$stat_weighted_avg_post_event_ab_return
# resultsDF$avg_prepost_event_ab_return               <-  resultsDF$avg_pre_event_ab_return + resultsDF$avg_post_event_ab_return 
# 
# 
# 
# rpna_dataTotr1000$rank_weighted_avg_prepost_event_ab_return <- rpna_dataTotr1000$rank_weighted_avg_pre_event_ab_return + rpna_dataTotr1000$rank_weighted_avg_post_event_ab_return
# rpna_dataTotr1000$stat_weighted_avg_prepost_event_ab_return <- rpna_dataTotr1000$stat_weighted_avg_pre_event_ab_return + rpna_dataTotr1000$stat_weighted_avg_post_event_ab_return 
# rpna_dataTotr1000$avg_prepost_event_ab_return               <- rpna_dataTotr1000$avg_pre_event_ab_return + rpna_dataTotr1000$avg_post_event_ab_return
# 
# 
# resultsDF$rank_weighted_avg_prepost_event_ab_volatility <- resultsDF$rank_weighted_avg_pre_event_ab_volatility  + resultsDF$rank_weighted_avg_post_event_ab_volatility 
# resultsDF$stat_weighted_avg_prepost_event_ab_volatility <- resultsDF$stat_weighted_avg_pre_event_ab_volatility + resultsDF$stat_weighted_avg_post_event_ab_volatility
# resultsDF$avg_prepost_event_ab_volatility               <- resultsDF$avg_pre_event_ab_volatility  + resultsDF$avg_post_event_ab_volatility 
# 
# 
# 
# rpna_dataTotr1000$rank_weighted_avg_prepost_event_ab_volatility <- rpna_dataTotr1000$rank_weighted_avg_pre_event_ab_volatility + rpna_dataTotr1000$rank_weighted_avg_post_event_ab_volatility
# rpna_dataTotr1000$stat_weighted_avg_prepost_event_ab_volatility <- rpna_dataTotr1000$stat_weighted_avg_pre_event_ab_volatility + rpna_dataTotr1000$stat_weighted_avg_post_event_ab_volatility
# rpna_dataTotr1000$avg_prepost_event_ab_volatility               <- rpna_dataTotr1000$avg_pre_event_ab_volatility + rpna_dataTotr1000$avg_post_event_ab_volatility


resultsDF$volatility_correction <- resultsDF$avg_post_event_ab_volatility/resultsDF$avg_pre_event_ab_volatility
rpna_dataTotr1000$volatility_correction <- rpna_dataTotr1000$avg_post_event_ab_volatility/rpna_dataTotr1000$avg_pre_event_ab_volatility

# resultsDF$return_correction     <- abs(resultsDF$avg_post_event_ab_return)/abs(resultsDF$avg_pre_event_ab_return)
resultsDF$return_correction      <- 1/sd(resultsDF$avg_pre_event_ab_return)
# resultsDF$stat_return_correction     <- abs(resultsDF$stat_weighted_avg_post_event_ab_return)/abs(resultsDF$stat_weighted_avg_pre_event_ab_return)
resultsDF$stat_return_correction      <- 1/sd(resultsDF$stat_weighted_avg_pre_event_ab_return)
# resultsDF$rank_return_correction     <- abs(resultsDF$rank_weighted_avg_post_event_ab_return)/abs(resultsDF$rank_weighted_avg_pre_event_ab_return)
resultsDF$rank_return_correction      <- 1/sd(resultsDF$rank_weighted_avg_pre_event_ab_return)

# resultsDF$rank_weighted_avg_prepost_event_ab_return <-  resultsDF$volatility_correction * resultsDF$return_correction * resultsDF$rank_weighted_avg_post_event_ab_return
# resultsDF$stat_weighted_avg_prepost_event_ab_return <-    resultsDF$volatility_correction * resultsDF$return_correction * resultsDF$stat_weighted_avg_post_event_ab_return
# resultsDF$avg_prepost_event_ab_return         <-  resultsDF$volatility_correction * resultsDF$return_correction * resultsDF$avg_post_event_ab_return 
resultsDF$rank_weighted_avg_prepost_event_ab_return <-  resultsDF$volatility_correction * resultsDF$rank_return_correction 
resultsDF$stat_weighted_avg_prepost_event_ab_return <-    resultsDF$volatility_correction * resultsDF$stat_return_correction 
resultsDF$avg_prepost_event_ab_return         <-  resultsDF$volatility_correction * resultsDF$return_correction 

# rpna_dataTotr1000$return_correction     <- abs(rpna_dataTotr1000$avg_post_event_ab_return)/abs(rpna_dataTotr1000$avg_pre_event_ab_return)
rpna_dataTotr1000$return_correction      <- 1/sd(rpna_dataTotr1000$avg_pre_event_ab_return)
# rpna_dataTotr1000$stat_return_correction     <- abs(rpna_dataTotr1000$stat_weighted_avg_post_event_ab_return)/abs(rpna_dataTotr1000$stat_weighted_avg_pre_event_ab_return)
rpna_dataTotr1000$stat_return_correction      <- 1/sd(rpna_dataTotr1000$stat_weighted_avg_pre_event_ab_return)
# rpna_dataTotr1000$rank_return_correction     <- abs(rpna_dataTotr1000$rank_weighted_avg_post_event_ab_return)/abs(rpna_dataTotr1000$rank_weighted_avg_pre_event_ab_return)
rpna_dataTotr1000$rank_return_correction      <- 1/sd(rpna_dataTotr1000$rank_weighted_avg_pre_event_ab_return)

# rpna_dataTotr1000$rank_weighted_avg_prepost_event_ab_return <-  rpna_dataTotr1000$volatility_correction * rpna_dataTotr1000$return_correction * rpna_dataTotr1000$rank_weighted_avg_post_event_ab_return
# rpna_dataTotr1000$stat_weighted_avg_prepost_event_ab_return <-    rpna_dataTotr1000$volatility_correction * rpna_dataTotr1000$return_correction * rpna_dataTotr1000$stat_weighted_avg_post_event_ab_return
# rpna_dataTotr1000$avg_prepost_event_ab_return <-  rpna_dataTotr1000$volatility_correction * rpna_dataTotr1000$return_correction * rpna_dataTotr1000$avg_post_event_ab_return 
rpna_dataTotr1000$rank_weighted_avg_prepost_event_ab_return <-  rpna_dataTotr1000$volatility_correction * rpna_dataTotr1000$rank_return_correction 
rpna_dataTotr1000$stat_weighted_avg_prepost_event_ab_return <-    rpna_dataTotr1000$volatility_correction * rpna_dataTotr1000$stat_return_correction 
rpna_dataTotr1000$avg_prepost_event_ab_return         <-  rpna_dataTotr1000$volatility_correction * rpna_dataTotr1000$return_correction 


print("All metrics computed")

################################
################################
################################
################################
################################
################################
################################ Plotting functions


PlotDataFrameStats <- function (DataFrame, XLab = "", YLab = "", Title = "", AxisIncluded = FALSE, 
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

  g <- ggplot(DataFrame)
  
  g <- g + geom_line(aes(x = DataFrame$MINUTES, y = DataFrame$`SIGNIFICANCE(RANK)`),colour = 'red') 
  g <- g + geom_line(aes(x = DataFrame$MINUTES, y = DataFrame$`SIGNIFICANCE`),colour = 'blue') 
  g <- g + geom_line(aes(x = DataFrame$MINUTES, y = DataFrame$`ABNORMAL_THRESHOLD`),colour = 'black', size = 1.5,linetype="dashed")
  

  g <- g + xlab(XLab) + ylab(YLab) + ggtitle(Title) + theme(title = element_text(size = 16, 
                                                                                 face = "bold")) + theme(axis.text.x = element_text(size = 14)) + 
    theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
          legend.text = element_text(size = 16)) + theme(legend.position = "bottom", 
                                                         legend.title = element_blank())+theme(axis.text=element_text(size=14),
                                                                                               axis.title=element_text(size=16,face="bold"))

  if (percent){
    g <- g +   scale_y_continuous(labels = percent_format(),limits = c(-0, 1)) 
  }

  if (!is.null(FullExportingPath)) 
    RP_ExportPlot(g, FullExportingPath, "")
  return(g)
}

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
  
#   if ("ABNORMAL_THRESHOLD" %in% colnames(DataFrame)){
#     g <- g + geom_line(aes(ABNORMAL_THRESHOLD),linetype="dotted") 
#   }
  
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
  
  g1 <- PlotDataFrameStats(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",percent= TRUE, Title = paste0(my_event," statistical significance"), FullExportingPath = NULL)
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


getPlotBestProfiles <- function(aggregate_criteria, data, all_group_events, all_category_events){
  plotLimit <- 6
  if (aggregate_criteria == "GROUP"){
    plotLimit <- 6
    all_events <- all_group_events
  } else {
    plotLimit <- 16
    all_events <- all_category_events
  }
  Counter <- 1
  Events <- NULL
  i <- 1
  while (Counter < plotLimit & i <= dim(data)[1]){
    rowProfile <- rpna_dataTotr1000[i,]
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
    
    gstats <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
    
    print(gstats)
    
    print("done")
    # eval.parent(expr = paste0("g", i, " <- g"))
    
    if((rowProfile$my_event %in% all_events) & Counter < plotLimit){
      if (Counter == 1){
        assign(paste0("gret", Counter), gret)
        assign(paste0("gvol", Counter), gvol)
        Counter <- Counter+1
        Events <- c(Events,rowProfile$my_event)
      } else if (!(rowProfile$my_event %in% Events)){
        assign(paste0("gret", Counter), gret)
        assign(paste0("gvol", Counter), gvol)
        Counter <- Counter+1
        Events <- c(Events,rowProfile$my_event)
      }
    }

    print(Counter)
    print(i)
    #     width=15
    #     height=9
    #     gout <- RP_ExportMultiplePlot(g,g, plotlist = NULL, filename = "best", outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 2, width = width, height = height)
    #     print(gout)
    print("profile displayed")
    i <- i+1
    
  }
  
  print("multiple plot aggregating done")
  width=15
  height=9
  g <- NULL
  if (aggregate_criteria == "GROUP"){
    g <- ExportMultiplot(gret1,gvol1,gret2,gvol2,gret3,gvol3,gret4,gvol4,gret5,gvol5,plotlist = NULL, filename = paste0(my_criterium,"bestGroups"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  } else {
    g <- ExportMultiplot(gret1,gvol1,
                          gret2,gvol2,
                          gret3,gvol3,
                          gret4,gvol4,
                          gret5,gvol5,
                          gret6,gvol6,
                          gret7,gvol7,
                          gret8,gvol8,
                          gret9,gvol9,
                          gret10,gvol10,
                          plotlist = NULL, filename = paste0(my_criterium,"bestCategories"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  }
  print("multiple plot done")
  return(g)
}
  
################################
################################
################################
################################
################################
################################
################################ End of Plotting functions

print("Analysis done")
criteria <- c("rank_return_post_event","ord_return_post_event","return_post_event","rank_volatility_post_event","ord_volatility_post_event","volatility_post_event",
              "rank_return_pre_event","ord_return_pre_event","return_pre_event","rank_volatility_pre_event","ord_volatility_pre_event","volatility_pre_event","volatility_correction")

# criteria <- c("rank_return_post_event","return_post_event","rank_volatility_post_event","volatility_post_event")
            

criteria <- c(criteria,"avg_prepost_event_ab_return","rank_avg_prepost_event_ab_return","volatility_correction")

all_group_events <- readRDS(file=paste0(outputDataPath,"new_rpna_all_group_events.rds"))
all_category_events <- readRDS(file=paste0(outputDataPath,"new_rpna_all_category_events.rds"))

for (my_criterium in criteria){
  ### outputing the 10 best profiles
  if (my_criterium == "rank_return_post_event"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$rank_weighted_avg_post_event_ab_return, decreasing = TRUE),]
  }
  
  if (my_criterium == "ord_return_post_event"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$stat_weighted_avg_post_event_ab_return, decreasing = TRUE),]
  }
  
  if (my_criterium == "return_post_event"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$avg_post_event_ab_return, decreasing = TRUE),]
  }
  
  if (my_criterium == "rank_volatility_post_event"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$rank_weighted_avg_post_event_ab_volatility, decreasing = TRUE),]
  }
  
  if (my_criterium == "ord_volatility_post_event"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$stat_weighted_avg_post_event_ab_volatility, decreasing = TRUE),]
  }
  
  if (my_criterium == "volatility_post_event"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$avg_post_event_ab_volatility, decreasing = TRUE),]
  }
  
  if (my_criterium == "avg_prepost_event_ab_return"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$avg_prepost_event_ab_return, decreasing = TRUE),]
  }
  
  if (my_criterium == "ord_avg_prepost_event_ab_return"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$stat_weighted_avg_prepost_event_ab_return, decreasing = TRUE),]
  }
  if (my_criterium == "rank_avg_prepost_event_ab_return"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$rank_weighted_avg_prepost_event_ab_return, decreasing = TRUE),]
  }
  
  if (my_criterium == "volatility_correction"){
    rpna_dataTotr1000 <- rpna_dataTotr1000[order(rpna_dataTotr1000$volatility_correction, decreasing = TRUE),]
  }

  
  filename <- getPlotBestProfiles("GROUP",rpna_dataTotr1000,all_group_events,all_category_events)
  print(filename)
  
  filename <- getPlotBestProfiles("CATEGORY",rpna_dataTotr1000,all_group_events,all_category_events)
  print(filename)
  
  groupCounter <- 1
  categoryCounter <- 1
  eventsGroup <- NULL
  eventsCategory <- NULL
  i <- 1
  while (groupCounter < 6 | categoryCounter < 16){
    rowProfile <- rpna_dataTotr1000[i,]
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
    
    # eval.parent(expr = paste0("g", i, " <- g"))
    
    if((rowProfile$my_event %in% all_group_events) & groupCounter < 6){
      if (groupCounter == 1){
        assign(paste0("ggret", groupCounter), gret)
        assign(paste0("ggvol", groupCounter), gvol)
        groupCounter <- groupCounter+1
        eventsGroup <- c(eventsGroup,rowProfile$my_event)
      } else if (!(rowProfile$my_event %in% eventsGroup)){
        assign(paste0("ggret", groupCounter), gret)
        assign(paste0("ggvol", groupCounter), gvol)
        groupCounter <- groupCounter+1
        eventsGroup <- c(eventsGroup,rowProfile$my_event)
      }
    }
    
    if((rowProfile$my_event %in% all_category_events) & categoryCounter < 16){
      if (categoryCounter == 1){
        assign(paste0("gret", categoryCounter), gret)
        assign(paste0("gvol", categoryCounter), gvol)
        categoryCounter <- categoryCounter+1
        eventsCategory <- c(eventsCategory,rowProfile$my_event)
      } else if (!(rowProfile$my_event %in% eventsCategory)){
        assign(paste0("gret", categoryCounter), gret)
        assign(paste0("gvol", categoryCounter), gvol)
        categoryCounter <- categoryCounter+1
        eventsCategory <- c(eventsCategory,rowProfile$my_event)
      }
    }
    
    print(categoryCounter)
    print(groupCounter)
    print(i)
    #     width=15
    #     height=9
    #     gout <- RP_ExportMultiplePlot(g,g, plotlist = NULL, filename = "best", outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 2, width = width, height = height)
    #     print(gout)
    print("profile displayed")
    i <- i+1
    
  }
  
  print("multiple plot aggregating done")
  width=15
  height=9
  RP_ExportMultiplePlot(ggret1,ggvol1,ggret2,ggvol2,ggret3,ggvol3,ggret4,ggvol4,ggret5,ggvol5,plotlist = NULL, filename = paste0(my_criterium,"bestGroups"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  # RP_ExportMultiplePlot(gg1,gg2,gg3,gg4,plotlist = NULL, filename = paste0(my_criterium,"bestGroups"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 4, width = width, height = height)
  

  # g4, g5,g6,g7,g8,g9,g10,g11,g12,g14,g14,g15,g16,g17,g18,g19,g20,plotlist = NULL, filename = paste0(my_criterium,"bestCategories"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  width=15
  height=9
  RP_ExportMultiplePlot(gret1,gvol1,
                        gret2,gvol2,
                        gret3,gvol3,
                        gret4,gvol4,
                        gret5,gvol5,
                        gret6,gvol6,
                        gret7,gvol7,
                        gret8,gvol8,
                        gret9,gvol9,
                        gret10,gvol10,
                        plotlist = NULL, filename = paste0(my_criterium,"bestCategories"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  # RP_ExportMultiplePlot(g1,g2, g3, g4, g5,g6,g7,g8,plotlist = NULL, filename = paste0(my_criterium,"bestCategories"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 4, width = width, height = height)
  print("multiple plot done")
  # print(gout)
  #   width=15
  #   height=9
  #   g <-  RP_ExportMultiplePlot(unlist(plotlists), plotlist = NULL, filename = "best", outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 2, width = width, height = height)
  #     # RP_ExportMultiplePlot(unlist(plotlists), plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 5, width = 10, height = 15) 
  #   print(g)
  #   g <- RP_ExportMultiplePlot(plotlist = plotlists, filename = NULL, outputDataPath = NULL, cols = 5, width = 10, height = 15) 
  #   print(g)
}


print("Done outputing profiles")

# resultsFilteredDF <- resultsDF[resultsDF$relevance == "HIGH",]
# resultsFilteredDF <- resultsDF[resultsDF$event_relevance == "HIGH",]
# resultsFilteredDF <- resultsDF[resultsDF$aggregate_criteria == "GROUP",]

trim_categories <- function(sameEventProfiles){
  uniqueRow <- sameEventProfiles[which(sameEventProfiles$rank_weighted_avg_post_event_ab_return == max(sameEventProfiles$rank_weighted_avg_post_event_ab_return)),]
  if(dim(uniqueRow)[1]>1){
    uniqueRow <- uniqueRow[which(uniqueRow$event_number_event_filtering == max(sameEventProfiles$event_number_event_filtering)),]
  }
  return(uniqueRow)
}

library(dplyr)

#####################
#####################
#####################
##################### Metrics per relevance, event relevance
resultsDT <- as.data.table(resultsDF)
print(dim(resultsDT))
resultsDT[,vol_rel := mean(avg_post_event_ab_volatility),.(relevance)]
resultsDT[,ret_rel := mean(avg_post_event_ab_return),.(relevance)]

resultsDT[,vol_ev_rel := mean(avg_post_event_ab_volatility),.(event_relevance)]
resultsDT[,ret_ev_rel := mean(avg_post_event_ab_return),.(event_relevance)]

resultsDT[,vol_rel_ev_rel := mean(avg_post_event_ab_volatility),.(relevance,event_relevance)]
resultsDT[,ret_rel_ev_rel := mean(avg_post_event_ab_return),.(relevance,event_relevance)]


toPlotDF <- as.data.frame(unique(resultsDT[,.(ret_rel,vol_rel,ret_ev_rel,vol_ev_rel,vol_rel_ev_rel,ret_rel_ev_rel,relevance,event_relevance)]))
print(dim(toPlotDT))

library(ggplot2)

toPlotDFFiltered <- unique(toPlotDF[,c("relevance","ret_rel")])
colnames(toPlotDFFiltered) <- c("RELEVANCE","RETURN")
toPlotDFFiltered$RETURN <- toPlotDFFiltered$RETURN*100
RP_PlotProportion(DataFrame = toPlotDFFiltered,horizontal = F,Weight = F,Label = T,MyTitle = "Return per relevance",FullExportingPath = paste0(outputDataPath,"PICTURES/Relevance"))

toPlotDFFiltered <- unique(toPlotDF[,c("event_relevance","ret_ev_rel")])
colnames(toPlotDFFiltered) <- c("EVENT_RELEVANCE","RETURN")
toPlotDFFiltered$RETURN <- toPlotDFFiltered$RETURN*100
RP_PlotProportion(DataFrame = toPlotDFFiltered,horizontal = F,Weight = F,MyTitle = "Return per Event_Relevance",FullExportingPath = paste0(outputDataPath,"PICTURES/Event_Relevance"))

toPlotDFFiltered <- unique(toPlotDF[toPlotDF$relevance == "HIGH",c("event_relevance","ret_rel_ev_rel")])
colnames(toPlotDFFiltered) <- c("EVENT_RELEVANCE","RETURN")
RP_PlotProportion(DataFrame = toPlotDFFiltered,horizontal = F,Weight = F,Label = T,MyTitle = "Return per event_relevance for  relevance HIGH",FullExportingPath = paste0(outputDataPath,"PICTURES/HIGHRelevance"))

toPlotDFFiltered <- unique(toPlotDF[toPlotDF$relevance == "MEDIUM",c("event_relevance","ret_rel_ev_rel")])
colnames(toPlotDFFiltered) <- c("EVENT_RELEVANCE","RETURN")
RP_PlotProportion(DataFrame = toPlotDFFiltered,horizontal = F,Weight = F,Label = T,MyTitle = "Return per event_relevance for relevance MEDIUM",FullExportingPath = paste0(outputDataPath,"PICTURES/MEDIUMRelevance"))

toPlotDFFiltered <- unique(toPlotDF[toPlotDF$relevance == "LOW",c("event_relevance","ret_rel_ev_rel")])
colnames(toPlotDFFiltered) <- c("EVENT_RELEVANCE","RETURN")
RP_PlotProportion(DataFrame = toPlotDFFiltered,horizontal = F,Weight = F,Label = T,MyTitle = "Return per event_relevance for relevance LOW",FullExportingPath = paste0(outputDataPath,"PICTURES/LOWRelevance"))
# 
# #####
# toPlotDFFiltered <- unique(toPlotDF[toPlotDF$event_relevance == "HIGH",c("event_relevance","ret_rel_ev_rel")])
# colnames(toPlotDFFiltered) <- c("RELEVANCE","RETURN")
# RP_PlotProportion(DataFrame = toPlotDFFiltered,horizontal = F,Weight = F,Label = T,MyTitle = "Return per relevance for event_relevance HIGH",FullExportingPath = paste0(outputDataPath,"PICTURES/HIGHEventRelevance"))
# 
# toPlotDFFiltered <- unique(toPlotDF[toPlotDF$event_relevance == "MEDIUM",c("event_relevance","ret_rel_ev_rel")])
# colnames(toPlotDFFiltered) <- c("RELEVANCE","RETURN")
# RP_PlotProportion(DataFrame = toPlotDFFiltered,horizontal = F,Weight = F,Label = T,MyTitle = "Return per relevance for event_relevance MEDIUM",FullExportingPath = paste0(outputDataPath,"PICTURES/MEDIUMEventRelevance"))
# 
# toPlotDFFiltered <- unique(toPlotDF[toPlotDF$event_relevance == "LOW",c("event_relevance","ret_rel_ev_rel")])
# colnames(toPlotDFFiltered) <- c("RELEVANCE","RETURN")
# RP_PlotProportion(DataFrame = toPlotDFFiltered,horizontal = F,Weight = F,Label = T,MyTitle = "Return per relevance for event_relevance LOW",FullExportingPath = paste0(outputDataPath,"PICTURES/LOWEventRelevance"))

print("stop")

# RELEVANCE_COMBOS <- c("HIGH_HIGH","HIGH_MEDIUM","HIGH_LOW","MEDIUM_MEDIUM","MEDIUM_LOW","LOW_LOW")
# for (my_event_relevance_combo in  RELEVANCE_COMBOS){
#   if(my_event_relevance_combo == "HIGH_HIGH"){
#     my_relevance <- "HIGH"
#     my_event_relevance <- "HIGH"
#   }
#   
#   if(my_event_relevance_combo == "HIGH_MEDIUM"){
#     my_relevance <- "HIGH"
#     my_event_relevance <- "MEDIUM"
#   }
#   
#   if(my_event_relevance_combo == "HIGH_LOW"){
#     my_relevance <- "HIGH"
#     my_event_relevance <- "LOW"
#   }
#   
#   
#   if(my_event_relevance_combo == "MEDIUM_MEDIUM"){
#     my_relevance <- "MEDIUM"
#     my_event_relevance <- "MEDIUM"
#   }
#   
#   if(my_event_relevance_combo == "MEDIUM_LOW"){
#     my_relevance <- "MEDIUM"
#     my_event_relevance <- "LOW"
#   }
#   
#   if(my_event_relevance_combo == "LOW_LOW"){
#     my_relevance <- "LOW"
#     my_event_relevance <- "LOW"
#   }
#   
#   toPlotDFFiltered <- toPlotDF[toPlotDF$relevance == my_relevance & toPlotDF$event_relevance == my_event_relevance,]
#   print(dim(toPlotDFFiltered))
# }



######################
######################
######################
###################### No filtering

print(dim(resultsDF))
resultsBestDF <- ddply(resultsDF, .(my_event), trim_categories)
print(dim(resultsBestDF))
resultsBestDF <- resultsBestDF[order(resultsBestDF$avg_post_event_ab_return,decreasing = T),]


BestRetToPlot <- resultsBestDF[1:100,c("my_event","avg_post_event_ab_return")] 
BestVolToPlot <- resultsBestDF[1:100,c("my_event","avg_post_event_ab_volatility")] 

RP_PlotProportion(DataFrame = BestRetToPlot,horizontal = T,Weight = F,FullExportingPath = paste0(outputDataPath,"PICTURES/BestRetToPlot"))
RP_PlotProportion(DataFrame = BestVolToPlot,horizontal = T,Weight = F,FullExportingPath = paste0(outputDataPath,"PICTURES/BestVolToPlot"))

print("best df")


resultsDFFiltered <- resultsDF[resultsDF$relevance == "HIGH",]
resultsDFFilteredBest <- ddply(resultsDFFiltered, .(my_event), trim_categories)
print(dim(resultsDFFilteredBest))
resultsDFFilteredBest <- resultsDFFilteredBest[order(resultsDFFilteredBest$avg_post_event_ab_return,decreasing = T),]

BestRetToPlot <- resultsDFFilteredBest[1:100,c("my_event","avg_post_event_ab_return")] 
BestVolToPlot <- resultsDFFilteredBest[1:100,c("my_event","avg_post_event_ab_volatility")] 

RP_PlotProportion(DataFrame = BestRetToPlot,horizontal = T,Weight = F)
RP_PlotProportion(DataFrame = BestVolToPlot,horizontal = T,Weight = F)

##################
##################
##################
################## RPNA file loading
# rpna_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"spr_r1000_rpna_abvol_abvol_corrado_df.rds"))

print("outputing statistics")


