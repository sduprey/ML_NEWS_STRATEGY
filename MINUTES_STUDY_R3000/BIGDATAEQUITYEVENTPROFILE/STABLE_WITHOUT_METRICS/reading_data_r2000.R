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




############
############
############
############ my functions



##############
##############
##############
############## Computing the spread sentiment category
bootstrap_CI <- FALSE
data <- readRDS(file=paste0(outputDataPath,bootstrap_CI,"r2000_bigdata_abvol_abvol_corrado_df.rds"))
print(dim(data))
data$localSource[data$localSource == "DJ"] <- "DJPR"
data$localSource[data$localSource == "PREMIUM"] <- "PREMIUM_PACK"
data$localSource[data$localSource == "WEBNONPREMIUM"] <- "WEB_NON_PREMIUM"

stats_sign <- colnames(data)[which(!is.na(as.numeric(colnames(data))))]
ord <- paste0("ORD",stats_sign)
rets <- paste0("RET",stats_sign)
vol <- paste0("VOLU",stats_sign)
vola <- paste0("VOLA",stats_sign)
numeric_columns <- c(stats_sign,rets,ord,vol,vola)
mean_columns <- c(stats_sign,ord,vol,vola)
spread_columns <- c(rets)


joining_columns <- setdiff(colnames(data), numeric_columns)

joining_columns <- setdiff(joining_columns, c("sentiment_criteria","corrado_methodo","event_number_event_filtering"))


computeSpread <- function(df,toMean , toSpread){
  
  if(sum(df$sentiment_criteria == "NEGATIVE") & sum(df$sentiment_criteria == "POSITIVE")){
    
    negativeSentiment <- df[df$sentiment_criteria == "NEGATIVE",]
    positiveSentiment <- df[df$sentiment_criteria == "POSITIVE",]
    
    if(dim(negativeSentiment)[1] >1){
      print("to investigate")
      negativeSentiment <- negativeSentiment[which.max(negativeSentiment$event_number_event_filtering),]
    } 
    if(dim(positiveSentiment)[1] >1){
      print("to investigate")
      positiveSentiment <- positiveSentiment[which.max(positiveSentiment$event_number_event_filtering),]
    } 
    
    rowToAdd <- negativeSentiment
    rowToAdd[,toMean] <- (positiveSentiment[,toMean] + negativeSentiment[,toMean])/2
    rowToAdd[,toSpread] <- (positiveSentiment[,toSpread] - negativeSentiment[,toSpread])
    rowToAdd$sentiment_criteria <- "SPREAD"
    return(rbind(df,rowToAdd))
  }
  return(df)
}

print("computing the spread")
data_spread_augmented <- ddply(.data = data, .variables = joining_columns, .fun = function(x){computeSpread(x,mean_columns,spread_columns)})
print("spread computed")
RP_SaveDataFrame(data_spread_augmented, outputDataPath = outputDataPath, filename = "spr_r2000_bigdata_abvol_abvol_corrado_df")
print("spread saved")

##############
##############
##############
##############  End of computing the spread sentiment category


##############
##############
##############
############## Ordering the best profiles

data <- readRDS(file=paste0(outputDataPath,"spr_r2000_bigdata_abvol_abvol_corrado_df.rds"))
data <- data[data$event_number_event_filtering >= 50,]


# data$infinity_return <- abs(data$RET180)
# data$infinity_confidence <- data$`180`

data$correcting_factor <- 2*(data$sentiment_criteria == "POSITIVE"  | data$sentiment_criteria == "SPREAD")-1
data$correcting_factor[data$sentiment_criteria == "ALL"] <- 0
#### first one
# data$infinity_return <- (data$RET180)*data$correcting_factor
#### second one
# data$infinity_return <- (data$RET180)*(data$`180`)*data$correcting_factor
#### third one
stats_post_sign <- colnames(data)[which(as.numeric(colnames(data)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)
data$infinity_return <- rowSums(data[,rets_post]*data$correcting_factor*data[,stats_post_sign],na.rm = TRUE)


####### all generics

all_group_events <- sort(unique(data$my_event[data$aggregate_criteria == "GROUP"]))
all_category_events <- sort(unique(data$my_event[data$aggregate_criteria == "CATEGORY"]))

RP_SaveDataFrame(all_group_events, outputDataPath = outputDataPath, filename = "bigdata_all_group_events")
RP_SaveDataFrame(all_category_events, outputDataPath = outputDataPath, filename = "bigdata_all_category_events")


###########
########### Fro groups
dataGroup <- data[data$aggregate_criteria =="GROUP",]

dataGroupDT <- as.data.table(dataGroup)
selectBestProfile <- function(discriminating_value){
  
  return(max(discriminating_value))
}
print(dim(dataGroupDT))
# setkey(dataDT,"my_event")
dataGroupDT[,max_prof := selectBestProfile(infinity_return),by=c("my_event")]
print(dim(dataGroupDT))
dataGroupDT <- as.data.frame(unique(dataGroupDT[infinity_return >= max_prof,]))
print(dim(dataGroupDT))
dataGroupDT <- unique(dataGroupDT[,c("my_event","infinity_return")])
dataGroupDT <- dataGroupDT[order(dataGroupDT$infinity_return,decreasing = TRUE),]
RP_SaveDataFrame(dataGroupDT, outputDataPath = outputDataPath, filename = "bigdata_best_profile_group_ordered_r2000_corrado_df")

###########
########### For categories
dataCategory <- data[data$aggregate_criteria =="CATEGORY",]

dataCategoryDT <- as.data.table(dataCategory)
selectBestProfile <- function(discriminating_value){
  
  return(max(discriminating_value))
}
print(dim(dataCategoryDT))
# setkey(dataDT,"my_event")
dataCategoryDT[,max_prof := selectBestProfile(infinity_return),by=c("my_event")]
print(dim(dataCategoryDT))
dataCategoryDT <- as.data.frame(unique(dataCategoryDT[infinity_return >= max_prof,]))
print(dim(dataCategoryDT))
dataCategoryDT <- unique(dataCategoryDT[,c("my_event","infinity_return")])
dataCategoryDT <- dataCategoryDT[order(dataCategoryDT$infinity_return,decreasing = TRUE),]
RP_SaveDataFrame(dataCategoryDT, outputDataPath = outputDataPath, filename = "bigdata_best_profile_category_ordered_r2000_corrado_df")
print("best profiles computed")



##############
##############
##############
############## end of ordering the best profiles




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

outputGraphicsTogetherBestProfileStats <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R2000"){
  
  dataframestats <- dataFrame[,c("MINUTES","COR_STATS_SIGN","ORD_STATS_SIGN")]
  colnames(dataframestats) <- c("MINUTES","SIGNIFICANCE(RANK)","SIGNIFICANCE")
  dataframerets <- dataFrame[,c("MINUTES","RETS")]
  colnames(dataframerets) <- c("MINUTES","RETURNS")
  
  
  
  significance_threshold <- 1 - 0.05
  dataframestats$ABNORMAL_THRESHOLD <- significance_threshold
  
  dataframestats[is.na(dataframestats)] <- 0
  
  g1 <- PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",percent= TRUE, Title = my_event, FullExportingPath = NULL)
  # g2 <- RP_PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  return(g1)
}

outputGraphicsTogetherBestProfileRets <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R2000"){
  
  
  dataframerets <- dataFrame[,c("MINUTES","RETS")]
  colnames(dataframerets) <- c("MINUTES","RETURNS")
  
  
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}


outputGraphicsTogetherBestProfileVol <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R2000"){
  
  
  dataframerets <- dataFrame[,c("MINUTES","VOLUME")]
  colnames(dataframerets) <- c("MINUTES","VOLUME")
  
  
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}

outputGraphicsTogetherBestProfileVola <- function(product_criteria,aggregate_criteria,sentiment_criteria,similarity_gap_filter,ens_filter,event_number_event_filtering, gics_sector, my_event, localSource, dataFrame,  plotInArborescence, Russell_version = "R2000"){
  
  
  dataframerets <- dataFrame[,c("MINUTES","VOLATILITY")]
  
  
  # g1 <- RP_PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",Title = my_event, FullExportingPath = NULL)
  g2 <- PlotDataFrame(dataframerets,AxisIncluded = T,XLab = "Minute Lags",YLab = "BPS cumulated minute returns",Title = my_event, FullExportingPath = NULL)
  # g <- RP_ExportMultiplePlot(g1,g2, plotlist = NULL, filename = NULL, outputDataPath = NULL, cols = 1, width = 10, height = 15)
  
  return(g2)
  
}

##############
##############
##############
############## Testing stuff



######## ranking the profile

if(dim(data)[1]>0){
  rowProfile <- data[1,]
  
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
  g1 <- outputGraphicsTogetherBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R2000")
  
  # Render your graph
  print(g1)    
  
  g2 <- outputGraphicsTogetherBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R2000")
  
  # Render your graph
  print(g2)   
  
  
  g3 <- outputGraphicsTogetherBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R2000")
  
  # Render your graph
  print(g3)   
  
  g4 <- outputGraphicsTogetherBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R2000")
  
  # Render your graph
  print(g4)   
  
}



