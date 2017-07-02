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
  ToPlotDataFrame <- ToPlotDataFrame[ToPlotDataFrame$variable != "threshold",]
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
  # g <- g + geom_line(aes(x = DataFrame$MINUTES, y = DataFrame$threshold),colour = 'black', size = 1.5,linetype="dashed")
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

PlotDataFrameStatsTog <- function (DataFrame, XLab = "", YLab = "", Title = "", AxisIncluded = FALSE, 
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

load(file = paste0(outputDataPath,"shit.RData" ))
print(dim(dataframestats))
# g1 <- PlotDataFrameStatsTog(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",percent= TRUE, Title = "Statistical Significance", FullExportingPath = NULL)
# print(g1)

g1 <- PlotDataFrame(dataframestats,AxisIncluded = T,XLab = "Minute Lags",YLab = "p-value",percent= TRUE, Title = "Statistical Significance", FullExportingPath = NULL)
print(g1)


PlotProportion_Comparison <- function (DataFrame, MyTitle = "", horizontal = F, Legend = F, 
                                       Label = T, Weight = T, Comparison = F, FullExportingPath = NULL) 
{
  if (is.null(colnames(DataFrame))) {
    colnames(DataFrame) <- paste0("COLUMN_", seq(1, dim(DataFrame)[2]))
  }
  if (class(DataFrame) == "matrix") {
    DataFrame <- as.data.frame(DataFrame)
  }
  originalNames <- colnames(DataFrame)
  if (Legend || Comparison) {
    colnames(DataFrame) <- c("features", "weights", "types")
    g <- ggplot(data = DataFrame, aes(x = features, y = weights, 
                                      fill = types))
  }
  else {
    colnames(DataFrame) <- c("features", "weights")
    g <- ggplot(data = DataFrame, aes(x = features, y = weights))
  }
  if (horizontal) {
    DataFrame <- transform(DataFrame, features = reorder(features, 
                                                         weights))
  }
  else {
    DataFrame <- transform(DataFrame, features = reorder(features, 
                                                         -(weights)))
  }
  if (Comparison) {
    g <- g + geom_bar(stat = "identity", position = "dodge", 
                      alpha = 0.5) + ggtitle(MyTitle) + ylab(originalNames[2]) + 
      xlab(originalNames[1]) + theme(text = element_text(size = 16), 
                                     legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
      theme(legend.position = c(0.8, 0.3), legend.box = "vertical") + 
      theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
            title = element_text(size = 18, face = "bold")) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                       size = 18))
  }
  else {
    g <- g + geom_bar(stat = "identity", alpha = 0.5) + ggtitle(MyTitle) + 
      ylab(originalNames[2]) + xlab(originalNames[1]) + 
      theme(text = element_text(size = 16), legend.title = element_blank(), 
            plot.title = element_text(hjust = 0.5)) + theme(legend.position = c(0.8, 
                                                                                0.3), legend.box = "vertical") + theme(axis.text.x = element_text(size = 16), 
                                                                                                                       axis.text.y = element_text(size = 16), title = element_text(size = 18, 
                                                                                                                                                                                   face = "bold")) + theme(axis.text.x = element_text(angle = 45, 
                                                                                                                                                                                                                                      hjust = 1, size = 18))
  }
  if (Weight) {
    g <- g + scale_y_continuous(labels = percent_format())
  }
  else {
    g <- g + scale_y_continuous()
  }
  if (!Label) {
    g <- g + theme(axis.line = element_blank(), axis.text.x = element_blank(), 
                   axis.ticks = element_blank())
  }
  if (horizontal) {
    g <- g + coord_flip()
  }
  print(g)
  if (!is.null(FullExportingPath)) {
    RP_ExportPlot(g, FullExportingPath, paste(originalNames[1], 
                                              originalNames[2], originalNames[2], sep = "_"))
  }
  return(g)
}



LevelComparison <- readRDS(file=paste0(outputDataPath,"LevelComparison.rds"))


PlotProportion_Comparison(DataFrame = LevelComparison)


