### Launching all pair spread computations
### Trying to forecast the spread between ?/? bonds futures
# library("SIT")
library("RPQuantUtils")
library("RPToolsDB")
require(ggplot2)
require("ppcor")
require(graphics)
require("TTR")
require(plyr)
# require(reshape)
require(reshape2)
require(RColorBrewer)
require(stats)
require(Rsolnp)
require(zoo)
require(xts)
require(vars)
# require(Quandl)
require(rpart)
require(randomForest)
# require(rpart.plot)
# require(rattle)
# install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
# require(caret)
require(xgboost)
library("RPBackTesting", lib.loc="C:/Program Files/R/R-3.2.2/library")


source("./RCode/RP_Plotting_Utils.R")
source("./RCode/RP_Macro_Monthly_Utils.R")
source("./RCode/RP_Spread_Utils.R")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")
outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")


heatmapMatrix <- function(mat, xlab = "X", ylab = "Y", zlab = "Z", low = "white", high = "black",
                          grid = "grey", limits = NULL, legend.position = "top", colours = NULL){
  nr <- nrow(mat)
  nc <- ncol(mat)
  rnames <- rownames(mat)
  cnames <- colnames(mat)
  if(is.null(rnames)) rnames <- paste(xlab, 1:nr, sep = "_")
  if(is.null(cnames)) cnames <- paste(ylab, 1:nc, sep = "_")
  x <- rep(rnames, nc)
  y <- rep(cnames, each = nr)
  df <- data.frame(factor(x, levels = unique(x)),
                   factor(y, levels = unique(y)),
                   as.vector(mat))
  colnames(df) <- c(xlab, ylab, zlab)
  p <- ggplot(df, aes_string(ylab, xlab)) +
    geom_tile(aes_string(fill = zlab), colour = grid) +
    theme(legend.position=legend.position)
  if(is.null(colours)){
    p + scale_fill_gradient(low = low, high = high, limits = limits)
  }else{
    p + scale_fill_gradientn(colours = colours)
  }
}

# my_pairs = list (c("US","DE"),c("US","EU"),c("US","FR"),c("US","GB"),c("US","JP"))
my_pairs = list(c("US","DE"),c("US","GB"),c("US","JP"))

#### results
my_threshold <- 100
for (my_pair in my_pairs){
  
  # SaveDataFrame(my_total_df,outputDataPathMonth,paste(my_pair[1], my_pair[2],"variable_importance_spread_results_month_2007",sep=""))
  filename <- paste(my_pair[1], my_pair[2],"variable_importance_spread_results_month_2007.rds",sep="")
  my_total_df <- readRDS(paste(outputDataPathMonth,filename, sep = ""))
  print(dim(my_total_df))
  # we only keep the first 100 most important predictors by importance count over time
  # my_filtered_df <- my_total_df[(sort(rowSums(my_total_df[,-1]), index.return=TRUE, decreasing = TRUE))$ix<=my_threshold,] 
  my_index <- rowSums(my_total_df[,-1])!=0
  my_total_df <- my_total_df[my_index,]
  my_filtered_df <- my_total_df[(sort(rowSums(my_total_df[,-1]!=0), index.return=TRUE, decreasing = TRUE))$ix<=my_threshold,] 
  
  my_filtered_df$Feature <- as.factor(my_filtered_df$Feature) 
  my_filtered_df <- transform(my_filtered_df, Feature=reorder(Feature,(sort(rowSums(my_filtered_df[,-1]!=0), index.return=TRUE, decreasing = TRUE))$ix))
  my_filtered_df <- transform(my_filtered_df, Feature=reorder(Feature,rowSums(my_filtered_df[,-1])))
  
  
  my_filtered_df.m <- melt(my_filtered_df,id="Feature")
  colnames(my_filtered_df.m) <- c("Feature","Dates","Weight")
  # my_filtered_df.m <- ddply(my_filtered_df.m, .(variable), transform)
  # my_filtered_df.m <- ddply(my_filtered_df.m, .(variable), transform, rescale = rescale(value))
  p <- ggplot(my_filtered_df.m, aes(Dates, Feature)) + geom_tile(aes(fill = Weight),colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue")
  print(p)
  ExportPlot(p,outputDataPathMonth,paste0(my_pair[2],"heatmap"), width=10, height=15)
  print("Done")
#   # we draw our own heat map
#   p <- ggplot(my_filtered_df.m, aes_string(Feature, Dates)) +
#     geom_tile(aes_string(fill = Weight), colour = grid) +
#     theme(legend.position=legend.position)
#   
#   print(p)
#   if(is.null(colours)){
#     p + scale_fill_gradient(low = low, high = high, limits = limits)
# #   }
#   print(p)
#   p1 <- heatmapMatrix(my_filtered_df, xlab = "X", ylab = "Class", zlab = "Corr", high = "blue")
#   print(p1)
#   print("done")
}
