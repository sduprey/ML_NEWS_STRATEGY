### Launching all pair spread computations
### Trying to forecast the spread between ?/? bonds futures
# library("SIT")
library("RPQuantUtils")
library("RPToolsDB")
library("RPBackTesting")
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
library(caret)
library(ROCR)

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
# outputDataPathMonth <- paste(outputDataPath,"Month_2007/01_11_2015/",sep="")
# outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007/01_11_2015/",sep="")
outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")
outputDataPathStrategyMonth <- paste(outputDataPath,"Month_2007/",sep="")


results <- readRDS(paste(outputDataPathStrategyMonth,"all_pairs_results_month_2007.rds", sep = ""))

# results$STRATEGY_RETURN <- (results$JP_STRATEGY_RETURN+results$GB_STRATEGY_RETURN+results$DE_STRATEGY_RETURN)/3
# results$STRATEGY_TODAY <- CumFromRetToPricesStart(results$STRATEGY_RETURN)
# 
# results$US_WEIGHT <- (results$DE_FIRST_WEIGHT + results$GB_FIRST_WEIGHT + results$JP_FIRST_WEIGHT)/3
# results$DE_WEIGHT <- results$DE_SECOND_WEIGHT/3
# results$JP_WEIGHT <- results$JP_SECOND_WEIGHT/3
# results$GB_WEIGHT <- results$GB_SECOND_WEIGHT/3

### Deutschland visualization
my_result_spread_name <- "US_DE"
my_pair <- c("US","DE")
my_pair[1]<-"US"
my_pair[2]<-"DE"


print("Outputing statistical results for our individual trading strategy US_DE")
WeightMatrix <- results[,c("DATES","DE_FIRST_WEIGHT","DE_SECOND_WEIGHT")]
colnames(WeightMatrix) <- c("DATE","DE_FIRST_WEIGHT","DE_SECOND_WEIGHT")
# ReturnSerie <- results[,c("DATES","STRATEGY_RETURN")]
# colnames(ReturnSerie) <- c("Date","STRATEGY_RETURN")
ReturnSerie <- results[,c("DE_STRATEGY_RETURN")]
turnover <- RP_GetTurnOver(WeightMatrix,-1)
RP_ReturnStats(ReturnSerie,21,TRUE,WeightMatrix,0.05,F)

# Long/short position returns distribution
long_US <- results$DE_FIRST_WEIGHT>=0
long_DE <- results$DE_SECOND_WEIGHT>=0
LONG_STRATEGY_RETURN_US <- results$DE_FIRST_WEIGHT[long_US]*results$DE_FIRST_BOND_NEXT_RETURN[long_US]
LONG_STRATEGY_RETURN_DE <- results$DE_SECOND_WEIGHT[long_DE]*results$DE_SECOND_BOND_NEXT_OPEN_RETURN[long_DE]


print("Histogram return for long position both US and DE")
hist((LONG_STRATEGY_RETURN_US+LONG_STRATEGY_RETURN_DE))
print("Mean return for long position both US and DE")
mean((LONG_STRATEGY_RETURN_US+LONG_STRATEGY_RETURN_DE))
print("Histogram return for long position for US only")
hist(LONG_STRATEGY_RETURN_US)
print("Mean return for long position for US only")
mean(LONG_STRATEGY_RETURN_US)
print("Histogram return for long position for DE only")
hist(LONG_STRATEGY_RETURN_DE)
print("Mean return for long position for DE only")
mean(LONG_STRATEGY_RETURN_DE)

# Short position returns distribution
short_US <- results$DE_FIRST_WEIGHT<=0
short_DE <- results$DE_SECOND_WEIGHT<=0
SHORT_STRATEGY_RETURN_US <- results$DE_FIRST_WEIGHT[short_US]*results$DE_FIRST_BOND_NEXT_RETURN[short_US]
SHORT_STRATEGY_RETURN_DE <- results$DE_SECOND_WEIGHT[short_DE]*results$DE_SECOND_BOND_NEXT_OPEN_RETURN[short_DE]
print("Histogram return for short position both US and DE")
hist((SHORT_STRATEGY_RETURN_US+SHORT_STRATEGY_RETURN_DE))
print("Mean return for short position both US and DE")
mean((SHORT_STRATEGY_RETURN_US+SHORT_STRATEGY_RETURN_DE))
print("Histogram return for long position for US only")
hist(SHORT_STRATEGY_RETURN_US)
print("Mean return for long position for US only")
mean(SHORT_STRATEGY_RETURN_US)
print("Histogram return for long position for DE only")
hist(SHORT_STRATEGY_RETURN_DE)
print("Mean return for long position for DE only")
mean(SHORT_STRATEGY_RETURN_DE)

### Nice visualization of superposed densities
return_decomposition_df <- data.frame(returns=LONG_STRATEGY_RETURN_US,typo="LONG_US")
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=LONG_STRATEGY_RETURN_DE,typo="LONG_DE"))
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=SHORT_STRATEGY_RETURN_US,typo="SHORT_US"))
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=SHORT_STRATEGY_RETURN_DE,typo="SHORT_DE"))

my_title <-"Distribution of returns per trade type"
my_xaxis_title <- paste("Returns")
my_yaxis_title <- paste("Percentage","\n")

g <- ggplot(return_decomposition_df, aes(x=returns, fill=typo)) +geom_density(alpha=.3)+
  scale_y_continuous(labels = percent_format())+scale_x_continuous(limits=c(-0.1,0.1))+facet_wrap(~typo, ncol = 2)
g <- g +ylab(my_yaxis_title)+xlab(my_xaxis_title)+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),title =element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.85), legend.box = "vertical")+
  theme(legend.background = element_rect(fill="gray90"))+
  theme(legend.key.size = unit(1., "cm"))+
  theme(legend.text =  element_text(size=12,colour="black"))+
  theme(legend.title =  element_text(size=12,colour="black"))
print(g)

ExportPlot(g,outputDataPath,paste("LongShortDecomposition",my_pair[1],"_",my_pair[2],sep=""))
g <- ggplot(return_decomposition_df, aes(x=returns, fill=typo)) +geom_density(alpha=.3)+
  scale_y_continuous(labels = percent_format())+scale_x_continuous(limits=c(-0.1,0.1))
g <- g +ylab(my_yaxis_title)+xlab(my_xaxis_title)+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),title =element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.85), legend.box = "vertical")+
  theme(legend.background = element_rect(fill="gray90"))+
  theme(legend.key.size = unit(1., "cm"))+
  theme(legend.text =  element_text(size=12,colour="black"))+
  theme(legend.title =  element_text(size=12,colour="black"))
print(g)
ExportPlot(g,outputDataPath,paste("LongShortDecompositionTogether",my_pair[1],"_",my_pair[2],sep=""))


## Splitting return between long/short trades

toplot_df <-  melt(results[,c("DATES", "DE_STRATEGY_TODAY","DE_FIRST_BOND","DE_SECOND_BOND")],"DATES")
my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,my_result_spread_name)


toplot_df <-  melt(results[,c("DATES","DE_FIRST_WEIGHT","DE_SECOND_WEIGHT")],"DATES")

my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,paste(my_result_spread_name,"_weights",sep=""))

toplot_df <-  melt(results[,c("DATES","DE_FIRST_LEG","DE_SECOND_LEG")],"DATES")

my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,paste(my_result_spread_name,"_legs",sep=""))



### Great Britain visualization
my_result_spread_name <- "US_GB"
my_pair[1]<-"US"
my_pair[2]<-"GB"
# Long/short position returns distribution
long_US <- results$GB_FIRST_WEIGHT>=0
long_GB <- results$GB_SECOND_WEIGHT>=0
LONG_STRATEGY_RETURN_US <- results$GB_FIRST_WEIGHT[long_US]*results$GB_FIRST_BOND_NEXT_RETURN[long_US]
LONG_STRATEGY_RETURN_GB <- results$GB_SECOND_WEIGHT[long_GB]*results$GB_SECOND_BOND_NEXT_OPEN_RETURN[long_GB]

print("Histogram return for long position both US and GB")
hist((LONG_STRATEGY_RETURN_US+LONG_STRATEGY_RETURN_GB))
print("Mean return for long position both US and GB")
mean((LONG_STRATEGY_RETURN_US+LONG_STRATEGY_RETURN_GB))
print("Histogram return for long position for US only")
hist(LONG_STRATEGY_RETURN_US)
print("Mean return for long position for US only")
mean(LONG_STRATEGY_RETURN_US)
print("Histogram return for long position for GB only")
hist(LONG_STRATEGY_RETURN_GB)
print("Mean return for long position for GB only")
mean(LONG_STRATEGY_RETURN_GB)

# Short position returns distribution
short_US <- results$GB_FIRST_WEIGHT<=0
short_GB <- results$GB_SECOND_WEIGHT<=0
SHORT_STRATEGY_RETURN_US <- results$GB_FIRST_WEIGHT[short_US]*results$GB_FIRST_BOND_NEXT_RETURN[short_US]
SHORT_STRATEGY_RETURN_GB <- results$GB_SECOND_WEIGHT[short_GB]*results$GB_SECOND_BOND_NEXT_OPEN_RETURN[short_GB]
print("Histogram return for short position both US and GB")
hist((SHORT_STRATEGY_RETURN_US+SHORT_STRATEGY_RETURN_GB))
print("Mean return for short position both US and GB")
mean((SHORT_STRATEGY_RETURN_US+SHORT_STRATEGY_RETURN_GB))
print("Histogram return for long position for US only")
hist(SHORT_STRATEGY_RETURN_US)
print("Mean return for long position for US only")
mean(SHORT_STRATEGY_RETURN_US)
print("Histogram return for long position for GB only")
hist(SHORT_STRATEGY_RETURN_GB)
print("Mean return for long position for GB only")
mean(SHORT_STRATEGY_RETURN_GB)

### Nice visualization of superposed densities
return_decomposition_df <- data.frame(returns=LONG_STRATEGY_RETURN_US,typo="LONG_US")
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=LONG_STRATEGY_RETURN_GB,typo="LONG_GB"))
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=SHORT_STRATEGY_RETURN_US,typo="SHORT_US"))
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=SHORT_STRATEGY_RETURN_GB,typo="SHORT_GB"))

my_title <-"Distribution of returns per trade type"
my_xaxis_title <- paste("Returns")
my_yaxis_title <- paste("Percentage","\n")

g <- ggplot(return_decomposition_df, aes(x=returns, fill=typo)) +geom_density(alpha=.3)+
  scale_y_continuous(labels = percent_format())+scale_x_continuous(limits=c(-0.1,0.1))+facet_wrap(~typo, ncol = 2)
g <- g +ylab(my_yaxis_title)+xlab(my_xaxis_title)+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),title =element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.85), legend.box = "vertical")+
  theme(legend.background = element_rect(fill="gray90"))+
  theme(legend.key.size = unit(1., "cm"))+
  theme(legend.text =  element_text(size=12,colour="black"))+
  theme(legend.title =  element_text(size=12,colour="black"))
print(g)

ExportPlot(g,outputDataPath,paste("LongShortDecomposition",my_pair[1],"_",my_pair[2],sep=""))
g <- ggplot(return_decomposition_df, aes(x=returns, fill=typo)) +geom_density(alpha=.3)+
  scale_y_continuous(labels = percent_format())+scale_x_continuous(limits=c(-0.1,0.1))
g <- g +ylab(my_yaxis_title)+xlab(my_xaxis_title)+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),title =element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.85), legend.box = "vertical")+
  theme(legend.background = element_rect(fill="gray90"))+
  theme(legend.key.size = unit(1., "cm"))+
  theme(legend.text =  element_text(size=12,colour="black"))+
  theme(legend.title =  element_text(size=12,colour="black"))
print(g)
ExportPlot(g,outputDataPath,paste("LongShortDecompositionTogether",my_pair[1],"_",my_pair[2],sep=""))

print("Outputing statistical results for our individual trading strategy US_GB")
WeightMatrix <- results[,c("DATES","GB_FIRST_WEIGHT","GB_SECOND_WEIGHT")]
colnames(WeightMatrix) <- c("DATE","GB_FIRST_WEIGHT","GB_SECOND_WEIGHT")
# ReturnSerie <- results[,c("DATES","STRATEGY_RETURN")]
# colnames(ReturnSerie) <- c("Date","STRATEGY_RETURN")
ReturnSerie <- results[,c("GB_STRATEGY_RETURN")]
turnover <- RP_GetTurnOver(WeightMatrix,-1)
RP_ReturnStats(ReturnSerie,21,TRUE,WeightMatrix,0.05,F)





toplot_df <-  melt(results[,c("DATES", "GB_STRATEGY_TODAY","GB_FIRST_BOND","GB_SECOND_BOND")],"DATES")

my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,my_result_spread_name)


toplot_df <-  melt(results[,c("DATES","GB_FIRST_WEIGHT","GB_SECOND_WEIGHT")],"DATES")

my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,paste(my_result_spread_name,"_weights",sep=""))

toplot_df <-  melt(results[,c("DATES","GB_FIRST_LEG","GB_SECOND_LEG")],"DATES")

my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,paste(my_result_spread_name,"_legs",sep=""))


### Japan visualization
my_result_spread_name <- "US_JP"
my_pair[1]<-"US"
my_pair[2]<-"JP"

# Long/short position returns distribution
long_US <- results$JP_FIRST_WEIGHT>=0
long_JP <- results$JP_SECOND_WEIGHT>=0
LONG_STRATEGY_RETURN_US <- results$JP_FIRST_WEIGHT[long_US]*results$JP_FIRST_BOND_NEXT_RETURN[long_US]
LONG_STRATEGY_RETURN_JP <- results$JP_SECOND_WEIGHT[long_JP]*results$JP_SECOND_BOND_NEXT_OPEN_RETURN[long_JP]

print("Histogram return for long position both US and JP")
hist((LONG_STRATEGY_RETURN_US+LONG_STRATEGY_RETURN_JP))
print("Mean return for long position both US and JP")
mean((LONG_STRATEGY_RETURN_US+LONG_STRATEGY_RETURN_JP))
print("Histogram return for long position for US only")
hist(LONG_STRATEGY_RETURN_US)
print("Mean return for long position for US only")
mean(LONG_STRATEGY_RETURN_US)
print("Histogram return for long position for JP only")
hist(LONG_STRATEGY_RETURN_JP)
print("Mean return for long position for JP only")
mean(LONG_STRATEGY_RETURN_JP)

# Short position returns distribution
short_US <- results$JP_FIRST_WEIGHT<=0
short_JP <- results$JP_SECOND_WEIGHT<=0
SHORT_STRATEGY_RETURN_US <- results$JP_FIRST_WEIGHT[short_US]*results$JP_FIRST_BOND_NEXT_RETURN[short_US]
SHORT_STRATEGY_RETURN_JP <- results$JP_SECOND_WEIGHT[short_JP]*results$JP_SECOND_BOND_NEXT_OPEN_RETURN[short_JP]
print("Histogram return for short position both US and JP")
hist((SHORT_STRATEGY_RETURN_US+SHORT_STRATEGY_RETURN_JP))
print("Mean return for short position both US and JP")
mean((SHORT_STRATEGY_RETURN_US+SHORT_STRATEGY_RETURN_JP))
print("Histogram return for long position for US only")
hist(SHORT_STRATEGY_RETURN_US)
print("Mean return for long position for US only")
mean(SHORT_STRATEGY_RETURN_US)
print("Histogram return for long position for JP only")
hist(SHORT_STRATEGY_RETURN_JP)
print("Mean return for long position for JP only")
mean(SHORT_STRATEGY_RETURN_JP)
### Nice visualization of superposed densities
return_decomposition_df <- data.frame(returns=LONG_STRATEGY_RETURN_US,typo="LONG_US")
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=LONG_STRATEGY_RETURN_JP,typo="LONG_JP"))
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=SHORT_STRATEGY_RETURN_US,typo="SHORT_US"))
return_decomposition_df <- rbind(return_decomposition_df, data.frame(returns=SHORT_STRATEGY_RETURN_JP,typo="SHORT_JP"))

my_title <-"Distribution of returns per trade type"
my_xaxis_title <- paste("Returns")
my_yaxis_title <- paste("Percentage","\n")

g <- ggplot(return_decomposition_df, aes(x=returns, fill=typo)) +geom_density(alpha=.3)+
  scale_y_continuous(labels = percent_format())+scale_x_continuous(limits=c(-0.1,0.1))+facet_wrap(~typo, ncol = 2)
g <- g +ylab(my_yaxis_title)+xlab(my_xaxis_title)+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),title =element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.85), legend.box = "vertical")+
  theme(legend.background = element_rect(fill="gray90"))+
  theme(legend.key.size = unit(1., "cm"))+
  theme(legend.text =  element_text(size=12,colour="black"))+
  theme(legend.title =  element_text(size=12,colour="black"))
print(g)

ExportPlot(g,outputDataPath,paste("LongShortDecomposition",my_pair[1],"_",my_pair[2],sep=""))
g <- ggplot(return_decomposition_df, aes(x=returns, fill=typo)) +geom_density(alpha=.3)+
  scale_y_continuous(labels = percent_format())+scale_x_continuous(limits=c(-0.1,0.1))
g <- g +ylab(my_yaxis_title)+xlab(my_xaxis_title)+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),title =element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.85), legend.box = "vertical")+
  theme(legend.background = element_rect(fill="gray90"))+
  theme(legend.key.size = unit(1., "cm"))+
  theme(legend.text =  element_text(size=12,colour="black"))+
  theme(legend.title =  element_text(size=12,colour="black"))
print(g)
ExportPlot(g,outputDataPath,paste("LongShortDecompositionTogether",my_pair[1],"_",my_pair[2],sep=""))

print("Outputing statistical results for our individual trading strategy US_JP")
WeightMatrix <- results[,c("DATES","JP_FIRST_WEIGHT","JP_SECOND_WEIGHT")]
colnames(WeightMatrix) <- c("DATE","JP_FIRST_WEIGHT","JP_SECOND_WEIGHT")
# ReturnSerie <- results[,c("DATES","STRATEGY_RETURN")]
# colnames(ReturnSerie) <- c("Date","STRATEGY_RETURN")
ReturnSerie <- results[,c("JP_STRATEGY_RETURN")]
turnover <- RP_GetTurnOver(WeightMatrix,-1)
RP_ReturnStats(ReturnSerie,21,TRUE,WeightMatrix,0.05,F)


toplot_df <-  melt(results[,c("DATES", "JP_STRATEGY_TODAY","JP_FIRST_BOND","JP_SECOND_BOND")],"DATES")

my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"_",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,my_result_spread_name)


toplot_df <-  melt(results[,c("DATES","JP_FIRST_WEIGHT","JP_SECOND_WEIGHT")],"DATES")

my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,paste(my_result_spread_name,"_weights",sep=""))

toplot_df <-  melt(results[,c("DATES","JP_FIRST_LEG","JP_SECOND_LEG")],"DATES")

my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,paste(my_result_spread_name,"_legs",sep=""))


### Combining each individual pair trading stragegy by equally weighting between each pairs

# results$STRATEGY_RETURN <- (results$JP_STRATEGY_RETURN+results$GB_STRATEGY_RETURN+results$DE_STRATEGY_RETURN)/3
# results$US_WEIGHT <- (results$DE_FIRST_WEIGHT + results$GB_FIRST_WEIGHT + results$JP_FIRST_WEIGHT)/3
# results$DE_WEIGHT <- results$DE_SECOND_WEIGHT/3
# results$JP_WEIGHT <- results$JP_SECOND_WEIGHT/3
# results$GB_WEIGHT <- results$GB_SECOND_WEIGHT/3


# print(results$DE_FIRST_WEIGHT+results$DE_SECOND_WEIGHT)
# results$JP_FIRST_WEIGHT
# print(results$JP_FIRST_WEIGHT+results$JP_SECOND_WEIGHT)
# results$GB_FIRST_WEIGHT
# print(results$GB_FIRST_WEIGHT+results$GB_SECOND_WEIGHT)
# 
# print(results$GB_FIRST_BOND_RETURN-results$DE_FIRST_BOND_RETURN)
# print(results$GB_FIRST_BOND_RETURN-results$DE_FIRST_BOND_RETURN)
# print(results$JP_FIRST_BOND_RETURN-results$DE_FIRST_BOND_RETURN)
# print(results$JP_FIRST_BOND_RETURN-results$GB_FIRST_BOND_RETURN)

weights_decomposition_df <- data.frame(weights=results$US_WEIGHT,typo="US")
weights_decomposition_df <- rbind(weights_decomposition_df, data.frame(weights=results$JP_WEIGHT,typo="JP"))
weights_decomposition_df <- rbind(weights_decomposition_df, data.frame(weights=results$GB_WEIGHT,typo="GB"))
weights_decomposition_df <- rbind(weights_decomposition_df, data.frame(weights=results$DE_WEIGHT,typo="DE"))
my_title <-"Distribution of weights per country"
my_xaxis_title <- paste("Weights")
my_yaxis_title <- paste("Percentage","\n")

g <- ggplot(weights_decomposition_df, aes(x=weights, fill=typo)) +geom_density(alpha=.3)+
  scale_y_continuous(labels = percent_format())+scale_x_continuous(limits=c(-2,2))+facet_wrap(~typo, ncol = 2)
g <- g +ylab(my_yaxis_title)+xlab(my_xaxis_title)+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),title =element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.85), legend.box = "vertical")+
  theme(legend.background = element_rect(fill="gray90"))+
  theme(legend.key.size = unit(1., "cm"))+
  theme(legend.text =  element_text(size=12,colour="black"))+
  theme(legend.title =  element_text(size=12,colour="black"))
print(g)

ExportPlot(g,outputDataPath,paste("WeightDecomposition","spread",sep=""))
g <- ggplot(weights_decomposition_df, aes(x=weights, fill=typo)) +geom_density(alpha=.3)+
  scale_y_continuous(labels = percent_format())+scale_x_continuous(limits=c(-2,2))
g <- g +ylab(my_yaxis_title)+xlab(my_xaxis_title)+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),title =element_text(size=18, face='bold'))+
  theme(legend.position=c(0.9,0.85), legend.box = "vertical")+
  theme(legend.background = element_rect(fill="gray90"))+
  theme(legend.key.size = unit(1., "cm"))+
  theme(legend.text =  element_text(size=12,colour="black"))+
  theme(legend.title =  element_text(size=12,colour="black"))
print(g)
ExportPlot(g,outputDataPath,paste("WeightDecomposition","",sep=""))

print("Outputing statistical results for our equally weighted strategy")
WeightMatrix <- results[,c("DATES","US_WEIGHT","DE_WEIGHT","JP_WEIGHT","GB_WEIGHT")]
colnames(WeightMatrix) <- c("DATE","US_WEIGHT","DE_WEIGHT","JP_WEIGHT","GB_WEIGHT")
ReturnSerie <- results[,c("STRATEGY_RETURN")]
turnover <- RP_GetTurnOver(WeightMatrix,-1)
RP_ReturnStats(ReturnSerie,21,TRUE,WeightMatrix,0.05,F)


toplot_df <-  melt(results[,c("DATES", "STRATEGY_TODAY","JP_FIRST_BOND","JP_SECOND_BOND","DE_SECOND_BOND","GB_SECOND_BOND")],"DATES")
my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,my_result_spread_name)


toplot_df <-  melt(results[,c("DATES", "STRATEGY_TODAY","EQ_WEIGHTS_BOND")],"DATES")
my_title <- paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,paste(my_result_spread_name,"_bench",sep=""))

toplot_df <-  melt(results[,c("DATES","US_WEIGHT","DE_WEIGHT","JP_WEIGHT","GB_WEIGHT")],"DATES")

my_title <-paste("Macro Sentiment ML over Ravenpack ESS metrics and taxonomy ",my_pair[1],"/",my_pair[2],sep="")
g<-ggplot(
  toplot_df,aes(
    x = DATES,y = value,group = variable,color = variable
  )
) +
  geom_line() +
  scale_x_date() +
  ggtitle(my_title) + xlab("Time") + ylab("Cumulated sentiment") +
  theme(title = element_text(size = 12, face = 'bold')) +
  theme(legend.position = c(0.2,0.8), legend.box = "vertical") +
  theme(legend.background = element_rect(fill = "gray90")) +
  theme(legend.key.size = unit(0.7, "cm"))
print(g)

ExportPlot(g,outputDataPathStrategyMonth,paste(my_result_spread_name,"_weights",sep=""))



### Unidirectionnality predictionnability metrics
prediction_results <- tryCatch(  readRDS(paste(outputDataPathStrategyMonth,"all_pairs_prediction_results_month_2007.rds", sep = "")),
                                 error = function(e) {NULL})
if (!is.null(prediction_results)){
  
  # correlation between prediction and returns for US
  print("Correlation US unidirection prediction")
  print(cor(prediction_results$USPrediction,prediction_results$USEffective))
  
  for (my_quantile in c(0.1,1/5,1/3)){
    US_df <- prediction_results[,c("USPrediction","USEffective")]
    US_df$my_tiles <- with(US_df, cut(USPrediction, 
                                      breaks=quantile(USPrediction, probs=seq(0,1, by=my_quantile)), 
                                      include.lowest=TRUE))
    
    my_IR_computation <- function(x){
      IR <- mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)*sqrt(12)
      return(IR) 
    }
    
    agg_US_df <- aggregate(US_df$USEffective, by=list(US_df$my_tiles),FUN=my_IR_computation)
    colnames(agg_US_df) <- c("Pred_quantile","Mean_Ret")
    ## Histogram plotting of the return average per prediction quantile
    g <- ggplot(agg_US_df, aes(x=Pred_quantile,y=Mean_Ret)) +
      geom_histogram(stat = "identity")
    print(g)
  }
  
  
  Predictionbullish <- prediction_results$USPrediction >= 0 
  NextReturnbullish <- prediction_results$USEffective >= 0 
  
  Predictionbearish <- prediction_results$USPrediction <= 0 
  NextReturnbearish <- prediction_results$USEffective <= 0 
  
  print("Long confusion matrix")
  print(confusionMatrix(Predictionbullish, NextReturnbullish))
  
  print("Short confusion matrix")
  print(confusionMatrix(Predictionbearish, NextReturnbearish))
  
  print("Correlation GB unidirection prediction")
  print(cor(prediction_results$GBPrediction,prediction_results$GBEffective))
  for (my_quantile in c(0.1,1/5,1/3)){
    df <- prediction_results[,c("GBPrediction","GBEffective")]
    df$my_tiles <- with(df, cut(GBPrediction, 
                                breaks=quantile(GBPrediction, probs=seq(0,1, by=my_quantile)), 
                                include.lowest=TRUE))
    agg_df <- aggregate(df$GBEffective, by=list(df$my_tiles),FUN=my_IR_computation)
    colnames(agg_df) <- c("Pred_quantile","Mean_Ret")
    ## Histogram plotting of the return average per prediction quantile
    g <- ggplot(agg_df, aes(x=Pred_quantile,y=Mean_Ret)) +
      geom_histogram(stat = "identity")
    print(g)
  }
  Predictionbullish <- prediction_results$GBPrediction >= 0 
  NextReturnbullish <- prediction_results$GBEffective >= 0 
  
  Predictionbearish <- prediction_results$GBPrediction <= 0 
  NextReturnbearish <- prediction_results$GBEffective <= 0 
  
  print("Long confusion matrix")
  print(confusionMatrix(Predictionbullish, NextReturnbullish))
  
  print("Short confusion matrix")
  print(confusionMatrix(Predictionbearish, NextReturnbearish))
  
  print("Correlation DE unidirection prediction")
  print(cor(prediction_results$DEPrediction,prediction_results$DEEffective))
  for (my_quantile in c(0.1,1/5,1/3)){
    df <- prediction_results[,c("DEPrediction","DEEffective")]
    df$my_tiles <- with(df, cut(DEPrediction, 
                                breaks=quantile(DEPrediction, probs=seq(0,1, by=my_quantile)), 
                                include.lowest=TRUE))
    agg_df <- aggregate(df$DEEffective, by=list(df$my_tiles),FUN=my_IR_computation)
    colnames(agg_df) <- c("Pred_quantile","Mean_Ret")
    ## Histogram plotting of the return average per prediction quantile
    g <- ggplot(agg_df, aes(x=Pred_quantile,y=Mean_Ret)) +
      geom_histogram(stat = "identity")
    print(g)
  }
  Predictionbullish <- prediction_results$DEPrediction >= 0 
  NextReturnbullish <- prediction_results$DEEffective >= 0 
  
  Predictionbearish <- prediction_results$DEPrediction <= 0 
  NextReturnbearish <- prediction_results$DEEffective <= 0 
  
  print("Long confusion matrix")
  print(confusionMatrix(Predictionbullish, NextReturnbullish))
  
  print("Short confusion matrix")
  print(confusionMatrix(Predictionbearish, NextReturnbearish))
  
  print("Correlation GB unidirection prediction")
  print(cor(prediction_results$GBPrediction,prediction_results$GBEffective))
  
  Predictionbullish <- prediction_results$GBPrediction >= 0 
  NextReturnbullish <- prediction_results$GBEffective >= 0 
  
  Predictionbearish <- prediction_results$GBPrediction <= 0 
  NextReturnbearish <- prediction_results$GBEffective <= 0 
  
  print("Long confusion matrix")
  print(confusionMatrix(Predictionbullish, NextReturnbullish))
  
  print("Short confusion matrix")
  print(confusionMatrix(Predictionbearish, NextReturnbearish))
  
  print("Correlation JP unidirection prediction")
  print(cor(prediction_results$JPPrediction,prediction_results$JPEffective))
  for (my_quantile in c(0.1,1/5,1/3)){
    df <- prediction_results[,c("JPPrediction","JPEffective")]
    df$my_tiles <- with(df, cut(JPPrediction, 
                                breaks=quantile(JPPrediction, probs=seq(0,1, by=my_quantile)), 
                                include.lowest=TRUE))
    agg_df <- aggregate(df$JPEffective, by=list(df$my_tiles),FUN=my_IR_computation)
    colnames(agg_df) <- c("Pred_quantile","Mean_Ret")
    ## Histogram plotting of the return average per prediction quantile
    g <- ggplot(agg_df, aes(x=Pred_quantile,y=Mean_Ret)) +
      geom_histogram(stat = "identity")
    print(g)
  }
  
  Predictionbullish <- prediction_results$JPPrediction >= 0 
  NextReturnbullish <- prediction_results$JPEffective >= 0 
  
  Predictionbearish <- prediction_results$JPPrediction <= 0 
  NextReturnbearish <- prediction_results$JPEffective <= 0 
  
  print("Long confusion matrix")
  print(confusionMatrix(Predictionbullish, NextReturnbullish))
  
  print("Short confusion matrix")
  print(confusionMatrix(Predictionbearish, NextReturnbearish))
  
}

#### All heat map variable importance viewing
#### results
my_pairs = list(c("US","DE"),c("US","GB"),c("US","JP"))

my_threshold <- 100
for (my_pair in my_pairs){
  
  # SaveDataFrame(my_total_df,outputDataPathMonth,paste(my_pair[1], my_pair[2],"variable_importance_spread_results_month_2007",sep=""))
  
  filename <- paste(my_pair[1], my_pair[2],"variable_importance_spread_results_month_2007.rds",sep="")
  
  my_total_df <- tryCatch( readRDS(paste(outputDataPathStrategyMonth,filename, sep = "")),
                           error = function(e) {NULL})
  if (!is.null(my_total_df)){
    
    
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
    ExportPlot(p,outputDataPathStrategyMonth,paste0(my_pair[2],"heatmap"), width=10, height=15)
    print("Done")
  }
}
