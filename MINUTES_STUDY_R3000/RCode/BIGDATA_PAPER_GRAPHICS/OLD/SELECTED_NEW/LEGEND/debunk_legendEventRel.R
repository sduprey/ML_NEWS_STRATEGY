#####################
##################### outputing paper graphics
library("RPToolsDB")
library("shiny")
library("DT")
library("RPPlotUtils")
library("dplyr")
library("formattable")
library("htmltools")
library("webshot")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")


source("./RCode/RP_BigData_EventStudy_Utilities.R")


agg_rowProfileCtrash <- readRDS("ribbon_legend.rds")

RP_PlotMetricsTogetherWithCITest <- function(agg_rowProfileCtrash,my_metrics,aggregate_criteria, type){
  
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
  
  
  g <- RP_PlotCIIntervalSuperposedTest(DataFrameOne =  dataframe_one,DataFrameTwo =  dataframe_two, DataFrameThree = dataframe_three,Title = MyTitle, my_metrics, FullExportingPath =  NULL,type)
  
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
    # scale_colour_manual(name="Legend", values = c("a" = "black", "b" = "red", "c" = "blue","a" = "black", "b" = "red", "c" = "blue","a" = "black", "b" = "red", "c" = "blue")) +
    # scale_linetype_manual(name="Legend", values = c("a" = "dashed", "b" = "dotted", "c" = "dotted")) +
    
    # scale_colour_manual(name="Legend", values = c("FREQ_MEAN" = "blue", "RET_MEAN" = "green", "VOL_MEAN" = "red")) +
    # scale_linetype_manual(name="Legend", values = c("FREQ_MEAN" = "dashed", "RET_MEAN" = "dotted", "VOL_MEAN" = "dotted")) +
    # adjust the colours to those you wanted
    # scale_colour_manual(values = c("black","red", "blue"))+
    # stick the legend on the bottom
    # guides(color=guide_legend("my title"))+
    theme( legend.position = "bottom")+
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold")) +
    theme(plot.title = element_text(size = 25, face = "bold"))
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
  DataFrame <- DataFrame[,c("MINUTES","RET_MEAN","FREQ_MEAN","VOL_MEAN")]
  colnames(DataFrame) <- c("MINUTES","FREQUENCY","RETURN","VOLATILITY")
  
  #   DataFrame <- DataFrame[,c("MINUTES","FREQUENCY","RETURN","VOLATILITY")]
  #   DataFrame$VVOLATILITY <- DataFrame$VOLATILITY+1
  #   colnames(DataFrame) <- c("MINUTES","+ SCHEDULED","+ UNSCHEDULED","- SCHEDULED","- UNSCHEDULED")
  
  DataFrame <- DataFrame[,c("MINUTES","FREQUENCY","RETURN","VOLATILITY")]
  # DataFrame$VVOLATILITY <- DataFrame$VOLATILITY+1
  colnames(DataFrame) <- c("MINUTES","Low","Medium","High")
  
  
  DataFrame <- melt(DataFrame,"MINUTES")
  # g <- ggplot(DataFrame, aes(x=MINUTES, group=variable ,y=value, linetype= variable, colour = variable, fill = variable))+
  g <- ggplot(DataFrame, aes(x=MINUTES, group=variable ,y=value, colour = variable, fill = variable))+
    
    geom_line(data=DataFrame,size=1.5,alpha=1)+#,show_guide = T, colour="#619CFF")+
    # geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=FREQ_CI_LOW,ymax=FREQ_CI_HIGH),show_guide = F,alpha=0.25,colour="#619CFF",fill="#619CFF")+#, fill="steelblue1", color="steelblue1")+
    # geom_line(data=DataFrame, aes(x=MINUTES, y=RET_MEAN), size=1.5,alpha=1,show_guide = T, colour="#00BA38")+
    # geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=RET_CI_LOW,ymax=RET_CI_HIGH),show_guide = F,alpha=0.25,colour="#00BA38",fill="#00BA38")+#, fill="steelblue2", color="steelblue3")+
    # geom_line(data=DataFrame, aes(x=MINUTES, y=VOL_MEAN), size=1.5,alpha=1,show_guide = T, colour="#F8766D")+
    # geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=VOL_CI_LOW,ymax=VOL_CI_HIGH),show_guide = F,alpha=0.25,colour="#F8766D",fill="#F8766D")+#, fill="steelblue3", color="steelblue3")+    xlab("Minute Lags") + 
    scale_color_manual(values=c("#F8766D","#619CFF","#00BA38")) +  
    # scale_linetype_manual(values=c("solid","solid","dotted","dotted"))+ 
    # scale_color_manual(values=c("#619CFF","#00BA38")) +  
    
    ylab(MyYLabel) +
    ggtitle(MyTitle)+
    theme(title = element_text(size = 28, face = "bold")) + 
    theme(axis.text.x = element_text(size = 30)) + 
    theme(axis.title.x = element_text(size = 30)) +
    theme(axis.text.y = element_text(size = 30)) + 
    theme(axis.title.y = element_text(size = 30)) +
    guides(fill = guide_legend(legend.text = element_text(size = 30)) + theme(legend.position =  c(0.2, 0.8), legend.title = element_blank()))+
    theme(legend.text = element_text(size = 30)) + theme(legend.position =  c(0.2, 0.8),legend.key.size = unit(2., 'lines'), legend.title = element_blank())
  # scale_fill_continuous(guide = guide_legend(title = "V")) 
  #     theme(legend.text = element_text(size = 22)) + theme(legend.position = "bottom", 
  #                                                        legend.title = element_blank())
  
  #     theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
  #           legend.text = element_text(size = 22)) +
  #     theme(legend.position = "bottom")+
  #     theme(axis.text=element_text(size=16),
  #           axis.title=element_text(size=18,face="bold")) +
  #     theme(plot.title = element_text(size = 25, face = "bold"))
  # g <- g + geom_vline(aes(xintercept=0),colour = 'black', size = 1.5,linetype="dashed")
  # g <- g + scale_colour_brewer(palette = "Greens")
  if (!is.null(FullExportingPath)){
    RP_ExportPlot(g,FullExportingPath,"")
  }
  return(g)
}

#   print(g)
# dashed = TRUE
# gret <- RP_PlotMetricsTogetherWithCI(agg_rowProfileCtrash,my_metrics ,"CATEGORY",type="RET")
# gvol <- RP_PlotMetricsTogetherWithCI(agg_rowProfileCtrash,my_metrics ,"CATEGORY",type="VOL")
# print(gret)
# print(gvol)

dashed = FALSE
gret <- RP_PlotMetricsTogetherWithCI(agg_rowProfileCtrash,my_metrics ,"CATEGORY",type="RET")
gvol <- RP_PlotMetricsTogetherWithCI(agg_rowProfileCtrash,my_metrics ,"CATEGORY",type="VOL")
print(gret)
print(gvol)
RP_ExportPlot(gret,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/LEGEND/"), filename = paste0("LLEGEMEV"))

# RP_ExportPlot(gvol,outputDataPath =paste0(outputDataPath,"PAPER_PICTURES/LEGEND/"), filename = paste0("LLEGEMDSCHc"))





