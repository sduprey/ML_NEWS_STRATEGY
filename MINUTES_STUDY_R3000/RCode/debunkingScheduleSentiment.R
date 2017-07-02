
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

##############
###########################################################################################
###########################################################################################
########################################################################################### new plotting functions

RP_PlotProfileTogetherAllSchedule <- function(rowProfilePOSITIVETRUE,rowProfilePOSITIVEFALSE, rowProfileNEGATIVETRUE,rowProfileNEGATIVEFALSE){
  
  ###########
  ###########
  ###########
  ########### First df rowProfilePOSITIVETRUE
  
  stats_sign <- colnames(rowProfilePOSITIVETRUE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVETRUE))))]
  rets <- paste0("RET",colnames(rowProfilePOSITIVETRUE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVETRUE))))])
  ord_stats_sign <- paste0("ORD",colnames(rowProfilePOSITIVETRUE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVETRUE))))])
  vol_stats_sign <- paste0("VOLU",colnames(rowProfilePOSITIVETRUE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVETRUE))))])
  vola_stats_sign <- paste0("VOLA",colnames(rowProfilePOSITIVETRUE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVETRUE))))])
  
  stats_sign <- rowProfilePOSITIVETRUE[,stats_sign]
  rets <- rowProfilePOSITIVETRUE[,rets]
  colnames(rets) <- colnames(stats_sign)
  ord_stats_sign <- rowProfilePOSITIVETRUE[,ord_stats_sign]
  colnames(ord_stats_sign) <- colnames(stats_sign)
  vol_stats_sign <- rowProfilePOSITIVETRUE[,vol_stats_sign]
  colnames(vol_stats_sign) <- colnames(stats_sign)
  vola_stats_sign <- rowProfilePOSITIVETRUE[,vola_stats_sign]
  colnames(vola_stats_sign) <- colnames(stats_sign)
  
  
  
  # dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  dataframePOSITIVETRUE <- as.data.frame(t(rbind(vol_stats_sign,vola_stats_sign,rets)))
  
  colnames(dataframePOSITIVETRUE) <- c("VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframePOSITIVETRUE$MINUTES <- as.numeric(colnames(rowProfilePOSITIVETRUE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVETRUE))))])
  dataframePOSITIVETRUE <- dataframePOSITIVETRUE[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  ###########
  ###########
  ###########
  ########### First df rowProfilePOSITIVEFALSE
  
  
  stats_sign <- colnames(rowProfilePOSITIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVEFALSE))))]
  rets <- paste0("RET",colnames(rowProfilePOSITIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVEFALSE))))])
  ord_stats_sign <- paste0("ORD",colnames(rowProfilePOSITIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVEFALSE))))])
  vol_stats_sign <- paste0("VOLU",colnames(rowProfilePOSITIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVEFALSE))))])
  vola_stats_sign <- paste0("VOLA",colnames(rowProfilePOSITIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVEFALSE))))])
  
  stats_sign <- rowProfilePOSITIVEFALSE[,stats_sign]
  rets <- rowProfilePOSITIVEFALSE[,rets]
  colnames(rets) <- colnames(stats_sign)
  ord_stats_sign <- rowProfilePOSITIVEFALSE[,ord_stats_sign]
  colnames(ord_stats_sign) <- colnames(stats_sign)
  vol_stats_sign <- rowProfilePOSITIVEFALSE[,vol_stats_sign]
  colnames(vol_stats_sign) <- colnames(stats_sign)
  vola_stats_sign <- rowProfilePOSITIVEFALSE[,vola_stats_sign]
  colnames(vola_stats_sign) <- colnames(stats_sign)
  
  
  
  # dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  dataframePOSITIVEFALSE <- as.data.frame(t(rbind(vol_stats_sign,vola_stats_sign,rets)))
  
  colnames(dataframePOSITIVEFALSE) <- c("VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframePOSITIVEFALSE$MINUTES <- as.numeric(colnames(rowProfilePOSITIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfilePOSITIVEFALSE))))])
  dataframePOSITIVEFALSE <- dataframePOSITIVEFALSE[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  
  ###########
  ###########
  ###########
  ########### First df rowProfileNEGATIVETRUE
  
  stats_sign <- colnames(rowProfileNEGATIVETRUE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVETRUE))))]
  rets <- paste0("RET",colnames(rowProfileNEGATIVETRUE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVETRUE))))])
  ord_stats_sign <- paste0("ORD",colnames(rowProfileNEGATIVETRUE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVETRUE))))])
  vol_stats_sign <- paste0("VOLU",colnames(rowProfileNEGATIVETRUE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVETRUE))))])
  vola_stats_sign <- paste0("VOLA",colnames(rowProfileNEGATIVETRUE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVETRUE))))])
  
  stats_sign <- rowProfileNEGATIVETRUE[,stats_sign]
  rets <- rowProfileNEGATIVETRUE[,rets]
  colnames(rets) <- colnames(stats_sign)
  ord_stats_sign <- rowProfileNEGATIVETRUE[,ord_stats_sign]
  colnames(ord_stats_sign) <- colnames(stats_sign)
  vol_stats_sign <- rowProfileNEGATIVETRUE[,vol_stats_sign]
  colnames(vol_stats_sign) <- colnames(stats_sign)
  vola_stats_sign <- rowProfileNEGATIVETRUE[,vola_stats_sign]
  colnames(vola_stats_sign) <- colnames(stats_sign)
  
  
  
  # dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  dataframeNEGATIVETRUE <- as.data.frame(t(rbind(vol_stats_sign,vola_stats_sign,rets)))
  
  colnames(dataframeNEGATIVETRUE) <- c("VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframeNEGATIVETRUE$MINUTES <- as.numeric(colnames(rowProfileNEGATIVETRUE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVETRUE))))])
  dataframeNEGATIVETRUE <- dataframeNEGATIVETRUE[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  ###########
  ###########
  ###########
  ########### First df rowProfileNEGATIVEFALSE
  
  
  stats_sign <- colnames(rowProfileNEGATIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVEFALSE))))]
  rets <- paste0("RET",colnames(rowProfileNEGATIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVEFALSE))))])
  ord_stats_sign <- paste0("ORD",colnames(rowProfileNEGATIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVEFALSE))))])
  vol_stats_sign <- paste0("VOLU",colnames(rowProfileNEGATIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVEFALSE))))])
  vola_stats_sign <- paste0("VOLA",colnames(rowProfileNEGATIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVEFALSE))))])
  
  stats_sign <- rowProfileNEGATIVEFALSE[,stats_sign]
  rets <- rowProfileNEGATIVEFALSE[,rets]
  colnames(rets) <- colnames(stats_sign)
  ord_stats_sign <- rowProfileNEGATIVEFALSE[,ord_stats_sign]
  colnames(ord_stats_sign) <- colnames(stats_sign)
  vol_stats_sign <- rowProfileNEGATIVEFALSE[,vol_stats_sign]
  colnames(vol_stats_sign) <- colnames(stats_sign)
  vola_stats_sign <- rowProfileNEGATIVEFALSE[,vola_stats_sign]
  colnames(vola_stats_sign) <- colnames(stats_sign)
  
  
  
  # dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  dataframeNEGATIVEFALSE <- as.data.frame(t(rbind(vol_stats_sign,vola_stats_sign,rets)))
  
  colnames(dataframeNEGATIVEFALSE) <- c("VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")
  dataframeNEGATIVEFALSE$MINUTES <- as.numeric(colnames(rowProfileNEGATIVEFALSE)[which(!is.na(as.numeric(colnames(rowProfileNEGATIVEFALSE))))])
  dataframeNEGATIVEFALSE <- dataframeNEGATIVEFALSE[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
  
  
  ###########
  ###########
  ###########
  ########### global plotting altogether
  
  gret <- outputGraphicsBestProfileCITogetherSchedule(dataframePOSITIVETRUE,dataframePOSITIVEFALSE,dataframeNEGATIVETRUE,dataframeNEGATIVEFALSE,Russell_version = "R1000", type = "RET")
  #   gvol <- outputGraphicsBestProfileCITogether(dataframeNeg,dataframePos,Russell_version = "R1000", type = "VOL")
  #   gvolu <- outputGraphicsBestProfileCITogether(dataframeNeg,dataframePos,Russell_version = "R1000", type = "VOLU")
  
  return(list(gret=gret))
}

outputGraphicsBestProfileCITogetherSchedule <- function(dataframePOSITIVETRUE,dataframePOSITIVEFALSE,dataframeNEGATIVETRUE,dataframeNEGATIVEFALSE,Russell_version = "R1000", type = "RET"){
  toplotDF <- NULL
  MyTitle <- ""
  #   if (type == "VOL"){
  #     toplotDFPos<- dataPos[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
  #     colnames(toplotDFPos) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
  #     toplotDFNeg<- dataNeg[,c("MINUTES","VOLATILITY_CI_LOW","VOLATILITY","VOLATILITY_CI_HIGH")]
  #     colnames(toplotDFNeg) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
  #     MyTitle <- "Abnormal volatility"
  #   }
  #   
  
  if(type !="RET"){
    print("unimplemented yet")
  }
  
  if(type =="RET"){
    toplotDFPOSITIVETRUE<- dataframePOSITIVETRUE[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFPOSITIVETRUE) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFPOSITIVEFALSE <- dataframePOSITIVEFALSE[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFPOSITIVEFALSE) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFNEGATIVETRUE <- dataframeNEGATIVETRUE[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFNEGATIVETRUE) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    toplotDFNEGATIVEFALSE <- dataframeNEGATIVEFALSE[,c("MINUTES","RETS_CI_LOW","RETS","RETS_CI_HIGH")]
    colnames(toplotDFNEGATIVEFALSE) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
    
    MyTitle <- "Abnormal returns"
  }
  
  #   if(type =="VOLU"){
  #     toplotDFPos<- dataPos[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
  #     colnames(toplotDFPos) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
  #     
  #     toplotDFNeg<- dataNeg[,c("MINUTES","VOLUME_CI_LOW","VOLUME","VOLUME_CI_HIGH")]
  #     colnames(toplotDFNeg) <- c("MINUTES","CI_LOW","MEAN","CI_HIGH")
  #     
  #     MyTitle <- "Abnormal volume"
  #   }
  
  g2 <- RP_PlotCIIntervalSuperposedSchedule(DataFramePosTrue =  toplotDFPOSITIVETRUE, DataFramePosFalse =  toplotDFPOSITIVEFALSE, DataFrameNegTrue =  toplotDFNEGATIVETRUE,DataFrameNegFalse =  toplotDFNEGATIVEFALSE, Title = MyTitle, FullExportingPath =  NULL)
  return(g2)
}



RP_PlotCIIntervalSuperposedSchedule <- function(DataFramePosTrue, DataFramePosFalse, DataFrameNegTrue,DataFrameNegFalse, Title = "", FullExportingPath=NULL) {
  
  print("superposing") 
  
  colnames(DataFramePosTrue) <- paste0("POSTRUE_",colnames(DataFramePosTrue))
  colnames(DataFramePosTrue)[1] <- "MINUTES"
  
  colnames(DataFramePosFalse) <- paste0("POSFALSE_",colnames(DataFramePosFalse))
  colnames(DataFramePosFalse)[1] <- "MINUTES"
  
  colnames(DataFrameNegTrue) <- paste0("NEGTRUE_",colnames(DataFrameNegTrue))
  colnames(DataFrameNegTrue)[1] <- "MINUTES"
  
  colnames(DataFrameNegFalse) <- paste0("NEGFALSE_",colnames(DataFrameNegFalse))
  colnames(DataFrameNegFalse)[1] <- "MINUTES"
  
  DataFrame <- merge(DataFramePosTrue, DataFramePosFalse, by="MINUTES")
  DataFrame <- merge(DataFrameNegTrue, DataFrame, by="MINUTES")
  DataFrame <- merge(DataFrameNegFalse, DataFrame, by="MINUTES")
  
#   mav <- function(x){stats::filter(x,rep(1/3,3), sides=2)}
#   
#   DataFrame$POS_CI_LOW <- 3/4*( DataFrame$POS_CI_LOW) + 1/4*(mav(DataFrame$POS_CI_LOW))
#   DataFrame$POS_CI_HIGH <- 3/4*(DataFrame$POS_CI_HIGH) + 1/4*mav(DataFrame$POS_CI_HIGH)
#   DataFrame$POS_MEAN <- 3/4*(DataFrame$POS_MEAN) + 1/4*mav(DataFrame$POS_MEAN)
#   DataFrame$NEG_MEAN <- 3/4*(DataFrame$NEG_MEAN) + 1/4*mav(DataFrame$NEG_MEAN)
#   DataFrame$NEG_CI_LOW <-  3/4*(DataFrame$NEG_CI_LOW)+ 1/4*mav(DataFrame$NEG_CI_LOW)
#   DataFrame$NEG_CI_HIGH <- 3/4*(DataFrame$NEG_CI_HIGH)+  1/4*mav(DataFrame$NEG_CI_HIGH)
#   
#   DataFrame <- DataFrame[complete.cases(DataFrame),]
  DataFrame <- DataFrame[abs(DataFrame$MINUTES) <= 90,]
  DataFrame <- DataFrame[DataFrame$MINUTES >= -50,]
#   
#   #   DataFrame <- DataFrame[abs(DataFrame$MINUTES) <= 100,]
#   #   
#   #   DataFrameDown <- DataFrame[DataFrame$MINUTES <=  0,]
#   #   DataFrameUp <- DataFrame[DataFrame$MINUTES >  0,]
#   #   DataFrameUp$MINUTES <- 2* DataFrameUp$MINUTES
#   #   
#   #   DataFrame <- DataFrame[abs(DataFrame$MINUTES) <= 180,]
#   #   
#   #   DataFrame <- rbind(DataFrameDown,DataFrameUp)
  
  
  g <- ggplot(DataFrame)+
    geom_line(data=DataFrame, aes(x=MINUTES, y=POSTRUE_MEAN), size=1.5,alpha=1,show_guide = T, colour="#619CFF",linetype="solid")+
    geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=POSTRUE_CI_LOW,ymax=POSTRUE_CI_HIGH),show_guide = F,alpha=0.25,colour="#619CFF",fill="#619CFF")+
    geom_line(data=DataFrame, aes(x=MINUTES, y=POSFALSE_MEAN), size=1.5,alpha=1,show_guide = T, colour="#00BA38",linetype="solid")+
    geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=POSFALSE_CI_LOW,ymax=POSFALSE_CI_HIGH),show_guide = F,alpha=0.25,colour="#00BA38",fill="#00BA38")+
    geom_line(data=DataFrame, aes(x=MINUTES, y=NEGTRUE_MEAN), size=1.5,alpha=1,show_guide = T, colour="#619CFF",linetype="dotted")+
    geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=NEGTRUE_CI_LOW,ymax=NEGTRUE_CI_HIGH),show_guide = F,alpha=0.25,colour="#619CFF",fill="#619CFF")+
    geom_line(data=DataFrame, aes(x=MINUTES, y=NEGFALSE_MEAN), size=1.5,alpha=1,show_guide = T, colour="#00BA38",linetype="dotted")+
    geom_ribbon(data=DataFrame,aes(x=MINUTES,ymin=NEGFALSE_CI_LOW,ymax=NEGFALSE_CI_HIGH),show_guide = F,alpha=0.25,colour="#00BA38",fill="#00BA38")+
    
    xlab("Minute Lags") + 
    ylab("BPS cumulated minute return") +
    ggtitle(Title)+
    theme(title = element_text(size = 28, face = "bold")) + 
    theme(axis.text.x = element_text(size = 24)) + 
    theme(axis.title.x = element_text(size = 24)) +
    theme(axis.text.y = element_text(size = 24)) + 
    theme(axis.title.y = element_text(size = 24)) +
    theme(legend.position = c(0.9, 0.9), legend.box = "vertical", 
          legend.text = element_text(size = 22)) + theme(legend.position = "bottom", 
                                                         legend.title = element_blank())
  
  g <- g + geom_vline(aes(xintercept=0),colour = 'black', size = 1.5,linetype="dashed")
  if (!is.null(FullExportingPath)){
    RP_ExportPlot(g,FullExportingPath,"")
  }
  return(g)
}

###########################################################################################
###########################################################################################
###########################################################################################

# save(sch_results, file = paste0(outputDataPath, "dualSentSchedule.RData"))
load(file = paste0(outputDataPath, "dualSentSchedule.RData"))


schedulity<-"POSITIVETRUE"
rowProfilePOSITIVETRUE <- whole_results[schedulity][1]$`POSITIVETRUE`
schedulity <- "POSITIVEFALSE" 
rowProfilePOSITIVEFALSE <- whole_results[schedulity][1]$`POSITIVEFALSE`
# results <- RP_PlotProfileTogether(rowProfilePOSITIVETRUE,rowProfilePOSITIVEFALSE)
# RP_ExportPlot(results$gret,outputDataPath = outputDataPath,filename = paste0("PAPER_PICTURES/SCHEDULED_SPLIT/",my_metric,"split_sent_bigdata_returns_category_average_profile_ci"))
# print(results$gret)



schedulity<-"NEGATIVETRUE"
rowProfileNEGATIVETRUE <- whole_results[schedulity][1]$`NEGATIVETRUE`
schedulity <- "NEGATIVEFALSE" 
rowProfileNEGATIVEFALSE <- whole_results[schedulity][1]$`NEGATIVEFALSE`
# results <- RP_PlotProfileTogether(rowProfileNEGATIVETRUE,rowProfileNEGATIVEFALSE)
# RP_ExportPlot(results$gret,outputDataPath = outputDataPath,filename = paste0("PAPER_PICTURES/SCHEDULED_SPLIT/",my_metric,"split_sent_bigdata_returns_category_average_profile_ci"))
# print(results$gret)




results <- RP_PlotProfileTogetherAllSchedule(rowProfilePOSITIVETRUE,rowProfilePOSITIVEFALSE, rowProfileNEGATIVETRUE,rowProfileNEGATIVEFALSE)
print(results$gret)
RP_ExportPlot(results$gret,outputDataPath = outputDataPath,filename = paste0("PAPER_PICTURES/SCHEDULED_SPLIT/all_together_split_sent_bigdata_returns_category_average_profile_ci"))





