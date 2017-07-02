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

bigdata_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"spr_r1000_bigdata_abvol_abvol_corrado_df.rds"))
bigdata_best_profile_group_ordered_r1000_corrado_df <- readRDS(file=paste0(outputDataPath,"bigdata_best_profile_group_ordered_r1000_corrado_df.rds"))
bigdata_best_profile_category_ordered_r1000_corrado_df <- readRDS(file=paste0(outputDataPath,"bigdata_best_profile_category_ordered_r1000_corrado_df.rds"))

# dataTotr2000 <- readRDS(file=paste0(outputDataPath,"spr_r2000_bigdata_abvol_abvol_corrado_df.rds"))
# best_profile_group_ordered_r2000_corrado_df <- readRDS(file=paste0(outputDataPath,"bigdata_best_profile_group_ordered_r2000_corrado_df.rds"))
# best_profile_category_ordered_r2000_corrado_df <- readRDS(file=paste0(outputDataPath,"bigdata_best_profile_category_ordered_r2000_corrado_df.rds"))
bigdata_dataTotr2000 <- bigdata_dataTotr1000
bigdata_best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
bigdata_best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df


bigdata_all_group_events <- readRDS(file=paste0(outputDataPath,"bigdata_all_group_events.rds"))
bigdata_all_category_events <- readRDS(file=paste0(outputDataPath,"bigdata_all_category_events.rds"))


bigdata_dataTotr1000$correcting_factor <- 2*(bigdata_dataTotr1000$sentiment_criteria == "POSITIVE"  | bigdata_dataTotr1000$sentiment_criteria == "SPREAD" )-1
bigdata_dataTotr1000$correcting_factor[bigdata_dataTotr1000$sentiment_criteria == "ALL"] <- 0
stats_post_sign <- colnames(bigdata_dataTotr1000)[which(as.numeric(colnames(bigdata_dataTotr1000)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)
bigdata_dataTotr1000$infinity_return <- rowSums(bigdata_dataTotr1000[,rets_post]*bigdata_dataTotr1000$correcting_factor*bigdata_dataTotr1000[,stats_post_sign],na.rm = TRUE)
bigdata_dataTotr1000 <- bigdata_dataTotr1000[bigdata_dataTotr1000$event_number_event_filtering >= 50,]

bigdata_dataTotr2000$correcting_factor <- 2*(bigdata_dataTotr2000$sentiment_criteria == "POSITIVE"  | bigdata_dataTotr2000$sentiment_criteria == "SPREAD" )-1
bigdata_dataTotr2000$correcting_factor[bigdata_dataTotr2000$sentiment_criteria == "ALL"] <- 0
stats_post_sign <- colnames(bigdata_dataTotr2000)[which(as.numeric(colnames(bigdata_dataTotr2000)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)
bigdata_dataTotr2000$infinity_return <- rowSums(bigdata_dataTotr2000[,rets_post]*bigdata_dataTotr2000$correcting_factor*bigdata_dataTotr2000[,stats_post_sign],na.rm = TRUE)
bigdata_dataTotr2000 <- bigdata_dataTotr2000[bigdata_dataTotr2000$event_number_event_filtering >= 50,]
##################
##################
##################
################## End of big data file loading



##################
##################
##################
################## RPNA file loading
rpna_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"spr_r1000_rpna_abvol_abvol_corrado_df.rds"))
rpna_best_profile_group_ordered_r1000_corrado_df <- readRDS(file=paste0(outputDataPath,"rpna_best_profile_group_ordered_r1000_corrado_df.rds"))
rpna_best_profile_category_ordered_r1000_corrado_df <- readRDS(file=paste0(outputDataPath,"rpna_best_profile_category_ordered_r1000_corrado_df.rds"))

# dataTotr2000 <- readRDS(file=paste0(outputDataPath,"spr_r2000_rpna_abvol_abvol_corrado_df.rds"))
# best_profile_group_ordered_r2000_corrado_df <- readRDS(file=paste0(outputDataPath,"rpna_best_profile_group_ordered_r2000_corrado_df.rds"))
# best_profile_category_ordered_r2000_corrado_df <- readRDS(file=paste0(outputDataPath,"rpna_best_profile_category_ordered_r2000_corrado_df.rds"))
rpna_dataTotr2000 <- rpna_dataTotr1000
rpna_best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
rpna_best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df


rpna_all_group_events <- readRDS(file=paste0(outputDataPath,"rpna_all_group_events.rds"))
rpna_all_category_events <- readRDS(file=paste0(outputDataPath,"rpna_all_category_events.rds"))

rpna_dataTotr1000$correcting_factor <- 2*(rpna_dataTotr1000$sentiment_criteria == "POSITIVE"  | rpna_dataTotr1000$sentiment_criteria == "SPREAD" )-1
rpna_dataTotr1000$correcting_factor[rpna_dataTotr1000$sentiment_criteria == "ALL"] <- 0
stats_post_sign <- colnames(rpna_dataTotr1000)[which(as.numeric(colnames(rpna_dataTotr1000)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)
rpna_dataTotr1000$infinity_return <- rowSums(rpna_dataTotr1000[,rets_post]*rpna_dataTotr1000$correcting_factor*rpna_dataTotr1000[,stats_post_sign],na.rm = TRUE)
rpna_dataTotr1000 <- rpna_dataTotr1000[rpna_dataTotr1000$event_number_event_filtering >= 50,]

rpna_dataTotr2000$correcting_factor <- 2*(rpna_dataTotr2000$sentiment_criteria == "POSITIVE"  | rpna_dataTotr2000$sentiment_criteria == "SPREAD" )-1
rpna_dataTotr2000$correcting_factor[rpna_dataTotr2000$sentiment_criteria == "ALL"] <- 0
stats_post_sign <- colnames(rpna_dataTotr2000)[which(as.numeric(colnames(rpna_dataTotr2000)) >= 0)]
rets_post <- paste0("RET",stats_post_sign)
rpna_dataTotr2000$infinity_return <- rowSums(rpna_dataTotr2000[,rets_post]*rpna_dataTotr2000$correcting_factor*rpna_dataTotr2000[,stats_post_sign],na.rm = TRUE)
rpna_dataTotr2000 <- rpna_dataTotr2000[rpna_dataTotr2000$event_number_event_filtering >= 50,]

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

################################
################################
################################
################################
################################
################################
################################ End of Plotting functions


function(input, output, session) {
  observeEvent(input$sentiment_criteria, {
    if(input$sentiment_criteria == "BEST"){
      shinyjs::disable("similarity_gap_filter")
      shinyjs::disable("relevance")
      shinyjs::disable("event_relevance")
      shinyjs::disable("localSource")
    } else {
      shinyjs::enable("similarity_gap_filter")
      shinyjs::enable("relevance")
      shinyjs::enable("event_relevance")
      shinyjs::enable("localSource")
    }
  })
  
  output$filtering_criteria <- renderText({ 
    updatingList <- NULL
    if (input$ravenpack_type == "RDBA"){
      dataTotr1000 <- bigdata_dataTotr1000
      dataTotr2000 <- bigdata_dataTotr2000
      all_group_events <- bigdata_all_group_events
      all_category_events <- bigdata_all_category_events
      best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
      best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
      best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
      best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
    }
    
    if (input$ravenpack_type == "RPNA"){
      dataTotr1000 <- rpna_dataTotr1000
      dataTotr2000 <- rpna_dataTotr2000
      all_group_events <- rpna_all_group_events
      all_category_events <- rpna_all_category_events
      best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
      best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
      best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
      best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
    }
    
    if(input$russell_universe == "R1000"){
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
    } else {
      if(input$sort_profiles){
        if(input$aggregate_criteria == "GROUP"){
          updatingList <- best_profile_group_ordered_r2000_corrado_df$my_event
        }
        if(input$aggregate_criteria == "CATEGORY"){
          updatingList <- best_profile_category_ordered_r2000_corrado_df$my_event
        }
      } else {
        if(input$aggregate_criteria == "GROUP"){
          updatingList <- all_group_events
        }
        if(input$aggregate_criteria == "CATEGORY"){
          updatingList <- all_category_events
        }
      }
    } 
    
    updateSelectInput(session, "my_event",
                      label ="EVENT",
                      choices = updatingList,
                      selected = input$my_event
    )
    
    
    if(!input$plot_together){
      
      data <- NULL
      if (input$russell_universe == "R1000"){
        data <- dataTotr1000
      } else {
        data <- dataTotr2000
      }
      
      data <- data[data$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      
      if(input$sentiment_criteria == "BEST"){
        
        
        dataf <- data[data$my_event == input$my_event,]
        lyrics <- ""
        if(input$ravenpack_type == "RDBA"){
          lyrics <- "RDBA"
        } else {
          lyrics <- "RPNA"
        }
        
        if (dim(dataf)[1]>0){
          dataf <- dataf[order(dataf$infinity_return,decreasing = TRUE),]
          rowProfile <- dataf[1,]
          return(paste0(lyrics," ,Source : ", rowProfile$localSource," ,Sector : ",rowProfile$gics_sector," ,Sentiment : ",rowProfile$sentiment_criteria," ,Similarity days : ",rowProfile$similarity_gap_filter,
                 " ,Relevance : ",rowProfile$relevance, " ,Event relevance : ",rowProfile$event_relevance))
          
        }
      } else {
        "your selection applies"
      }
      
    } else {
      
      ###################
      ###################
      ###################
      ################### Plotting together
      bigdata <- NULL
      rpnadata <- NULL
      
      
      if (input$russell_universe == "R1000"){
        bigdata <- bigdata_dataTotr1000
        rpnadata <- rpna_dataTotr1000
      } else {
        bigdata <- bigdata_dataTotr2000
        rpnadata <- rpna_dataTotr2000
      }
      
      bigdata  <- bigdata[bigdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      rpnadata <- rpnadata[rpnadata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      
      
      
      
      if(input$sentiment_criteria == "BEST"){
        
        
        bigdatadataf <- bigdata[bigdata$my_event == input$my_event,]
        rpnadataf <- rpnadata[rpnadata$my_event == input$my_event,]
        rdbaLyrics <- ""
        if (dim(bigdatadataf)[1]>0){
          bigdatadataf <- bigdatadataf[order(bigdatadataf$infinity_return,decreasing = TRUE),]
          rowProfile <- bigdatadataf[1,]
          
          rdbaLyrics <- paste0("RDBA Source : ",rowProfile$localSource," ,Sector : ",rowProfile$gics_sector," ,Sentiment : ",rowProfile$sentiment_criteria," ,Similarity days : ",rowProfile$similarity_gap_filter,
                               " ,Relevance : ",rowProfile$relevance, " ,Event relevance : ",rowProfile$event_relevance)
          
        }
        rpnaLyrics <- ""
        if (dim(rpnadataf)[1]>0){
          rpnadataf <- rpnadataf[order(rpnadataf$infinity_return,decreasing = TRUE),]
          rowProfile <- rpnadataf[1,]
          
          rpnaLyrics <- paste0("RPNA Source : ",rowProfile$localSource," ,Sector : ",rowProfile$gics_sector," ,Sentiment : ",rowProfile$sentiment_criteria," ,Similarity days : ",rowProfile$similarity_gap_filter,
                               " ,Relevance : ",rowProfile$relevance, " ,Event relevance : ",rowProfile$event_relevance)
          
        }
        
        output <- c(rdbaLyrics,rpnaLyrics)
        if((length(rdbaLyrics) + length(rpnaLyrics))>0){
          return(output)
        } 
      } else {
        return("your selection applies")
      }
      
    }
  })
  
  # Fill in the spot we created for a plot
  output$eventMinutesPlotHigh <- renderPlot({
    if(!input$plot_together){
      data <- NULL
      if (input$ravenpack_type == "RDBA"){
        dataTotr1000 <- bigdata_dataTotr1000
        dataTotr2000 <- bigdata_dataTotr2000
        all_group_events <- bigdata_all_group_events
        all_category_events <- bigdata_all_category_events
        best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
        best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
        best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
        best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
      }
      
      if (input$ravenpack_type == "RPNA"){
        dataTotr1000 <- rpna_dataTotr1000
        dataTotr2000 <- rpna_dataTotr2000
        all_group_events <- rpna_all_group_events
        all_category_events <- rpna_all_category_events
        best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
        best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
        best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
        best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
      }
      
      if (input$russell_universe == "R1000"){
        data <- dataTotr1000
      } else {
        data <- dataTotr2000
      }
      
      data <- data[data$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      if(input$sentiment_criteria != "BEST"){
        data <- data[data$localSource == input$localSource,]
        data <- data[data$similarity_gap_filter == input$similarity_gap_filter,]
        data <- data[data$sentiment_criteria == input$sentiment_criteria,]
        data <- data[data$relevance == input$relevance,]
        data <- data[data$event_relevance == input$event_relevance,]
      }
      dataf <- data[data$my_event == input$my_event,]
      
      if (dim(dataf)[1] >0){
        
        
        
        
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
        
      } else {
        
        pp <- readPNG("SORRY.PNG")
        rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
      }
    } else {
      
      ###################
      ###################
      ###################
      ################### Plotting together
      bigdata <- NULL
      rpnadata <- NULL
      
      
      if (input$russell_universe == "R1000"){
        bigdata <- bigdata_dataTotr1000
        rpnadata <- rpna_dataTotr1000
      } else {
        bigdata <- bigdata_dataTotr2000
        rpnadata <- rpna_dataTotr2000
      }
      
      bigdata  <- bigdata[bigdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      rpnadata <- rpnadata[rpnadata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      
      if(input$sentiment_criteria != "BEST"){
        bigdata <- bigdata[bigdata$localSource == input$localSource,]
        bigdata <- bigdata[bigdata$similarity_gap_filter == input$similarity_gap_filter,]
        bigdata <- bigdata[bigdata$sentiment_criteria == input$sentiment_criteria,]
        bigdata <- bigdata[bigdata$relevance == input$relevance,]
        bigdata <- bigdata[bigdata$event_relevance == input$event_relevance,]
        
        rpnadata <- rpnadata[rpnadata$localSource == input$localSource,]
        rpnadata <- rpnadata[rpnadata$similarity_gap_filter == input$similarity_gap_filter,]
        rpnadata <- rpnadata[rpnadata$sentiment_criteria == input$sentiment_criteria,]
        rpnadata <- rpnadata[rpnadata$relevance == input$relevance,]
        rpnadata <- rpnadata[rpnadata$event_relevance == input$event_relevance,]
        
      }
      
      bigdataf <- bigdata[bigdata$my_event == input$my_event,]
      rpnadataf <- rpnadata[rpnadata$my_event == input$my_event,]
      
      
      if ((dim(bigdataf)[1] >0) & (dim(rpnadataf)[1] >0 ) ){
        print("plotting them together")
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
        
        #### plotting them together
        if (input$my_plot == "RETURNS_SIGNIFICANCE"){
          g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
          
          # Render your graph
          print(g)    
          
        } else {
          g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
          
          # Render your graph
          print(g)  
        }
      } else { 
        print("Not enough data for one")
        if(input$ravenpack_type == "RPNA"){
          
          
          if ((dim(rpnadataf)[1] >0 )){
            print("plotting rpna only")
            dataf <- rpnadataf
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
          }else {
            
            pp <- readPNG("SORRY.PNG")
            rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
          }
        }
        
        if(input$ravenpack_type == "RDBA"){
          if ((dim(bigdataf)[1] >0 )){
            print("plotting rbda only")
            dataf <- bigdataf
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
        } else {
          
          pp <- readPNG("SORRY.PNG")
          rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
        }
      }
      
      
      
    }
  })
  
  
  output$eventMinutesPlotLow <- renderPlot({
    if(!input$plot_together){
      data <- NULL
      if (input$ravenpack_type == "RDBA"){
        dataTotr1000 <- bigdata_dataTotr1000
        dataTotr2000 <- bigdata_dataTotr2000
        all_group_events <- bigdata_all_group_events
        all_category_events <- bigdata_all_category_events
        best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
        best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
        best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
        best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
      }
      
      if (input$ravenpack_type == "RPNA"){
        dataTotr1000 <- rpna_dataTotr1000
        dataTotr2000 <- rpna_dataTotr2000
        all_group_events <- rpna_all_group_events
        all_category_events <- rpna_all_category_events
        best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
        best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
        best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
        best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
      }
      if (input$russell_universe == "R1000"){
        data <- dataTotr1000
      } else {
        data <- dataTotr2000
      }
      
      data <- data[data$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      if(input$sentiment_criteria != "BEST"){
        data <- data[data$localSource == input$localSource,]
        data <- data[data$similarity_gap_filter == input$similarity_gap_filter,]
        data <- data[data$sentiment_criteria == input$sentiment_criteria,]
        data <- data[data$relevance == input$relevance,]
        data <- data[data$event_relevance == input$event_relevance,]
      }
      
      
      dataf <- data[data$my_event == input$my_event,]
      
      if (dim(dataf)[1] >0){
        
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
      } else {
        print("not enough data")
      }
    } else {
      ###################
      ###################
      ###################
      ################### Plotting together
      bigdata <- NULL
      rpnadata <- NULL
      
      
      if (input$russell_universe == "R1000"){
        bigdata <- bigdata_dataTotr1000
        rpnadata <- rpna_dataTotr1000
      } else {
        bigdata <- bigdata_dataTotr2000
        rpnadata <- rpna_dataTotr2000
      }
      
      bigdata  <- bigdata[bigdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      rpnadata <- rpnadata[rpnadata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
      
      if(input$sentiment_criteria != "BEST"){
        bigdata <- bigdata[bigdata$localSource == input$localSource,]
        bigdata <- bigdata[bigdata$similarity_gap_filter == input$similarity_gap_filter,]
        bigdata <- bigdata[bigdata$sentiment_criteria == input$sentiment_criteria,]
        bigdata <- bigdata[bigdata$relevance == input$relevance,]
        bigdata <- bigdata[bigdata$event_relevance == input$event_relevance,]
        
        rpnadata <- rpnadata[rpnadata$localSource == input$localSource,]
        rpnadata <- rpnadata[rpnadata$similarity_gap_filter == input$similarity_gap_filter,]
        rpnadata <- rpnadata[rpnadata$sentiment_criteria == input$sentiment_criteria,]
        rpnadata <- rpnadata[rpnadata$relevance == input$relevance,]
        rpnadata <- rpnadata[rpnadata$event_relevance == input$event_relevance,]
      }
      
      bigdataf <- bigdata[bigdata$my_event == input$my_event,]
      rpnadataf <- rpnadata[rpnadata$my_event == input$my_event,]
      
      
      if ((dim(bigdataf)[1] >0) & (dim(rpnadataf)[1] >0 ) ){
        print("plotting them together")
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
        
        if (input$my_plot == "RETURNS_SIGNIFICANCE"){
          g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
          # Render your graph
          print(g)    
        } else {
          g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$my_event, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
          # Render your graph
          print(g)  
        }
      } else { 
        print("not enough data for one")
        if(input$ravenpack_type == "RPNA"){
          if ((dim(rpnadataf)[1] >0 )){
            print("plotting rpna only")
            dataf <- rpnadataf
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
          }else {
            
            pp <- readPNG("SORRY.PNG")
            rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
          }
        }
        
        if(input$ravenpack_type == "RDBA"){
          if ((dim(bigdataf)[1] >0 )){
            print("plotting rdba only")
            dataf <- bigdataf
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
          }else {
            
            pp <- readPNG("SORRY.PNG")
            rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
          }
        }
      }
      
      
      
      
    }
  })
  
}
