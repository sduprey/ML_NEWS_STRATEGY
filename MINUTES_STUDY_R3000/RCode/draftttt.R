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


CategoriesDT <- readRDS(file = paste0(outputDataPath,"EventSimilaritySourcesDT.rds"))



big_data <- readRDS(file=paste0(outputDataPath,"new_spr_r1000_bigdata_abvol_abvol_corrado_df.rds"))
rpna_data <- readRDS(file=paste0(outputDataPath,"new_spr_r1000_rpna_abvol_abvol_corrado_df.rds"))
for (cor in unique(rpna_data$corrado_methodo)){
  for(lap in unique(rpna_data$lapse)){
    for(rel in unique(rpna_data$relevance)){
      for(evrel in unique(rpna_data$event_relevance)){
        for(evt in unique(rpna_data$my_event)){
          for(sent in unique(rpna_data$sentiment_criteria)){
            for(sour in unique(rpna_data$localSource)){
              for(sim in unique(rpna_data$similarity_gap_filter)){
#                 print(cor)
#                 print(lap)
#                 print(rel)
#                 print(evrel)
#                 print(evt)
#                 print(sent)
#                 print(sour)
#                 print(sim)
#                 
                bid_index <- 
                  big_data$corrado_methodo == cor &
                  big_data$lapse == lap &
                  big_data$relevance == rel &
                  big_data$event_relevance == evrel &
                  big_data$my_event == evt &
                  big_data$sentiment_criteria == sent &
                  big_data$localSource == sour &
                  big_data$similarity_gap_filter == sim
#                 print("bid_index")
#                 print(length(which(bid_index)))
                
                rpna_index <- 
                  rpna_data$corrado_methodo == cor &
                  rpna_data$lapse == lap &
                  rpna_data$relevance == rel &
                  rpna_data$event_relevance == evrel &
                  rpna_data$my_event == evt &
                  rpna_data$sentiment_criteria == sent &
                  rpna_data$localSource == sour &
                  rpna_data$similarity_gap_filter == sim
                
#                 print("rpna_index")
#                 print(length(which(rpna_index)))
#                 
                if((length(which(bid_index)) ==1) & (length(which(rpna_index)) ==1)){
                  print(evt)
                  rpna_event_nb <- rpna_data[rpna_index,"event_number_event_filtering"]
                  print("rpna")
                  print(rpna_event_nb)
                  big_data_event_nb <- big_data[bid_index,"event_number_event_filtering"]
                  print("rbda")
                  print(big_data_event_nb)
                  big_data[bid_index,"event_number_event_filtering"] <- max(rpna_event_nb,big_data_event_nb)
                  rpna_data[rpna_index,"event_number_event_filtering"] <- min(rpna_event_nb,big_data_event_nb)
                }
              }
            }
          }
        }
      }
    }
  }
}

saveRDS(big_data,file=paste0(outputDataPath,"nnew_spr_r1000_bigdata_abvol_abvol_corrado_df.rds"))
saveRDS(rpna_data,file=paste0(outputDataPath,"nnew_spr_r1000_rpna_abvol_abvol_corrado_df.rds"))