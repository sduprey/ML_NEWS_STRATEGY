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


card_post_returnrpna_best_group               <-readRDS(paste0(outputDataPath,filename="card_post_returnrpna_best_group.rds"))
card_post_ranked_returnrpna_best_group        <-readRDS(paste0(outputDataPath,filename="card_post_ranked_returnrpna_best_group.rds"))
post_returnrpna_best_group                    <-readRDS(paste0(outputDataPath,filename="post_returnrpna_best_group.rds"))
post_ranked_returnrpna_best_group             <-readRDS(paste0(outputDataPath,filename="post_ranked_returnrpna_best_group.rds"))
post_volatilityrpna_best_group                <-readRDS(paste0(outputDataPath,filename="post_volatilityrpna_best_group.rds"))
post_ranked_volatilityrpna_best_group         <-readRDS(paste0(outputDataPath,filename="post_ranked_volatilityrpna_best_group.rds"))
card_pre_returnrpna_best_group                <-readRDS(paste0(outputDataPath,filename="card_pre_returnrpna_best_group.rds"))
card_pre_ranked_returnrpna_best_group         <-readRDS(paste0(outputDataPath,filename="card_pre_ranked_returnrpna_best_group.rds"))
pre_returnrpna_best_group                     <-readRDS(paste0(outputDataPath,filename="pre_returnrpna_best_group.rds"))
pre_ranked_returnrpna_best_group              <-readRDS(paste0(outputDataPath,filename="pre_ranked_returnrpna_best_group.rds"))
pre_volatilityrpna_best_group                 <-readRDS(paste0(outputDataPath,filename="pre_volatilityrpna_best_group.rds"))
pre_ranked_volatilityrpna_best_group          <-readRDS(paste0(outputDataPath,filename="pre_ranked_volatilityrpna_best_group.rds"))
volatility_correctionrpna_best_group          <-readRDS(paste0(outputDataPath,filename="volatility_correctionrpna_best_group.rds"))
ranked_volatility_correctionrpna_best_group   <-readRDS(paste0(outputDataPath,filename="ranked_volatility_correctionrpna_best_group.rds"))
return_correctionrpna_best_group              <-readRDS(paste0(outputDataPath,filename="return_correctionrpna_best_group.rds"))
ranked_return_correctionrpna_best_group       <-readRDS(paste0(outputDataPath,filename="ranked_return_correctionrpna_best_group.rds"))
card_return_correctionrpna_best_group         <-readRDS(paste0(outputDataPath,filename="card_return_correctionrpna_best_group.rds"))
card_ranked_return_correctionrpna_best_group  <-readRDS(paste0(outputDataPath,filename="card_ranked_return_correctionrpna_best_group.rds"))



card_post_returnrpna_best_category               <-readRDS(paste0(outputDataPath,filename="card_post_returnrpna_best_category.rds"))
card_post_ranked_returnrpna_best_category        <-readRDS(paste0(outputDataPath,filename="card_post_ranked_returnrpna_best_category.rds"))
post_returnrpna_best_category                    <-readRDS(paste0(outputDataPath,filename="post_returnrpna_best_category.rds"))
post_ranked_returnrpna_best_category             <-readRDS(paste0(outputDataPath,filename="post_ranked_returnrpna_best_category.rds"))
post_volatilityrpna_best_category                <-readRDS(paste0(outputDataPath,filename="post_volatilityrpna_best_category.rds"))
post_ranked_volatilityrpna_best_category         <-readRDS(paste0(outputDataPath,filename="post_ranked_volatilityrpna_best_category.rds"))
card_pre_returnrpna_best_category                <-readRDS(paste0(outputDataPath,filename="card_pre_returnrpna_best_category.rds"))
card_pre_ranked_returnrpna_best_category         <-readRDS(paste0(outputDataPath,filename="card_pre_ranked_returnrpna_best_category.rds"))
pre_returnrpna_best_category                     <-readRDS(paste0(outputDataPath,filename="pre_returnrpna_best_category.rds"))
pre_ranked_returnrpna_best_category              <-readRDS(paste0(outputDataPath,filename="pre_ranked_returnrpna_best_category.rds"))
pre_volatilityrpna_best_category                 <-readRDS(paste0(outputDataPath,filename="pre_volatilityrpna_best_category.rds"))
pre_ranked_volatilityrpna_best_category          <-readRDS(paste0(outputDataPath,filename="pre_ranked_volatilityrpna_best_category.rds"))
volatility_correctionrpna_best_category          <-readRDS(paste0(outputDataPath,filename="volatility_correctionrpna_best_category.rds"))
ranked_volatility_correctionrpna_best_category   <-readRDS(paste0(outputDataPath,filename="ranked_volatility_correctionrpna_best_category.rds"))
return_correctionrpna_best_category              <-readRDS(paste0(outputDataPath,filename="return_correctionrpna_best_category.rds"))
ranked_return_correctionrpna_best_category       <-readRDS(paste0(outputDataPath,filename="ranked_return_correctionrpna_best_category.rds"))
card_return_correctionrpna_best_category         <-readRDS(paste0(outputDataPath,filename="card_return_correctionrpna_best_category.rds"))
card_ranked_return_correctionrpna_best_category  <-readRDS(paste0(outputDataPath,filename="card_ranked_return_correctionrpna_best_category.rds"))

mappingList <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnrpna_best_category,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnrpna_best_category,       
    "CATEGORYPOSTRETURNNONE"=          post_returnrpna_best_category,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnrpna_best_category,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilityrpna_best_category,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_category,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnrpna_best_category,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_category,        
    "CATEGORYPRERETURNNONE"=           pre_returnrpna_best_category,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnrpna_best_category,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilityrpna_best_category,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_category,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_category,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_category,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionrpna_best_category,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_category,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionrpna_best_category,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_category,
    "GROUPPOSTRETURNCARD"=             card_post_returnrpna_best_group,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnrpna_best_group,       
    "GROUPPOSTRETURNNONE"=             post_returnrpna_best_group,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnrpna_best_group,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilityrpna_best_group,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_group,    
    "GROUPPRERETURNCARD"=           card_pre_returnrpna_best_group,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_group,        
    "GROUPPRERETURNNONE"=           pre_returnrpna_best_group,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnrpna_best_group,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilityrpna_best_group,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_group,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_group,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_group,
    "GROUPPREPOSTRETURNNONE"=           return_correctionrpna_best_group,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_group,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionrpna_best_group,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_group
  ) 

getBestProfilesDataset <- function(aggregate_criteria,periods,methodo,weighting){
  if (methodo == "VOLATILITY" && weighting == "CARD"){
    weighting <- "RANK"
  }
  toFetch <- paste0(aggregate_criteria,periods,methodo,weighting)
  return(mappingList[[toFetch]])
}


for (aggregate_criteria in c("GROUP","CATEGORY")){
  for(periods in c("PRE","POST","PREPOST")){
    for(methodo in c("RETURN","VOLATILITY")){
      for(weighting in c("NONE","RANK","CARD")){
        toFetch <- paste0(aggregate_criteria,periods,methodo,weighting)
        print(toFetch)

        test <- getBestProfilesDataset(aggregate_criteria,periods,methodo,weighting)
        if(is.null(test)){
          print("problem")
        }
        print(dim(test))
      } 
    }
  }
}
  