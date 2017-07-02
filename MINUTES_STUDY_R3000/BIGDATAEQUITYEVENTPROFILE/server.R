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


##################################################################################################################################################################################################################################################################################################################
##################################################################################################################################################################
######################################################################################################################################################################################################
################## Big data file loading
####################################################################################################################################################################################
########################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################

bigdata_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"all_lapses_bigdata_metrics_clean_prod.rds"))

bigdata_all_group_events <- readRDS(file=paste0(outputDataPath,"prod_bigdata_all_group_events.rds"))
bigdata_all_category_events <- readRDS(file=paste0(outputDataPath,"prod_bigdata_all_category_events.rds"))

metricsList <- 
  list(		  
    "POSTRETURNCARD"=          "card_post_return",              
    "POSTRETURNCARDRANK"=      "card_post_ranked_return",       
    "POSTRETURNNONE"=          "post_return",                   
    "POSTRETURNRANK"=          "post_ranked_return",            
    "POSTVOLATILITYNONE"=      "post_volatility",           
    "POSTVOLATILITYRANK"=      "post_ranked_volatility",    
    "PRERETURNCARD"=           "card_pre_return",               
    "PRERETURNCARDRANK"=       "card_pre_ranked_return",        
    "PRERETURNNONE"=           "pre_return",                     
    "PRERETURNRANK"=           "pre_ranked_return",             
    "PREVOLATILITYNONE"=       "pre_volatility",            
    "PREVOLATILITYRANK"=       "pre_ranked_volatility",     
    "PREPOSTVOLATILITYNONE"=   "volatility_correction",  
    "PREPOSTVOLATILITYRANK"=   "ranked_volatility_correction",
    "PREPOSTRETURNNONE"=       "return_correction",          
    "PREPOSTRETURNRANK"=       "ranked_return_correction",   
    "PREPOSTRETURNCARD"=       "card_return_correction",     
    "PREPOSTRETURNCARDRANK"=   "card_ranked_return_correction"
  ) 

#######################
#######################
#######################
####################### big data lapse -1

card_post_returnbigdata_best_group_minus_one               <-readRDS(paste0(outputDataPath,filename="-1card_post_returnbigdataf_best_group.rds"))
card_post_ranked_returnbigdata_best_group_minus_one        <-readRDS(paste0(outputDataPath,filename="-1card_post_ranked_returnbigdataf_best_group.rds"))
post_returnbigdata_best_group_minus_one                    <-readRDS(paste0(outputDataPath,filename="-1post_returnbigdataf_best_group.rds"))
post_ranked_returnbigdata_best_group_minus_one             <-readRDS(paste0(outputDataPath,filename="-1post_ranked_returnbigdataf_best_group.rds"))
post_volatilitybigdata_best_group_minus_one                <-readRDS(paste0(outputDataPath,filename="-1post_volatilitybigdataf_best_group.rds"))
post_ranked_volatilitybigdata_best_group_minus_one         <-readRDS(paste0(outputDataPath,filename="-1post_ranked_volatilitybigdataf_best_group.rds"))
card_pre_returnbigdata_best_group_minus_one                <-readRDS(paste0(outputDataPath,filename="-1card_pre_returnbigdataf_best_group.rds"))
card_pre_ranked_returnbigdata_best_group_minus_one         <-readRDS(paste0(outputDataPath,filename="-1card_pre_ranked_returnbigdataf_best_group.rds"))
pre_returnbigdata_best_group_minus_one                     <-readRDS(paste0(outputDataPath,filename="-1pre_returnbigdataf_best_group.rds"))
pre_ranked_returnbigdata_best_group_minus_one              <-readRDS(paste0(outputDataPath,filename="-1pre_ranked_returnbigdataf_best_group.rds"))
pre_volatilitybigdata_best_group_minus_one                 <-readRDS(paste0(outputDataPath,filename="-1pre_volatilitybigdataf_best_group.rds"))
pre_ranked_volatilitybigdata_best_group_minus_one          <-readRDS(paste0(outputDataPath,filename="-1pre_ranked_volatilitybigdataf_best_group.rds"))
volatility_correctionbigdata_best_group_minus_one          <-readRDS(paste0(outputDataPath,filename="-1volatility_correctionbigdataf_best_group.rds"))
ranked_volatility_correctionbigdata_best_group_minus_one   <-readRDS(paste0(outputDataPath,filename="-1ranked_volatility_correctionbigdataf_best_group.rds"))
return_correctionbigdata_best_group_minus_one              <-readRDS(paste0(outputDataPath,filename="-1return_correctionbigdataf_best_group.rds"))
ranked_return_correctionbigdata_best_group_minus_one       <-readRDS(paste0(outputDataPath,filename="-1ranked_return_correctionbigdataf_best_group.rds"))
card_return_correctionbigdata_best_group_minus_one         <-readRDS(paste0(outputDataPath,filename="-1card_return_correctionbigdataf_best_group.rds"))
card_ranked_return_correctionbigdata_best_group_minus_one  <-readRDS(paste0(outputDataPath,filename="-1card_ranked_return_correctionbigdataf_best_group.rds"))



card_post_returnbigdata_best_category_minus_one               <-readRDS(paste0(outputDataPath,filename="-1card_post_returnbigdataf_best_category.rds"))
card_post_ranked_returnbigdata_best_category_minus_one        <-readRDS(paste0(outputDataPath,filename="-1card_post_ranked_returnbigdataf_best_category.rds"))
post_returnbigdata_best_category_minus_one                    <-readRDS(paste0(outputDataPath,filename="-1post_returnbigdataf_best_category.rds"))
post_ranked_returnbigdata_best_category_minus_one             <-readRDS(paste0(outputDataPath,filename="-1post_ranked_returnbigdataf_best_category.rds"))
post_volatilitybigdata_best_category_minus_one                <-readRDS(paste0(outputDataPath,filename="-1post_volatilitybigdataf_best_category.rds"))
post_ranked_volatilitybigdata_best_category_minus_one         <-readRDS(paste0(outputDataPath,filename="-1post_ranked_volatilitybigdataf_best_category.rds"))
card_pre_returnbigdata_best_category_minus_one                <-readRDS(paste0(outputDataPath,filename="-1card_pre_returnbigdataf_best_category.rds"))
card_pre_ranked_returnbigdata_best_category_minus_one         <-readRDS(paste0(outputDataPath,filename="-1card_pre_ranked_returnbigdataf_best_category.rds"))
pre_returnbigdata_best_category_minus_one                     <-readRDS(paste0(outputDataPath,filename="-1pre_returnbigdataf_best_category.rds"))
pre_ranked_returnbigdata_best_category_minus_one              <-readRDS(paste0(outputDataPath,filename="-1pre_ranked_returnbigdataf_best_category.rds"))
pre_volatilitybigdata_best_category_minus_one                 <-readRDS(paste0(outputDataPath,filename="-1pre_volatilitybigdataf_best_category.rds"))
pre_ranked_volatilitybigdata_best_category_minus_one          <-readRDS(paste0(outputDataPath,filename="-1pre_ranked_volatilitybigdataf_best_category.rds"))
volatility_correctionbigdata_best_category_minus_one          <-readRDS(paste0(outputDataPath,filename="-1volatility_correctionbigdataf_best_category.rds"))
ranked_volatility_correctionbigdata_best_category_minus_one   <-readRDS(paste0(outputDataPath,filename="-1ranked_volatility_correctionbigdataf_best_category.rds"))
return_correctionbigdata_best_category_minus_one              <-readRDS(paste0(outputDataPath,filename="-1return_correctionbigdataf_best_category.rds"))
ranked_return_correctionbigdata_best_category_minus_one       <-readRDS(paste0(outputDataPath,filename="-1ranked_return_correctionbigdataf_best_category.rds"))
card_return_correctionbigdata_best_category_minus_one         <-readRDS(paste0(outputDataPath,filename="-1card_return_correctionbigdataf_best_category.rds"))
card_ranked_return_correctionbigdata_best_category_minus_one  <-readRDS(paste0(outputDataPath,filename="-1card_ranked_return_correctionbigdataf_best_category.rds"))






bigdataMappingList_minus_one <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnbigdata_best_category_minus_one,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnbigdata_best_category_minus_one,       
    "CATEGORYPOSTRETURNNONE"=          post_returnbigdata_best_category_minus_one,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnbigdata_best_category_minus_one,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_category_minus_one,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_category_minus_one,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnbigdata_best_category_minus_one,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_category_minus_one,        
    "CATEGORYPRERETURNNONE"=           pre_returnbigdata_best_category_minus_one,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnbigdata_best_category_minus_one,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilitybigdata_best_category_minus_one,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_category_minus_one,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_category_minus_one,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_category_minus_one,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionbigdata_best_category_minus_one,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_category_minus_one,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_category_minus_one,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_category_minus_one,
    "GROUPPOSTRETURNCARD"=             card_post_returnbigdata_best_group_minus_one,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnbigdata_best_group_minus_one,       
    "GROUPPOSTRETURNNONE"=             post_returnbigdata_best_group_minus_one,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnbigdata_best_group_minus_one,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_group_minus_one,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_group_minus_one,    
    "GROUPPRERETURNCARD"=           card_pre_returnbigdata_best_group_minus_one,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_group_minus_one,        
    "GROUPPRERETURNNONE"=           pre_returnbigdata_best_group_minus_one,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnbigdata_best_group_minus_one,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilitybigdata_best_group_minus_one,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_group_minus_one,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_group_minus_one,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_group_minus_one,
    "GROUPPREPOSTRETURNNONE"=           return_correctionbigdata_best_group_minus_one,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_group_minus_one,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_group_minus_one,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_group_minus_one
  ) 

#######################
#######################
#######################
####################### end of big data lapse -1

#######################
#######################
#######################
####################### big data lapse 2

card_post_returnbigdata_best_group_two               <-readRDS(paste0(outputDataPath,filename="2card_post_returnbigdataf_best_group.rds"))
card_post_ranked_returnbigdata_best_group_two        <-readRDS(paste0(outputDataPath,filename="2card_post_ranked_returnbigdataf_best_group.rds"))
post_returnbigdata_best_group_two                    <-readRDS(paste0(outputDataPath,filename="2post_returnbigdataf_best_group.rds"))
post_ranked_returnbigdata_best_group_two             <-readRDS(paste0(outputDataPath,filename="2post_ranked_returnbigdataf_best_group.rds"))
post_volatilitybigdata_best_group_two                <-readRDS(paste0(outputDataPath,filename="2post_volatilitybigdataf_best_group.rds"))
post_ranked_volatilitybigdata_best_group_two         <-readRDS(paste0(outputDataPath,filename="2post_ranked_volatilitybigdataf_best_group.rds"))
card_pre_returnbigdata_best_group_two                <-readRDS(paste0(outputDataPath,filename="2card_pre_returnbigdataf_best_group.rds"))
card_pre_ranked_returnbigdata_best_group_two         <-readRDS(paste0(outputDataPath,filename="2card_pre_ranked_returnbigdataf_best_group.rds"))
pre_returnbigdata_best_group_two                     <-readRDS(paste0(outputDataPath,filename="2pre_returnbigdataf_best_group.rds"))
pre_ranked_returnbigdata_best_group_two              <-readRDS(paste0(outputDataPath,filename="2pre_ranked_returnbigdataf_best_group.rds"))
pre_volatilitybigdata_best_group_two                 <-readRDS(paste0(outputDataPath,filename="2pre_volatilitybigdataf_best_group.rds"))
pre_ranked_volatilitybigdata_best_group_two          <-readRDS(paste0(outputDataPath,filename="2pre_ranked_volatilitybigdataf_best_group.rds"))
volatility_correctionbigdata_best_group_two          <-readRDS(paste0(outputDataPath,filename="2volatility_correctionbigdataf_best_group.rds"))
ranked_volatility_correctionbigdata_best_group_two   <-readRDS(paste0(outputDataPath,filename="2ranked_volatility_correctionbigdataf_best_group.rds"))
return_correctionbigdata_best_group_two              <-readRDS(paste0(outputDataPath,filename="2return_correctionbigdataf_best_group.rds"))
ranked_return_correctionbigdata_best_group_two       <-readRDS(paste0(outputDataPath,filename="2ranked_return_correctionbigdataf_best_group.rds"))
card_return_correctionbigdata_best_group_two         <-readRDS(paste0(outputDataPath,filename="2card_return_correctionbigdataf_best_group.rds"))
card_ranked_return_correctionbigdata_best_group_two  <-readRDS(paste0(outputDataPath,filename="2card_ranked_return_correctionbigdataf_best_group.rds"))



card_post_returnbigdata_best_category_two               <-readRDS(paste0(outputDataPath,filename="2card_post_returnbigdataf_best_category.rds"))
card_post_ranked_returnbigdata_best_category_two        <-readRDS(paste0(outputDataPath,filename="2card_post_ranked_returnbigdataf_best_category.rds"))
post_returnbigdata_best_category_two                    <-readRDS(paste0(outputDataPath,filename="2post_returnbigdataf_best_category.rds"))
post_ranked_returnbigdata_best_category_two             <-readRDS(paste0(outputDataPath,filename="2post_ranked_returnbigdataf_best_category.rds"))
post_volatilitybigdata_best_category_two                <-readRDS(paste0(outputDataPath,filename="2post_volatilitybigdataf_best_category.rds"))
post_ranked_volatilitybigdata_best_category_two         <-readRDS(paste0(outputDataPath,filename="2post_ranked_volatilitybigdataf_best_category.rds"))
card_pre_returnbigdata_best_category_two                <-readRDS(paste0(outputDataPath,filename="2card_pre_returnbigdataf_best_category.rds"))
card_pre_ranked_returnbigdata_best_category_two         <-readRDS(paste0(outputDataPath,filename="2card_pre_ranked_returnbigdataf_best_category.rds"))
pre_returnbigdata_best_category_two                     <-readRDS(paste0(outputDataPath,filename="2pre_returnbigdataf_best_category.rds"))
pre_ranked_returnbigdata_best_category_two              <-readRDS(paste0(outputDataPath,filename="2pre_ranked_returnbigdataf_best_category.rds"))
pre_volatilitybigdata_best_category_two                 <-readRDS(paste0(outputDataPath,filename="2pre_volatilitybigdataf_best_category.rds"))
pre_ranked_volatilitybigdata_best_category_two          <-readRDS(paste0(outputDataPath,filename="2pre_ranked_volatilitybigdataf_best_category.rds"))
volatility_correctionbigdata_best_category_two          <-readRDS(paste0(outputDataPath,filename="2volatility_correctionbigdataf_best_category.rds"))
ranked_volatility_correctionbigdata_best_category_two   <-readRDS(paste0(outputDataPath,filename="2ranked_volatility_correctionbigdataf_best_category.rds"))
return_correctionbigdata_best_category_two              <-readRDS(paste0(outputDataPath,filename="2return_correctionbigdataf_best_category.rds"))
ranked_return_correctionbigdata_best_category_two       <-readRDS(paste0(outputDataPath,filename="2ranked_return_correctionbigdataf_best_category.rds"))
card_return_correctionbigdata_best_category_two         <-readRDS(paste0(outputDataPath,filename="2card_return_correctionbigdataf_best_category.rds"))
card_ranked_return_correctionbigdata_best_category_two  <-readRDS(paste0(outputDataPath,filename="2card_ranked_return_correctionbigdataf_best_category.rds"))






bigdataMappingList_two <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnbigdata_best_category_two,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnbigdata_best_category_two,       
    "CATEGORYPOSTRETURNNONE"=          post_returnbigdata_best_category_two,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnbigdata_best_category_two,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_category_two,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_category_two,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnbigdata_best_category_two,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_category_two,        
    "CATEGORYPRERETURNNONE"=           pre_returnbigdata_best_category_two,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnbigdata_best_category_two,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilitybigdata_best_category_two,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_category_two,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_category_two,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_category_two,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionbigdata_best_category_two,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_category_two,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_category_two,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_category_two,
    "GROUPPOSTRETURNCARD"=             card_post_returnbigdata_best_group_two,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnbigdata_best_group_two,       
    "GROUPPOSTRETURNNONE"=             post_returnbigdata_best_group_two,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnbigdata_best_group_two,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_group_two,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_group_two,    
    "GROUPPRERETURNCARD"=           card_pre_returnbigdata_best_group_two,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_group_two,        
    "GROUPPRERETURNNONE"=           pre_returnbigdata_best_group_two,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnbigdata_best_group_two,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilitybigdata_best_group_two,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_group_two,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_group_two,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_group_two,
    "GROUPPREPOSTRETURNNONE"=           return_correctionbigdata_best_group_two,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_group_two,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_group_two,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_group_two
  ) 

#######################
#######################
#######################
####################### end of big data lapse 2
#######################
#######################
#######################
####################### big data lapse 3

card_post_returnbigdata_best_group_three               <-readRDS(paste0(outputDataPath,filename="3card_post_returnbigdataf_best_group.rds"))
card_post_ranked_returnbigdata_best_group_three        <-readRDS(paste0(outputDataPath,filename="3card_post_ranked_returnbigdataf_best_group.rds"))
post_returnbigdata_best_group_three                    <-readRDS(paste0(outputDataPath,filename="3post_returnbigdataf_best_group.rds"))
post_ranked_returnbigdata_best_group_three             <-readRDS(paste0(outputDataPath,filename="3post_ranked_returnbigdataf_best_group.rds"))
post_volatilitybigdata_best_group_three                <-readRDS(paste0(outputDataPath,filename="3post_volatilitybigdataf_best_group.rds"))
post_ranked_volatilitybigdata_best_group_three         <-readRDS(paste0(outputDataPath,filename="3post_ranked_volatilitybigdataf_best_group.rds"))
card_pre_returnbigdata_best_group_three                <-readRDS(paste0(outputDataPath,filename="3card_pre_returnbigdataf_best_group.rds"))
card_pre_ranked_returnbigdata_best_group_three         <-readRDS(paste0(outputDataPath,filename="3card_pre_ranked_returnbigdataf_best_group.rds"))
pre_returnbigdata_best_group_three                     <-readRDS(paste0(outputDataPath,filename="3pre_returnbigdataf_best_group.rds"))
pre_ranked_returnbigdata_best_group_three              <-readRDS(paste0(outputDataPath,filename="3pre_ranked_returnbigdataf_best_group.rds"))
pre_volatilitybigdata_best_group_three                 <-readRDS(paste0(outputDataPath,filename="3pre_volatilitybigdataf_best_group.rds"))
pre_ranked_volatilitybigdata_best_group_three          <-readRDS(paste0(outputDataPath,filename="3pre_ranked_volatilitybigdataf_best_group.rds"))
volatility_correctionbigdata_best_group_three          <-readRDS(paste0(outputDataPath,filename="3volatility_correctionbigdataf_best_group.rds"))
ranked_volatility_correctionbigdata_best_group_three   <-readRDS(paste0(outputDataPath,filename="3ranked_volatility_correctionbigdataf_best_group.rds"))
return_correctionbigdata_best_group_three              <-readRDS(paste0(outputDataPath,filename="3return_correctionbigdataf_best_group.rds"))
ranked_return_correctionbigdata_best_group_three       <-readRDS(paste0(outputDataPath,filename="3ranked_return_correctionbigdataf_best_group.rds"))
card_return_correctionbigdata_best_group_three         <-readRDS(paste0(outputDataPath,filename="3card_return_correctionbigdataf_best_group.rds"))
card_ranked_return_correctionbigdata_best_group_three  <-readRDS(paste0(outputDataPath,filename="3card_ranked_return_correctionbigdataf_best_group.rds"))



card_post_returnbigdata_best_category_three               <-readRDS(paste0(outputDataPath,filename="3card_post_returnbigdataf_best_category.rds"))
card_post_ranked_returnbigdata_best_category_three        <-readRDS(paste0(outputDataPath,filename="3card_post_ranked_returnbigdataf_best_category.rds"))
post_returnbigdata_best_category_three                    <-readRDS(paste0(outputDataPath,filename="3post_returnbigdataf_best_category.rds"))
post_ranked_returnbigdata_best_category_three             <-readRDS(paste0(outputDataPath,filename="3post_ranked_returnbigdataf_best_category.rds"))
post_volatilitybigdata_best_category_three                <-readRDS(paste0(outputDataPath,filename="3post_volatilitybigdataf_best_category.rds"))
post_ranked_volatilitybigdata_best_category_three         <-readRDS(paste0(outputDataPath,filename="3post_ranked_volatilitybigdataf_best_category.rds"))
card_pre_returnbigdata_best_category_three                <-readRDS(paste0(outputDataPath,filename="3card_pre_returnbigdataf_best_category.rds"))
card_pre_ranked_returnbigdata_best_category_three         <-readRDS(paste0(outputDataPath,filename="3card_pre_ranked_returnbigdataf_best_category.rds"))
pre_returnbigdata_best_category_three                     <-readRDS(paste0(outputDataPath,filename="3pre_returnbigdataf_best_category.rds"))
pre_ranked_returnbigdata_best_category_three              <-readRDS(paste0(outputDataPath,filename="3pre_ranked_returnbigdataf_best_category.rds"))
pre_volatilitybigdata_best_category_three                 <-readRDS(paste0(outputDataPath,filename="3pre_volatilitybigdataf_best_category.rds"))
pre_ranked_volatilitybigdata_best_category_three          <-readRDS(paste0(outputDataPath,filename="3pre_ranked_volatilitybigdataf_best_category.rds"))
volatility_correctionbigdata_best_category_three          <-readRDS(paste0(outputDataPath,filename="3volatility_correctionbigdataf_best_category.rds"))
ranked_volatility_correctionbigdata_best_category_three   <-readRDS(paste0(outputDataPath,filename="3ranked_volatility_correctionbigdataf_best_category.rds"))
return_correctionbigdata_best_category_three              <-readRDS(paste0(outputDataPath,filename="3return_correctionbigdataf_best_category.rds"))
ranked_return_correctionbigdata_best_category_three       <-readRDS(paste0(outputDataPath,filename="3ranked_return_correctionbigdataf_best_category.rds"))
card_return_correctionbigdata_best_category_three         <-readRDS(paste0(outputDataPath,filename="3card_return_correctionbigdataf_best_category.rds"))
card_ranked_return_correctionbigdata_best_category_three  <-readRDS(paste0(outputDataPath,filename="3card_ranked_return_correctionbigdataf_best_category.rds"))



bigdataMappingList_three <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnbigdata_best_category_three,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnbigdata_best_category_three,       
    "CATEGORYPOSTRETURNNONE"=          post_returnbigdata_best_category_three,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnbigdata_best_category_three,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_category_three,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_category_three,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnbigdata_best_category_three,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_category_three,        
    "CATEGORYPRERETURNNONE"=           pre_returnbigdata_best_category_three,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnbigdata_best_category_three,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilitybigdata_best_category_three,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_category_three,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_category_three,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_category_three,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionbigdata_best_category_three,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_category_three,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_category_three,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_category_three,
    "GROUPPOSTRETURNCARD"=             card_post_returnbigdata_best_group_three,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnbigdata_best_group_three,       
    "GROUPPOSTRETURNNONE"=             post_returnbigdata_best_group_three,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnbigdata_best_group_three,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_group_three,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_group_three,    
    "GROUPPRERETURNCARD"=           card_pre_returnbigdata_best_group_three,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_group_three,        
    "GROUPPRERETURNNONE"=           pre_returnbigdata_best_group_three,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnbigdata_best_group_three,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilitybigdata_best_group_three,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_group_three,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_group_three,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_group_three,
    "GROUPPREPOSTRETURNNONE"=           return_correctionbigdata_best_group_three,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_group_three,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_group_three,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_group_three
  ) 

#######################
#######################
#######################
####################### end of big data lapse 3

#######################
#######################
#######################
####################### big data lapse 4

card_post_returnbigdata_best_group_four               <-readRDS(paste0(outputDataPath,filename="4card_post_returnbigdataf_best_group.rds"))
card_post_ranked_returnbigdata_best_group_four        <-readRDS(paste0(outputDataPath,filename="4card_post_ranked_returnbigdataf_best_group.rds"))
post_returnbigdata_best_group_four                    <-readRDS(paste0(outputDataPath,filename="4post_returnbigdataf_best_group.rds"))
post_ranked_returnbigdata_best_group_four             <-readRDS(paste0(outputDataPath,filename="4post_ranked_returnbigdataf_best_group.rds"))
post_volatilitybigdata_best_group_four                <-readRDS(paste0(outputDataPath,filename="4post_volatilitybigdataf_best_group.rds"))
post_ranked_volatilitybigdata_best_group_four         <-readRDS(paste0(outputDataPath,filename="4post_ranked_volatilitybigdataf_best_group.rds"))
card_pre_returnbigdata_best_group_four                <-readRDS(paste0(outputDataPath,filename="4card_pre_returnbigdataf_best_group.rds"))
card_pre_ranked_returnbigdata_best_group_four         <-readRDS(paste0(outputDataPath,filename="4card_pre_ranked_returnbigdataf_best_group.rds"))
pre_returnbigdata_best_group_four                     <-readRDS(paste0(outputDataPath,filename="4pre_returnbigdataf_best_group.rds"))
pre_ranked_returnbigdata_best_group_four              <-readRDS(paste0(outputDataPath,filename="4pre_ranked_returnbigdataf_best_group.rds"))
pre_volatilitybigdata_best_group_four                 <-readRDS(paste0(outputDataPath,filename="4pre_volatilitybigdataf_best_group.rds"))
pre_ranked_volatilitybigdata_best_group_four          <-readRDS(paste0(outputDataPath,filename="4pre_ranked_volatilitybigdataf_best_group.rds"))
volatility_correctionbigdata_best_group_four          <-readRDS(paste0(outputDataPath,filename="4volatility_correctionbigdataf_best_group.rds"))
ranked_volatility_correctionbigdata_best_group_four   <-readRDS(paste0(outputDataPath,filename="4ranked_volatility_correctionbigdataf_best_group.rds"))
return_correctionbigdata_best_group_four              <-readRDS(paste0(outputDataPath,filename="4return_correctionbigdataf_best_group.rds"))
ranked_return_correctionbigdata_best_group_four       <-readRDS(paste0(outputDataPath,filename="4ranked_return_correctionbigdataf_best_group.rds"))
card_return_correctionbigdata_best_group_four         <-readRDS(paste0(outputDataPath,filename="4card_return_correctionbigdataf_best_group.rds"))
card_ranked_return_correctionbigdata_best_group_four  <-readRDS(paste0(outputDataPath,filename="4card_ranked_return_correctionbigdataf_best_group.rds"))



card_post_returnbigdata_best_category_four               <-readRDS(paste0(outputDataPath,filename="4card_post_returnbigdataf_best_category.rds"))
card_post_ranked_returnbigdata_best_category_four        <-readRDS(paste0(outputDataPath,filename="4card_post_ranked_returnbigdataf_best_category.rds"))
post_returnbigdata_best_category_four                    <-readRDS(paste0(outputDataPath,filename="4post_returnbigdataf_best_category.rds"))
post_ranked_returnbigdata_best_category_four             <-readRDS(paste0(outputDataPath,filename="4post_ranked_returnbigdataf_best_category.rds"))
post_volatilitybigdata_best_category_four                <-readRDS(paste0(outputDataPath,filename="4post_volatilitybigdataf_best_category.rds"))
post_ranked_volatilitybigdata_best_category_four         <-readRDS(paste0(outputDataPath,filename="4post_ranked_volatilitybigdataf_best_category.rds"))
card_pre_returnbigdata_best_category_four                <-readRDS(paste0(outputDataPath,filename="4card_pre_returnbigdataf_best_category.rds"))
card_pre_ranked_returnbigdata_best_category_four         <-readRDS(paste0(outputDataPath,filename="4card_pre_ranked_returnbigdataf_best_category.rds"))
pre_returnbigdata_best_category_four                     <-readRDS(paste0(outputDataPath,filename="4pre_returnbigdataf_best_category.rds"))
pre_ranked_returnbigdata_best_category_four              <-readRDS(paste0(outputDataPath,filename="4pre_ranked_returnbigdataf_best_category.rds"))
pre_volatilitybigdata_best_category_four                 <-readRDS(paste0(outputDataPath,filename="4pre_volatilitybigdataf_best_category.rds"))
pre_ranked_volatilitybigdata_best_category_four          <-readRDS(paste0(outputDataPath,filename="4pre_ranked_volatilitybigdataf_best_category.rds"))
volatility_correctionbigdata_best_category_four          <-readRDS(paste0(outputDataPath,filename="4volatility_correctionbigdataf_best_category.rds"))
ranked_volatility_correctionbigdata_best_category_four   <-readRDS(paste0(outputDataPath,filename="4ranked_volatility_correctionbigdataf_best_category.rds"))
return_correctionbigdata_best_category_four              <-readRDS(paste0(outputDataPath,filename="4return_correctionbigdataf_best_category.rds"))
ranked_return_correctionbigdata_best_category_four       <-readRDS(paste0(outputDataPath,filename="4ranked_return_correctionbigdataf_best_category.rds"))
card_return_correctionbigdata_best_category_four         <-readRDS(paste0(outputDataPath,filename="4card_return_correctionbigdataf_best_category.rds"))
card_ranked_return_correctionbigdata_best_category_four  <-readRDS(paste0(outputDataPath,filename="4card_ranked_return_correctionbigdataf_best_category.rds"))






bigdataMappingList_four <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnbigdata_best_category_four,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnbigdata_best_category_four,       
    "CATEGORYPOSTRETURNNONE"=          post_returnbigdata_best_category_four,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnbigdata_best_category_four,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_category_four,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_category_four,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnbigdata_best_category_four,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_category_four,        
    "CATEGORYPRERETURNNONE"=           pre_returnbigdata_best_category_four,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnbigdata_best_category_four,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilitybigdata_best_category_four,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_category_four,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_category_four,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_category_four,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionbigdata_best_category_four,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_category_four,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_category_four,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_category_four,
    "GROUPPOSTRETURNCARD"=             card_post_returnbigdata_best_group_four,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnbigdata_best_group_four,       
    "GROUPPOSTRETURNNONE"=             post_returnbigdata_best_group_four,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnbigdata_best_group_four,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_group_four,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_group_four,    
    "GROUPPRERETURNCARD"=           card_pre_returnbigdata_best_group_four,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_group_four,        
    "GROUPPRERETURNNONE"=           pre_returnbigdata_best_group_four,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnbigdata_best_group_four,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilitybigdata_best_group_four,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_group_four,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_group_four,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_group_four,
    "GROUPPREPOSTRETURNNONE"=           return_correctionbigdata_best_group_four,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_group_four,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_group_four,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_group_four
  ) 

#######################
#######################
#######################
####################### end of big data lapse 4

#######################
#######################
#######################
####################### big data lapse 5

card_post_returnbigdata_best_group_five               <-readRDS(paste0(outputDataPath,filename="5card_post_returnbigdataf_best_group.rds"))
card_post_ranked_returnbigdata_best_group_five        <-readRDS(paste0(outputDataPath,filename="5card_post_ranked_returnbigdataf_best_group.rds"))
post_returnbigdata_best_group_five                    <-readRDS(paste0(outputDataPath,filename="5post_returnbigdataf_best_group.rds"))
post_ranked_returnbigdata_best_group_five             <-readRDS(paste0(outputDataPath,filename="5post_ranked_returnbigdataf_best_group.rds"))
post_volatilitybigdata_best_group_five                <-readRDS(paste0(outputDataPath,filename="5post_volatilitybigdataf_best_group.rds"))
post_ranked_volatilitybigdata_best_group_five         <-readRDS(paste0(outputDataPath,filename="5post_ranked_volatilitybigdataf_best_group.rds"))
card_pre_returnbigdata_best_group_five                <-readRDS(paste0(outputDataPath,filename="5card_pre_returnbigdataf_best_group.rds"))
card_pre_ranked_returnbigdata_best_group_five         <-readRDS(paste0(outputDataPath,filename="5card_pre_ranked_returnbigdataf_best_group.rds"))
pre_returnbigdata_best_group_five                     <-readRDS(paste0(outputDataPath,filename="5pre_returnbigdataf_best_group.rds"))
pre_ranked_returnbigdata_best_group_five              <-readRDS(paste0(outputDataPath,filename="5pre_ranked_returnbigdataf_best_group.rds"))
pre_volatilitybigdata_best_group_five                 <-readRDS(paste0(outputDataPath,filename="5pre_volatilitybigdataf_best_group.rds"))
pre_ranked_volatilitybigdata_best_group_five          <-readRDS(paste0(outputDataPath,filename="5pre_ranked_volatilitybigdataf_best_group.rds"))
volatility_correctionbigdata_best_group_five          <-readRDS(paste0(outputDataPath,filename="5volatility_correctionbigdataf_best_group.rds"))
ranked_volatility_correctionbigdata_best_group_five   <-readRDS(paste0(outputDataPath,filename="5ranked_volatility_correctionbigdataf_best_group.rds"))
return_correctionbigdata_best_group_five              <-readRDS(paste0(outputDataPath,filename="5return_correctionbigdataf_best_group.rds"))
ranked_return_correctionbigdata_best_group_five       <-readRDS(paste0(outputDataPath,filename="5ranked_return_correctionbigdataf_best_group.rds"))
card_return_correctionbigdata_best_group_five         <-readRDS(paste0(outputDataPath,filename="5card_return_correctionbigdataf_best_group.rds"))
card_ranked_return_correctionbigdata_best_group_five  <-readRDS(paste0(outputDataPath,filename="5card_ranked_return_correctionbigdataf_best_group.rds"))



card_post_returnbigdata_best_category_five               <-readRDS(paste0(outputDataPath,filename="5card_post_returnbigdataf_best_category.rds"))
card_post_ranked_returnbigdata_best_category_five        <-readRDS(paste0(outputDataPath,filename="5card_post_ranked_returnbigdataf_best_category.rds"))
post_returnbigdata_best_category_five                    <-readRDS(paste0(outputDataPath,filename="5post_returnbigdataf_best_category.rds"))
post_ranked_returnbigdata_best_category_five             <-readRDS(paste0(outputDataPath,filename="5post_ranked_returnbigdataf_best_category.rds"))
post_volatilitybigdata_best_category_five                <-readRDS(paste0(outputDataPath,filename="5post_volatilitybigdataf_best_category.rds"))
post_ranked_volatilitybigdata_best_category_five         <-readRDS(paste0(outputDataPath,filename="5post_ranked_volatilitybigdataf_best_category.rds"))
card_pre_returnbigdata_best_category_five                <-readRDS(paste0(outputDataPath,filename="5card_pre_returnbigdataf_best_category.rds"))
card_pre_ranked_returnbigdata_best_category_five         <-readRDS(paste0(outputDataPath,filename="5card_pre_ranked_returnbigdataf_best_category.rds"))
pre_returnbigdata_best_category_five                     <-readRDS(paste0(outputDataPath,filename="5pre_returnbigdataf_best_category.rds"))
pre_ranked_returnbigdata_best_category_five              <-readRDS(paste0(outputDataPath,filename="5pre_ranked_returnbigdataf_best_category.rds"))
pre_volatilitybigdata_best_category_five                 <-readRDS(paste0(outputDataPath,filename="5pre_volatilitybigdataf_best_category.rds"))
pre_ranked_volatilitybigdata_best_category_five          <-readRDS(paste0(outputDataPath,filename="5pre_ranked_volatilitybigdataf_best_category.rds"))
volatility_correctionbigdata_best_category_five          <-readRDS(paste0(outputDataPath,filename="5volatility_correctionbigdataf_best_category.rds"))
ranked_volatility_correctionbigdata_best_category_five   <-readRDS(paste0(outputDataPath,filename="5ranked_volatility_correctionbigdataf_best_category.rds"))
return_correctionbigdata_best_category_five              <-readRDS(paste0(outputDataPath,filename="5return_correctionbigdataf_best_category.rds"))
ranked_return_correctionbigdata_best_category_five       <-readRDS(paste0(outputDataPath,filename="5ranked_return_correctionbigdataf_best_category.rds"))
card_return_correctionbigdata_best_category_five         <-readRDS(paste0(outputDataPath,filename="5card_return_correctionbigdataf_best_category.rds"))
card_ranked_return_correctionbigdata_best_category_five  <-readRDS(paste0(outputDataPath,filename="5card_ranked_return_correctionbigdataf_best_category.rds"))






bigdataMappingList_five <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnbigdata_best_category_five,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnbigdata_best_category_five,       
    "CATEGORYPOSTRETURNNONE"=          post_returnbigdata_best_category_five,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnbigdata_best_category_five,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_category_five,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_category_five,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnbigdata_best_category_five,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_category_five,        
    "CATEGORYPRERETURNNONE"=           pre_returnbigdata_best_category_five,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnbigdata_best_category_five,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilitybigdata_best_category_five,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_category_five,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_category_five,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_category_five,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionbigdata_best_category_five,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_category_five,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_category_five,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_category_five,
    "GROUPPOSTRETURNCARD"=             card_post_returnbigdata_best_group_five,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnbigdata_best_group_five,       
    "GROUPPOSTRETURNNONE"=             post_returnbigdata_best_group_five,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnbigdata_best_group_five,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilitybigdata_best_group_five,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilitybigdata_best_group_five,    
    "GROUPPRERETURNCARD"=           card_pre_returnbigdata_best_group_five,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnbigdata_best_group_five,        
    "GROUPPRERETURNNONE"=           pre_returnbigdata_best_group_five,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnbigdata_best_group_five,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilitybigdata_best_group_five,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilitybigdata_best_group_five,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionbigdata_best_group_five,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionbigdata_best_group_five,
    "GROUPPREPOSTRETURNNONE"=           return_correctionbigdata_best_group_five,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionbigdata_best_group_five,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionbigdata_best_group_five,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionbigdata_best_group_five
  ) 

#######################
#######################
#######################


##################################################################################################################################################################################################################################################################################################################
##################################################################################################################################################################
######################################################################################################################################################################################################
################## RPNA file loading
####################################################################################################################################################################################
########################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################

rpna_dataTotr1000 <- readRDS(file=paste0(outputDataPath,"all_lapses_rpna_metrics_clean_prod.rds"))

rpna_all_group_events <- readRDS(file=paste0(outputDataPath,"prod_rpna_all_group_events.rds"))
rpna_all_category_events <- readRDS(file=paste0(outputDataPath,"prod_rpna_all_category_events.rds"))

metricsList <- 
  list(		  
    "POSTRETURNCARD"=          "card_post_return",              
    "POSTRETURNCARDRANK"=      "card_post_ranked_return",       
    "POSTRETURNNONE"=          "post_return",                   
    "POSTRETURNRANK"=          "post_ranked_return",            
    "POSTVOLATILITYNONE"=      "post_volatility",           
    "POSTVOLATILITYRANK"=      "post_ranked_volatility",    
    "PRERETURNCARD"=           "card_pre_return",               
    "PRERETURNCARDRANK"=       "card_pre_ranked_return",        
    "PRERETURNNONE"=           "pre_return",                     
    "PRERETURNRANK"=           "pre_ranked_return",             
    "PREVOLATILITYNONE"=       "pre_volatility",            
    "PREVOLATILITYRANK"=       "pre_ranked_volatility",     
    "PREPOSTVOLATILITYNONE"=   "volatility_correction",  
    "PREPOSTVOLATILITYRANK"=   "ranked_volatility_correction",
    "PREPOSTRETURNNONE"=       "return_correction",          
    "PREPOSTRETURNRANK"=       "ranked_return_correction",   
    "PREPOSTRETURNCARD"=       "card_return_correction",     
    "PREPOSTRETURNCARDRANK"=   "card_ranked_return_correction"
  ) 

#################
#################
#################
################# RPNA lapse -1

card_post_returnrpna_best_group_minus_one               <-readRDS(paste0(outputDataPath,filename="-1card_post_returnrpnaf_best_group.rds"))
card_post_ranked_returnrpna_best_group_minus_one        <-readRDS(paste0(outputDataPath,filename="-1card_post_ranked_returnrpnaf_best_group.rds"))
post_returnrpna_best_group_minus_one                    <-readRDS(paste0(outputDataPath,filename="-1post_returnrpnaf_best_group.rds"))
post_ranked_returnrpna_best_group_minus_one             <-readRDS(paste0(outputDataPath,filename="-1post_ranked_returnrpnaf_best_group.rds"))
post_volatilityrpna_best_group_minus_one                <-readRDS(paste0(outputDataPath,filename="-1post_volatilityrpnaf_best_group.rds"))
post_ranked_volatilityrpna_best_group_minus_one         <-readRDS(paste0(outputDataPath,filename="-1post_ranked_volatilityrpnaf_best_group.rds"))
card_pre_returnrpna_best_group_minus_one                <-readRDS(paste0(outputDataPath,filename="-1card_pre_returnrpnaf_best_group.rds"))
card_pre_ranked_returnrpna_best_group_minus_one         <-readRDS(paste0(outputDataPath,filename="-1card_pre_ranked_returnrpnaf_best_group.rds"))
pre_returnrpna_best_group_minus_one                     <-readRDS(paste0(outputDataPath,filename="-1pre_returnrpnaf_best_group.rds"))
pre_ranked_returnrpna_best_group_minus_one              <-readRDS(paste0(outputDataPath,filename="-1pre_ranked_returnrpnaf_best_group.rds"))
pre_volatilityrpna_best_group_minus_one                 <-readRDS(paste0(outputDataPath,filename="-1pre_volatilityrpnaf_best_group.rds"))
pre_ranked_volatilityrpna_best_group_minus_one          <-readRDS(paste0(outputDataPath,filename="-1pre_ranked_volatilityrpnaf_best_group.rds"))
volatility_correctionrpna_best_group_minus_one          <-readRDS(paste0(outputDataPath,filename="-1volatility_correctionrpnaf_best_group.rds"))
ranked_volatility_correctionrpna_best_group_minus_one   <-readRDS(paste0(outputDataPath,filename="-1ranked_volatility_correctionrpnaf_best_group.rds"))
return_correctionrpna_best_group_minus_one              <-readRDS(paste0(outputDataPath,filename="-1return_correctionrpnaf_best_group.rds"))
ranked_return_correctionrpna_best_group_minus_one       <-readRDS(paste0(outputDataPath,filename="-1ranked_return_correctionrpnaf_best_group.rds"))
card_return_correctionrpna_best_group_minus_one         <-readRDS(paste0(outputDataPath,filename="-1card_return_correctionrpnaf_best_group.rds"))
card_ranked_return_correctionrpna_best_group_minus_one  <-readRDS(paste0(outputDataPath,filename="-1card_ranked_return_correctionrpnaf_best_group.rds"))

card_post_returnrpna_best_category_minus_one               <-readRDS(paste0(outputDataPath,filename="-1card_post_returnrpnaf_best_category.rds"))
card_post_ranked_returnrpna_best_category_minus_one        <-readRDS(paste0(outputDataPath,filename="-1card_post_ranked_returnrpnaf_best_category.rds"))
post_returnrpna_best_category_minus_one                    <-readRDS(paste0(outputDataPath,filename="-1post_returnrpnaf_best_category.rds"))
post_ranked_returnrpna_best_category_minus_one             <-readRDS(paste0(outputDataPath,filename="-1post_ranked_returnrpnaf_best_category.rds"))
post_volatilityrpna_best_category_minus_one                <-readRDS(paste0(outputDataPath,filename="-1post_volatilityrpnaf_best_category.rds"))
post_ranked_volatilityrpna_best_category_minus_one         <-readRDS(paste0(outputDataPath,filename="-1post_ranked_volatilityrpnaf_best_category.rds"))
card_pre_returnrpna_best_category_minus_one                <-readRDS(paste0(outputDataPath,filename="-1card_pre_returnrpnaf_best_category.rds"))
card_pre_ranked_returnrpna_best_category_minus_one         <-readRDS(paste0(outputDataPath,filename="-1card_pre_ranked_returnrpnaf_best_category.rds"))
pre_returnrpna_best_category_minus_one                     <-readRDS(paste0(outputDataPath,filename="-1pre_returnrpnaf_best_category.rds"))
pre_ranked_returnrpna_best_category_minus_one              <-readRDS(paste0(outputDataPath,filename="-1pre_ranked_returnrpnaf_best_category.rds"))
pre_volatilityrpna_best_category_minus_one                 <-readRDS(paste0(outputDataPath,filename="-1pre_volatilityrpnaf_best_category.rds"))
pre_ranked_volatilityrpna_best_category_minus_one          <-readRDS(paste0(outputDataPath,filename="-1pre_ranked_volatilityrpnaf_best_category.rds"))
volatility_correctionrpna_best_category_minus_one          <-readRDS(paste0(outputDataPath,filename="-1volatility_correctionrpnaf_best_category.rds"))
ranked_volatility_correctionrpna_best_category_minus_one   <-readRDS(paste0(outputDataPath,filename="-1ranked_volatility_correctionrpnaf_best_category.rds"))
return_correctionrpna_best_category_minus_one              <-readRDS(paste0(outputDataPath,filename="-1return_correctionrpnaf_best_category.rds"))
ranked_return_correctionrpna_best_category_minus_one       <-readRDS(paste0(outputDataPath,filename="-1ranked_return_correctionrpnaf_best_category.rds"))
card_return_correctionrpna_best_category_minus_one         <-readRDS(paste0(outputDataPath,filename="-1card_return_correctionrpnaf_best_category.rds"))
card_ranked_return_correctionrpna_best_category_minus_one  <-readRDS(paste0(outputDataPath,filename="-1card_ranked_return_correctionrpnaf_best_category.rds"))


mappingList_minus_one <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnrpna_best_category_minus_one,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnrpna_best_category_minus_one,       
    "CATEGORYPOSTRETURNNONE"=          post_returnrpna_best_category_minus_one,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnrpna_best_category_minus_one,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilityrpna_best_category_minus_one,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_category_minus_one,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnrpna_best_category_minus_one,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_category_minus_one,        
    "CATEGORYPRERETURNNONE"=           pre_returnrpna_best_category_minus_one,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnrpna_best_category_minus_one,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilityrpna_best_category_minus_one,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_category_minus_one,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_category_minus_one,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_category_minus_one,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionrpna_best_category_minus_one,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_category_minus_one,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionrpna_best_category_minus_one,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_category_minus_one,
    "GROUPPOSTRETURNCARD"=             card_post_returnrpna_best_group_minus_one,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnrpna_best_group_minus_one,       
    "GROUPPOSTRETURNNONE"=             post_returnrpna_best_group_minus_one,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnrpna_best_group_minus_one,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilityrpna_best_group_minus_one,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_group_minus_one,    
    "GROUPPRERETURNCARD"=           card_pre_returnrpna_best_group_minus_one,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_group_minus_one,        
    "GROUPPRERETURNNONE"=           pre_returnrpna_best_group_minus_one,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnrpna_best_group_minus_one,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilityrpna_best_group_minus_one,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_group_minus_one,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_group_minus_one,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_group_minus_one,
    "GROUPPREPOSTRETURNNONE"=           return_correctionrpna_best_group_minus_one,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_group_minus_one,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionrpna_best_group_minus_one,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_group_minus_one
  ) 

#################
#################
#################
################# end of RPNA lapse -1

#################
#################
#################
################# RPNA lapse 2

card_post_returnrpna_best_group_two               <-readRDS(paste0(outputDataPath,filename="2card_post_returnrpnaf_best_group.rds"))
card_post_ranked_returnrpna_best_group_two        <-readRDS(paste0(outputDataPath,filename="2card_post_ranked_returnrpnaf_best_group.rds"))
post_returnrpna_best_group_two                    <-readRDS(paste0(outputDataPath,filename="2post_returnrpnaf_best_group.rds"))
post_ranked_returnrpna_best_group_two             <-readRDS(paste0(outputDataPath,filename="2post_ranked_returnrpnaf_best_group.rds"))
post_volatilityrpna_best_group_two                <-readRDS(paste0(outputDataPath,filename="2post_volatilityrpnaf_best_group.rds"))
post_ranked_volatilityrpna_best_group_two         <-readRDS(paste0(outputDataPath,filename="2post_ranked_volatilityrpnaf_best_group.rds"))
card_pre_returnrpna_best_group_two                <-readRDS(paste0(outputDataPath,filename="2card_pre_returnrpnaf_best_group.rds"))
card_pre_ranked_returnrpna_best_group_two         <-readRDS(paste0(outputDataPath,filename="2card_pre_ranked_returnrpnaf_best_group.rds"))
pre_returnrpna_best_group_two                     <-readRDS(paste0(outputDataPath,filename="2pre_returnrpnaf_best_group.rds"))
pre_ranked_returnrpna_best_group_two              <-readRDS(paste0(outputDataPath,filename="2pre_ranked_returnrpnaf_best_group.rds"))
pre_volatilityrpna_best_group_two                 <-readRDS(paste0(outputDataPath,filename="2pre_volatilityrpnaf_best_group.rds"))
pre_ranked_volatilityrpna_best_group_two          <-readRDS(paste0(outputDataPath,filename="2pre_ranked_volatilityrpnaf_best_group.rds"))
volatility_correctionrpna_best_group_two          <-readRDS(paste0(outputDataPath,filename="2volatility_correctionrpnaf_best_group.rds"))
ranked_volatility_correctionrpna_best_group_two   <-readRDS(paste0(outputDataPath,filename="2ranked_volatility_correctionrpnaf_best_group.rds"))
return_correctionrpna_best_group_two              <-readRDS(paste0(outputDataPath,filename="2return_correctionrpnaf_best_group.rds"))
ranked_return_correctionrpna_best_group_two       <-readRDS(paste0(outputDataPath,filename="2ranked_return_correctionrpnaf_best_group.rds"))
card_return_correctionrpna_best_group_two         <-readRDS(paste0(outputDataPath,filename="2card_return_correctionrpnaf_best_group.rds"))
card_ranked_return_correctionrpna_best_group_two  <-readRDS(paste0(outputDataPath,filename="2card_ranked_return_correctionrpnaf_best_group.rds"))

card_post_returnrpna_best_category_two               <-readRDS(paste0(outputDataPath,filename="2card_post_returnrpnaf_best_category.rds"))
card_post_ranked_returnrpna_best_category_two        <-readRDS(paste0(outputDataPath,filename="2card_post_ranked_returnrpnaf_best_category.rds"))
post_returnrpna_best_category_two                    <-readRDS(paste0(outputDataPath,filename="2post_returnrpnaf_best_category.rds"))
post_ranked_returnrpna_best_category_two             <-readRDS(paste0(outputDataPath,filename="2post_ranked_returnrpnaf_best_category.rds"))
post_volatilityrpna_best_category_two                <-readRDS(paste0(outputDataPath,filename="2post_volatilityrpnaf_best_category.rds"))
post_ranked_volatilityrpna_best_category_two         <-readRDS(paste0(outputDataPath,filename="2post_ranked_volatilityrpnaf_best_category.rds"))
card_pre_returnrpna_best_category_two                <-readRDS(paste0(outputDataPath,filename="2card_pre_returnrpnaf_best_category.rds"))
card_pre_ranked_returnrpna_best_category_two         <-readRDS(paste0(outputDataPath,filename="2card_pre_ranked_returnrpnaf_best_category.rds"))
pre_returnrpna_best_category_two                     <-readRDS(paste0(outputDataPath,filename="2pre_returnrpnaf_best_category.rds"))
pre_ranked_returnrpna_best_category_two              <-readRDS(paste0(outputDataPath,filename="2pre_ranked_returnrpnaf_best_category.rds"))
pre_volatilityrpna_best_category_two                 <-readRDS(paste0(outputDataPath,filename="2pre_volatilityrpnaf_best_category.rds"))
pre_ranked_volatilityrpna_best_category_two          <-readRDS(paste0(outputDataPath,filename="2pre_ranked_volatilityrpnaf_best_category.rds"))
volatility_correctionrpna_best_category_two          <-readRDS(paste0(outputDataPath,filename="2volatility_correctionrpnaf_best_category.rds"))
ranked_volatility_correctionrpna_best_category_two   <-readRDS(paste0(outputDataPath,filename="2ranked_volatility_correctionrpnaf_best_category.rds"))
return_correctionrpna_best_category_two              <-readRDS(paste0(outputDataPath,filename="2return_correctionrpnaf_best_category.rds"))
ranked_return_correctionrpna_best_category_two       <-readRDS(paste0(outputDataPath,filename="2ranked_return_correctionrpnaf_best_category.rds"))
card_return_correctionrpna_best_category_two         <-readRDS(paste0(outputDataPath,filename="2card_return_correctionrpnaf_best_category.rds"))
card_ranked_return_correctionrpna_best_category_two  <-readRDS(paste0(outputDataPath,filename="2card_ranked_return_correctionrpnaf_best_category.rds"))


mappingList_two <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnrpna_best_category_two,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnrpna_best_category_two,       
    "CATEGORYPOSTRETURNNONE"=          post_returnrpna_best_category_two,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnrpna_best_category_two,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilityrpna_best_category_two,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_category_two,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnrpna_best_category_two,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_category_two,        
    "CATEGORYPRERETURNNONE"=           pre_returnrpna_best_category_two,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnrpna_best_category_two,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilityrpna_best_category_two,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_category_two,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_category_two,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_category_two,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionrpna_best_category_two,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_category_two,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionrpna_best_category_two,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_category_two,
    "GROUPPOSTRETURNCARD"=             card_post_returnrpna_best_group_two,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnrpna_best_group_two,       
    "GROUPPOSTRETURNNONE"=             post_returnrpna_best_group_two,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnrpna_best_group_two,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilityrpna_best_group_two,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_group_two,    
    "GROUPPRERETURNCARD"=           card_pre_returnrpna_best_group_two,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_group_two,        
    "GROUPPRERETURNNONE"=           pre_returnrpna_best_group_two,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnrpna_best_group_two,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilityrpna_best_group_two,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_group_two,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_group_two,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_group_two,
    "GROUPPREPOSTRETURNNONE"=           return_correctionrpna_best_group_two,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_group_two,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionrpna_best_group_two,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_group_two
  ) 

#################
#################
#################
################# end of RPNA lapse 2
#################
#################
#################
################# RPNA lapse 3

card_post_returnrpna_best_group_three               <-readRDS(paste0(outputDataPath,filename="3card_post_returnrpnaf_best_group.rds"))
card_post_ranked_returnrpna_best_group_three        <-readRDS(paste0(outputDataPath,filename="3card_post_ranked_returnrpnaf_best_group.rds"))
post_returnrpna_best_group_three                    <-readRDS(paste0(outputDataPath,filename="3post_returnrpnaf_best_group.rds"))
post_ranked_returnrpna_best_group_three             <-readRDS(paste0(outputDataPath,filename="3post_ranked_returnrpnaf_best_group.rds"))
post_volatilityrpna_best_group_three                <-readRDS(paste0(outputDataPath,filename="3post_volatilityrpnaf_best_group.rds"))
post_ranked_volatilityrpna_best_group_three         <-readRDS(paste0(outputDataPath,filename="3post_ranked_volatilityrpnaf_best_group.rds"))
card_pre_returnrpna_best_group_three                <-readRDS(paste0(outputDataPath,filename="3card_pre_returnrpnaf_best_group.rds"))
card_pre_ranked_returnrpna_best_group_three         <-readRDS(paste0(outputDataPath,filename="3card_pre_ranked_returnrpnaf_best_group.rds"))
pre_returnrpna_best_group_three                     <-readRDS(paste0(outputDataPath,filename="3pre_returnrpnaf_best_group.rds"))
pre_ranked_returnrpna_best_group_three              <-readRDS(paste0(outputDataPath,filename="3pre_ranked_returnrpnaf_best_group.rds"))
pre_volatilityrpna_best_group_three                 <-readRDS(paste0(outputDataPath,filename="3pre_volatilityrpnaf_best_group.rds"))
pre_ranked_volatilityrpna_best_group_three          <-readRDS(paste0(outputDataPath,filename="3pre_ranked_volatilityrpnaf_best_group.rds"))
volatility_correctionrpna_best_group_three          <-readRDS(paste0(outputDataPath,filename="3volatility_correctionrpnaf_best_group.rds"))
ranked_volatility_correctionrpna_best_group_three   <-readRDS(paste0(outputDataPath,filename="3ranked_volatility_correctionrpnaf_best_group.rds"))
return_correctionrpna_best_group_three              <-readRDS(paste0(outputDataPath,filename="3return_correctionrpnaf_best_group.rds"))
ranked_return_correctionrpna_best_group_three       <-readRDS(paste0(outputDataPath,filename="3ranked_return_correctionrpnaf_best_group.rds"))
card_return_correctionrpna_best_group_three         <-readRDS(paste0(outputDataPath,filename="3card_return_correctionrpnaf_best_group.rds"))
card_ranked_return_correctionrpna_best_group_three  <-readRDS(paste0(outputDataPath,filename="3card_ranked_return_correctionrpnaf_best_group.rds"))

card_post_returnrpna_best_category_three               <-readRDS(paste0(outputDataPath,filename="3card_post_returnrpnaf_best_category.rds"))
card_post_ranked_returnrpna_best_category_three        <-readRDS(paste0(outputDataPath,filename="3card_post_ranked_returnrpnaf_best_category.rds"))
post_returnrpna_best_category_three                    <-readRDS(paste0(outputDataPath,filename="3post_returnrpnaf_best_category.rds"))
post_ranked_returnrpna_best_category_three             <-readRDS(paste0(outputDataPath,filename="3post_ranked_returnrpnaf_best_category.rds"))
post_volatilityrpna_best_category_three                <-readRDS(paste0(outputDataPath,filename="3post_volatilityrpnaf_best_category.rds"))
post_ranked_volatilityrpna_best_category_three         <-readRDS(paste0(outputDataPath,filename="3post_ranked_volatilityrpnaf_best_category.rds"))
card_pre_returnrpna_best_category_three                <-readRDS(paste0(outputDataPath,filename="3card_pre_returnrpnaf_best_category.rds"))
card_pre_ranked_returnrpna_best_category_three         <-readRDS(paste0(outputDataPath,filename="3card_pre_ranked_returnrpnaf_best_category.rds"))
pre_returnrpna_best_category_three                     <-readRDS(paste0(outputDataPath,filename="3pre_returnrpnaf_best_category.rds"))
pre_ranked_returnrpna_best_category_three              <-readRDS(paste0(outputDataPath,filename="3pre_ranked_returnrpnaf_best_category.rds"))
pre_volatilityrpna_best_category_three                 <-readRDS(paste0(outputDataPath,filename="3pre_volatilityrpnaf_best_category.rds"))
pre_ranked_volatilityrpna_best_category_three          <-readRDS(paste0(outputDataPath,filename="3pre_ranked_volatilityrpnaf_best_category.rds"))
volatility_correctionrpna_best_category_three          <-readRDS(paste0(outputDataPath,filename="3volatility_correctionrpnaf_best_category.rds"))
ranked_volatility_correctionrpna_best_category_three   <-readRDS(paste0(outputDataPath,filename="3ranked_volatility_correctionrpnaf_best_category.rds"))
return_correctionrpna_best_category_three              <-readRDS(paste0(outputDataPath,filename="3return_correctionrpnaf_best_category.rds"))
ranked_return_correctionrpna_best_category_three       <-readRDS(paste0(outputDataPath,filename="3ranked_return_correctionrpnaf_best_category.rds"))
card_return_correctionrpna_best_category_three         <-readRDS(paste0(outputDataPath,filename="3card_return_correctionrpnaf_best_category.rds"))
card_ranked_return_correctionrpna_best_category_three  <-readRDS(paste0(outputDataPath,filename="3card_ranked_return_correctionrpnaf_best_category.rds"))


mappingList_three <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnrpna_best_category_three,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnrpna_best_category_three,       
    "CATEGORYPOSTRETURNNONE"=          post_returnrpna_best_category_three,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnrpna_best_category_three,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilityrpna_best_category_three,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_category_three,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnrpna_best_category_three,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_category_three,        
    "CATEGORYPRERETURNNONE"=           pre_returnrpna_best_category_three,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnrpna_best_category_three,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilityrpna_best_category_three,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_category_three,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_category_three,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_category_three,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionrpna_best_category_three,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_category_three,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionrpna_best_category_three,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_category_three,
    "GROUPPOSTRETURNCARD"=             card_post_returnrpna_best_group_three,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnrpna_best_group_three,       
    "GROUPPOSTRETURNNONE"=             post_returnrpna_best_group_three,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnrpna_best_group_three,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilityrpna_best_group_three,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_group_three,    
    "GROUPPRERETURNCARD"=           card_pre_returnrpna_best_group_three,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_group_three,        
    "GROUPPRERETURNNONE"=           pre_returnrpna_best_group_three,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnrpna_best_group_three,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilityrpna_best_group_three,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_group_three,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_group_three,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_group_three,
    "GROUPPREPOSTRETURNNONE"=           return_correctionrpna_best_group_three,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_group_three,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionrpna_best_group_three,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_group_three
  ) 

#################
#################
#################
################# end of RPNA lapse 3

#################
#################
#################
################# RPNA lapse 4

card_post_returnrpna_best_group_four               <-readRDS(paste0(outputDataPath,filename="4card_post_returnrpnaf_best_group.rds"))
card_post_ranked_returnrpna_best_group_four        <-readRDS(paste0(outputDataPath,filename="4card_post_ranked_returnrpnaf_best_group.rds"))
post_returnrpna_best_group_four                    <-readRDS(paste0(outputDataPath,filename="4post_returnrpnaf_best_group.rds"))
post_ranked_returnrpna_best_group_four             <-readRDS(paste0(outputDataPath,filename="4post_ranked_returnrpnaf_best_group.rds"))
post_volatilityrpna_best_group_four                <-readRDS(paste0(outputDataPath,filename="4post_volatilityrpnaf_best_group.rds"))
post_ranked_volatilityrpna_best_group_four         <-readRDS(paste0(outputDataPath,filename="4post_ranked_volatilityrpnaf_best_group.rds"))
card_pre_returnrpna_best_group_four                <-readRDS(paste0(outputDataPath,filename="4card_pre_returnrpnaf_best_group.rds"))
card_pre_ranked_returnrpna_best_group_four         <-readRDS(paste0(outputDataPath,filename="4card_pre_ranked_returnrpnaf_best_group.rds"))
pre_returnrpna_best_group_four                     <-readRDS(paste0(outputDataPath,filename="4pre_returnrpnaf_best_group.rds"))
pre_ranked_returnrpna_best_group_four              <-readRDS(paste0(outputDataPath,filename="4pre_ranked_returnrpnaf_best_group.rds"))
pre_volatilityrpna_best_group_four                 <-readRDS(paste0(outputDataPath,filename="4pre_volatilityrpnaf_best_group.rds"))
pre_ranked_volatilityrpna_best_group_four          <-readRDS(paste0(outputDataPath,filename="4pre_ranked_volatilityrpnaf_best_group.rds"))
volatility_correctionrpna_best_group_four          <-readRDS(paste0(outputDataPath,filename="4volatility_correctionrpnaf_best_group.rds"))
ranked_volatility_correctionrpna_best_group_four   <-readRDS(paste0(outputDataPath,filename="4ranked_volatility_correctionrpnaf_best_group.rds"))
return_correctionrpna_best_group_four              <-readRDS(paste0(outputDataPath,filename="4return_correctionrpnaf_best_group.rds"))
ranked_return_correctionrpna_best_group_four       <-readRDS(paste0(outputDataPath,filename="4ranked_return_correctionrpnaf_best_group.rds"))
card_return_correctionrpna_best_group_four         <-readRDS(paste0(outputDataPath,filename="4card_return_correctionrpnaf_best_group.rds"))
card_ranked_return_correctionrpna_best_group_four  <-readRDS(paste0(outputDataPath,filename="4card_ranked_return_correctionrpnaf_best_group.rds"))

card_post_returnrpna_best_category_four               <-readRDS(paste0(outputDataPath,filename="4card_post_returnrpnaf_best_category.rds"))
card_post_ranked_returnrpna_best_category_four        <-readRDS(paste0(outputDataPath,filename="4card_post_ranked_returnrpnaf_best_category.rds"))
post_returnrpna_best_category_four                    <-readRDS(paste0(outputDataPath,filename="4post_returnrpnaf_best_category.rds"))
post_ranked_returnrpna_best_category_four             <-readRDS(paste0(outputDataPath,filename="4post_ranked_returnrpnaf_best_category.rds"))
post_volatilityrpna_best_category_four                <-readRDS(paste0(outputDataPath,filename="4post_volatilityrpnaf_best_category.rds"))
post_ranked_volatilityrpna_best_category_four         <-readRDS(paste0(outputDataPath,filename="4post_ranked_volatilityrpnaf_best_category.rds"))
card_pre_returnrpna_best_category_four                <-readRDS(paste0(outputDataPath,filename="4card_pre_returnrpnaf_best_category.rds"))
card_pre_ranked_returnrpna_best_category_four         <-readRDS(paste0(outputDataPath,filename="4card_pre_ranked_returnrpnaf_best_category.rds"))
pre_returnrpna_best_category_four                     <-readRDS(paste0(outputDataPath,filename="4pre_returnrpnaf_best_category.rds"))
pre_ranked_returnrpna_best_category_four              <-readRDS(paste0(outputDataPath,filename="4pre_ranked_returnrpnaf_best_category.rds"))
pre_volatilityrpna_best_category_four                 <-readRDS(paste0(outputDataPath,filename="4pre_volatilityrpnaf_best_category.rds"))
pre_ranked_volatilityrpna_best_category_four          <-readRDS(paste0(outputDataPath,filename="4pre_ranked_volatilityrpnaf_best_category.rds"))
volatility_correctionrpna_best_category_four          <-readRDS(paste0(outputDataPath,filename="4volatility_correctionrpnaf_best_category.rds"))
ranked_volatility_correctionrpna_best_category_four   <-readRDS(paste0(outputDataPath,filename="4ranked_volatility_correctionrpnaf_best_category.rds"))
return_correctionrpna_best_category_four              <-readRDS(paste0(outputDataPath,filename="4return_correctionrpnaf_best_category.rds"))
ranked_return_correctionrpna_best_category_four       <-readRDS(paste0(outputDataPath,filename="4ranked_return_correctionrpnaf_best_category.rds"))
card_return_correctionrpna_best_category_four         <-readRDS(paste0(outputDataPath,filename="4card_return_correctionrpnaf_best_category.rds"))
card_ranked_return_correctionrpna_best_category_four  <-readRDS(paste0(outputDataPath,filename="4card_ranked_return_correctionrpnaf_best_category.rds"))


mappingList_four <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnrpna_best_category_four,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnrpna_best_category_four,       
    "CATEGORYPOSTRETURNNONE"=          post_returnrpna_best_category_four,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnrpna_best_category_four,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilityrpna_best_category_four,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_category_four,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnrpna_best_category_four,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_category_four,        
    "CATEGORYPRERETURNNONE"=           pre_returnrpna_best_category_four,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnrpna_best_category_four,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilityrpna_best_category_four,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_category_four,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_category_four,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_category_four,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionrpna_best_category_four,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_category_four,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionrpna_best_category_four,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_category_four,
    "GROUPPOSTRETURNCARD"=             card_post_returnrpna_best_group_four,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnrpna_best_group_four,       
    "GROUPPOSTRETURNNONE"=             post_returnrpna_best_group_four,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnrpna_best_group_four,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilityrpna_best_group_four,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_group_four,    
    "GROUPPRERETURNCARD"=           card_pre_returnrpna_best_group_four,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_group_four,        
    "GROUPPRERETURNNONE"=           pre_returnrpna_best_group_four,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnrpna_best_group_four,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilityrpna_best_group_four,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_group_four,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_group_four,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_group_four,
    "GROUPPREPOSTRETURNNONE"=           return_correctionrpna_best_group_four,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_group_four,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionrpna_best_group_four,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_group_four
  ) 

#################
#################
#################
################# end of RPNA lapse 4

#################
#################
#################
################# RPNA lapse 5

card_post_returnrpna_best_group_five               <-readRDS(paste0(outputDataPath,filename="5card_post_returnrpnaf_best_group.rds"))
card_post_ranked_returnrpna_best_group_five        <-readRDS(paste0(outputDataPath,filename="5card_post_ranked_returnrpnaf_best_group.rds"))
post_returnrpna_best_group_five                    <-readRDS(paste0(outputDataPath,filename="5post_returnrpnaf_best_group.rds"))
post_ranked_returnrpna_best_group_five             <-readRDS(paste0(outputDataPath,filename="5post_ranked_returnrpnaf_best_group.rds"))
post_volatilityrpna_best_group_five                <-readRDS(paste0(outputDataPath,filename="5post_volatilityrpnaf_best_group.rds"))
post_ranked_volatilityrpna_best_group_five         <-readRDS(paste0(outputDataPath,filename="5post_ranked_volatilityrpnaf_best_group.rds"))
card_pre_returnrpna_best_group_five                <-readRDS(paste0(outputDataPath,filename="5card_pre_returnrpnaf_best_group.rds"))
card_pre_ranked_returnrpna_best_group_five         <-readRDS(paste0(outputDataPath,filename="5card_pre_ranked_returnrpnaf_best_group.rds"))
pre_returnrpna_best_group_five                     <-readRDS(paste0(outputDataPath,filename="5pre_returnrpnaf_best_group.rds"))
pre_ranked_returnrpna_best_group_five              <-readRDS(paste0(outputDataPath,filename="5pre_ranked_returnrpnaf_best_group.rds"))
pre_volatilityrpna_best_group_five                 <-readRDS(paste0(outputDataPath,filename="5pre_volatilityrpnaf_best_group.rds"))
pre_ranked_volatilityrpna_best_group_five          <-readRDS(paste0(outputDataPath,filename="5pre_ranked_volatilityrpnaf_best_group.rds"))
volatility_correctionrpna_best_group_five          <-readRDS(paste0(outputDataPath,filename="5volatility_correctionrpnaf_best_group.rds"))
ranked_volatility_correctionrpna_best_group_five   <-readRDS(paste0(outputDataPath,filename="5ranked_volatility_correctionrpnaf_best_group.rds"))
return_correctionrpna_best_group_five              <-readRDS(paste0(outputDataPath,filename="5return_correctionrpnaf_best_group.rds"))
ranked_return_correctionrpna_best_group_five       <-readRDS(paste0(outputDataPath,filename="5ranked_return_correctionrpnaf_best_group.rds"))
card_return_correctionrpna_best_group_five         <-readRDS(paste0(outputDataPath,filename="5card_return_correctionrpnaf_best_group.rds"))
card_ranked_return_correctionrpna_best_group_five  <-readRDS(paste0(outputDataPath,filename="5card_ranked_return_correctionrpnaf_best_group.rds"))

card_post_returnrpna_best_category_five               <-readRDS(paste0(outputDataPath,filename="5card_post_returnrpnaf_best_category.rds"))
card_post_ranked_returnrpna_best_category_five        <-readRDS(paste0(outputDataPath,filename="5card_post_ranked_returnrpnaf_best_category.rds"))
post_returnrpna_best_category_five                    <-readRDS(paste0(outputDataPath,filename="5post_returnrpnaf_best_category.rds"))
post_ranked_returnrpna_best_category_five             <-readRDS(paste0(outputDataPath,filename="5post_ranked_returnrpnaf_best_category.rds"))
post_volatilityrpna_best_category_five                <-readRDS(paste0(outputDataPath,filename="5post_volatilityrpnaf_best_category.rds"))
post_ranked_volatilityrpna_best_category_five         <-readRDS(paste0(outputDataPath,filename="5post_ranked_volatilityrpnaf_best_category.rds"))
card_pre_returnrpna_best_category_five                <-readRDS(paste0(outputDataPath,filename="5card_pre_returnrpnaf_best_category.rds"))
card_pre_ranked_returnrpna_best_category_five         <-readRDS(paste0(outputDataPath,filename="5card_pre_ranked_returnrpnaf_best_category.rds"))
pre_returnrpna_best_category_five                     <-readRDS(paste0(outputDataPath,filename="5pre_returnrpnaf_best_category.rds"))
pre_ranked_returnrpna_best_category_five              <-readRDS(paste0(outputDataPath,filename="5pre_ranked_returnrpnaf_best_category.rds"))
pre_volatilityrpna_best_category_five                 <-readRDS(paste0(outputDataPath,filename="5pre_volatilityrpnaf_best_category.rds"))
pre_ranked_volatilityrpna_best_category_five          <-readRDS(paste0(outputDataPath,filename="5pre_ranked_volatilityrpnaf_best_category.rds"))
volatility_correctionrpna_best_category_five          <-readRDS(paste0(outputDataPath,filename="5volatility_correctionrpnaf_best_category.rds"))
ranked_volatility_correctionrpna_best_category_five   <-readRDS(paste0(outputDataPath,filename="5ranked_volatility_correctionrpnaf_best_category.rds"))
return_correctionrpna_best_category_five              <-readRDS(paste0(outputDataPath,filename="5return_correctionrpnaf_best_category.rds"))
ranked_return_correctionrpna_best_category_five       <-readRDS(paste0(outputDataPath,filename="5ranked_return_correctionrpnaf_best_category.rds"))
card_return_correctionrpna_best_category_five         <-readRDS(paste0(outputDataPath,filename="5card_return_correctionrpnaf_best_category.rds"))
card_ranked_return_correctionrpna_best_category_five  <-readRDS(paste0(outputDataPath,filename="5card_ranked_return_correctionrpnaf_best_category.rds"))


mappingList_five <- 
  list(		  
    "CATEGORYPOSTRETURNCARD"=          card_post_returnrpna_best_category_five,              
    "CATEGORYPOSTRETURNCARDRANK"=      card_post_ranked_returnrpna_best_category_five,       
    "CATEGORYPOSTRETURNNONE"=          post_returnrpna_best_category_five,                   
    "CATEGORYPOSTRETURNRANK"=          post_ranked_returnrpna_best_category_five,            
    "CATEGORYPOSTVOLATILITYNONE"=          post_volatilityrpna_best_category_five,           
    "CATEGORYPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_category_five,    
    "CATEGORYPRERETURNCARD"=           card_pre_returnrpna_best_category_five,               
    "CATEGORYPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_category_five,        
    "CATEGORYPRERETURNNONE"=           pre_returnrpna_best_category_five,                     
    "CATEGORYPRERETURNRANK"=           pre_ranked_returnrpna_best_category_five,             
    "CATEGORYPREVOLATILITYNONE"=           pre_volatilityrpna_best_category_five,            
    "CATEGORYPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_category_five,     
    "CATEGORYPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_category_five,  
    "CATEGORYPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_category_five,
    "CATEGORYPREPOSTRETURNNONE"=           return_correctionrpna_best_category_five,          
    "CATEGORYPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_category_five,   
    "CATEGORYPREPOSTRETURNCARD"=       card_return_correctionrpna_best_category_five,     
    "CATEGORYPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_category_five,
    "GROUPPOSTRETURNCARD"=             card_post_returnrpna_best_group_five,              
    "GROUPPOSTRETURNCARDRANK"=         card_post_ranked_returnrpna_best_group_five,       
    "GROUPPOSTRETURNNONE"=             post_returnrpna_best_group_five,                   
    "GROUPPOSTRETURNRANK"=          post_ranked_returnrpna_best_group_five,            
    "GROUPPOSTVOLATILITYNONE"=          post_volatilityrpna_best_group_five,           
    "GROUPPOSTVOLATILITYRANK"=      post_ranked_volatilityrpna_best_group_five,    
    "GROUPPRERETURNCARD"=           card_pre_returnrpna_best_group_five,               
    "GROUPPRERETURNCARDRANK"=       card_pre_ranked_returnrpna_best_group_five,        
    "GROUPPRERETURNNONE"=           pre_returnrpna_best_group_five,                     
    "GROUPPRERETURNRANK"=           pre_ranked_returnrpna_best_group_five,             
    "GROUPPREVOLATILITYNONE"=           pre_volatilityrpna_best_group_five,            
    "GROUPPREVOLATILITYRANK"=       pre_ranked_volatilityrpna_best_group_five,     
    "GROUPPREPOSTVOLATILITYNONE"=       volatility_correctionrpna_best_group_five,  
    "GROUPPREPOSTVOLATILITYRANK"=       ranked_volatility_correctionrpna_best_group_five,
    "GROUPPREPOSTRETURNNONE"=           return_correctionrpna_best_group_five,          
    "GROUPPREPOSTRETURNRANK"=       ranked_return_correctionrpna_best_group_five,   
    "GROUPPREPOSTRETURNCARD"=       card_return_correctionrpna_best_group_five,     
    "GROUPPREPOSTRETURNCARDRANK"=   card_ranked_return_correctionrpna_best_group_five
  ) 

#################
#################
#################
################# end of RPNA lapse 5


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

getProperLapseDataset <- function(incoming_dataset,filtering_lapse = -1){
  my_lapse <- filtering_lapse
  if(filtering_lapse == 6){
    my_lapse <- -1
  }
  outputing_dataset <- incoming_dataset[incoming_dataset$lapse == my_lapse,]
  return(outputing_dataset)
}

getBestProfilesDataset <- function(aggregate_criteria,periods,methodo,weighting,analytics,lapses_filtering){
 
  df <- NULL
  if(weighting == "RANK+CARD"){
    weighting <- "CARD"
  }
  
  
  
  if (methodo == "VOLATILITY" && weighting == "CARD"){
    weighting <- "RANK"
  }
  toFetch <- paste0(aggregate_criteria,periods,methodo,weighting)
  print("Loading")
  print(toFetch)
  if (analytics == "RPNA"){
    if(lapses_filtering == 6){
      df <- mappingList_minus_one[[toFetch]]
    } 
    if(lapses_filtering == 2){
      df <- mappingList_two[[toFetch]]
    } 
    if(lapses_filtering == 3){
      df <- mappingList_three[[toFetch]]
    } 
    if(lapses_filtering == 4){
      df <- mappingList_four[[toFetch]]
    } 
    if(lapses_filtering == 5){
      df <- mappingList_five[[toFetch]]
    } 
    
    if(is.null(df)){
      df <- mappingList_minus_one[[toFetch]]
    }
    
  } else {
  

    if(lapses_filtering == 2){
      df <- bigdataMappingList_two[[toFetch]]
    }
    
    if(lapses_filtering == 3){
      df <- bigdataMappingList_three[[toFetch]]
    }
    if(lapses_filtering == 4){
      df <- bigdataMappingList_four[[toFetch]]
    }
    
    if(lapses_filtering == 5){
      df <- bigdataMappingList_five[[toFetch]]
    }
    
    if(lapses_filtering == 6){
      df <- bigdataMappingList_minus_one[[toFetch]]
    }
    
    if(is.null(df)){
      df <- bigdataMappingList_minus_one[[toFetch]]
    }
  }
  return(df)
}


getBestProfilesMetrics <- function(periods,methodo,weighting){
  
  if(weighting == "RANK+CARD"){
    weighting <- "CARD"
  }
  
  if (methodo == "VOLATILITY" && weighting == "CARD"){
    weighting <- "RANK"
  }
  toFetch <- paste0(periods,methodo,weighting)
  print("Loading")
  print(toFetch)
  metricsName <- metricsList[[toFetch]]
  return(metricsName)
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
  ToPlotDataFrame <- ToPlotDataFrame[(ToPlotDataFrame$variable != "ABNORMAL_THRESHOLD"),]
  ToPlotDataFrame <- ToPlotDataFrame[(ToPlotDataFrame$variable != "threshold"),]
  
  
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
  
  if ("ABNORMAL_THRESHOLD" %in% colnames(DataFrame)){
    g <- g + geom_hline(aes(yintercept=0.95),colour = 'black', size = 1.5,linetype="dashed")
  }
  
  if(sum((grepl("RET",colnames(DataFrame)))>0) | (sum(grepl("ret",colnames(DataFrame)))>0)){
    g <- g + geom_vline(aes(xintercept=0),colour = 'black', size = 1.5,linetype="dashed")
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
    dataframerets$RDBA_RETURNS <- 5*dataframerets$RDBA_RETURNS
    dataframerets$RPNA_RETURNS <- 5*dataframerets$RPNA_RETURNS
  } else {
    dataframerets <- dataFrame[,c("MINUTES","RETS")]
    colnames(dataframerets) <- c("MINUTES","RETURNS")
    dataframerets$RETURNS <- 5*dataframerets$RETURNS
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
  
#   dataframevol <- dataframevol+0.8
#   dataframevol$MINUTES <- dataframevol$MINUTES - 0.8

  translatingFactor <- (1.2-min(dataframevol[,-1]))
  dataframevol <- dataframevol+translatingFactor
  dataframevol$MINUTES <- dataframevol$MINUTES - translatingFactor
  
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
  
  # dataframervola <- dataframervola+0.8
  # dataframervola$MINUTES <- dataframervola$MINUTES - 0.8
  translatingFactor <- (1.2-min(dataframervola[,-1]))
  dataframervola <- dataframervola+translatingFactor
  dataframervola$MINUTES <- dataframervola$MINUTES - translatingFactor
  
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
    dataframerets[,c("MINUTES",paste0(event_one,"_return_1"),paste0(event_two,"_return_2"))] <- 5*dataframerets[,c("MINUTES",paste0(event_one,"_return_1"),paste0(event_two,"_return_2"))]
  } else {
    dataframerets <- dataFrame[,c("MINUTES","RETS")]
    colnames(dataframerets) <- c("MINUTES","RETURNS")
    dataframerets$RETURNS <- 5*dataframerets$RETURNS
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
  
  
#   
#   dataframevol <- dataframevol+0.8
#   dataframevol$MINUTES <- dataframevol$MINUTES - 0.8
  translatingFactor <- (1.2-min(dataframevol[,-1]))
  dataframevol <- dataframevol+translatingFactor
  dataframevol$MINUTES <- dataframevol$MINUTES - translatingFactor
  
  
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
#   dataframervola <- dataframervola+0.8
  # dataframervola$MINUTES <- dataframervola$MINUTES - 0.8
  translatingFactor <- (1.2-min(dataframervola[,-1]))
  dataframervola <- dataframervola+translatingFactor
  dataframervola$MINUTES <- dataframervola$MINUTES - translatingFactor
  
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


getPlotBestProfiles <- function(aggregate_criteria, data, all_group_events, all_category_events,suffix){
  plotLimit <- 6
  if (aggregate_criteria == "GROUP"){
    plotLimit <- 6
    all_events <- all_group_events
  } else {
    plotLimit <- 16
    all_events <- all_category_events
  }
  
  for (i in 1:min(plotLimit, dim(data)[1])){
    rowProfile <- data[i,]
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
    
    
    gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
    
    gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
    assign(paste0("gret", i), gret)
    assign(paste0("gvol", i), gvol)
  }
  width=20
  height=22.5
  gname <- NULL
  if (aggregate_criteria == "GROUP"){
    gname <- ExportMultiplot(gret1,gvol1,gret2,gvol2,gret3,gvol3,gret4,gvol4,gret5,gvol5,plotlist = NULL, filename = paste0("bestGroups",suffix), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  } else {
    gname <- ExportMultiplot(gret1,gvol1,
                             gret2,gvol2,
                             gret3,gvol3,
                             gret4,gvol4,
                             gret5,gvol5,
                             gret6,gvol6,
                             gret7,gvol7,
                             gret8,gvol8,
                             gret9,gvol9,
                             gret10,gvol10,
                             plotlist = NULL, filename = paste0("bestCategories",suffix), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  }
  # print("multiple plot done")
  return(gname)
  #   
  #   Counter <- 1
  #   Events <- NULL
  #   i <- 1
  #   fo
  #   
  #   while (Counter < plotLimit & i <= dim(data)[1]){
  #     rowProfile <- rpna_dataTotr1000[i,]
  #     stats_sign <- colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))]
  #     rets <- paste0("RET",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     ord_stats_sign <- paste0("ORD",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     vol_stats_sign <- paste0("VOLU",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     vola_stats_sign <- paste0("VOLA",colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     
  #     stats_sign <- rowProfile[,stats_sign]
  #     rets <- rowProfile[,rets]
  #     colnames(rets) <- colnames(stats_sign)
  #     ord_stats_sign <- rowProfile[,ord_stats_sign]
  #     colnames(ord_stats_sign) <- colnames(stats_sign)
  #     vol_stats_sign <- rowProfile[,vol_stats_sign]
  #     colnames(vol_stats_sign) <- colnames(stats_sign)
  #     vola_stats_sign <- rowProfile[,vola_stats_sign]
  #     colnames(vola_stats_sign) <- colnames(stats_sign)
  #     
  #     dataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
  #     colnames(dataframe) <- c("COR_STATS_SIGN","ORD_STATS_SIGN","VOLUME","VOLATILITY","RETS")
  #     dataframe$MINUTES <- as.numeric(colnames(rowProfile)[which(!is.na(as.numeric(colnames(rowProfile))))])
  #     dataframe <- dataframe[,c("MINUTES","COR_STATS_SIGN","ORD_STATS_SIGN","VOLUME","VOLATILITY","RETS")]
  #     
  #     
  #     gret <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
  #     
  #     gvol <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
  #     
  #     # eval.parent(expr = paste0("g", i, " <- g"))
  #     
  #     if((rowProfile$EVENT %in% all_events) & Counter < plotLimit){
  #       if (Counter == 1){
  #         assign(paste0("gret", Counter), gret)
  #         assign(paste0("gvol", Counter), gvol)
  #         Counter <- Counter+1
  #         Events <- c(Events,rowProfile$EVENT)
  #       } else if (!(rowProfile$EVENT %in% Events)){
  #         assign(paste0("gret", Counter), gret)
  #         assign(paste0("gvol", Counter), gvol)
  #         Counter <- Counter+1
  #         Events <- c(Events,rowProfile$EVENT)
  #       }
  #     }
  #     
  #     print(Counter)
  #     print(i)
  #     #     width=15
  #     #     height=9
  #     #     gout <- RP_ExportMultiplePlot(g,g, plotlist = NULL, filename = "best", outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 2, width = width, height = height)
  #     #     print(gout)
  #     print("profile displayed")
  #     i <- i+1
  #     
  #   }
  #   
  #   print("multiple plot aggregating done")
  #   width=15
  #   height=9
  #   g <- NULL
  #   if (aggregate_criteria == "GROUP"){
  #     g <- ExportMultiplot(gret1,gvol1,gret2,gvol2,gret3,gvol3,gret4,gvol4,gret5,gvol5,plotlist = NULL, filename = paste0("bestGroups"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  #   } else {
  #     g <- ExportMultiplot(gret1,gvol1,
  #                          gret2,gvol2,
  #                          gret3,gvol3,
  #                          gret4,gvol4,
  #                          gret5,gvol5,
  #                          gret6,gvol6,
  #                          gret7,gvol7,
  #                          gret8,gvol8,
  #                          gret9,gvol9,
  #                          gret10,gvol10,
  #                          plotlist = NULL, filename = paste0("bestCategories"), outputDataPath = paste0(outputDataPath,"BEST_PICTURES/"), cols = 5, width = width, height = height)
  #   }
  #   print("multiple plot done")
  #   return(g)
}

################################
################################
################################
################################
################################
################################
################################ End of Plotting functions


function(input, output, session) {
  column_to_plot <- c("my_event",
                      "relevance",
                      "event_relevance",
                      "sentiment_criteria",
                      "similarity_gap_filter",
                      "event_number_event_filtering",
                      "localSource",
                      "RANKING") 
  
  column_to_plot_name <-  c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SIMILARITY","SENTIMENT","COUNT","SOURCE", "RANKING")
  
  reorderedColumns <- c(column_to_plot, setdiff(colnames(rpna_dataTotr1000), column_to_plot))
  
  column_to_hide_indice <- which(!(reorderedColumns %in% column_to_plot))

  
  observe({

    if(input$mynavlist == "Comparative Plots"){
      shinyjs::enable("sec_ravenpack_type")
      shinyjs::enable("sec_aggregate_criteria")
      shinyjs::enable("sec_ravenpack_type")
      shinyjs::enable("sec_aggregate_criteria")
      shinyjs::enable("sec_event_number_event_filtering", "Minimum number of event observations")
      shinyjs::enable("sec_my_event")
      # shinyjs::enable("sec_my_plot")
      # shinyjs::enable("sec_sort_profiles")
      shinyjs::enable("sec_sentiment_criteria")
      shinyjs::enable("sec_relevance")
      shinyjs::enable("sec_event_relevance")
      shinyjs::enable("sec_similarity_gap_filter")
      shinyjs::enable("sec_localSource")
      shinyjs::enable("sec_filtering_criteria")
    } else {
      shinyjs::disable("sec_ravenpack_type")
      shinyjs::disable("sec_aggregate_criteria")
      shinyjs::disable("sec_ravenpack_type")
      shinyjs::disable("sec_aggregate_criteria")
      shinyjs::disable("sec_event_number_event_filtering", "Minimum number of event observations")
      shinyjs::disable("sec_my_event")
      # shinyjs::disable("sec_my_plot")
      # shinyjs::disable("sec_sort_profiles")
      shinyjs::disable("sec_sentiment_criteria")
      shinyjs::disable("sec_relevance")
      shinyjs::disable("sec_event_relevance")
      shinyjs::disable("sec_similarity_gap_filter")
      shinyjs::disable("sec_localSource")
      shinyjs::disable("sec_filtering_criteria")
    }
    
    
  })
  
  
  observeEvent(input$ravenpack_type,{
    
    if (input$ravenpack_type == "RBDA"){
      shinyjs::enable("event_relevance")
      if(input$similarity_gap_filter %in% c(0,1,7,30,90,186,365)){
        
        
        my_sim <-  input$similarity_gap_filter
        if(is.null(my_sim)){
          my_sim <- 0
        }
        if(my_sim == ""){
          my_sim <- 0
        }
        updateSelectInput(session, "similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90,186,365),
                          selected = my_sim
        )
        
      } else {
        updateSelectInput(session, "similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90,186,365),
                          selected = 0
        )
      }
    } else {
      shinyjs::disable("event_relevance")
      if(input$similarity_gap_filter %in% c(0,1,7,30,90)){
        my_sim <-  input$similarity_gap_filter
        if(is.null(my_sim)){
          my_sim <- 0
        }
        if(my_sim == ""){
          my_sim <- 0
        }
        updateSelectInput(session, "similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90),
                          selected = my_sim
        )
      } else {
        updateSelectInput(session, "similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90),
                          selected = 0
        )
      }
    }
    
    updatingList <- NULL
    dataTotr1000 <- NULL
    all_group_events <- NULL
    all_category_events <- NULL
    best_profile_group_ordered_r1000_corrado_df <- NULL
    best_profile_category_ordered_r1000_corrado_df <- NULL
    if (input$ravenpack_type == "RBDA"){
      dataTotr1000 <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
      # dataTotr2000 <- bigdata_dataTotr2000
      all_group_events <- bigdata_all_group_events
      all_category_events <- bigdata_all_category_events

    }
    
    if (input$ravenpack_type == "RPNA"){
      dataTotr1000 <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
      # dataTotr2000 <- rpna_dataTotr2000
      all_group_events <- rpna_all_group_events
      all_category_events <- rpna_all_category_events

    }
    
    
    #     if(input$sort_profiles){
    #       if(input$aggregate_criteria == "GROUP"){
    #         updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
    #       }
    #       if(input$aggregate_criteria == "CATEGORY"){
    #         updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
    #       }
    #     } else {
    #       if(input$aggregate_criteria == "GROUP"){
    #         updatingList <- all_group_events
    #       }
    #       if(input$aggregate_criteria == "CATEGORY"){
    #         updatingList <- all_category_events
    #       }
    #     }
    #     
    #     updateSelectInput(session, "my_event",
    #                       label ="EVENT",
    #                       choices = updatingList,
    #                       selected = input$my_event
    #     )
    #   })  
    
    updatingList <- NULL
    if(input$aggregate_criteria == "GROUP"){
      updatingList <- all_group_events
    }
    if(input$aggregate_criteria == "CATEGORY"){
      updatingList <- all_category_events
    }
    
    
    my_selection <- input$my_event
    
    if(is.null(my_selection)){
      my_selection <- updatingList[1]
    } 
      
    if(!(my_selection %in% updatingList)){
      my_selection <- updatingList[1]
    } 
    
    updateSelectInput(session, "my_event",
                      label ="EVENT",
                      choices = updatingList,
                      selected = my_selection
    )
  })  
  
  observeEvent(input$sec_ravenpack_type,{
    
    if (input$sec_ravenpack_type == "RBDA"){
      shinyjs::enable("sec_event_relevance")
      if(input$sec_similarity_gap_filter %in% c(0,1,7,30,90,186,365)){
        
        my_sim <- input$sec_similarity_gap_filter
        if(is.null(my_sim)){
          my_sim <- 0
        }
        if(my_sim == ""){
          my_sim <- 0
        }
        updateSelectInput(session, "sec_similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90,186,365),
                          selected = my_sim
        )
        
      } else {

        updateSelectInput(session, "sec_similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90,186,365),
                          selected = 0
        )
      }
    } else {
      shinyjs::disable("sec_event_relevance")
      if(input$sec_similarity_gap_filter %in% c(0,1,7,30,90)){
        my_sim <- input$sec_similarity_gap_filter
        if(is.null(my_sim)){
          my_sim <- 0
        }
        if(my_sim == ""){
          my_sim <- 0
        }
        updateSelectInput(session, "sec_similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90),
                          selected = my_sim
        )
      } else {

        print(0)
        updateSelectInput(session, "sec_similarity_gap_filter",
                          label ="SIMILARITY EVENT DAYS",
                          choices = c(0,1,7,30,90),
                          selected = 0
        )
      }
    }
    
    updatingList <- NULL
    dataTotr1000 <- NULL
    all_group_events <- NULL
    all_category_events <- NULL
    best_profile_group_ordered_r1000_corrado_df <- NULL
    best_profile_category_ordered_r1000_corrado_df <- NULL
    if (input$sec_ravenpack_type == "RBDA"){
      dataTotr1000 <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
      # dataTotr2000 <- bigdata_dataTotr2000
      all_group_events <- bigdata_all_group_events
      all_category_events <- bigdata_all_category_events

    }
    
    if (input$sec_ravenpack_type == "RPNA"){
      dataTotr1000 <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
      # dataTotr2000 <- rpna_dataTotr2000
      all_group_events <- rpna_all_group_events
      all_category_events <- rpna_all_category_events

    }
    
    
    #     if(input$sec_sort_profiles){
    #       if(input$sec_aggregate_criteria == "GROUP"){
    #         updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
    #       }
    #       if(input$sec_aggregate_criteria == "CATEGORY"){
    #         updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
    #       }
    #     } else {
    #       if(input$sec_aggregate_criteria == "GROUP"){
    #         updatingList <- all_group_events
    #       }
    #       if(input$sec_aggregate_criteria == "CATEGORY"){
    #         updatingList <- all_category_events
    #       }
    #     }
    #     
    #     updateSelectInput(session, "sec_my_event",
    #                       label ="EVENT",
    #                       choices = updatingList,
    #                       selected = input$sec_my_event
    #     )
    
    updatingList <- NULL
    if(input$sec_aggregate_criteria == "GROUP"){
      updatingList <- all_group_events
    }
    if(input$sec_aggregate_criteria == "CATEGORY"){
      updatingList <- all_category_events
    }
    
    my_selection <-  input$sec_my_event
    
    
    
    if(is.null(my_selection)){
      my_selection <- updatingList[1]
    } 
    
    if(!(my_selection %in% updatingList)){
      my_selection <- updatingList[1]
    } 
    
    updateSelectInput(session, "sec_my_event",
                      label ="EVENT",
                      choices = updatingList,
                      selected = my_selection
    )
  })  
  
  observeEvent(input$my_plot,{
    updateSelectInput(session, "sec_my_plot",
                      label ="PLOT TYPE",
                      selected = input$my_plot
    )
  })
  
  
  observeEvent(input$aggregate_criteria,{
    if(input$mynavlist != "Comparative Plots"){
      output$global_best_profiles_table <-   DT::renderDataTable(v$firstGlobalBestdata,selection='single',server = FALSE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
    } 
  })
  observeEvent(input$my_event,{
    #######
    #######
    ####### Updating the sentimentn criteria
    
    if(input$mynavlist != "Comparative Plots"){
      # dataTableProxy('best_profiles_table') %>% selectRows(1)
      # dataTableProxy(w$firstBestData) %>% selectRows(1)
      output$best_profiles_table <-   DT::renderDataTable(w$firstBestData,selection='single',server = FALSE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
      # output$global_best_profiles_table <-   DT::renderDataTable(v$firstGlobalBestdata,selection='single',server = TRUE,options=list(colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
      
      # dataTableProxy(output$best_profiles_table) %>% selectRows(1)
      # selectRows(dataTableProxy(output$best_profiles_table),1)
    }
    
    
    seconddata <- NULL
    
    if (input$ravenpack_type == "RBDA"){
      seconddata <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    } else {
      seconddata <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    
    seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    seconddataf <- seconddata[seconddata$my_event == input$my_event,]
    ####################
    ####################
    #################### updating sentiment
    restricted_sentiment_universe <- unique(seconddataf$sentiment_criteria)
    toSelect <- NULL
    if(length(restricted_sentiment_universe) == 1){
      toSelect <- restricted_sentiment_universe
    }
    
    if(is.null(toSelect)){
      toSelect <- "POSITIVE"
    }
    
    updateSelectInput(session, "sentiment_criteria",
                      label ="SENTIMENT",
                      choices = restricted_sentiment_universe,
                      selected = toSelect
    )
    
    ####################
    ####################
    #################### similarity gap
    restricted_similarity_universe <- unique(seconddataf$similarity_gap_filter)
    
    if(input$similarity_gap_filter %in% restricted_similarity_universe){
      
      
      my_sim <- input$similarity_gap_filter
      if(is.null(my_sim)){
        my_sim <- 0
      }
      if(my_sim == ""){
        my_sim <- 0
      }
      
      updateSelectInput(session, "similarity_gap_filter",
                        label ="SIMILARITY EVENT DAYS",
                        choices = restricted_similarity_universe,
                        selected = my_sim
      )
      
    } else {
      
      my_sim <- min(restricted_similarity_universe)
      if(is.null(my_sim)){
        my_sim <- 0
      }
      if(my_sim == ""){
        my_sim <- 0
      }
      updateSelectInput(session, "similarity_gap_filter",
                        label ="SIMILARITY EVENT DAYS",
                        choices = restricted_similarity_universe,
                        selected = my_sim
      )
    }
    
    
  })
  
  observeEvent(input$sec_my_event,{
    #######
    #######
    ####### Updating the sentimentn criteria
    seconddata <- NULL
    
    if (input$sec_ravenpack_type == "RBDA"){
      seconddata <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
    } else {
      seconddata <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
    }
    
    seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$sec_event_number_event_filtering),]
    seconddataf <- seconddata[seconddata$my_event == input$sec_my_event,]
    
    restricted_sentiment_universe <- unique(seconddataf$sentiment_criteria)
    toSelect <- NULL
    if(length(restricted_sentiment_universe) == 1){
      toSelect <- restricted_sentiment_universe
    }
    if(is.null(toSelect)){
      toSelect <- "POSITIVE"
    }
    updateSelectInput(session, "sec_sentiment_criteria",
                      label ="SENTIMENT",
                      choices = unique(seconddataf$sentiment_criteria),
                      selected = toSelect
    )
    
    ####################
    ####################
    #################### similarity gap
    restricted_similarity_universe <- unique(seconddataf$similarity_gap_filter)
    
    my_sim <- input$sec_similarity_gap_filter
    if(is.null(my_sim)){
      my_sim <- 0
    }
    if(my_sim == ""){
      my_sim <- 0
    }

    updateSelectInput(session, "sec_similarity_gap_filter",
                      label = "SIMILARITY EVENT DAYS",
                      choices = restricted_similarity_universe,
                      selected = my_sim
    )
    
  })
  
  
  
  observeEvent(input$aggregate_criteria,{
    updatingList <- NULL
    dataTotr1000 <- NULL
    all_group_events <- NULL
    all_category_events <- NULL
    best_profile_group_ordered_r1000_corrado_df <- NULL
    best_profile_category_ordered_r1000_corrado_df <- NULL
    if (input$ravenpack_type == "RBDA"){
      dataTotr1000 <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
      # dataTotr2000 <- bigdata_dataTotr2000
      all_group_events <- bigdata_all_group_events
      all_category_events <- bigdata_all_category_events

    }
    
    if (input$ravenpack_type == "RPNA"){
      dataTotr1000 <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
      # dataTotr2000 <- rpna_dataTotr2000
      all_group_events <- rpna_all_group_events
      all_category_events <- rpna_all_category_events

    }
    
    
    #     if(input$sort_profiles){
    #       if(input$aggregate_criteria == "GROUP"){
    #         updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
    #       }
    #       if(input$aggregate_criteria == "CATEGORY"){
    #         updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
    #       }
    #     } else {
    #       if(input$aggregate_criteria == "GROUP"){
    #         updatingList <- all_group_events
    #       }
    #       if(input$aggregate_criteria == "CATEGORY"){
    #         updatingList <- all_category_events
    #       }
    #     }
    #     
    #     updateSelectInput(session, "my_event",
    #                       label ="EVENT",
    #                       choices = updatingList,
    #                       selected = input$my_event
    #     )
    
    updatingList <- NULL
    if(input$aggregate_criteria == "GROUP"){
      updatingList <- all_group_events
    }
    if(input$aggregate_criteria == "CATEGORY"){
      updatingList <- all_category_events
    }
    
    my_selection <-  
      input$my_event
    
    if(is.null(my_selection)){
      my_selection <- updatingList[1]
    } 
    if(!(my_selection %in% updatingList)){
      my_selection <- updatingList[1]
    } 
    updateSelectInput(session, "my_event",
                      label ="EVENT",
                      choices = updatingList,
                      selected = my_selection
    )
    
  })
  
  #   observeEvent(input$sort_profiles,{
  #     updatingList <- NULL
  #     dataTotr1000 <- NULL
  #     all_group_events <- NULL
  #     all_category_events <- NULL
  #     best_profile_group_ordered_r1000_corrado_df <- NULL
  #     best_profile_category_ordered_r1000_corrado_df <- NULL
  #     if (input$ravenpack_type == "RBDA"){
  #       dataTotr1000 <- bigdata_dataTotr1000
  #       # dataTotr2000 <- bigdata_dataTotr2000
  #       all_group_events <- bigdata_all_group_events
  #       all_category_events <- bigdata_all_category_events
  #       best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
  #       # best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
  #       best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
  #       # best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
  #     }
  #     
  #     if (input$ravenpack_type == "RPNA"){
  #       dataTotr1000 <- rpna_dataTotr1000
  #       # dataTotr2000 <- rpna_dataTotr2000
  #       all_group_events <- rpna_all_group_events
  #       all_category_events <- rpna_all_category_events
  #       best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
  #       # best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
  #       best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
  #       # best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
  #     }
  #     
  #     
  #     if(input$sort_profiles){
  #       if(input$aggregate_criteria == "GROUP"){
  #         updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
  #       }
  #       if(input$aggregate_criteria == "CATEGORY"){
  #         updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
  #       }
  #     } else {
  #       if(input$aggregate_criteria == "GROUP"){
  #         updatingList <- all_group_events
  #       }
  #       if(input$aggregate_criteria == "CATEGORY"){
  #         updatingList <- all_category_events
  #       }
  #     }
  #     
  #     updateSelectInput(session, "my_event",
  #                       label ="EVENT",
  #                       choices = updatingList,
  #                       selected = input$my_event
  #     )
  #   })
  
  observeEvent(input$sec_aggregate_criteria,{
    updatingList <- NULL
    dataTotr1000 <- NULL
    all_group_events <- NULL
    all_category_events <- NULL
    best_profile_group_ordered_r1000_corrado_df <- NULL
    best_profile_category_ordered_r1000_corrado_df <- NULL
    if (input$sec_ravenpack_type == "RBDA"){
      dataTotr1000 <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
      # dataTotr2000 <- bigdata_dataTotr2000
      all_group_events <- bigdata_all_group_events
      all_category_events <- bigdata_all_category_events

    }
    
    if (input$sec_ravenpack_type == "RPNA"){
      dataTotr1000 <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
      # dataTotr2000 <- rpna_dataTotr2000
      all_group_events <- rpna_all_group_events
      all_category_events <- rpna_all_category_events

    }
    
    
    #     if(input$sec_sort_profiles){
    #       if(input$sec_aggregate_criteria == "GROUP"){
    #         updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
    #       }
    #       if(input$sec_aggregate_criteria == "CATEGORY"){
    #         updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
    #       }
    #     } else {
    #       if(input$sec_aggregate_criteria == "GROUP"){
    #         updatingList <- all_group_events
    #       }
    #       if(input$sec_aggregate_criteria == "CATEGORY"){
    #         updatingList <- all_category_events
    #       }
    #     }
    #     
    #     updateSelectInput(session, "sec_my_event",
    #                       label ="EVENT",
    #                       choices = updatingList,
    #                       selected = input$sec_my_event
    #     )
    
    updatingList <- NULL
    if(input$sec_aggregate_criteria == "GROUP"){
      updatingList <- all_group_events
    }
    if(input$sec_aggregate_criteria == "CATEGORY"){
      updatingList <- all_category_events
    }
    
    my_selection <-  
      input$sec_my_event
    
    if(is.null(my_selection)){
      my_selection <- updatingList[1]
    } 
    if(!(my_selection %in% updatingList)){
      my_selection <- updatingList[1]
    } 
    updateSelectInput(session, "sec_my_event",
                      label ="EVENT",
                      choices = updatingList,
                      selected = my_selection
    )
    
  })
  
#   observeEvent(input$sec_sort_profiles,{
#     updatingList <- NULL
#     dataTotr1000 <- NULL
#     all_group_events <- NULL
#     all_category_events <- NULL
#     best_profile_group_ordered_r1000_corrado_df <- NULL
#     best_profile_category_ordered_r1000_corrado_df <- NULL
#     if (input$sec_ravenpack_type == "RBDA"){
#       dataTotr1000 <- bigdata_dataTotr1000
#       # dataTotr2000 <- bigdata_dataTotr2000
#       all_group_events <- bigdata_all_group_events
#       all_category_events <- bigdata_all_category_events
#       best_profile_group_ordered_r1000_corrado_df <- bigdata_best_profile_group_ordered_r1000_corrado_df
#       # best_profile_group_ordered_r2000_corrado_df <- bigdata_best_profile_group_ordered_r2000_corrado_df
#       best_profile_category_ordered_r1000_corrado_df <- bigdata_best_profile_category_ordered_r1000_corrado_df
#       # best_profile_category_ordered_r2000_corrado_df <- bigdata_best_profile_category_ordered_r2000_corrado_df
#     }
#     
#     if (input$sec_ravenpack_type == "RPNA"){
#       dataTotr1000 <- rpna_dataTotr1000
#       # dataTotr2000 <- rpna_dataTotr2000
#       all_group_events <- rpna_all_group_events
#       all_category_events <- rpna_all_category_events
#       best_profile_group_ordered_r1000_corrado_df <- rpna_best_profile_group_ordered_r1000_corrado_df
#       # best_profile_group_ordered_r2000_corrado_df <- rpna_best_profile_group_ordered_r2000_corrado_df
#       best_profile_category_ordered_r1000_corrado_df <- rpna_best_profile_category_ordered_r1000_corrado_df
#       # best_profile_category_ordered_r2000_corrado_df <- rpna_best_profile_category_ordered_r2000_corrado_df
#     }
#     
#     
#     if(input$sec_sort_profiles){
#       if(input$sec_aggregate_criteria == "GROUP"){
#         updatingList <- best_profile_group_ordered_r1000_corrado_df$my_event
#       }
#       if(input$sec_aggregate_criteria == "CATEGORY"){
#         updatingList <- best_profile_category_ordered_r1000_corrado_df$my_event
#       }
#     } else {
#       if(input$sec_aggregate_criteria == "GROUP"){
#         updatingList <- all_group_events
#       }
#       if(input$sec_aggregate_criteria == "CATEGORY"){
#         updatingList <- all_category_events
#       }
#     }
#     
#     updateSelectInput(session, "sec_my_event",
#                       label ="EVENT",
#                       choices = updatingList,
#                       selected = input$sec_my_event
#     )
#   })
  
  
  
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ################################################################################################## Refreshing GUI
  output$sec_filtering_criteria <- renderText({ 
    ###################
    ###################
    ###################
    ################### Plotting together
    seconddata <- NULL
    
    if (input$sec_ravenpack_type == "RBDA"){
      seconddata <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
    } else {
      seconddata <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
    }
    
    seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$sec_event_number_event_filtering),]
    seconddataf <- seconddata[seconddata$my_event == input$sec_my_event,]
    
    if (dim(seconddataf)[1]>0){
      seconddataf <- seconddataf[order(seconddataf$infinity_return,decreasing = TRUE),]
      rowProfile <- seconddataf[1,]    
      ############# Right text field    
      #       if(input$sec_sentiment_criteria == "BEST"){
      #         outputLyrics <- ""
      #         
      #         outputLyrics <- paste0("Source : ",rowProfile$localSource," ,Sector : ",rowProfile$gics_sector," ,Sentiment : ",rowProfile$sentiment_criteria," ,Similarity days : ",rowProfile$similarity_gap_filter,
      #                                " ,Relevance : ",rowProfile$relevance, " ,Event relevance : ",rowProfile$event_relevance, " ,Nb Events  : ",rowProfile$event_number_event_filtering)
      #         return(outputLyrics)
      #       } else {
      seconddataf <- seconddataf[seconddataf$localSource == input$sec_localSource,]
      seconddataf <- seconddataf[seconddataf$similarity_gap_filter == input$sec_similarity_gap_filter,]
      seconddataf <- seconddataf[seconddataf$sentiment_criteria == input$sec_sentiment_criteria,]
      seconddataf <- seconddataf[seconddataf$relevance == input$sec_relevance,]
      seconddataf <- seconddataf[seconddataf$event_relevance == input$sec_event_relevance,]
      
      return(paste0("Your selection applies ,Nb Events  : ",seconddataf$event_number_event_filtering))
      # }
    } 
    
  })
  
  
  #   observeEvent(input$best_profiles_table_rows_selected, {
  #     print("##############")
  #     print(head(input$best_profiles_table))
  #     print(input$best_profiles_table[input$best_profiles_table_rows_selected,])
  #     
  #     output$bestProfilePlot <- renderPlot({
  #       plot(1:10,1:10)
  #     })
  #       
  #     
  #   })
  
  output$globalBestProfilePlotRet <- renderPlot({
    selectedrowindex <- input$global_best_profiles_table_rows_selected
    #           print("########")
    #           print(selectedrowindex)
    if(!is.null(selectedrowindex)){
      currentTable <- as.data.frame(v$firstGlobalBestdata)
      
      rowProfile <- currentTable[selectedrowindex,]
      
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
      #             print("##############")
      #             print(dim(rowProfile))
      #             print(rowProfile$EVENT)
      #       save(rowProfile,dataframe,file=paste0(outputDataPath,"bestPlotDebugBis.RData"))
      g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
      return(g)    
      
    }
  })
  
  output$bestProfilePlotRet <- renderPlot({
    selectedrowindex <- input$best_profiles_table_rows_selected
    if(!is.null(selectedrowindex)){
      currentTable <- as.data.frame(w$firstBestData)
      
      rowProfile <- currentTable[selectedrowindex,]
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
      #       print("##############")
      #       print(rowProfile)
      #       save(rowProfile,dataframe,file=paste0(outputDataPath,"bestPlotDebugBis.RData"))
      g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
      return(g)    
      
    }
  })
  
  output$globalBestProfilePlotStat <- renderPlot({
    selectedrowindex <- input$global_best_profiles_table_rows_selected
    if(!is.null(selectedrowindex)){
      currentTable <- as.data.frame(v$firstGlobalBestdata)
      
      rowProfile <- currentTable[selectedrowindex,]
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
      
      g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
      return(g)    
      
    }
  })
  
  
  output$bestProfilePlotStat <- renderPlot({
    selectedrowindex <- input$best_profiles_table_rows_selected
    if(!is.null(selectedrowindex)){
      currentTable <- as.data.frame(w$firstBestData)
      
      rowProfile <- currentTable[selectedrowindex,]
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
      g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
      return(g)    
      
    }
  })
  
  output$globalBestProfilePlotVola <- renderPlot({
    selectedrowindex <- input$global_best_profiles_table_rows_selected
    if(!is.null(selectedrowindex)){
      currentTable <- as.data.frame(v$firstGlobalBestdata)
      
      rowProfile <- currentTable[selectedrowindex,]
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
      g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
      return(g)    
      
    }
  })
  
  output$bestProfilePlotVola <- renderPlot({
    selectedrowindex <- input$best_profiles_table_rows_selected
    if(!is.null(selectedrowindex)){
      currentTable <- as.data.frame(w$firstBestData)
      
      rowProfile <- currentTable[selectedrowindex,]
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
      g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
      return(g)    
      
    }
  })
  
  output$globalBestProfilePlotVolu <- renderPlot({
    selectedrowindex <- input$global_best_profiles_table_rows_selected
    if(!is.null(selectedrowindex)){
      currentTable <- as.data.frame(v$firstGlobalBestdata)
      
      rowProfile <- currentTable[selectedrowindex,]
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
      g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
      return(g)    
      
    }
  })
  
  output$bestProfilePlotVolu <- renderPlot({
    selectedrowindex <- input$best_profiles_table_rows_selected
    if(!is.null(selectedrowindex)){
      currentTable <- as.data.frame(w$firstBestData)
      
      rowProfile <- currentTable[selectedrowindex,]
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
      g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
      return(g)    
      
    }
  })
  
  
  
  output$global_best_profiles_table <-   DT::renderDataTable(v$firstGlobalBestdata,selection='single',server = FALSE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
  
  v <- reactiveValues(firstGlobalBestdata = NULL)
  
  
  observeEvent(input$lapses_filtering, {
    # print("entering here from aggregate_criteria observe event")
    # print("Entering here")
    firstGlobalBestdata <- NULL
    
    
    firstGlobalBestdata <- getBestProfilesDataset(input$aggregate_criteria, input$periods, input$methodo, input$weighting, input$ravenpack_type, input$lapses_filtering)
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "RANKING")                               
    # "infinity_return_global") 
    firstGlobalBestdata  <- firstGlobalBestdata[firstGlobalBestdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="my_event")] <- "EVENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="relevance")] <- "RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="localSource")] <- "SOURCE"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return")] <- "RANKING"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return_global")] <- "RANKING"
    
    firstGlobalBestdata <- firstGlobalBestdata[,c(my_names,setdiff(colnames(firstGlobalBestdata),my_names))]
    # print(head(firstGlobalBestdata[,my_names],100))
    
    firstGlobalBestdata$RANKING <- (firstGlobalBestdata$RANKING - min(firstGlobalBestdata$RANKING))/(max(firstGlobalBestdata$RANKING) - min(firstGlobalBestdata$RANKING))
    
    firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)]*0.01
    
    
    firstGlobalBestdata$RANKING <- as.integer(100*firstGlobalBestdata$RANKING)
    firstGlobalBestdata <- firstGlobalBestdata[order(firstGlobalBestdata$RANKING,decreasing = TRUE),]
    

    
    firstGlobalBestdata <- as.data.table(firstGlobalBestdata)
    firstGlobalBestdata <- firstGlobalBestdata[order(-RANKING),]
    v$firstGlobalBestdata <- firstGlobalBestdata
  })
  
  observeEvent(input$aggregate_criteria, {
    # print("entering here from aggregate_criteria observe event")
    # print("Entering here")
    firstGlobalBestdata <- NULL
    
    
    firstGlobalBestdata <- getBestProfilesDataset(input$aggregate_criteria, input$periods, input$methodo, input$weighting, input$ravenpack_type, input$lapses_filtering)
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "RANKING")                               
    # "infinity_return_global") 
    firstGlobalBestdata  <- firstGlobalBestdata[firstGlobalBestdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="my_event")] <- "EVENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="relevance")] <- "RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="localSource")] <- "SOURCE"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return")] <- "RANKING"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return_global")] <- "RANKING"
    
    firstGlobalBestdata <- firstGlobalBestdata[,c(my_names,setdiff(colnames(firstGlobalBestdata),my_names))]
    # print(head(firstGlobalBestdata[,my_names],100))
    
    firstGlobalBestdata$RANKING <- (firstGlobalBestdata$RANKING - min(firstGlobalBestdata$RANKING))/(max(firstGlobalBestdata$RANKING) - min(firstGlobalBestdata$RANKING))
    
    firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)]*0.01
    
    
    firstGlobalBestdata$RANKING <- as.integer(100*firstGlobalBestdata$RANKING)
    firstGlobalBestdata <- firstGlobalBestdata[order(firstGlobalBestdata$RANKING,decreasing = TRUE),]
    
            
    firstGlobalBestdata <- as.data.table(firstGlobalBestdata)
    firstGlobalBestdata <- firstGlobalBestdata[order(-RANKING),]
    
    v$firstGlobalBestdata <- firstGlobalBestdata
  })
  
  
  observeEvent(input$periods, {
    # print("entering here from periods observe event")
    
    firstGlobalBestdata <- NULL
    
    firstGlobalBestdata <- getBestProfilesDataset(input$aggregate_criteria, input$periods, input$methodo, input$weighting, input$ravenpack_type, input$lapses_filtering)
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "RANKING")                               
    # "infinity_return_global") 
    
    firstGlobalBestdata  <- firstGlobalBestdata[firstGlobalBestdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="my_event")] <- "EVENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="relevance")] <- "RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="localSource")] <- "SOURCE"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return")] <- "RANKING"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return_global")] <- "RANKING"
    
    firstGlobalBestdata <- firstGlobalBestdata[,c(my_names,setdiff(colnames(firstGlobalBestdata),my_names))]
    # print(head(firstGlobalBestdata[,my_names],100))
    
    firstGlobalBestdata$RANKING <- (firstGlobalBestdata$RANKING - min(firstGlobalBestdata$RANKING))/(max(firstGlobalBestdata$RANKING) - min(firstGlobalBestdata$RANKING))
    
    firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)]*0.01
    
    
    firstGlobalBestdata$RANKING <- as.integer(100*firstGlobalBestdata$RANKING)
    firstGlobalBestdata <- firstGlobalBestdata[order(firstGlobalBestdata$RANKING,decreasing = TRUE),]
    
    
    firstGlobalBestdata <- as.data.table(firstGlobalBestdata)
    firstGlobalBestdata <- firstGlobalBestdata[order(-RANKING),]
    v$firstGlobalBestdata <- firstGlobalBestdata
  })
  
  observeEvent(input$methodo, {
    # print("entering here from periods observe event")
    
    firstGlobalBestdata <- NULL
    
    firstGlobalBestdata <- getBestProfilesDataset(input$aggregate_criteria, input$periods, input$methodo, input$weighting, input$ravenpack_type, input$lapses_filtering)
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "RANKING")                               
    # "infinity_return_global") 
    firstGlobalBestdata  <- firstGlobalBestdata[firstGlobalBestdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="my_event")] <- "EVENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="relevance")] <- "RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="localSource")] <- "SOURCE"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return")] <- "RANKING"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return_global")] <- "RANKING"
    
    firstGlobalBestdata <- firstGlobalBestdata[,c(my_names,setdiff(colnames(firstGlobalBestdata),my_names))]
    # print(head(firstGlobalBestdata[,my_names],100))
    
    firstGlobalBestdata$RANKING <- (firstGlobalBestdata$RANKING - min(firstGlobalBestdata$RANKING))/(max(firstGlobalBestdata$RANKING) - min(firstGlobalBestdata$RANKING))
    
    firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)]*0.01
    
    
    firstGlobalBestdata$RANKING <- as.integer(100*firstGlobalBestdata$RANKING)
    firstGlobalBestdata <- firstGlobalBestdata[order(firstGlobalBestdata$RANKING,decreasing = TRUE),]
    
    
    firstGlobalBestdata <- as.data.table(firstGlobalBestdata)
    firstGlobalBestdata <- firstGlobalBestdata[order(-RANKING),]
    v$firstGlobalBestdata <- firstGlobalBestdata
  })
  
  observeEvent(input$weighting, {
    # print("entering here from periods observe event")
    firstGlobalBestdata <- NULL
    firstGlobalBestdata <- getBestProfilesDataset(input$aggregate_criteria, input$periods, input$methodo, input$weighting, input$ravenpack_type, input$lapses_filtering)
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "RANKING")                               
    # "infinity_return_global") 
    
    firstGlobalBestdata  <- firstGlobalBestdata[firstGlobalBestdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="my_event")] <- "EVENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="relevance")] <- "RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="localSource")] <- "SOURCE"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return")] <- "RANKING"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return_global")] <- "RANKING"
    
    firstGlobalBestdata <- firstGlobalBestdata[,c(my_names,setdiff(colnames(firstGlobalBestdata),my_names))]
    # print(head(firstGlobalBestdata[,my_names],100))
    
    firstGlobalBestdata$RANKING <- (firstGlobalBestdata$RANKING - min(firstGlobalBestdata$RANKING))/(max(firstGlobalBestdata$RANKING) - min(firstGlobalBestdata$RANKING))
    
    firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)]*0.01
    
    
    firstGlobalBestdata$RANKING <- as.integer(100*firstGlobalBestdata$RANKING)
    firstGlobalBestdata <- firstGlobalBestdata[order(firstGlobalBestdata$RANKING,decreasing = TRUE),]

    
    firstGlobalBestdata <- as.data.table(firstGlobalBestdata)
    firstGlobalBestdata <- firstGlobalBestdata[order(-RANKING),]
    v$firstGlobalBestdata <- firstGlobalBestdata
  })
  
  observeEvent(input$event_number_event_filtering, {
    # print("entering here from periods observe event")
    firstGlobalBestdata <- NULL
    firstGlobalBestdata <- getBestProfilesDataset(input$aggregate_criteria, input$periods, input$methodo, input$weighting,  input$ravenpack_type, input$lapses_filtering)
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "RANKING")                               
    # "infinity_return_global") 
    
    firstGlobalBestdata  <- firstGlobalBestdata[firstGlobalBestdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="my_event")] <- "EVENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="relevance")] <- "RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="localSource")] <- "SOURCE"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return")] <- "RANKING"
    # colnames(firstGlobalBestdata)[which(colnames(firstGlobalBestdata)=="infinity_return_global")] <- "RANKING"
    
    firstGlobalBestdata <- firstGlobalBestdata[,c(my_names,setdiff(colnames(firstGlobalBestdata),my_names))]
    # print(head(firstGlobalBestdata[,my_names],100))
    
    firstGlobalBestdata$RANKING <- (firstGlobalBestdata$RANKING - min(firstGlobalBestdata$RANKING))/(max(firstGlobalBestdata$RANKING) - min(firstGlobalBestdata$RANKING))
    
    firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("technical",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("imbalance",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-loss",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-gain",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("stock-prices",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("regulatory",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("war-conflict",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("taxes",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("transportation",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("security",firstGlobalBestdata$EVENT)]*0.01
    firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)] <- firstGlobalBestdata$RANKING[grepl("civil-unrest",firstGlobalBestdata$EVENT)]*0.01
    
    
    firstGlobalBestdata$RANKING <- as.integer(100*firstGlobalBestdata$RANKING)
    firstGlobalBestdata <- firstGlobalBestdata[order(firstGlobalBestdata$RANKING,decreasing = TRUE),]
    

    firstGlobalBestdata <- as.data.table(firstGlobalBestdata)
    firstGlobalBestdata <- firstGlobalBestdata[order(-RANKING),]
    v$firstGlobalBestdata <- firstGlobalBestdata
  })
  
  
  
  output$best_profiles_table <-   DT::renderDataTable(w$firstBestData,selection='single',server = FALSE,options=list(pageLength  = 100,colNames  = column_to_plot_name,columnDefs = list(list(visible=FALSE, targets=column_to_hide_indice))))
  
  w <- reactiveValues(firstBestData = NULL)

  observeEvent(input$lapses_filtering, {
    
    my_metrics <- getBestProfilesMetrics(input$inside_periods, input$inside_methodo, input$inside_weighting)
    
    firstBestData <- NULL
    seconddata <- NULL
    if(input$ravenpack_type == "RPNA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$ravenpack_type == "RBDA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    
    firstBestData  <- firstBestData[firstBestData$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "infinity_return") 
    firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
    
    
    firstBestData$RANKING <- firstBestData[,my_metrics]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstBestData)[which(colnames(firstBestData)=="my_event")] <- "EVENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="relevance")] <- "RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstBestData)[which(colnames(firstBestData)=="localSource")] <- "SOURCE"
    # colnames(firstBestData)[which(colnames(firstBestData)=="infinity_return")] <- "RANKING"
    
    firstBestData <- firstBestData[,c(my_names,setdiff(colnames(firstBestData),my_names))]
    # print(head(firstBestData[,my_names],100))
    
    firstBestData$RANKING <- as.integer(100  * (firstBestData$RANKING - min(firstBestData$RANKING))/(max(firstBestData$RANKING) - min(firstBestData$RANKING)))
    firstBestData <- as.data.table(firstBestData)
    firstBestData <- firstBestData[order(-RANKING),]
    w$firstBestData <- firstBestData
    # return(firstBestData)
  })
  
  # firstBestData <- eventReactive(input$my_event, {
  observeEvent(input$my_event, {
    
    my_metrics <- getBestProfilesMetrics(input$inside_periods, input$inside_methodo, input$inside_weighting)
    
    firstBestData <- NULL
    seconddata <- NULL
    if(input$ravenpack_type == "RPNA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$ravenpack_type == "RBDA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    
    firstBestData  <- firstBestData[firstBestData$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "infinity_return") 
    firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
    
    
    firstBestData$RANKING <- firstBestData[,my_metrics]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstBestData)[which(colnames(firstBestData)=="my_event")] <- "EVENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="relevance")] <- "RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstBestData)[which(colnames(firstBestData)=="localSource")] <- "SOURCE"
    # colnames(firstBestData)[which(colnames(firstBestData)=="infinity_return")] <- "RANKING"
    
    firstBestData <- firstBestData[,c(my_names,setdiff(colnames(firstBestData),my_names))]
    # print(head(firstBestData[,my_names],100))
    
    firstBestData$RANKING <- as.integer(100  * (firstBestData$RANKING - min(firstBestData$RANKING))/(max(firstBestData$RANKING) - min(firstBestData$RANKING)))
    firstBestData <- as.data.table(firstBestData)
    firstBestData <- firstBestData[order(-RANKING),]
    w$firstBestData <- firstBestData
    # return(firstBestData)
  })
  
  
  observeEvent(input$inside_periods, {
    
    my_metrics <- getBestProfilesMetrics(input$inside_periods, input$inside_methodo, input$inside_weighting)
    
    firstBestData <- NULL
    seconddata <- NULL
    if(input$ravenpack_type == "RPNA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$ravenpack_type == "RBDA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    
    firstBestData  <- firstBestData[firstBestData$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "infinity_return") 
    firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
    
    
    firstBestData$RANKING <- firstBestData[,my_metrics]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstBestData)[which(colnames(firstBestData)=="my_event")] <- "EVENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="relevance")] <- "RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstBestData)[which(colnames(firstBestData)=="localSource")] <- "SOURCE"
    # colnames(firstBestData)[which(colnames(firstBestData)=="infinity_return")] <- "RANKING"
    
    firstBestData <- firstBestData[,c(my_names,setdiff(colnames(firstBestData),my_names))]
    # print(head(firstBestData[,my_names],100))
    
    firstBestData$RANKING <- as.integer(100  * (firstBestData$RANKING - min(firstBestData$RANKING))/(max(firstBestData$RANKING) - min(firstBestData$RANKING)))
    firstBestData <- as.data.table(firstBestData)
    firstBestData <- firstBestData[order(-RANKING),]
    w$firstBestData <- firstBestData
    # return(firstBestData)
  })
  
  
  observeEvent(input$inside_methodo, {
    
    my_metrics <- getBestProfilesMetrics(input$inside_periods, input$inside_methodo, input$inside_weighting)
    
    firstBestData <- NULL
    seconddata <- NULL
    if(input$ravenpack_type == "RPNA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$ravenpack_type == "RBDA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    
    firstBestData  <- firstBestData[firstBestData$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "infinity_return") 
    firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
    
    
    firstBestData$RANKING <- firstBestData[,my_metrics]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstBestData)[which(colnames(firstBestData)=="my_event")] <- "EVENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="relevance")] <- "RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstBestData)[which(colnames(firstBestData)=="localSource")] <- "SOURCE"
    # colnames(firstBestData)[which(colnames(firstBestData)=="infinity_return")] <- "RANKING"
    
    firstBestData <- firstBestData[,c(my_names,setdiff(colnames(firstBestData),my_names))]
    # print(head(firstBestData[,my_names],100))
    
    firstBestData$RANKING <- as.integer(100  * (firstBestData$RANKING - min(firstBestData$RANKING))/(max(firstBestData$RANKING) - min(firstBestData$RANKING)))
    firstBestData <- as.data.table(firstBestData)
    firstBestData <- firstBestData[order(-RANKING),]
    w$firstBestData <- firstBestData
    # return(firstBestData)
  })
  
  
  observeEvent(input$inside_weighting, {
    
    my_metrics <- getBestProfilesMetrics(input$inside_periods, input$inside_methodo, input$inside_weighting)
    
    firstBestData <- NULL
    seconddata <- NULL
    if(input$ravenpack_type == "RPNA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$ravenpack_type == "RBDA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    
    firstBestData  <- firstBestData[firstBestData$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "infinity_return") 
    firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
    
    
    firstBestData$RANKING <- firstBestData[,my_metrics]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstBestData)[which(colnames(firstBestData)=="my_event")] <- "EVENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="relevance")] <- "RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstBestData)[which(colnames(firstBestData)=="localSource")] <- "SOURCE"
    # colnames(firstBestData)[which(colnames(firstBestData)=="infinity_return")] <- "RANKING"
    
    firstBestData <- firstBestData[,c(my_names,setdiff(colnames(firstBestData),my_names))]
    # print(head(firstBestData[,my_names],100))
    
    firstBestData$RANKING <- as.integer(100  * (firstBestData$RANKING - min(firstBestData$RANKING))/(max(firstBestData$RANKING) - min(firstBestData$RANKING)))
    firstBestData <- as.data.table(firstBestData)
    firstBestData <- firstBestData[order(-RANKING),]
    w$firstBestData <- firstBestData
    # return(firstBestData)
  })
  
  
  observeEvent(input$event_number_event_filtering, {
    
    my_metrics <- getBestProfilesMetrics(input$inside_periods, input$inside_methodo, input$inside_weighting)
    
    firstBestData <- NULL
    seconddata <- NULL
    if(input$ravenpack_type == "RPNA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$ravenpack_type == "RBDA"){
      firstBestData <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    
    firstBestData  <- firstBestData[firstBestData$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    
    column_to_plot <- c("my_event",
                        "relevance",
                        "event_relevance",
                        "sentiment_criteria",
                        "similarity_gap_filter",
                        "event_number_event_filtering",
                        "localSource",
                        "infinity_return") 
    firstBestData <- firstBestData[firstBestData$my_event == input$my_event,]
    
    
    firstBestData$RANKING <- firstBestData[,my_metrics]
    
    my_names <- c("EVENT","RELEVANCE", "EVENT_RELEVANCE", "SENTIMENT","SIMILARITY","COUNT","SOURCE", "RANKING")
    colnames(firstBestData)[which(colnames(firstBestData)=="my_event")] <- "EVENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="relevance")] <- "RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_relevance")] <- "EVENT_RELEVANCE"
    colnames(firstBestData)[which(colnames(firstBestData)=="sentiment_criteria")] <- "SENTIMENT"
    colnames(firstBestData)[which(colnames(firstBestData)=="similarity_gap_filter")] <- "SIMILARITY"
    colnames(firstBestData)[which(colnames(firstBestData)=="event_number_event_filtering")] <- "COUNT"
    colnames(firstBestData)[which(colnames(firstBestData)=="localSource")] <- "SOURCE"
    # colnames(firstBestData)[which(colnames(firstBestData)=="infinity_return")] <- "RANKING"
    
    firstBestData <- firstBestData[,c(my_names,setdiff(colnames(firstBestData),my_names))]
    # print(head(firstBestData[,my_names],100))
    
    firstBestData$RANKING <- as.integer(100  * (firstBestData$RANKING - min(firstBestData$RANKING))/(max(firstBestData$RANKING) - min(firstBestData$RANKING)))
    firstBestData <- as.data.table(firstBestData)
    firstBestData <- firstBestData[order(-RANKING),]
    w$firstBestData <- firstBestData
    # return(firstBestData)
  })
  
  
  
  
  output$filtering_criteria <- renderText({ 
    
    ###################
    ###################
    ###################
    ################### Plotting together
    
    firstdata <- NULL
    
    if (input$ravenpack_type == "RBDA"){
      firstdata <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    } else {
      firstdata <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }

    firstdata  <- firstdata[firstdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]

    ############# Left text field
    firstdataf <- firstdata[firstdata$my_event == input$my_event,]
    outputLyrics <- ""
    if (dim(firstdataf)[1]>0){
      firstdataf <- firstdataf[order(firstdataf$infinity_return,decreasing = TRUE),]
      rowProfile <- firstdataf[1,]
      
      #       if(input$sentiment_criteria == "BEST"){
      #         
      #         
      #         
      #         outputLyrics <- paste0("Source : ",rowProfile$localSource," ,Sector : ",rowProfile$gics_sector," ,Sentiment : ",rowProfile$sentiment_criteria," ,Similarity days : ",rowProfile$similarity_gap_filter,
      #                                " ,Relevance : ",rowProfile$relevance, " ,Event relevance : ",rowProfile$event_relevance, " ,Nb Events  : ",rowProfile$event_number_event_filtering)
      #         return(outputLyrics)
      #       } else {
      firstdataf <- firstdataf[firstdataf$localSource == input$localSource,]
      firstdataf <- firstdataf[firstdataf$similarity_gap_filter == input$similarity_gap_filter,]
      firstdataf <- firstdataf[firstdataf$sentiment_criteria == input$sentiment_criteria,]
      firstdataf <- firstdataf[firstdataf$relevance == input$relevance,]
      firstdataf <- firstdataf[firstdataf$event_relevance == input$event_relevance,]
      
      return(paste0("Your selection applies ,Nb Events  : ",firstdataf$event_number_event_filtering))
      
      # }
    } 
    
  })
  ###################
  ###################
  ###################
  ################### First plot
  
  
  
  
  
  output$eventMinutesPlotHigh <- renderPlot({
    firstdata <- NULL
    seconddata <- NULL
    if(input$ravenpack_type == "RPNA"){
      firstdata <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$ravenpack_type == "RBDA"){
      firstdata <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$sec_ravenpack_type == "RPNA"){
      seconddata <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
    }
    if(input$sec_ravenpack_type == "RBDA"){
      seconddata <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
    }
    
    
    
    firstdata  <- firstdata[firstdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$sec_event_number_event_filtering),]
    

    
    # if(input$sentiment_criteria != "BEST"){
    firstdata <- firstdata[firstdata$localSource == input$localSource,]
    firstdata <- firstdata[firstdata$similarity_gap_filter == input$similarity_gap_filter,]
    firstdata <- firstdata[firstdata$sentiment_criteria == input$sentiment_criteria,]
    firstdata <- firstdata[firstdata$relevance == input$relevance,]
    firstdata <- firstdata[firstdata$event_relevance == input$event_relevance,]
    # }

    # if(input$sec_sentiment_criteria != "BEST"){
    seconddata <- seconddata[seconddata$localSource == input$sec_localSource,]

    seconddata <- seconddata[seconddata$similarity_gap_filter == input$sec_similarity_gap_filter,]

    seconddata <- seconddata[seconddata$sentiment_criteria == input$sec_sentiment_criteria,]

    seconddata <- seconddata[seconddata$relevance == input$sec_relevance,]

    seconddata <- seconddata[seconddata$event_relevance == input$sec_event_relevance,]

    # }
    

    
    
    
    firstdataf <- firstdata[firstdata$my_event == input$my_event,]
    seconddataf <- seconddata[seconddata$my_event == input$sec_my_event,]
    
    
    if ((dim(firstdataf)[1] == 0) & (dim(seconddataf)[1] == 0 ) ){
      pp <- readPNG("SORRY.PNG")
      rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
      return
    }
    
    
    if ((dim(firstdataf)[1] >0) & (dim(seconddataf)[1] >0 ) ){
      print("plotting them together")
      ####### first big data 
      
      firstdataf <- firstdataf[order(firstdataf$infinity_return,decreasing = TRUE),]
      
      rowProfileOne <- firstdataf[1,]
      
      stats_sign <- colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))]
      rets <- paste0("RET",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      ord_stats_sign <- paste0("ORD",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      vol_stats_sign <- paste0("VOLU",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      vola_stats_sign <- paste0("VOLA",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      
      stats_sign <- rowProfileOne[,stats_sign]
      rets <- rowProfileOne[,rets]
      colnames(rets) <- colnames(stats_sign)
      ord_stats_sign <- rowProfileOne[,ord_stats_sign]
      colnames(ord_stats_sign) <- colnames(stats_sign)
      vol_stats_sign <- rowProfileOne[,vol_stats_sign]
      colnames(vol_stats_sign) <- colnames(stats_sign)
      vola_stats_sign <- rowProfileOne[,vola_stats_sign]
      colnames(vola_stats_sign) <- colnames(stats_sign)
      
      firstdataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
      firstProperNames <- paste0(rowProfileOne$my_event,c("COR_STATS_SIGN_1","ORD_STATS_SIGN_1","VOLUME_1","VOLATILITY_1","RETS_1"))
      colnames(firstdataframe) <- firstProperNames
      firstdataframe$MINUTES <- as.numeric(colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      firstdataframe <- firstdataframe[,c("MINUTES",firstProperNames)]
      
      #### second rpna
      seconddataf <- seconddataf[order(seconddataf$infinity_return,decreasing = TRUE),]
      
      rowProfileTwo <- seconddataf[1,]
      
      stats_sign <- colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))]
      rets <- paste0("RET",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      ord_stats_sign <- paste0("ORD",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      vol_stats_sign <- paste0("VOLU",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      vola_stats_sign <- paste0("VOLA",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      
      stats_sign <- rowProfileTwo[,stats_sign]
      rets <- rowProfileTwo[,rets]
      colnames(rets) <- colnames(stats_sign)
      ord_stats_sign <- rowProfileTwo[,ord_stats_sign]
      colnames(ord_stats_sign) <- colnames(stats_sign)
      vol_stats_sign <- rowProfileTwo[,vol_stats_sign]
      colnames(vol_stats_sign) <- colnames(stats_sign)
      vola_stats_sign <- rowProfileTwo[,vola_stats_sign]
      colnames(vola_stats_sign) <- colnames(stats_sign)
      
      seconddataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
      secondProperNames <- paste0(rowProfileTwo$my_event,c("COR_STATS_SIGN_2","ORD_STATS_SIGN_2","VOLUME_2","VOLATILITY_2","RETS_2"))
      colnames(seconddataframe) <- secondProperNames
      seconddataframe$MINUTES <- as.numeric(colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      seconddataframe <- seconddataframe[,c("MINUTES",secondProperNames)]
      ### merging both together
      dataframe <- merge(seconddataframe,firstdataframe,by="MINUTES")
      #       
      #       print("saving for debug@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      #       save(rowProfileOne, rowProfileTwo, dataframe,  file=paste0(outputDataPath,"entryTwo.RData"))
      #       print("saving for debug@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      #### plotting them together
      if (input$my_plot == "RETURNS_SIGNIFICANCE"){
        g <- outputTogetherGraphicsBestProfileRets(rowProfileOne$my_event,rowProfileTwo$my_event,dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
        
        # Render your graph
        print(g)    
        
      } else {
        g <- outputTogetherGraphicsBestProfileVola(rowProfileOne$my_event,rowProfileTwo$my_event, dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
        
        # Render your graph
        print(g)  
      }
    } else { 
      print("Not enough data for one 1")
      
      if ((dim(seconddataf)[1] >0 )){
        print("plotting second only")
        dataf <- seconddataf
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
          g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
          
          # Render your graph
          print(g)    
          
        } else {
          g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
          
          # Render your graph
          print(g)  
        }
      }
      
      
      
      if ((dim(firstdataf)[1] >0 )){
        print("plotting first only")
        dataf <- firstdataf
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
          g <- outputGraphicsBestProfileRets(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
          # Render your graph
          print(g)    
          
        } else {
          g <- outputGraphicsBestProfileVola(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
          # Render your graph
          print(g)  
        }
        
      } 
    }
    
  })
  
  
  output$eventMinutesPlotLow <- renderPlot({
    
    ###################
    ###################
    ###################
    ################### Plotting together
    
    
    
    firstdata <- NULL
    seconddata <- NULL
    if(input$ravenpack_type == "RPNA"){
      firstdata <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$ravenpack_type == "RBDA"){
      firstdata <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$lapses_filtering)
    }
    if(input$sec_ravenpack_type == "RPNA"){
      seconddata <- getProperLapseDataset(incoming_dataset = rpna_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
    }
    if(input$sec_ravenpack_type == "RBDA"){
      seconddata <- getProperLapseDataset(incoming_dataset = bigdata_dataTotr1000,filtering_lapse = input$sec_lapses_filtering)
    }
    
    
    
    firstdata  <- firstdata[firstdata$event_number_event_filtering >= as.numeric(input$event_number_event_filtering),]
    seconddata <- seconddata[seconddata$event_number_event_filtering >= as.numeric(input$sec_event_number_event_filtering),]
    
    # if(input$sentiment_criteria != "BEST"){
    firstdata <- firstdata[firstdata$localSource == input$localSource,]
    firstdata <- firstdata[firstdata$similarity_gap_filter == input$similarity_gap_filter,]
    firstdata <- firstdata[firstdata$sentiment_criteria == input$sentiment_criteria,]
    firstdata <- firstdata[firstdata$relevance == input$relevance,]
    firstdata <- firstdata[firstdata$event_relevance == input$event_relevance,]
    # }
    
    # if(input$sec_sentiment_criteria != "BEST"){
    seconddata <- seconddata[seconddata$localSource == input$sec_localSource,]
    seconddata <- seconddata[seconddata$similarity_gap_filter == input$sec_similarity_gap_filter,]
    seconddata <- seconddata[seconddata$sentiment_criteria == input$sec_sentiment_criteria,]
    seconddata <- seconddata[seconddata$relevance == input$sec_relevance,]
    seconddata <- seconddata[seconddata$event_relevance == input$sec_event_relevance,]
    
    # }
    
    firstdataf <- firstdata[firstdata$my_event == input$my_event,]
    seconddataf <- seconddata[seconddata$my_event == input$sec_my_event,]
    
    if ((dim(firstdataf)[1] == 0) & (dim(seconddataf)[1] == 0 ) ){
      pp <- readPNG("SORRY.PNG")
      rasterImage(image = pp,xleft = 0,ybottom = 0,xright = 1,ytop = 1)
      return
    }
    
    if ((dim(firstdataf)[1] >0) & (dim(seconddataf)[1] >0 ) ){
      print("plotting them together")
      ####### first big data 
      
      firstdataf <- firstdataf[order(firstdataf$infinity_return,decreasing = TRUE),]
      
      rowProfileOne <- firstdataf[1,]
      
      stats_sign <- colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))]
      rets <- paste0("RET",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      ord_stats_sign <- paste0("ORD",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      vol_stats_sign <- paste0("VOLU",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      vola_stats_sign <- paste0("VOLA",colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      
      stats_sign <- rowProfileOne[,stats_sign]
      rets <- rowProfileOne[,rets]
      colnames(rets) <- colnames(stats_sign)
      ord_stats_sign <- rowProfileOne[,ord_stats_sign]
      colnames(ord_stats_sign) <- colnames(stats_sign)
      vol_stats_sign <- rowProfileOne[,vol_stats_sign]
      colnames(vol_stats_sign) <- colnames(stats_sign)
      vola_stats_sign <- rowProfileOne[,vola_stats_sign]
      colnames(vola_stats_sign) <- colnames(stats_sign)
      
      firstdataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
      firstProperNames <- paste0(rowProfileOne$my_event,c("COR_STATS_SIGN_1","ORD_STATS_SIGN_1","VOLUME_1","VOLATILITY_1","RETS_1"))
      colnames(firstdataframe) <- firstProperNames
      firstdataframe$MINUTES <- as.numeric(colnames(rowProfileOne)[which(!is.na(as.numeric(colnames(rowProfileOne))))])
      firstdataframe <- firstdataframe[,c("MINUTES",firstProperNames)]
      
      #### second rpna
      seconddataf <- seconddataf[order(seconddataf$infinity_return,decreasing = TRUE),]
      
      rowProfileTwo <- seconddataf[1,]
      
      stats_sign <- colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))]
      rets <- paste0("RET",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      ord_stats_sign <- paste0("ORD",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      vol_stats_sign <- paste0("VOLU",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      vola_stats_sign <- paste0("VOLA",colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      
      stats_sign <- rowProfileTwo[,stats_sign]
      rets <- rowProfileTwo[,rets]
      colnames(rets) <- colnames(stats_sign)
      ord_stats_sign <- rowProfileTwo[,ord_stats_sign]
      colnames(ord_stats_sign) <- colnames(stats_sign)
      vol_stats_sign <- rowProfileTwo[,vol_stats_sign]
      colnames(vol_stats_sign) <- colnames(stats_sign)
      vola_stats_sign <- rowProfileTwo[,vola_stats_sign]
      colnames(vola_stats_sign) <- colnames(stats_sign)
      
      seconddataframe <- as.data.frame(t(rbind(stats_sign,ord_stats_sign,vol_stats_sign,vola_stats_sign,rets)))
      secondProperNames <- paste0(rowProfileTwo$my_event,c("COR_STATS_SIGN_2","ORD_STATS_SIGN_2","VOLUME_2","VOLATILITY_2","RETS_2"))
      colnames(seconddataframe) <- secondProperNames
      seconddataframe$MINUTES <- as.numeric(colnames(rowProfileTwo)[which(!is.na(as.numeric(colnames(rowProfileTwo))))])
      seconddataframe <- seconddataframe[,c("MINUTES",secondProperNames)]
      
      ### merging both together
      dataframe <- merge(seconddataframe,firstdataframe,by="MINUTES")
      
      if (input$my_plot == "RETURNS_SIGNIFICANCE"){
        g <- outputTogetherGraphicsBestProfileStats(rowProfileOne$my_event,rowProfileTwo$my_event,dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
        # Render your graph
        print(g)    
      } else {
        g <- outputTogetherGraphicsBestProfileVol(rowProfileOne$my_event,rowProfileTwo$my_event,dataFrame = dataframe, FALSE, Russell_version = "R1000",Together = TRUE)
        # Render your graph
        print(g)  
      }
    } else { 
      print("not enough data for one 2")
      
      if ((dim(seconddataf)[1] >0 )){
        print("plotting second only")
        dataf <- seconddataf
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
          g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
          # Render your graph
          print(g)    
        } else {
          g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
          # Render your graph
          print(g)  
        }
      }
      
      if ((dim(firstdataf)[1] >0 )){
        print("plotting first only")
        dataf <- firstdataf
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
          g <- outputGraphicsBestProfileStats(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
          # Render your graph
          print(g)    
        } else {
          g <- outputGraphicsBestProfileVol(rowProfile$product_criteria,rowProfile$aggregate_criteria,rowProfile$sentiment_criteria,rowProfile$similarity_gap_filter,rowProfile$ens_filter,rowProfile$event_number_event_filtering, rowProfile$gics_sector, rowProfile$EVENT, rowProfile$localSource, dataFrame = dataframe, FALSE, Russell_version = "R1000")
          # Render your graph
          print(g)  
        }
      }
      
      
    }
    
    
  })
  
  
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ##################################################################################################
  ################################################################################################## Refreshing GUI
  
  
}
