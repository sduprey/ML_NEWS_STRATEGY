diff.data.frame <- function(obj){
  sapply(obj,FUN = function(x) diff(x))
}

is.column.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) is.finite(x))
}

is.column.infinite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.infinite(x)))
}

is.infinite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) is.infinite(x))
}

is.column.nan.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.nan(x)))
}

is.nan.data.frame <- function(obj){
  sapply(obj,FUN = function(x) is.nan(x))
}

stat <- function(x) {
#   if(is.numeric(x)){
#     return  (c(mean = mean(x,na.rm=TRUE)))
#   }else{
#     return  (c(x))
#   }
#   
#   return()
#   
  x$rp_entity_id
  
  ranked_entity <- aggregate(x[,c("ess","ens","ens_similarity_gap")], list(x$rp_entity_id),FUN=mean)
  
  ranked_entity <- ranked_entity[order(ranked_entity$ens_similarity_gap,decreasing = TRUE),]
  row_df <- NULL
  for (i in 1:dim(ranked_entity)[1]){
    row_df <- data.frame(DATE=unique(x$DATE), MACRO_KEY=unique(x$macro_key), COUNTRY_CODE=unique(x$country_code), RP_ENTITY_ID=ranked_entity$rp_entity_id, ESS=ranked_entity$ess, ENS=ranked_entity$ens, ENS_SIMILARITY_GAP=ranked_entity$ens_similarity_gap)
  }
  
 
}
