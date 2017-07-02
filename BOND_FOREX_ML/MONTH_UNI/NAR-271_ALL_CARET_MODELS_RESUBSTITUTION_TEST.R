## Testing all caret models one after the others
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
require(caret)

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

# SaveDataFrame(to_store, outputDataPathMonth,"resubstitution_df")

my_resubstitution_filename <- paste("resubstitution_df", ".rds",sep="")

to_store <- tryCatch( readRDS(paste(outputDataPathMonth,my_resubstitution_filename, sep = "")),
                      error = function(e) {NULL})

predictive_columns <- to_store$predicting_columns
calibration_data <- to_store$calib
output_column <- to_store$output


train_X <- calibration_data[,predictive_columns]
train_Y <- calibration_data[,output_column]

# Getting rid of NAs
my_non_na_rows <- complete.cases(train_X)
train_X <- train_X[my_non_na_rows,]
train_Y <- train_Y[my_non_na_rows]

# fill variable m with the fast working models  
m <- c("avNNet", "bagEarth", "bagEarthGCV", 
       "bayesglm", "bdk", "blackboost", "Boruta", "brnn", "BstLm" , 
       "bstTree", "cforest", "ctree", "ctree2", "cubist", "DENFIS", 
       "dnn", "earth", "elm", "enet", "enpls", "evtree", 
       "extraTrees",  "gamLoess",  "gaussprLinear", "gaussprPoly", "gaussprRadial", 
       "gcvEarth","glm", "glmboost", "glmnet", "icr", "kernelpls", 
       "kknn", "knn",  "krlsRadial", "lars" , "lasso", 
       "leapBackward", "leapForward", "leapSeq", "lm", "M5", "M5Rules", 
       "mlpWeightDecay", "neuralnet" , "partDSA", 
       "pcaNNet", "pcr", "penalized", "pls", "plsRglm", "ppr", 
       "qrf" , "ranger",  "rf", "rfRules", "rbfDDA",
       "ridge", "rknn", "rknnBel", "rlm", "rpart", "rpart2", "rqlasso", 
       "rqnc", "RRF", "RRFglobal",  "rvmPoly", "rvmRadial", 
       "SBC", "simpls", "spls", "superpc" , 
       "svmLinear", "svmLinear2", "svmPoly", "svmRadial", "svmRadialCost", 
       "treebag", "widekernelpls", "WM", "xgbLinear", 
       "xgbTree", "xyf")

# m <- c( "Boruta", "rpart", "rpart2", "xgbLinear", "xgbTree", "xyf")
# m <- c("rpart", "rpart2", "xgbLinear", "xgbTree")

# load all packages (does not really work due to other dependencies)
suppressPackageStartupMessages(ll <-lapply(m, require, character.only = TRUE))

# define x and y for regression
y <- train_Y
x <- train_X

# use all your codes
# library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)

# use lapply/loop to run everything
my_testing_function <- function(i,outputDataPathMonth) {
  cat("----------------------------------------------------","\n")
  set.seed(123); cat(i," <- loaded\n")
  t2 <- tryCatch(train(y=y, x=x, (i), trControl = trainControl(method = "boot632")),error = function(e) {NULL})
  output = list()
  if (!is.null(t2)){
    print("Best parameters found")
    cat(round(t2$results$Rsquared[which.min(t2$results$RMSE)],4),"\t");
    cat(round(t2$results$RMSE[which.min(t2$results$RMSE)],4),"\t")
    cat(t2$times$everything[3],"\n")
    my_parameters <- t2$results[which.min(t2$results$RMSE),]
    my_RMSE <- t2$results[which.min(t2$results$RMSE), "RMSE"]
    output$RMSE <- my_RMSE
    output$parameters <- my_parameters
    output$algorithm <- i
    SaveDataFrame(output,outputDataPathMonth,paste0(i,"caret_models_results"))
    
  }
  return(output)
}

# results <- train(y=y, x=x, "rpart", trControl = trainControl(method = "boot632"))
# my_parameters <- results$results[which.min(results$results$RMSE),]
# my_RMSE <- results$results[which.min(results$results$RMSE), "RMSE"]

to_run <- function(i){my_testing_function(i,outputDataPathMonth)}

# # use lapply/loop to run everything
list_t2 <- lapply(m,to_run)
print("Done")
SaveDataFrame(list_t2,outputDataPathMonth,"all_caret_models_results")

## Getting the best algorithm
RMSEs <- sapply(list_t2,function(x) {x$RMSE})
my_best_model <- list_t2[[which.min(RMSEs)]]


# Aggregating results together 
### Finding the best tuned model per minimum RMSE

