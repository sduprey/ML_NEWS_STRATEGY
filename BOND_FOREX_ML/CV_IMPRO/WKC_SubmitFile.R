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

source("./RCode/RP_KLUtility.R")
source("./RCode/RP_KLReturns.R")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")
# outputDataPath <- "C:/My_Kaggle_Challenges_Data/Winton/"



# algorithm_used <- "rpart_cv"
algorithm_used <- "rpart"
# algorithm_used <- "xgboost"
# algorithm_used <- "xgboost_cv"

submission_rsd_path <- paste0(outputDataPath,paste0("pre_submission1",algorithm_used),".rds")

data_matrix <- readRDS(submission_rsd_path)


sub_matrix <-  matrix(t(data_matrix), ncol = 1, byrow = FALSE)
colnames(sub_matrix) <- c("Predicted")

names_vector <- vector(mode="character", length=60000*62)
for (i in seq(1,60000)){
  for (j in seq(1,62)){
    names_vector[(i-1)*62+j] <- paste0(i,"_",j)
  }
}

sub_df <- data.frame(Predicted = sub_matrix, Id = names_vector)

sub_df <- sub_df[c("Id","Predicted")]
SaveDataFrame(sub_df,outputDataPath,paste0("submission1",algorithm_used))




submission_rsd_path <- paste0(outputDataPath,paste0("submission1",algorithm_used),".rds")

sub_df <- readRDS(submission_rsd_path)

submission_csv_filename <- paste0(outputDataPath,algorithm_used, "1submission.csv")
write.table(sub_df, file = submission_csv_filename,  sep = ",", row.names = FALSE,quote=F)

