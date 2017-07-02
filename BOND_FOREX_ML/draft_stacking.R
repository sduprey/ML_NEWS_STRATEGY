## Testing the stacking model technique
#### Avoiding overfitting when parameter tuning

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
## for the modeling
require(rpart)
require(randomForest)
require(xgboost)
## require caret (for dummy variables)
require(caret)
## require Metrics to compute error
require(Metrics)
## require RCurl to fetch the dataset
require(RCurl)

source("./RCode/RP_Plotting_Utils.R")
source("./RCode/RP_Macro_Monthly_Utils.R")
source("./RCode/RP_Spread_Utils.R")
source("./RCode/RP_Dates_Utils.R")
source("./RCode/RP_Df_Utils.R")


user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")

## Tuning the xgboost parameters
# maximum depth of the tree
# maximum number of iteration

data("agaricus.train")
data("agaricus.test")
train <- agaricus.train
test <- agaricus.test

bst <- xgboost(data=train$data, label=train$label,nrounds = 2,objective = "binary:logistic",eval_metric="auc")

pred <- predict(bst, test$data)

cv.res <- xgb.cv(data=train$data, nfold=5, label=train$label,nrounds = 2,objective = "binary:logistic",eval_metric="auc")


set.seed(10)  
y<-c(1:1000)  
x1<-c(1:1000)*runif(1000,min=0,max=2)  
x2<-c(1:1000)*runif(1000,min=0,max=2)  
x3<-c(1:1000)*runif(1000,min=0,max=2) 

lm_fit<-lm(y~x1+x2+x3)  
summary(lm_fit)  

set.seed(10)  
all_data<-data.frame(y,x1,x2,x3)  
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))  
training<- all_data[positions,]  
testing<- all_data[-positions,]  



lm_fit<-lm(y~x1+x2+x3,data=training)  
predictions<-predict(lm_fit,newdata=testing)  
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))  


library(foreach)  
length_divisor<-4  
iterations<-1000  
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {  
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))  
  train_pos<-1:nrow(training) %in% training_positions  
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])  
  predict(lm_fit,newdata=testing)  
}  
predictions<-rowMeans(predictions)  
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))  
