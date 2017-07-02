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

# ## Tuning the xgboost parameters
# # maximum depth of the tree
# # maximum number of iteration
# 
# data("agaricus.train")
# data("agaricus.test")
# train <- agaricus.train
# test <- agaricus.test
# 
# bst <- xgboost(data=train$data, label=train$label,nrounds = 2,objective = "binary:logistic",eval_metric="auc")
# 
# pred <- predict(bst, test$data)
# 
# cv.res <- xgb.cv(data=train$data, nfold=5, label=train$label,nrounds = 2,objective = "binary:logistic",eval_metric="auc")
# 


require(caret)
require(xgboost)
require(data.table)
require(vcd)
require(e1071)



## classification and caret cross validation
library(caret)
data(iris)
tc <- trainControl("cv",10)
rpart.grid <- expand.grid(.cp=0.2)


  
(train.rpart <- train(Species ~., data=iris, method="rpart",trControl=tc,tuneGrid=rpart.grid))

# Load Arthritis dataset in memory.
data(Arthritis)
# Create a copy of the dataset with data.table package (data.table is 100% compliant with R dataframe but its syntax is a lot more consistent and its performance are really good).
df <- data.table(Arthritis, keep.rownames = F)

# Let's add some new categorical features to see if it helps. Of course these feature are highly correlated to the Age feature. Usually it's not a good thing in ML, but Tree algorithms (including boosted trees) are able to select the best features, even in case of highly correlated features.
# For the first feature we create groups of age by rounding the real age. Note that we transform it to factor (categorical data) so the algorithm treat them as independant values.
df[,AgeDiscret:= as.factor(round(Age/10,0))]

# Here is an even stronger simplification of the real age with an arbitrary split at 30 years old. I choose this value based on nothing. We will see later if simplifying the information based on arbitrary values is a good strategy (I am sure you already have an idea of how well it will work!).
df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))]

# We remove ID as there is nothing to learn from this feature (it will just add some noise as the dataset is small).
df[,ID:=NULL]

#-------------Basic Training using XGBoost in caret Library-----------------
# Set up control parameters for caret::train
# Here we use 10-fold cross-validation, repeating twice, and using random search for tuning hyper-parameters.
fitControl <- trainControl(method = "cv", number = 10, repeats = 2, search = "random")
# train a xgbTree model using caret::train
model <- train(factor(Improved)~., data = df, method = "xgbTree", trControl = fitControl)

# Instead of tree for our boosters, you can also fit a linear regression or logistic regression model using xgbLinear
# model <- train(factor(Improved)~., data = df, method = "xgbLinear", trControl = fitControl)

# See model results
print(model)

