require(rpart)
require(randomForest)
# require(rpart.plot)
# require(rattle)
# install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
# require(caret)
require(xgboost)
require(e1071)

source("./RCode/RP_MLReturns.R")

predict_next_day_bidirectional_spread_xgboost <- function(predictive_columns , output_one_column, output_two_column, calibration_data, prediction_data ){
  X_cal <- calibration_data[,predictive_columns]
  y_first <- calibration_data[,output_one_column]
  y_second <- calibration_data[,output_two_column]
  ####### y_third <- calibration_data[,output_one_column]-calibration_data[,output_two_column]
  train_X <- as.matrix(X_cal, missing='NAN')
  train_Y_first <- as.matrix(y_first, missing='NAN')
  train_Y_second <- as.matrix(y_second, missing='NAN')
  ####### train_Y_third <- as.matrix(y_third)
  
  test_X <- as.matrix(prediction_data[,predictive_columns])
  # we train two different trees with the same training set
  bst_first <- xgboost(data = train_X, label = train_Y_first, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear", missing='NAN')
  # we train two different trees with the same training set
  bst_second <- xgboost(data = train_X, label = train_Y_second, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear", missing='NAN')
  # we train two different trees with the same training set
  #######bst_third <- xgboost(data = train_X, label = train_Y_third, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
  
  my_prediction_first <- predict(bst_first,test_X, missing='NAN')
  my_prediction_second <- predict(bst_second,test_X, missing='NAN')
  #######my_prediction_third <- predict(bst_third,test_X)
  #######return(list(first=my_prediction_first , second=my_prediction_second , spread=my_prediction_third ))
  return(list(first=my_prediction_first , second=my_prediction_second, model_one=bst_first, model_two=bst_second))
  
}

predict_next_day_bidirectional_spread_rpart <- function(predictive_columns , output_one_column, output_two_column, calibration_data, prediction_data ){
  data_calibration_one <-  calibration_data[,c(predictive_columns,output_one_column)]
  formula_one <- paste(output_one_column,"~",paste(paste(predictive_columns,collapse="+")))
  data_calibration_two <-  calibration_data[,c(predictive_columns,output_two_column)]
  formula_two <- paste(output_two_column,"~",paste(paste(predictive_columns,collapse="+")))
  ####### data_calibration_three <-  calibration_data[,c(predictive_columns,output_one_column,output_two_column)]
  ####### formula_three <- paste(output_one_column ,"-",output_two_column,"~",paste(paste(predictive_columns,collapse="+")))
  
  # first directional prediction 
  recursive_tree_one <- rpart(formula_one, data=data_calibration_one, cp = 10^(-6))
  ## Pruning the tree
  cpstat = dim(data_calibration_one)[1]*recursive_tree_one$cptable[,3]+2*(recursive_tree_one$cptable[,2] + 1)
  nsplit <- round(recursive_tree_one$cptable[which.min(cpstat), ], 3)[2]
  ### Pruning to the found number of splits
  cp307 = which(recursive_tree_one$cptable[, 2] == nsplit)
  recursive_tree_one_pruned = prune(recursive_tree_one, recursive_tree_one$cptable[min(cp307,dim(recursive_tree_one$cptable)[1]), 1])
  my_prediction_one <- predict(recursive_tree_one_pruned,prediction_data)
  
  # second directional prediction
  recursive_tree_two <- rpart(formula_two, data=data_calibration_two, cp = 10^(-6))
  ## Pruning the tree
  cpstat = dim(data_calibration_two)[1]*recursive_tree_two$cptable[,3]+2*(recursive_tree_two$cptable[,2] + 1)
  nsplit <- round(recursive_tree_two$cptable[which.min(cpstat), ], 3)[2]
  ### Pruning to the found number of splits
  cp307 = which(recursive_tree_two$cptable[, 2] == nsplit)
  recursive_tree_two_pruned = prune(recursive_tree_two, recursive_tree_two$cptable[min(cp307,dim(recursive_tree_two$cptable)[1]), 1])
  
  my_prediction_two <- predict(recursive_tree_two_pruned,prediction_data)
  
  
  ########   # third directional prediction
  ########   recursive_tree_three <- rpart(formula_three, data=data_calibration_three, cp = 10^(-6))
  ########   ## Pruning the tree
  ########   cpstat = dim(data_calibration_three)[1]*recursive_tree_three$cptable[,3]+2*(recursive_tree_three$cptable[,2] + 1)
  ########   nsplit <- round(recursive_tree_three$cptable[which.min(cpstat), ], 3)[2]
  ########   ### Pruning to the found number of splits
  ########   cp307 = which(recursive_tree_three$cptable[, 2] == nsplit)
  ########   recursive_tree_three_pruned = prune(recursive_tree_three, recursive_tree_three$cptable[min(cp307,dim(recursive_tree_three$cptable)[1]), 1])
  ########   
  ########   my_prediction_three <- predict(recursive_tree_three_pruned,prediction_data)
  
  #######return(list(first=my_prediction_one, second=my_prediction_two, spread=my_prediction_three))
  return(list(first=my_prediction_one, second=my_prediction_two, model_one=recursive_tree_one_pruned, model_two=recursive_tree_two_pruned))
}


predict_next_day_bidirectional_spread_rpart_unpruned <- function(predictive_columns , output_one_column, output_two_column, calibration_data, prediction_data ){
  data_calibration_one <-  calibration_data[,c(predictive_columns,output_one_column)]
  formula_one <- paste(output_one_column,"~",paste(paste(predictive_columns,collapse="+")))
  data_calibration_two <-  calibration_data[,c(predictive_columns,output_two_column)]
  formula_two <- paste(output_two_column,"~",paste(paste(predictive_columns,collapse="+")))
  #######  data_calibration_three <-  calibration_data[,c(predictive_columns,output_one_column,output_two_column)]
  ####### formula_three <- paste(output_one_column ,"-",output_two_column,"~",paste(paste(predictive_columns,collapse="+")))
  
  # first directional prediction 
  recursive_tree_one <- rpart(formula_one, data=data_calibration_one, cp = 10^(-6))
  my_prediction_one <- predict(recursive_tree_one,prediction_data)
  
  # second directional prediction
  recursive_tree_two <- rpart(formula_two, data=data_calibration_two, cp = 10^(-6))
  my_prediction_two <- predict(recursive_tree_two,prediction_data)
  
  
  # third directional prediction
  #######recursive_tree_three <- rpart(formula_three, data=data_calibration_three, cp = 10^(-6))
  #######my_prediction_three <- predict(recursive_tree_three,prediction_data)
  
  #######return(list(first=my_prediction_one, second=my_prediction_two, spread=my_prediction_three))
  return(list(first=my_prediction_one, second=my_prediction_two, model_one=recursive_tree_one, model_two=recursive_tree_two))
}


predict_next_day_bidirectional_spread_random_forest <- function(predictive_columns , output_one_column, output_two_column, calibration_data, prediction_data ){
  data_calibration_one <-  calibration_data[,c(predictive_columns,output_one_column)]
  formula_one <- paste(output_one_column,"~",paste(paste(predictive_columns,collapse="+")))
  data_calibration_two <-  calibration_data[,c(predictive_columns,output_two_column)]
  formula_two <- paste(output_two_column,"~",paste(paste(predictive_columns,collapse="+")))
  #######  data_calibration_three <-  calibration_data[,c(predictive_columns,output_one_column,output_two_column)]
  ####### formula_three <- paste(output_one_column ,"-",output_two_column,"~",paste(paste(predictive_columns,collapse="+")))
  
  
  ######## first directional prediction
  # here we just don t do any processing
  rf_one <- randomForest(as.formula(formula_one), data=data_calibration_one)
  my_prediction_one <- predict(rf_one,prediction_data)
  # here we simply skip the not a numbers
#   rf_one <- randomForest(as.formula(formula_one), data=data_calibration_one, na.action = na.omit)
#   my_prediction_one <- predict(rf_one,prediction_data)
  # here we extrapolate the missing values using Breiman algorithm
  # data_calibration_one <- droplevels(data_calibration_one)
#   data_calibration_one_imputed <- rfImpute(as.formula(formula_one), data_calibration_one)
#   rf_one_imputed <- randomForest(as.formula(formula_one), data=data_calibration_one_imputed)
#   my_prediction_one <- predict(rf_one_imputed,prediction_data)
  
  
  ######## second directional prediction
  # here we just don t do any processing
  rf_two <- randomForest(as.formula(formula_two), data=data_calibration_two)
  my_prediction_two <- predict(rf_two,prediction_data)
# here we simply skip the not a numbers
#   rf_two <- randomForest(as.formula(formula_two), data=data_calibration_two, na.action = na.omit)
#   my_prediction_two <- predict(rf_two,prediction_data)
# here we extrapolate the missing values using Breiman algorithm
#   # data_calibration_two <- droplevels(data_calibration_two)
#   data_calibration_two_imputed <- rfImpute(as.formula(formula_two), data_calibration_two)
#   rf_two_imputed <- randomForest(as.formula(formula_two), data=data_calibration_two_imputed)
#   my_prediction_two <- predict(rf_two_imputed,prediction_data)
  

  #######return(list(first=my_prediction_one, second=my_prediction_two, spread=my_prediction_three))
  # return(list(first=my_prediction_one, second=my_prediction_two, model_one=rf_one_imputed, model_two=rf_two_imputed))
  return(list(first=my_prediction_one, second=my_prediction_two, model_one=rf_one, model_two=rf_two))
}



predict_next_day_bidirectional_spread_svm <- function(predictive_columns , output_one_column, output_two_column, calibration_data, prediction_data ){
  # getting of the column with no predicting values
  to_get_rid_of <- (colSums(calibration_data[,predictive_columns])==0) | (colSums(diff.data.frame(calibration_data[,predictive_columns]))==0)
  predictive_columns <- predictive_columns[-which(to_get_rid_of)]

  data_calibration_one <-  calibration_data[,c(predictive_columns,output_one_column)]
  formula_one <- paste(output_one_column,"~",paste(paste(predictive_columns,collapse="+")))
  data_calibration_two <-  calibration_data[,c(predictive_columns,output_two_column)]
  formula_two <- paste(output_two_column,"~",paste(paste(predictive_columns,collapse="+")))
  #######  data_calibration_three <-  calibration_data[,c(predictive_columns,output_one_column,output_two_column)]
  ####### formula_three <- paste(output_one_column ,"-",output_two_column,"~",paste(paste(predictive_columns,collapse="+")))
  
  
  ######## first directional prediction
  # here we just don t do any processing
  svm_one = svm(as.formula(formula_one), data=data_calibration_one, kernel="radial",cost=1,gamma=1/2)
  
  my_prediction_one <- predict(svm_one,prediction_data)
  # here we simply skip the not a numbers
  #   rf_one <- randomForest(as.formula(formula_one), data=data_calibration_one, na.action = na.omit)
  #   my_prediction_one <- predict(rf_one,prediction_data)
  # here we extrapolate the missing values using Breiman algorithm
  # data_calibration_one <- droplevels(data_calibration_one)
  #   data_calibration_one_imputed <- rfImpute(as.formula(formula_one), data_calibration_one)
  #   rf_one_imputed <- randomForest(as.formula(formula_one), data=data_calibration_one_imputed)
  #   my_prediction_one <- predict(rf_one_imputed,prediction_data)
  
  
  ######## second directional prediction
  # here we just don t do any processing
  svm_two = svm(as.formula(formula_two), data=data_calibration_two, kernel="radial",cost=1,gamma=1/2)
  my_prediction_two <- predict(svm_two,prediction_data)
  # here we simply skip the not a numbers
  #   rf_two <- randomForest(as.formula(formula_two), data=data_calibration_two, na.action = na.omit)
  #   my_prediction_two <- predict(rf_two,prediction_data)
  # here we extrapolate the missing values using Breiman algorithm
  #   # data_calibration_two <- droplevels(data_calibration_two)
  #   data_calibration_two_imputed <- rfImpute(as.formula(formula_two), data_calibration_two)
  #   rf_two_imputed <- randomForest(as.formula(formula_two), data=data_calibration_two_imputed)
  #   my_prediction_two <- predict(rf_two_imputed,prediction_data)
  
  
  #######return(list(first=my_prediction_one, second=my_prediction_two, spread=my_prediction_three))
  # return(list(first=my_prediction_one, second=my_prediction_two, model_one=rf_one_imputed, model_two=rf_two_imputed))
  return(list(first=my_prediction_one, second=my_prediction_two, model_one=svm_one, model_two=svm_two))
}

