

##### Second methodology
predict_next_day_bidirectional_xgboost <- function(predictive_columns_one , predictive_columns_two , output_one_column, output_two_column, calibration_data_one, calibration_data_two, prediction_data_one, prediction_data_two){
  X_cal_one <- calibration_data_one[,predictive_columns_one]
  X_cal_two <- calibration_data_two[,predictive_columns_two]
  y_one <- calibration_data_one[,output_one_column]
  y_two <- calibration_data_two[,output_two_column]
  
  train_X_one <- as.matrix(X_cal_one)
  train_X_two <- as.matrix(X_cal_two)
  
  train_Y_one <- as.matrix(y_one)
  train_Y_two <- as.matrix(y_two)
  
  test_X_one <- as.matrix(prediction_data_one[,predictive_columns_one])
  test_X_two <- as.matrix(prediction_data_two[,predictive_columns_two])
  # we train two different trees with the same training set
  
  bst_first <- xgboost(data = train_X_one, label = train_Y_one, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
  # we train two different trees with the same training set
  bst_second <- xgboost(data = train_X_two, label = train_Y_two, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
  
  my_prediction_first <- predict(bst_first,test_X_one)
  my_prediction_second <- predict(bst_second,test_X_two)
  
  return(list(first=my_prediction_first , second=my_prediction_second, model_one=bst_first, model_two=bst_second))
}

predict_next_day_bidirectional_rpart <- function(predictive_columns_one , predictive_columns_two , output_one_column, output_two_column, calibration_data_one, calibration_data_two, prediction_data_one, prediction_data_two){
  data_calibration_one <-  calibration_data_one[,c(predictive_columns_one,output_one_column)]
  formula_one <- paste(output_one_column,"~",paste(paste(predictive_columns_one,collapse="+")))
  
  data_calibration_two <-  calibration_data_two[,c(predictive_columns_two,output_two_column)]
  formula_two <- paste(output_two_column,"~",paste(paste(predictive_columns_two,collapse="+")))
  
  # first directional prediction 
  recursive_tree_one <- rpart(formula_one, data=data_calibration_one, cp = 10^(-6))
  # my_prediction_one <- predict(recursive_tree_one,prediction_data_one)
  ## Pruning the tree
  cpstat = dim(data_calibration_one)[1]*recursive_tree_one$cptable[,3]+2*(recursive_tree_one$cptable[,2] + 1)
  nsplit <- round(recursive_tree_one$cptable[which.min(cpstat), ], 3)[2]
  ### Pruning to the found number of splits
  cp307 = which(recursive_tree_one$cptable[, 2] == nsplit)
  recursive_tree_one_pruned = prune(recursive_tree_one, recursive_tree_one$cptable[min(cp307,dim(recursive_tree_one$cptable)[1]), 1])
  my_prediction_one <- predict(recursive_tree_one_pruned,prediction_data_one)
  
  # second directional prediction
  recursive_tree_two <- rpart(formula_two, data=data_calibration_two, cp = 10^(-6))
  # my_prediction_two <- predict(recursive_tree_two,prediction_data_two)
  ## Pruning the tree
  cpstat = dim(data_calibration_two)[1]*recursive_tree_two$cptable[,3]+2*(recursive_tree_two$cptable[,2] + 1)
  nsplit <- round(recursive_tree_two$cptable[which.min(cpstat), ], 3)[2]
  ### Pruning to the found number of splits
  cp307 = which(recursive_tree_two$cptable[, 2] == nsplit)
  recursive_tree_two_pruned = prune(recursive_tree_two, recursive_tree_two$cptable[min(cp307,dim(recursive_tree_two$cptable)[1]), 1])
  
  my_prediction_two <- predict(recursive_tree_two_pruned,prediction_data_two)
  
  return(list(first=my_prediction_one, second=my_prediction_two, model_one=recursive_tree_one_pruned, model_two=recursive_tree_two_pruned))
}

predict_next_day_bidirectional_rpart_unpruned <- function(predictive_columns_one , predictive_columns_two , output_one_column, output_two_column, calibration_data_one, calibration_data_two, prediction_data_one, prediction_data_two){
  data_calibration_one <-  calibration_data_one[,c(predictive_columns_one,output_one_column)]
  formula_one <- paste(output_one_column,"~",paste(paste(predictive_columns_one,collapse="+")))
  
  data_calibration_two <-  calibration_data_two[,c(predictive_columns_two,output_two_column)]
  formula_two <- paste(output_two_column,"~",paste(paste(predictive_columns_two,collapse="+")))
  
  # first directional prediction 
  recursive_tree_one <- rpart(formula_one, data=data_calibration_one, cp = 10^(-6))
  my_prediction_one <- predict(recursive_tree_one,prediction_data_one)
  
  # second directional prediction
  recursive_tree_two <- rpart(formula_two, data=data_calibration_two, cp = 10^(-6))
  my_prediction_two <- predict(recursive_tree_two,prediction_data_two)
  
  return(list(first=my_prediction_one, second=my_prediction_two, model_one=recursive_tree_one, model_two=recursive_tree_two))
}
