require(rpart)
require(randomForest)
# require(rpart.plot)
# require(rattle)
# install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
# require(caret)
require(xgboost)
require(e1071)
require(Metrics)
require(caret)
require(Matrix)
require(data.table)

RP_SimplePredictNextReturn <- function(algorithm, predictive_columns , output_column, calibration_data, prediction_data, weights = NULL ){
  if (algorithm == "xgboost_inner_cv"){
    # formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    formulaWithoutIntercept <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")),"-1")
    #     tr.mf  <- model.frame(as.formula(paste("cost ~",paste(names(train),collapse = "+"))),train)
    #     
    #     formula <- paste(output_column,"~.-1")
    # sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
    
    ####### Training data
    train_X <- sparse.model.matrix(as.formula(formulaWithoutIntercept), data = calibration_data,missing=-999.0)
    # train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    ## we drop the one hot encoding resulting from empty value predictors
    train_X <- train_X[,-grep("999",colnames(train_X))]
    train_Y <- as.matrix(calibration_data[,output_column], missing=-999.0,sparse=TRUE)
    #     ####### Out of sample predictor for next time period return prediction
    formulaTestWithoutIntercept <- paste(predictive_columns[1],"~",paste(paste(predictive_columns,collapse="+")),"-1")
    test_X <- sparse.model.matrix(as.formula(formulaTestWithoutIntercept), data = prediction_data,missing=-999.0)
    test_X <- test_X[,-grep("999",colnames(test_X))]
    ##### first methodology : most of the times too computationaly intensive
    # we fine tune our xgb with a 3 folds cv and give back the best boosted parameters
    bst <- NULL
    if (!is.null(weights)){
      # tunedParameters <- RP_SimpleInnerXgbCvNFoldFineTuningWithWeights(train_X, train_Y, weights, 10)
      tunedParameters <- RP_SimpleInnerXgbCvNFoldFineTuningWithWeights(train_X, train_Y, weights, 10)
      # we train xgboost trees accordingly
      bst <- xgboost(data = train_X, label = train_Y,
                     max.depth=tunedParameters$depth, nround=tunedParameters$rounds,eta = tunedParameters$eta,
                     subsample = tunedParameters$subsample, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
                     weight = weights, objective = "reg:linear", missing=-999.0,nthread=16)
     
    } else {
      tunedParameters <- RP_XgbCvNFoldFineTuning(train_X, train_Y, 10)
      # we train xgboost trees accordingly
      bst <- xgboost(data = train_X, label = train_Y, eta = tunedParameters$eta, gamma = tunedParameters$gamma, subsample = tunedParameters$subsample, max.depth = tunedParameters$depth,
                     min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
                     nround = tunedParameters$rounds, booster =tunedParameters$booster, objective = "reg:linear", missing='NAN')#,nthread = 7) 
      
      ##### second methodology : much less computationally intensive
      #     tunedParameters <- RP_XgbCvNFoldTuning(train_X, train_Y, 3)
      #     
      #     
      #     bst <- xgboost(data = train_X, label = train_Y,
      #                    max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
      #                    gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
      #                    min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
      #                    booster = tunedParameters$booster,objective = "reg:linear", missing='NAN',nthread=1)
      #     
    }
    
    # we predict using our model
    test_Y <- predict(bst,test_X, missing=-999.0)
    return(list(prediction=test_Y, model=bst))
  }
}

RP_SimplePredictNextReturnOld <- function(algorithm, predictive_columns , output_column, calibration_data, prediction_data, weights = NULL ){
  if (algorithm == "xgboost_inner_cv"){
    # formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    formulaWithoutIntercept <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")),"-1")
    #     tr.mf  <- model.frame(as.formula(paste("cost ~",paste(names(train),collapse = "+"))),train)
    #     
    #     formula <- paste(output_column,"~.-1")
    # sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
    
    ####### Training data
    train_X <- sparse.model.matrix(as.formula(formulaWithoutIntercept), data = calibration_data,missing=-999.0)
    # train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    ## we drop the one hot encoding resulting from empty value predictors
    train_X <- train_X[,-grep("999",colnames(train_X))]
    train_Y <- as.matrix(calibration_data[,output_column], missing=-999.0,sparse=TRUE)
    #     ####### Out of sample predictor for next time period return prediction
    formulaTestWithoutIntercept <- paste(predictive_columns[1],"~",paste(paste(predictive_columns,collapse="+")),"-1")
    test_X <- sparse.model.matrix(as.formula(formulaTestWithoutIntercept), data = prediction_data,missing=-999.0)
    test_X <- test_X[,-grep("999",colnames(test_X))]
    ##### first methodology : most of the times too computationaly intensive
    # we fine tune our xgb with a 3 folds cv and give back the best boosted parameters
    bst <- NULL
    if (!is.null(weights)){
      # we train xgboost trees accordingly
      bst <- xgboost(data = train_X, label = train_Y, nrounds =1000,
                     weight = weights, objective = "reg:linear", missing=-999.0,nthread=1)
      
    } 
    
    # we predict using our model
    test_Y <- predict(bst,test_X, missing=-999.0)
    return(list(prediction=test_Y, model=bst))
  }
}

RP_PredictNextReturn <- function(algorithm, predictive_columns , output_column, calibration_data, prediction_data, weights = NULL ){
  if (algorithm == "xgboost_inner_cv"){
    # formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    formulaWithoutIntercept <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")),"-1")
    #     tr.mf  <- model.frame(as.formula(paste("cost ~",paste(names(train),collapse = "+"))),train)
    #     
    #     formula <- paste(output_column,"~.-1")
    # sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
    
    ####### Training data
    train_X <- sparse.model.matrix(as.formula(formulaWithoutIntercept), data = calibration_data,missing=-999.0)
    # train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    ## we drop the one hot encoding resulting from empty value predictors
    train_X <- train_X[,-grep("999",colnames(train_X))]
    train_Y <- as.matrix(calibration_data[,output_column], missing=-999.0,sparse=TRUE)
    #     ####### Out of sample predictor for next time period return prediction
    formulaTestWithoutIntercept <- paste(predictive_columns[1],"~",paste(paste(predictive_columns,collapse="+")),"-1")
    test_X <- sparse.model.matrix(as.formula(formulaTestWithoutIntercept), data = prediction_data,missing=-999.0)
    test_X <- test_X[,-grep("999",colnames(test_X))]
    ##### first methodology : most of the times too computationaly intensive
    # we fine tune our xgb with a 3 folds cv and give back the best boosted parameters
    bst <- NULL
    if (!is.null(weights)){
      tunedParameters <- RP_InnerXgbCvNFoldFineTuningWithWeights(train_X, train_Y, weights, 3)
      # we train xgboost trees accordingly
      bst <- xgboost(data = train_X, label = train_Y,
                     max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
                     gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
                     min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,
                     booster = tunedParameters$booster, weight = weights, objective = "reg:linear", missing=-999.0,nthread=1)
      
    } else {
      tunedParameters <- RP_XgbCvNFoldFineTuning(train_X, train_Y, 10)
      # we train xgboost trees accordingly
      bst <- xgboost(data = train_X, label = train_Y, eta = tunedParameters$eta, gamma = tunedParameters$gamma, subsample = tunedParameters$subsample, max.depth = tunedParameters$depth,
                     min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
                     nround = tunedParameters$rounds, booster =tunedParameters$booster, objective = "reg:linear", missing='NAN')#,nthread = 7) 
      
      ##### second methodology : much less computationally intensive
      #     tunedParameters <- RP_XgbCvNFoldTuning(train_X, train_Y, 3)
      #     
      #     
      #     bst <- xgboost(data = train_X, label = train_Y,
      #                    max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
      #                    gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
      #                    min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
      #                    booster = tunedParameters$booster,objective = "reg:linear", missing='NAN',nthread=1)
      #     
    }
    
    # we predict using our model
    test_Y <- predict(bst,test_X, missing=-999.0)
    return(list(prediction=test_Y, model=bst, parameters=tunedParameters))
  }
  if (algorithm == "xgboost_cv"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    ####### Out of sample predictor for next time period return prediction
    test_X <- as.matrix(prediction_data[,predictive_columns])
    ##### first methodology : most of the times too computationaly intensive
    # we fine tune our xgb with a 3 folds cv and give back the best boosted parameters
    bst <- NULL
    if (!is.null(weights)){
      tunedParameters <- RP_XgbCvNFoldFineTuningWithWeights(train_X, train_Y, weights, 10)
      # we train xgboost trees accordingly
      bst <- xgboost(data = train_X, label = train_Y,
                     max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
                     gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
                     min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
                     booster = tunedParameters$booster, weight = weights, objective = "reg:linear", missing='NAN',nthread=1)
      
    } else {
      tunedParameters <- RP_XgbCvNFoldFineTuning(train_X, train_Y, 10)
      # we train xgboost trees accordingly
      bst <- xgboost(data = train_X, label = train_Y, eta = tunedParameters$eta, gamma = tunedParameters$gamma, subsample = tunedParameters$subsample, max.depth = tunedParameters$depth,
                     min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
                     nround = tunedParameters$rounds, booster =tunedParameters$booster, objective = "reg:linear", missing='NAN')#,nthread = 7) 
      
      ##### second methodology : much less computationally intensive
      #     tunedParameters <- RP_XgbCvNFoldTuning(train_X, train_Y, 3)
      #     
      #     
      #     bst <- xgboost(data = train_X, label = train_Y,
      #                    max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
      #                    gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
      #                    min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
      #                    booster = tunedParameters$booster,objective = "reg:linear", missing='NAN',nthread=1)
      #     
    }
    
    # we predict using our model
    test_Y <- predict(bst,test_X, missing='NAN')
    return(list(prediction=test_Y, model=bst))
  }
  if (algorithm == "xgboost"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    ####### Out of sample predictor for next time period return prediction
    test_X <- as.matrix(prediction_data[,predictive_columns])
    # we train xgboost trees accordingly
    bst <- xgboost(data = train_X, label = train_Y, max.depth = 50, eta = 1, nthread = 2, nround = 50, objective = "reg:linear", missing='NAN')
    
    # we predict using our model
    test_Y <- predict(bst,test_X, missing='NAN')
    return(list(prediction=test_Y, model=bst))
  }
  if (algorithm == "rpart_pruned"){
    ####### Training data
    calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
    ####### Wilkinson type formula notation
    formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    ####### Training our recursive tree 
    recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = 10^(-6))
    ####### Pruning the tree
    cpstat = dim(calibration_data_filtered)[1]*recursive_tree$cptable[,3]+2*(recursive_tree$cptable[,2] + 1)
    nsplit <- round(recursive_tree$cptable[which.min(cpstat), ], 3)[2]
    ####### Pruning to the optimal found number of splits
    cp_pruning = which(recursive_tree$cptable[, 2] == nsplit)
    recursive_tree_pruned = prune(recursive_tree, recursive_tree$cptable[min(cp_pruning,dim(recursive_tree$cptable)[1]), 1])
    ####### Predicting with our pruned tree
    prediction <- predict(recursive_tree_pruned,prediction_data)
    return(list(prediction=prediction, model=recursive_tree_pruned))
  }
  
  if (algorithm == "rpart_caret_cv"){
    if (!is.null(weights)){
      ####### Training data
      # calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
      calibration_data_filtered <-  calibration_data[,c(predictive_columns)]
      y_output <- calibration_data[,c(output_column)]
      ####### Wilkinson type formula notation
      formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
      
      # fitControl <- trainControl(method = 'cv', number=10)
      fitControl=trainControl(method = "repeatedcv",number=10,repeats=15)
      
      Grid <- expand.grid(cp=seq(0, 0.05, 0.005))
      fit.rpartCV <- train(formula, data=as.matrix(calibration_data_filtered), y=as.matrix(y_output),  method = 'rpart', trControl=fitControl, metric='RMSE',maximize=FALSE, tuneGrid = Grid)
      # fit.rpartCV <- train(formula, data=as.matrix(calibration_data_filtered),  method = 'rpart', trControl=fitControl, metric='RMSE',maximize=FALSE, tuneGrid = Grid)
      
      ####### Training our recursive tree from the tuned parameters
      tunedParameters <- RP_RpartCvNFoldFineTuningPrunedWeights(calibration_data_filtered, formula, output_column,weights)
      
      calibration_data_filtered$myWeigths <- weights
      recursive_tree <- rpart(formula, data=calibration_data_filtered,  weights = myWeigths, cp = tunedParameters$cp, minsplit = tunedParameters$minsplit)
      
      #     ##### second methodology : much less computationally intensive
      #     tunedParameters <- RP_RpartCvNFoldTuning(calibration_data_filtered, formula, output_column, 3)
      #     recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = tunedParameters$cp)
      
      ####### Predicting with our tree
      prediction <- predict(recursive_tree,prediction_data)
      return(list(prediction=prediction, model=recursive_tree))
    } else {
      ####### Training data
      calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
      ####### Wilkinson type formula notation
      formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
      
      fitControl <- trainControl(method = 'cv', number=10)
      Grid <- expand.grid(cp=seq(0, 0.05, 0.005))
      fit.rpartCV <- train(formula, data=subTrain, method = 'rpart', trControl=fitControl, metric='RMSE',maximize=FALSE, tuneGrid = Grid)
      
      ####### Training our recursive tree from the tuned parameters
      tunedParameters <- RP_RpartCvNFoldFineTuningPruned(calibration_data_filtered, formula, output_column)
      
      recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = tunedParameters$cp, minsplit = tunedParameters$minsplit)
      
      #     ##### second methodology : much less computationally intensive
      #     tunedParameters <- RP_RpartCvNFoldTuning(calibration_data_filtered, formula, output_column, 3)
      #     recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = tunedParameters$cp)
      
      ####### Predicting with our tree
      prediction <- predict(recursive_tree,prediction_data)
      return(list(prediction=prediction, model=recursive_tree))
    }
  }
  
  if (algorithm == "rpart_cv"){
    if (!is.null(weights)){
      ####### Training data
      calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
      ####### Wilkinson type formula notation
      formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
      
      ####### Training our recursive tree from the tuned parameters
      tunedParameters <- RP_RpartCvNFoldFineTuningPrunedWeights(calibration_data_filtered, formula, output_column,weights)
      
      calibration_data_filtered$myWeigths <- weights
      recursive_tree <- rpart(formula, data=calibration_data_filtered,  weights = myWeigths, cp = tunedParameters$cp, minsplit = tunedParameters$minsplit)
      
      #     ##### second methodology : much less computationally intensive
      #     tunedParameters <- RP_RpartCvNFoldTuning(calibration_data_filtered, formula, output_column, 3)
      #     recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = tunedParameters$cp)
      
      ####### Predicting with our tree
      prediction <- predict(recursive_tree,prediction_data)
      return(list(prediction=prediction, model=recursive_tree))
    } else {
      ####### Training data
      calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
      ####### Wilkinson type formula notation
      formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
      
      ####### Training our recursive tree from the tuned parameters
      tunedParameters <- RP_RpartCvNFoldFineTuningPruned(calibration_data_filtered, formula, output_column)
      
      recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = tunedParameters$cp, minsplit = tunedParameters$minsplit)
      
      #     ##### second methodology : much less computationally intensive
      #     tunedParameters <- RP_RpartCvNFoldTuning(calibration_data_filtered, formula, output_column, 3)
      #     recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = tunedParameters$cp)
      
      ####### Predicting with our tree
      prediction <- predict(recursive_tree,prediction_data)
      return(list(prediction=prediction, model=recursive_tree))
    }
  }
  
  if (algorithm == "rpart"){
    recursive_tree <- NULL
    if (!is.null(weights)){
      ####### Training data
      calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
      ####### Wilkinson type formula notation
      formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
      
      ####### Training our recursive tree from the tuned parameters
      calibration_data_filtered$myWeigths <- weights
      recursive_tree <- rpart(formula, data=calibration_data_filtered, weights = myWeigths, cp = 10^(-6))
    } else {
      ####### Training data
      calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
      ####### Wilkinson type formula notation
      formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
      
      ####### Training our recursive tree from the tuned parameters
      recursive_tree <- rpart(formula, data=calibration_data_filtered, weights = weights, cp = 10^(-6)) 
    }
    
    ####### Predicting with our tree
    prediction <- predict(recursive_tree,prediction_data)
    return(list(prediction=prediction, model=recursive_tree))
  }
  if (algorithm == "random_forest"){
    ####### Training data
    calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
    ####### Wilkinson type formula notation
    formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    ####### Training our random forest tree 
    rf <- randomForest(as.formula(formula), data=calibration_data_filtered, na.action = na.omit)
    ####### Predicting with our tree
    prediction <- predict(rf,prediction_data)
    return(list(prediction=prediction, model=rf))
  }
  
  if (algorithm == "random_forest_fill_nan"){
    ####### Training data
    calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
    ####### Wilkinson type formula notation
    formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    ####### Training our random forest tree 
    rf <- randomForest(as.formula(formula), data=calibration_data_filtered, na.action = na.omit)
    ####### Predicting with our tree
    prediction <- predict(rf,prediction_data)
    return(list(prediction=prediction, model=rf))
  }
  
  if (algorithm == "svm"){
    ### svm as they predict at least between two classes does not accept all similar values for a predictor
    calibration_data <- calibration_data[complete.cases(calibration_data),]
    to_get_rid_of <- (colSums(calibration_data[,predictive_columns])==0) | (colSums(diff.data.frame(calibration_data[,predictive_columns]))==0)
    predictive_columns <- predictive_columns[-which(to_get_rid_of)]
    ####### Training data
    calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
    ####### Wilkinson type formula notation
    formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    ####### Training our SVM
    svm = svm(as.formula(formula), data=calibration_data_filtered, kernel="radial",cost=1,gamma=1/2)
    ####### Predicting using our SVM
    prediction <- predict(svm,prediction_data)
    return(list(prediction=prediction, model=svm))
  }
}


RP_XgbCvNFoldTuning <- function(trainX, trainY, nbFold){
  my_best_depth <- 1
  my_best_rounds <- 1
  my_best_eta <- 1
  
  
  my_best_booster <- "gbtree"
  my_best_gamma <- 0
  my_best_subsample <- 1
  # only needed for skewed logistic regression
  # my_best_max_delta_step <- 0
  my_best_colsample_by_tree <- 1
  
  ### we drop those : too long infeasible
  my_best_min_child_weight <- 1
  # we drop this one and keep only this value
  my_best_lambda <- 0
  
  
  cvDivider <- floor(nrow(trainX) / (nbFold+1))
  
  smallestError <- +Inf
  for (depth in c(10,20,30)) { 
    for (eta in c(0.5,1)){
      for (min_child_weight in c(1,5,10)){
        for (subsample in c(0.1,0.5,1)){
          for (colsample_by_tree in c(0.1,0.5,1)){
            for (gamma in c(1,5)) { 
              for (rounds in seq(10,30,10)) {
                totalError <- c()
                indexCount <- 1
                for (cv in seq(1:nbFold)) {
                  # assign chunk to data test
                  dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
                  dataTest <- trainX[dataTestIndex,]
                  labelTest <- trainY[dataTestIndex,]
                  
                  # everything else to train
                  dataTrain <- trainX[-dataTestIndex,]
                  labelTrain <- trainY[-dataTestIndex,]
                  
                  bst <- xgboost(data = dataTrain,
                                 label = labelTrain,
                                 max.depth=depth, nround=rounds, eta = eta,
                                 gamma = gamma, subsample=subsample,colsample.by.tree=colsample_by_tree, min.child.weight=min_child_weight,
                                 lambda=my_best_lambda, booster = my_best_booster,
                                 objective = "reg:linear", missing='NAN', verbose=0,nthread=1)
                  predictions <- predict(bst, dataTest, missing='NAN', outputmargin=TRUE)
                  
                  err <- rmse(as.numeric(labelTest), as.numeric(predictions))
                  totalError <- c(totalError, err)
                }
                cv_mean <- mean(totalError,na.rm = TRUE)
                if (is.nan(cv_mean)){
                  cv_mean <- 100
                }
                if ( cv_mean < smallestError) {
                  smallestError <- cv_mean
                  print(paste(depth,rounds,eta,gamma,subsample,my_best_booster,min_child_weight,colsample_by_tree,my_best_lambda,smallestError))
                  
                  
                  my_best_depth <- depth
                  my_best_rounds <- rounds
                  my_best_eta <- eta
                  my_best_gamma <- gamma
                  my_best_subsample <- subsample
                  my_best_min_child_weight <- min_child_weight
                  my_best_colsample_by_tree <- colsample_by_tree
                } 
              } 
            }
          } 
        }
      }
    }
  }
  tunedParameters <- list()
  tunedParameters$depth <- my_best_depth
  tunedParameters$rounds <- my_best_rounds
  tunedParameters$eta <- my_best_eta
  tunedParameters$gamma <- my_best_gamma
  tunedParameters$subsample <-  my_best_subsample
  tunedParameters$booster <-  my_best_booster
  tunedParameters$min_child_weight <- my_best_min_child_weight
  tunedParameters$colsample_by_tree  <- my_best_colsample_by_tree 
  tunedParameters$lambda <- my_best_lambda 
  return(tunedParameters)
}

RP_InnerXgbCvNFoldFineTuningWithWeights <- function(trainX, trainY, weights, nbFold){
  
  my_best_depth <- 1
  my_best_rounds <- 1
  my_best_eta <- 1
  
  
  my_best_booster <- "gbtree"
  my_best_gamma <- 0
  my_best_subsample <- 1
  # only needed for skewed logistic regression
  # my_best_max_delta_step <- 0
  my_best_colsample_by_tree <- 1
  
  ### we drop those : too long infeasible
  my_best_min_child_weight <- 1
  # this value is only used for gblinear booster
  # my_best_lambda <- 0
  
  smallestError <- +Inf
  i <- 1
  for (depth in c(50,100,500)) { 
    for (eta in c(0.1,0.5,1)){
      for (min_child_weight in c(1,5,10)){
        for (subsample in c(0.1,0.5,1)){
          for (colsample_by_tree in c(0.1,0.5,1)){
            for (gamma in c(1,5)) { 
              
              param <- list("objective" = "reg:linear",    # multiclass classification 
                            "eval_metric" = "rmse",    # evaluation metric 
                            "nthread" = 8,   # number of threads to be used 
                            "max_depth" = depth,    # maximum depth of tree 
                            "eta" = eta,    # step size shrinkage 
                            "gamma" = gamma,    # minimum loss reduction 
                            "subsample" = subsample,    # part of data instances to grow tree 
                            "colsample_bytree" = colsample_by_tree,  # subsample ratio of columns when constructing each tree 
                            "min_child_weight" = min_child_weight,  # minimum sum of instance weight needed in a child 
                            # "lambda"=my_best_lambda, only used for gblinear booster
                            "booster" = my_best_booster
              )
              #                 bst <- xgboost(data = dataTrain,
              #                                label = labelTrain,
              #                                max.depth=depth, nround=rounds, eta = eta, gamma = gamma, subsample=subsample,
              #                                # max.delta.step=max_delta_step,
              #                                min.child.weight=min_child_weight, colsample.by.tree=colsample_by_tree,lambda=my_best_lambda,
              #                                booster = my_best_booster,weight = weightTrain, objective = "reg:linear", missing='NAN', verbose=0)#, nthread = 7                  predictions <- predict(bst, dataTest, missing='NAN', outputmargin=TRUE)
              
              nround.cv = 1000
              #               bst.cv <- xgb.cv(param=param, data=trainX, label=trainY, 
              #                                             nfold=nbFold, nrounds=nround.cv, weight = weights, missing=-999.0,verbose=FALSE) 
              #               ###########
              # 
              #               cv_nrounds_min <- which.min(bst.cv[, test.rmse.mean])
              #               cv_test_rmse_mean <-  bst.cv[cv_nrounds_min,test.rmse.mean]
              bst.cv <- xgb.cv(param=param, data=trainX, label=trainY, 
                               nfold=nbFold, nrounds=nround.cv, weight = weights, missing=-999.0,verbose=FALSE,prediction=TRUE) 
              ###########
              
              cv_nrounds_min <- which.min(bst.cv$dt[, test.rmse.mean])
              
              ypred <- bst.cv$pred
              y <- trainY
              
              cv_test_rmse_mean <-  bst.cv$dt[cv_nrounds_min,test.rmse.mean]
              
              if ( cv_test_rmse_mean < smallestError) {
                smallestError <- cv_test_rmse_mean
                print(paste(depth,cv_nrounds_min,eta,gamma,subsample,my_best_booster,min_child_weight,colsample_by_tree,smallestError))
                my_best_depth <- depth
                my_best_rounds <- cv_nrounds_min
                my_best_eta <- eta
                my_best_gamma <- gamma
                my_best_subsample <- subsample
                my_best_min_child_weight <- min_child_weight
                my_best_colsample_by_tree <- colsample_by_tree
              } else {
                print(paste0("Running",i))
                i <- i+1
              }
            } 
          }
        } 
      }
    }
  }
  tunedParameters <- list()
  tunedParameters$depth <- my_best_depth
  tunedParameters$rounds <- my_best_rounds
  tunedParameters$eta <- my_best_eta
  tunedParameters$gamma <- my_best_gamma
  tunedParameters$subsample <-  my_best_subsample
  tunedParameters$booster <-  my_best_booster
  tunedParameters$min_child_weight <- my_best_min_child_weight
  tunedParameters$colsample_by_tree  <- my_best_colsample_by_tree 
  return(tunedParameters)
}

RP_SimpleInnerXgbCvNFoldFineTuningWithWeights <- function(trainX, trainY, weights, nbFold){
  
  my_best_depth <- 1
  my_best_rounds <- 1
  my_best_eta <- 1
  
  
  my_best_booster <- "gbtree"
  my_best_gamma <- 0
  my_best_subsample <- 1
  # only needed for skewed logistic regression
  # my_best_max_delta_step <- 0
  my_best_colsample_by_tree <- 1
  
  ### we drop those : too long infeasible
  my_best_min_child_weight <- 1
  # this value is only used for gblinear booster
  # my_best_lambda <- 0
  
  smallestError <- +Inf
  i <- 1
  for (depth in c(6,10,50,100)) { 
    for (eta in c(0,0.1,0.3,0.5,1)){
      #       for (min_child_weight in c(1,5,10)){
      for (subsample in c(0,0.1,0.5,1)){
        for (colsample_by_tree in c(0.1,0.5,1)){
          for (gamma in c(0,1,5)) { 
            param <- list("objective" = "reg:linear",    # multiclass classification 
                          "eval_metric" = "rmse",    # evaluation metric 
                          "nthread" = 16,   # number of threads to be used 
                          "max_depth" = depth,    # maximum depth of tree 
                          "eta" = eta,  # step size shrinkage 
                          "gamma" = gamma,    # minimum loss reduction 
                          "subsample" = subsample,    # part of data instances to grow tree 
                          "colsample_bytree" = colsample_by_tree  # subsample ratio of columns when constructing each tree 
                          # "min_child_weight" = min_child_weight  # minimum sum of instance weight needed in a child 
                          #                             # "lambda"=my_best_lambda, only used for gblinear booster
                          #                             "booster" = my_best_booster
            )
            #                 bst <- xgboost(data = dataTrain,
            #                                label = labelTrain,
            #                                max.depth=depth, nround=rounds, eta = eta, gamma = gamma, subsample=subsample,
            #                                # max.delta.step=max_delta_step,
            #                                min.child.weight=min_child_weight, colsample.by.tree=colsample_by_tree,lambda=my_best_lambda,
            #                                booster = my_best_booster,weight = weightTrain, objective = "reg:linear", missing='NAN', verbose=0)#, nthread = 7                  predictions <- predict(bst, dataTest, missing='NAN', outputmargin=TRUE)
            
            nround.cv = 500
            bst.cv <- xgb.cv(param=param, data=trainX, label=trainY, 
                             nfold=nbFold, nrounds=nround.cv, weight = weights, missing=-999.0,verbose=FALSE) 
            ###########
            
            cv_nrounds_min <- which.min(bst.cv[, test.rmse.mean])
            cv_test_rmse_mean <-  bst.cv[cv_nrounds_min,test.rmse.mean]
            #               bst.cv <- xgb.cv(param=param, data=trainX, label=trainY, 
            #                                nfold=nbFold, nrounds=nround.cv, weight = weights, missing=-999.0,verbose=TRUE,prediction=TRUE) 
            #               ###########
            #               
            #               cv_nrounds_min <- which.min(bst.cv$dt[, test.rmse.mean])
            #               
            #               ypred <- bst.cv$pred
            #               y <- trainY
            #               
            #               cv_test_rmse_mean <-  bst.cv$dt[cv_nrounds_min,test.rmse.mean]
            
            if ( cv_test_rmse_mean < smallestError) {
              smallestError <- cv_test_rmse_mean
              print(paste(depth,cv_nrounds_min,eta,smallestError))
              my_best_depth <- depth
              my_best_rounds <- cv_nrounds_min
              my_best_gamma <- gamma
              my_best_subsample <- subsample
              # only needed for skewed logistic regression
              # my_best_max_delta_step <- 0
              my_best_colsample_by_tree <- colsample_by_tree
            } else {
              print(paste0("Running",i))
              i <- i+1
            }
          } 
        }
      } 
    }
  }
  #   }
  tunedParameters <- list()
  tunedParameters$depth <- my_best_depth
  tunedParameters$rounds <- my_best_rounds
  tunedParameters$eta <- my_best_eta
  tunedParameters$gamma <- my_best_gamma
  tunedParameters$subsample <- my_best_subsample
  # only needed for skewed logistic regression
  # my_best_max_delta_step <- 0
  tunedParameters$colsample_by_tree <- my_best_colsample_by_tree
  return(tunedParameters)
}

RP_XgbCvNFoldFineTuningWithWeights <- function(trainX, trainY, weights, nbFold){
  
  my_best_depth <- 1
  my_best_rounds <- 1
  my_best_eta <- 1
  
  
  my_best_booster <- "gbtree"
  my_best_gamma <- 0
  my_best_subsample <- 1
  # only needed for skewed logistic regression
  # my_best_max_delta_step <- 0
  my_best_colsample_by_tree <- 1
  
  ### we drop those : too long infeasible
  my_best_min_child_weight <- 1
  # we drop this one and keep only this value
  my_best_lambda <- 0
  
  
  cvDivider <- floor(nrow(trainX) / (nbFold+1))
  
  smallestError <- +Inf
  i <- 1
  for (depth in c(10,50,100)) { 
    for (eta in c(0.5,1)){
      for (min_child_weight in c(1,10,50,100)){
        for (subsample in c(0.1,0.5,1)){
          for (colsample_by_tree in c(0.1,0.5,1)){
            for (gamma in c(1,5)) { 
              for (rounds in c(seq(10,30,10),50,100)) {
                totalError <- c()
                indexCount <- 1
                for (cv in seq(1:nbFold)) {
                  # assign chunk to data test
                  dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
                  dataTest <- trainX[dataTestIndex,]
                  labelTest <- trainY[dataTestIndex,]
                  weightTest <- weights[dataTestIndex]
                  
                  # everything else to train
                  dataTrain <- trainX[-dataTestIndex,]
                  labelTrain <- trainY[-dataTestIndex,]
                  weightTrain <- weights[-dataTestIndex]
                  
                  bst <- xgboost(data = dataTrain,
                                 label = labelTrain,
                                 max.depth=depth, nround=rounds, eta = eta, gamma = gamma, subsample=subsample,
                                 # max.delta.step=max_delta_step,
                                 min.child.weight=min_child_weight, colsample.by.tree=colsample_by_tree,lambda=my_best_lambda,
                                 booster = my_best_booster,weight = weightTrain, objective = "reg:linear", missing='NAN', verbose=0)#, nthread = 7                  predictions <- predict(bst, dataTest, missing='NAN', outputmargin=TRUE)
                  
                  predictions <- predict(bst, dataTest, missing='NAN', outputmargin=TRUE)
                  
                  happened <- as.numeric(labelTest) 
                  predicted <- as.numeric(predictions)
                  # err <- rmse(happened, predicted)
                  # err <- sqrt(mean((happened-predicted)^2))
                  err <- mean(weightTest*(happened-predicted)^2,na.rm=TRUE)
                  totalError <- c(totalError, err)
                }
                cv_mean <- mean(totalError,na.rm = TRUE)
                if (is.nan(cv_mean)){
                  cv_mean <- 100
                }
                if ( cv_mean < smallestError) {
                  smallestError <- cv_mean
                  print(paste(depth,rounds,eta,gamma,subsample,my_best_booster,min_child_weight,colsample_by_tree,my_best_lambda,smallestError))
                  
                  
                  my_best_depth <- depth
                  my_best_rounds <- rounds
                  my_best_eta <- eta
                  my_best_gamma <- gamma
                  my_best_subsample <- subsample
                  my_best_min_child_weight <- min_child_weight
                  my_best_colsample_by_tree <- colsample_by_tree
                } else {
                  print(paste0("Running",i))
                  i <- i+1
                }
              } 
            }
          } 
        }
      }
    }
  }
  tunedParameters <- list()
  tunedParameters$depth <- my_best_depth
  tunedParameters$rounds <- my_best_rounds
  tunedParameters$eta <- my_best_eta
  tunedParameters$gamma <- my_best_gamma
  tunedParameters$subsample <-  my_best_subsample
  tunedParameters$booster <-  my_best_booster
  tunedParameters$min_child_weight <- my_best_min_child_weight
  tunedParameters$colsample_by_tree  <- my_best_colsample_by_tree 
  tunedParameters$lambda <- my_best_lambda 
  return(tunedParameters)
  
}

RP_XgbCvNFoldFineTuning <- function(trainX, trainY, nbFold){
  my_best_depth <- 1
  my_best_rounds <- 1
  my_best_eta <- 1
  my_best_gamma <- 0
  my_best_subsample <- 1
  my_best_booster <- "gbtree"
  # only needed for skewed logistic regression
  # my_best_max_delta_step <- 0
  my_best_min_child_weight <- 1
  my_best_colsample_by_tree <- 1
  my_best_lambda <- 1
  
  
  cvDivider <- floor(nrow(trainX) / (nbFold+1))
  
  smallestError <- +Inf
  for (depth in c(5,10,15,20,25,30)) { 
    for (eta in c(0,0.1,0.3,0.5,0.7,1)){
      for (lambda in c(0,1,3,5,7)){
        # for (max_delta_step in c(0,1,5,10)){
        for (min_child_weight in c(0,1,5,10,15,20)){
          for (subsample in c(0.1,0.3,0.5,0.7,1)){
            for (colsample_by_tree in c(0.1,0.3,0.5,0.7,1)){
              for (gamma in c(0,1,5,7,10)) { 
                for (rounds in seq(10,50,10)) {
                  for (my_booster in c("gblinear","gbtree")){
                    totalError <- c()
                    indexCount <- 1
                    for (cv in seq(1:nbFold)) {
                      # assign chunk to data test
                      dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
                      dataTest <- trainX[dataTestIndex,]
                      labelTest <- trainY[dataTestIndex,]
                      
                      # everything else to train
                      dataTrain <- trainX[-dataTestIndex,]
                      labelTrain <- trainY[-dataTestIndex,]
                      
                      bst <- xgboost(data = dataTrain,
                                     label = labelTrain,
                                     max.depth=depth, nround=rounds, eta = eta, gamma = gamma, subsample=subsample,
                                     # max.delta.step=max_delta_step,
                                     min.child.weight=min_child_weight, colsample.by.tree=colsample_by_tree,lambda=lambda,
                                     booster = my_booster,objective = "reg:linear", missing='NAN', verbose=0)#, nthread = 7
                      predictions <- predict(bst, dataTest, missing='NAN', outputmargin=TRUE)
                      
                      err <- rmse(as.numeric(labelTest), as.numeric(predictions))
                      totalError <- c(totalError, err)
                    }
                    cv_mean <- mean(totalError,na.rm = TRUE)
                    if (is.nan(cv_mean)){
                      cv_mean <- 100
                    }
                    if ( cv_mean < smallestError) {
                      smallestError <- cv_mean
                      print(paste(depth,rounds,eta,gamma,subsample,my_booster,min_child_weight,colsample_by_tree,lambda,smallestError))
                      my_best_depth <- depth
                      my_best_rounds <- rounds
                      my_best_eta <- eta
                      my_best_gamma <- gamma
                      my_best_subsample <- subsample
                      my_best_booster <- my_booster
                      # my_best_max_delta_step <- max_delta_step
                      my_best_min_child_weight <- min_child_weight
                      my_best_colsample_by_tree <- colsample_by_tree
                      my_best_lambda <- lambda
                      # }
                    }
                  } 
                }
              } 
            }
          } 
        }
      }
    }
  }
  tunedParameters <- list()
  tunedParameters$depth <- my_best_depth
  tunedParameters$rounds <- my_best_rounds
  tunedParameters$eta <- my_best_eta
  tunedParameters$gamma <- my_best_gamma
  tunedParameters$subsample <-  my_best_subsample
  tunedParameters$booster <-  my_best_booster
  # tunedParameters$max_delta_step <- my_best_max_delta_step
  tunedParameters$min_child_weight <- my_best_min_child_weight
  tunedParameters$colsample_by_tree  <- my_best_colsample_by_tree 
  tunedParameters$lambda <- my_best_lambda 
  return(tunedParameters)
}


RP_RpartCvNFoldFineTuningPrunedWeights <- function(calibration_data, formula, outputColumn, weights){
  my_cps = c(0,10^(-6),0.002,0.005,0.01,0.015,0.02,0.03,0.1,0.3,0.5,0.7,1)
  my_minsplits=c(1,5,seq(10,100,10))
  my_best_cp <- 0
  my_best_minsplit<- 10
  i <- 1
  smallestError <- +Inf
  for (my_cp in my_cps){
    for (my_minsplit in my_minsplits){
      ####### Training our recursive tree from the tuned parameters
      calibration_data$myWeigths <- weights
      fit <- rpart(formula, data=calibration_data, weights = myWeigths,cp = my_cp, minsplit = my_minsplit)
      # prune the tree 
      pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
      # resubstitution error after recalibration
      predictions <- predict(pfit,calibration_data)
      # err <- rmse(as.numeric(calibration_data[,outputColumn]), as.numeric(predictions))
      happened <- as.numeric(calibration_data[,outputColumn])
      predicted <- as.numeric(predictions)
      # err <- rmse(happened, predicted)
      # err <- sqrt(mean((happened-predicted)^2))
      err <- mean(weights*(happened-predicted)^2,na.rm=TRUE)
      
      if (err < smallestError) {
        smallestError <-  err
        print(paste(my_cp, my_minsplit, smallestError))
        my_best_cp <- my_cp
        my_best_minsplit <- my_minsplit
      } else {
        print(paste0("Running",i))
        i <- i+1
      }
    }
  }
  
  tunedParameters <- list()
  tunedParameters$cp <- my_best_cp
  tunedParameters$minsplit <- my_best_minsplit
  print("My tuned parameters")
  print(paste(my_best_cp, my_best_minsplit))
  print("My error")
  print(smallestError)
  return(tunedParameters)
}


RP_RpartCvNFoldFineTuningPruned <- function(calibration_data, formula, outputColumn, nbFold){
  my_cps = c(0,10^(-6),0.002,0.005,0.01,0.015,0.02,0.03,0.1,0.3,0.5,0.7,1)
  my_minsplits=c(1,5,seq(10,100,10))
  
  my_best_cp <- 0
  my_best_minsplit<- 10
  i <- 1
  smallestError <- +Inf
  for (my_cp in my_cps){
    for (my_minsplit in my_minsplits){
      ####### Training our recursive tree from the tuned parameters
      fit <- rpart(formula, data=calibration_data, cp = my_cp, minsplit = my_minsplit)
      # prune the tree 
      pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
      # resubstitution error after recalibration
      predictions <- predict(pfit,calibration_data)
      err <- rmse(as.numeric(calibration_data[,outputColumn]), as.numeric(predictions))
      if (err < smallestError) {
        smallestError <-  err
        print(paste(my_cp, my_minsplit, smallestError))
        my_best_cp <- my_cp
        my_best_minsplit <- my_minsplit
      } else {
        print(paste0("Running",i))
        i <- i+1
      }
    }
  }
  
  tunedParameters <- list()
  tunedParameters$cp <- my_best_cp
  tunedParameters$minsplit <- my_best_minsplit
  return(tunedParameters)
}

RP_RpartCvNFoldFineTuning <- function(calibration_data, formula, outputColumn, nbFold){
  my_cps = c(0,10^(-6),0.002,0.005,0.01,0.015,0.02,0.03,0.1,0.3,0.5,0.7,1)
  my_minsplits=c(1,5,seq(10,100,10))
  
  ## if we would want to be thorough
  #   rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01,
  #                 maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
  #                 surrogatestyle = 0, maxdepth = 30, ...)
  #   
  my_best_cp <- 0
  my_best_minsplit<- 10
  cvDivider <- floor(nrow(calibration_data) / (nbFold+1))
  i <- 1
  smallestError <- +Inf
  for (cp in my_cps) { 
    for (minsplit in my_minsplits) {
      totalError <- c()
      indexCount <- 1
      for (cv in seq(1:nbFold)) {
        # assign chunk to data test
        dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
        dataTest <- calibration_data[dataTestIndex,]
        # everything else to train
        dataTrain <- calibration_data[-dataTestIndex,]
        ####### Training our recursive tree from the tuned parameters
        recursive_tree <- rpart(formula, data=dataTrain, cp = cp, minsplit=minsplit)
        predictions <- predict(recursive_tree,dataTest)
        
        err <- rmse(as.numeric(dataTest[,outputColumn]), as.numeric(predictions))
        totalError <- c(totalError, err)
      }
      if (mean(totalError) < smallestError) {
        smallestError = mean(totalError)
        print(paste(cp,minsplit, smallestError))
        my_best_cp <- cp
        my_best_minsplit <- minsplit
        
      }  else {
        print(paste0("Running",i))
        i <- i+1
      }
    }
  }
  tunedParameters <- list()
  tunedParameters$cp <- my_best_cp
  tunedParameters$minsplit <- my_best_minsplit
  return(tunedParameters)
  
}


RP_RpartCvNFoldTuning <- function(calibration_data, formula, outputColumn, nbFold){
  my_cps = c(0,10^(-6),0.1,0.3,0.5,0.7,1)
  ## if we would want to be thorough
  #   rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01,
  #                 maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
  #                 surrogatestyle = 0, maxdepth = 30, ...)
  #   
  my_best_cp <- 0
  
  cvDivider <- floor(nrow(calibration_data) / (nbFold+1))
  
  smallestError <- +Inf
  for (cp in my_cps) { 
    totalError <- c()
    indexCount <- 1
    for (cv in seq(1:nbFold)) {
      # assign chunk to data test
      dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
      dataTest <- calibration_data[dataTestIndex,]
      # everything else to train
      dataTrain <- calibration_data[-dataTestIndex,]
      ####### Training our recursive tree from the tuned parameters
      recursive_tree <- rpart(formula, data=dataTrain, cp = cp)
      predictions <- predict(recursive_tree,dataTest)
      
      err <- rmse(as.numeric(dataTest[,outputColumn]), as.numeric(predictions))
      totalError <- c(totalError, err)
    }
    if (mean(totalError) < smallestError) {
      smallestError = mean(totalError)
      print(paste(cp,smallestError))
      my_best_cp <- cp
    }  
  }
  tunedParameters <- list()
  tunedParameters$cp <- my_best_cp
  return(tunedParameters)
  
}
