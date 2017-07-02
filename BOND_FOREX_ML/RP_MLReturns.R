require(rpart)
require(randomForest)
# require(rpart.plot)
# require(rattle)
# install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
require(caret)
require(xgboost)
require(e1071)


RP_AlCaPredictNextReturn <- function(tunedParameters, algorithm, predictive_columns , output_column, calibration_data, prediction_data , nfold = 50){
  if (algorithm == "xgboost_cv"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    ####### Out of sample predictor for next time period return prediction
    test_X <- as.matrix(prediction_data[,predictive_columns], missing='NAN',sparse=TRUE)
    
    bst <- xgboost(data = train_X, label = train_Y,
                   max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
                   gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
                   min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,
                   booster = tunedParameters$booster,objective = "reg:linear", missing='NAN',nthread=1)
    
    # we predict using our model
    test_Y <- predict(bst,test_X, missing='NAN')
    return(list(prediction=test_Y, model=bst))
  }
  
  if (algorithm == "rpart_cv"){
    ####### Training data
    calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
    ####### Wilkinson type formula notation
    formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    
    ####### Training our recursive tree from the tuned parameters
    recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = tunedParameters$cp)
    
    ####### Predicting with our tree
    prediction <- predict(recursive_tree,prediction_data)
    return(list(prediction=prediction, model=recursive_tree))
  }
}

RP_CalibrateNextReturn <- function(algorithm, predictive_columns , output_column, calibration_data, prediction_data , nfold = 50){
  if (algorithm == "xgboost_cv"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    ####### Out of sample predictor for next time period return prediction
    # test_X <- as.matrix(prediction_data[,predictive_columns])
    ##### first methodology : most of the times too computationaly intensive
    #     # we fine tune our xgb with a 3 folds cv and give back the best boosted parameters
    #     tunedParameters <- RP_XgbCvNFoldFineTuning(train_X, train_Y, 3)
    #     # we train xgboost trees accordingly
    #     bst <- xgboost(data = train_X, label = train_Y, eta = tunedParameters$eta, gamma = tunedParameters$gamma, subsample = tunedParameters$subsample, max.depth = tunedParameters$depth,
    #                    min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
    #                    nround = tunedParameters$rounds, booster =tunedParameters$booster, objective = "reg:linear", missing='NAN')#,nthread = 7) 
    ##### second methodology
    tunedParameters <- RP_InnerXgbCvNFoldTuning(train_X, train_Y, nfold)
    
    return(tunedParameters)
    
  }
  
  if (algorithm == "rpart_cv"){
    ####### Training data
    calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
    ####### Wilkinson type formula notation
    formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    
    ####### Training our recursive tree from the tuned parameters
    tunedParameters <- RP_RpartCvNFoldFineTuning(calibration_data_filtered, formula, output_column, nfold)
    
    return(tunedParameters)
  }
}

RP_AlCaCaretPredictNextReturn <- function(tunedParameters, algorithm, predictive_columns , output_column, calibration_data, prediction_data , nfold = 50){
  if (algorithm == "xgboost_caret_cv"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    ####### Out of sample predictor for next time period return prediction
    test_X <- as.matrix(prediction_data[,predictive_columns], missing='NAN',sparse=TRUE)
    
    bst <- xgboost(data = train_X, label = train_Y,
                   max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
                   objective = "reg:linear", missing='NAN',nthread=1)
    
    # we predict using our model
    test_Y <- predict(bst,test_X, missing='NAN')
    return(list(prediction=test_Y, model=bst))
  }
  
  
}

RP_CaretCalibrateNextReturn <- function(algorithm, predictive_columns , output_column, calibration_data, prediction_data , nfold = 50){
  if (algorithm == "xgboost_caret_cv"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    x <- calibration_data[,predictive_columns]
    y <- calibration_data[,output_column]
    ####### Out of sample predictor for next time period return prediction
    test_X <- as.matrix(prediction_data[,predictive_columns], missing='NAN',sparse=TRUE)
    
    
    #     xgb_trcontrol <- trainControl(
    #       method="cv",
    #       number = 5,
    #       verboseIter = TRUE,
    #       returnData=FALSE,
    #       returnResamp = "all",
    #       allowParallel = TRUE,
    #       
    #     )
    
    # t2 <- train(y=train_Y, x=train_X, "xgbLinear", trControl = xgb_trcontrol, tuneGrid = xgb_grid_1)
    # print("Using boot632 methodology")
    # t2 <- train(y=y, x=x,  "xgbTree", trControl = trainControl(method = "boot632"))
    print("Using nfold cv methodology")
    t2 <- train(y=y, x=x,  "xgbTree", trControl = trainControl(method="cv",number = 5,verboseIter = TRUE))
    
    my_parameters <- t2$results[which.min(t2$results$RMSE),]
    print("Best parameters found")
    cat(round(t2$results$Rsquared[which.min(t2$results$RMSE)],4),"\t");
    cat(round(t2$results$RMSE[which.min(t2$results$RMSE)],4),"\t")
    cat(t2$times$everything[3],"\n")
    
    tunedParameters <- list()
    tunedParameters$depth <- my_parameters$max_depth
    tunedParameters$rounds <- my_parameters$nrounds
    tunedParameters$eta <- my_parameters$eta
    
    
    return(tunedParameters)
    
  }
}

RP_ShrinkNextReturn <- function(tunedParameters, algorithm, predictive_columns , output_column, calibration_data, prediction_data , nfold = 50, my_proportion = 1/4){
  if (algorithm == "xgboost_cv"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    ####### Out of sample predictor for next time period return prediction
    test_X <- as.matrix(prediction_data[,predictive_columns])
    
    bst <- xgboost(data = train_X, label = train_Y,
                   max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
                   gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
                   min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,
                   booster = tunedParameters$booster,objective = "reg:linear", missing='NAN',nthread=1)
    
    
    
    ## we here don t predict but just get the importance of our model features
    
    importance_df <- as.data.frame(xgb.importance(predictive_columns, model = bst))
    my_important_features <- importance_df$Feature
    limit_size <- min(my_proportion * length(predictive_columns), length(my_important_features))
    # we loop over each size of the model and try reduced model with fewer predictors
    bestError <- +Inf
    bestFeatures <- my_important_features
    for (size_step in 1:limit_size){
      my_test_predictive_columns <- my_important_features[1:size_step]
      train_X <- as.matrix(calibration_data[,my_test_predictive_columns], missing='NAN',sparse=TRUE)
      train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
      ####### Out of sample predictor for next time period return prediction
      test_X <- as.matrix(prediction_data[,my_test_predictive_columns])
      
      bst <- xgboost(data = train_X, label = train_Y,
                     max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
                     gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
                     min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,
                     booster = tunedParameters$booster,objective = "reg:linear", missing='NAN',nthread=1)
      
      train_Y_pred <- predict(bst,train_X, missing='NAN')
      
      err <- rmse(as.numeric(train_Y), as.numeric(train_Y_pred))
      if (err < bestError){
        bestError <- err
        bestFeatures <- my_test_predictive_columns
      }
    }
    return(bestFeatures)
  }
}


RP_PredictNextReturn <- function(algorithm, predictive_columns , output_column, calibration_data, prediction_data , nfold = 50){
  if (algorithm == "xgboost_cv"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    ####### Out of sample predictor for next time period return prediction
    test_X <- as.matrix(prediction_data[,predictive_columns])
    ##### first methodology : most of the times too computationaly intensive
    #     # we fine tune our xgb with a 3 folds cv and give back the best boosted parameters
    #     tunedParameters <- RP_XgbCvNFoldFineTuning(train_X, train_Y, 3)
    #     # we train xgboost trees accordingly
    #     bst <- xgboost(data = train_X, label = train_Y, eta = tunedParameters$eta, gamma = tunedParameters$gamma, subsample = tunedParameters$subsample, max.depth = tunedParameters$depth,
    #                    min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
    #                    nround = tunedParameters$rounds, booster =tunedParameters$booster, objective = "reg:linear", missing='NAN')#,nthread = 7) 
    ##### second methodology
    tunedParameters <- RP_InnerXgbCvNFoldTuning(train_X, train_Y, nfold)
    
    
    bst <- xgboost(data = train_X, label = train_Y,
                   max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
                   gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
                   min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,
                   booster = tunedParameters$booster,objective = "reg:linear", missing='NAN',nthread=1)
    
    
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
    bst <- xgboost(data = train_X, label = train_Y, nthread = 1, nround = 50, objective = "reg:linear", missing='NAN')
    
    # we predict using our model
    test_Y <- predict(bst,test_X, missing='NAN')
    return(list(prediction=test_Y, model=bst))
  }
  
  if (algorithm == "xgboost_cv_old"){
    ####### Training data
    train_X <- as.matrix(calibration_data[,predictive_columns], missing='NAN',sparse=TRUE)
    train_Y <- as.matrix(calibration_data[,output_column], missing='NAN',sparse=TRUE)
    ####### Out of sample predictor for next time period return prediction
    test_X <- as.matrix(prediction_data[,predictive_columns])
    ##### first methodology : most of the times too computationaly intensive
    #     # we fine tune our xgb with a 3 folds cv and give back the best boosted parameters
    #     tunedParameters <- RP_XgbCvNFoldFineTuning(train_X, train_Y, 3)
    #     # we train xgboost trees accordingly
    #     bst <- xgboost(data = train_X, label = train_Y, eta = tunedParameters$eta, gamma = tunedParameters$gamma, subsample = tunedParameters$subsample, max.depth = tunedParameters$depth,
    #                    min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
    #                    nround = tunedParameters$rounds, booster =tunedParameters$booster, objective = "reg:linear", missing='NAN')#,nthread = 7) 
    ##### second methodology
    tunedParameters <- RP_XgbCvNFoldTuning(train_X, train_Y, 3)
    
    
    bst <- xgboost(data = train_X, label = train_Y,
                   max.depth=tunedParameters$depth, nround=tunedParameters$rounds, eta = tunedParameters$eta, 
                   gamma = tunedParameters$gamma, subsample=tunedParameters$subsample,
                   min.child.weight=tunedParameters$min_child_weight, colsample.by.tree=tunedParameters$colsample_by_tree,lambda=tunedParameters$lambda,
                   booster = tunedParameters$booster,objective = "reg:linear", missing='NAN',nthread=1)
    
    
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
  if (algorithm == "rpart_cv"){
    ####### Training data
    calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
    ####### Wilkinson type formula notation
    formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    
    ####### Training our recursive tree from the tuned parameters
    tunedParameters <- RP_RpartCvNFoldFineTuning(calibration_data_filtered, formula, output_column, 10)
    recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = tunedParameters$cp)
    
    
    ####### Predicting with our tree
    prediction <- predict(recursive_tree,prediction_data)
    return(list(prediction=prediction, model=recursive_tree))
  }
  if (algorithm == "rpart"){
    ####### Training data
    calibration_data_filtered <-  calibration_data[,c(predictive_columns,output_column)]
    ####### Wilkinson type formula notation
    formula <- paste(output_column,"~",paste(paste(predictive_columns,collapse="+")))
    
    ####### Training our recursive tree from the tuned parameters
    recursive_tree <- rpart(formula, data=calibration_data_filtered, cp = 10^(-6))
    
    
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

RP_InnerXgbCvNFoldTuning <- function(trainX, trainY, nbFold){
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
  
  i <- 1
  smallestError <- +Inf
  for (depth in c(6,10,20,50,100)) { 
    for (eta in c(0,0.1,0.3,0.5,1)){
      for (min_child_weight in c(1,5,10)){
        for (subsample in c(0,0.1,0.5,1)){
          for (colsample_by_tree in c(0.1,0.5,1)){
            for (gamma in c(0,1,5)) { 
              #                 for (depth in c(6)) { 
              #                   for (eta in c(0)){
              #                     for (min_child_weight in c(10)){
              #                       for (subsample in c(1)){
              #                         for (colsample_by_tree in c(1)){
              #                           for (gamma in c(0)) { 
              param <- list("objective" = "reg:linear",    # multiclass classification 
                            "eval_metric" = "rmse",    # evaluation metric 
                            "nthread" = 1,   # number of threads to be used 
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
              
              nround.cv = 100
              bst.cv <- xgb.cv(param=param, data=trainX, label=trainY, 
                               nfold=nbFold, nrounds=nround.cv, weight = weights, missing="NAN",verbose=FALSE) 
              ###########
              
              cv_nrounds_min <- which.min(bst.cv[, test.rmse.mean])
              cv_test_rmse_mean <-  bst.cv[cv_nrounds_min,test.rmse.mean]
              
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
  
  smallestError <- 100
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
  
  smallestError <- 100
  for (depth in c(5,10,15)) { 
    for (eta in c(0,0.1,0.5,1)){
      for (lambda in c(0,1,5)){
        # for (max_delta_step in c(0,1,5,10)){
        for (min_child_weight in c(0,1,5,10)){
          for (subsample in c(0.1,0.5,1)){
            for (colsample_by_tree in c(0.1,0.5,1)){
              for (gamma in c(0,1,5,10)) { 
                for (rounds in seq(10,30,10)) {
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

RP_RpartCvNFoldFineTuning <- function(calibration_data, formula, outputColumn, nbFold){
  my_cps = c(0,10^(-6),0.1,0.3,0.5,0.7,1)
  ## if we would want to be thorough
  #   rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01,
  #                 maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
  #                 surrogatestyle = 0, maxdepth = 30, ...)
  #   
  my_best_cp <- 0
  
  cvDivider <- floor(nrow(calibration_data) / (nbFold+1))
  
  smallestError <- 100
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
