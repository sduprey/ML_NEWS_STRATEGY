library("sqldf")
library("glmnet")



f1_score <- function(actual, predicted) {
  tp <- as.numeric(sum(actual == 1 & predicted == 1))
  tn <- as.numeric(sum(actual == 0 & predicted == 0))
  fp <- as.numeric(sum(actual == 0 & predicted == 1))
  fn <- as.numeric(sum(actual == 1 & predicted == 0))
  precision = tp / (tp + fp)
  recall = tp / (tp + fn)
  
  f1 = (precision*recall)/(precision + recall)
  return(f1)
}


true_negative <- function(actual, predicted) {
  # tp <- as.numeric(sum(actual == 1 & predicted == 1))
  tn <- as.numeric(sum(actual == 0 & predicted == 0))
  fp <- as.numeric(sum(actual == 0 & predicted == 1))
  # fn <- as.numeric(sum(actual == 1 & predicted == 0))
  
  true_negative <- tn/(tn+fp)
  return(true_negative)
}

pure_negative <- function(actual, predicted) {
  # tp <- as.numeric(sum(actual == 1 & predicted == 1))
  tn <- as.numeric(sum(actual == 0 & predicted == 0))
  fp <- as.numeric(sum(actual == 0 & predicted == 1))
  # fn <- as.numeric(sum(actual == 1 & predicted == 0))

  true_negative <- tn/(tn+fp)
  return(true_negative)
}


mc <- function(actual, predicted) {
  tp <- as.numeric(sum(actual == 1 & predicted == 1))
  tn <- as.numeric(sum(actual == 0 & predicted == 0))
  fp <- as.numeric(sum(actual == 0 & predicted == 1))
  fn <- as.numeric(sum(actual == 1 & predicted == 0))
  
  numer <- (tp * tn) - (fp * fn)
  denom <- ((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)) ^ 0.5
  
  numer / denom
}

computeErrorMatthew <- function(labels,preds) {
  matt <- data.table(thresh = c(seq(0.5, 0.9, by = 0.1),seq(0.90, 0.99, by = 0.01),seq(0.990, 0.999, by = 0.001),seq(0.999, 0.9999, by = 0.0001)))
  
  matt$scores <- sapply(matt$thresh, FUN =
                          function(x) mc(labels, (preds > quantile(preds, x)) * 1))
  
  return(list(maxo=max(matt$scores,na.rm=TRUE), alldf=matt))
}

computeBestNegativeRate <- function(labels,preds) {
  matt <- data.table(thresh = c(seq(0.5, 0.9, by = 0.1),seq(0.90, 0.99, by = 0.01),seq(0.990, 0.999, by = 0.001),seq(0.999, 0.9999, by = 0.0001)))
  
  matt$scores <- sapply(matt$thresh, FUN =
                          function(x) true_negative(labels, (preds > quantile(preds, x)) * 1))
  
  return(list(maxo=max(matt$scores,na.rm=TRUE), alldf=matt))
}


computeBestPureNegativeRate <- function(labels,preds) {
  matt <- data.table(thresh = c(seq(0.5, 0.9, by = 0.1),seq(0.90, 0.99, by = 0.01),seq(0.990, 0.999, by = 0.001),seq(0.999, 0.9999, by = 0.0001)))
  
  matt$scores <- sapply(matt$thresh, FUN =
                          function(x) pure_negative(labels, (preds > quantile(preds, x)) * 1))
  
  return(list(maxo=max(matt$scores,na.rm=TRUE), alldf=matt))
}

computeBestF1 <- function(labels,preds) {
  matt <- data.table(thresh = c(seq(0.5, 0.9, by = 0.1),seq(0.90, 0.99, by = 0.01),seq(0.990, 0.999, by = 0.001),seq(0.999, 0.9999, by = 0.0001)))
  
  matt$scores <- sapply(matt$thresh, FUN =
                          function(x) f1_score(labels, (preds > quantile(preds, x)) * 1))
  
  return(list(maxo=max(matt$scores,na.rm=TRUE), alldf=matt))
}


column.quantilize <- function(column, quantile.number, breaks.value = FALSE){
  breaks <- unique(quantile(column, probs=seq(0,1, by=1/quantile.number),, na.rm = TRUE))
  if(length(breaks) == 1){
    column[column == breaks] <- 1
    return(column)
  }
  cuts <- cut(column,breaks=breaks,include.lowest=TRUE)
  if(breaks.value){
    levels(cuts) <- breaks
  }else{
    levels(cuts) <- seq(1,quantile.number)
  }
  
  return(cuts)
}



continuous.dataframe.quantilize <- function(input, quantile.number, na.special.bucket = TRUE){
  quantilized <- apply(input,2,function(x) {column.quantilize(x, quantile.number)})
  if(na.special.bucket){
    quantilized[is.na(quantilized)] <- "-1"
  }
  return(quantilized)
}




column.likelihood <- function(column, output, keep.na){
  if(is.numeric(column)){
    column <- round(column,3)
  }
  outputSizedColumn <- column[1:length(output)]
  if(keep.na){
    InOutdf <- data.frame(CATEGORIES=as.factor(outputSizedColumn[!is.na(outputSizedColumn)]),TARGET=as.numeric(output[!is.na(outputSizedColumn)]),stringsAsFactors = FALSE)
  } else {
    InOutdf <- data.frame(CATEGORIES=as.factor(outputSizedColumn),TARGET=as.numeric(output),stringsAsFactors = FALSE)
  }
  
  # bayesianProba <- sqldf("select distinct CATEGORIES, sum(TARGET)/count(*) as PROB1, sum(1-TARGET)/count(*) as PROB0 from InOutdf")
  category_mapping <- sqldf("select distinct a.CATEGORIES, sum(a.TARGET)/count(*) as Freq from InOutdf a group by CATEGORIES")
  
  # 
  #   countYoneCategory <- aggregate(InOutdf$TARGET,by=list(InOutdf$CATEGORIES),sum)
  #   countAllCategory <- aggregate(InOutdf$TARGET,by=list(InOutdf$CATEGORIES),length)
  #   colnames(countYoneCategory) <- c("CATEGORIES","ONE")
  #   colnames(countAllCategory) <- c("CATEGORIES","COUNT")
  #   countTable <- table(InOutdf)
  #   bayesianProbalities <- as.data.frame(countTable/rowSums(countTable))
  # category_mapping <- bayesianProbalities[bayesianProbalities$TARGET == 1, c("CATEGORIES","Freq")]
  wholeInOutdfdf <- data.frame(CATEGORIES=column,ORDER=1:length(column))
  # Beware that merge deorder a data frame : we have to reorder afterwards
  wholeInOutdfdf <- merge(wholeInOutdfdf,category_mapping,by="CATEGORIES",all.x=T)
  wholeInOutdfdf <- wholeInOutdfdf[order(wholeInOutdfdf$ORDER),]

  return(as.numeric(wholeInOutdfdf$Freq))
}


quantilized.dataframe.likelihood <- function(input, output, keep.na = TRUE){
  data.frame.likelihood <- apply(X=input,MARGIN = 2,FUN = function(x){column.likelihood(x,output, keep.na)})
  return(data.frame.likelihood)
}

column.train.test.likelihood <- function(column, output, trainindex, keep.na){
  restrictedColumn <- column[trainindex]
  if(keep.na){
    InOutdf <- data.frame(CATEGORIES=as.factor(restrictedColumn[!is.na(restrictedColumn)]),TARGET=as.numeric(output[!is.na(restrictedColumn)]),stringsAsFactors = FALSE)
  } else {
    InOutdf <- data.frame(CATEGORIES=as.factor(column),TARGET=as.numeric(output),stringsAsFactors = FALSE)
  }
  
  # bayesianProba <- sqldf("select distinct CATEGORIES, sum(TARGET)/count(*) as PROB1, sum(1-TARGET)/count(*) as PROB0 from InOutdf")
  category_mapping <- sqldf("select distinct a.CATEGORIES, sum(a.TARGET)/count(*) as Freq from InOutdf a group by CATEGORIES")
  
  # 
  #   countYoneCategory <- aggregate(InOutdf$TARGET,by=list(InOutdf$CATEGORIES),sum)
  #   countAllCategory <- aggregate(InOutdf$TARGET,by=list(InOutdf$CATEGORIES),length)
  #   colnames(countYoneCategory) <- c("CATEGORIES","ONE")
  #   colnames(countAllCategory) <- c("CATEGORIES","COUNT")
  #   countTable <- table(InOutdf)
  #   bayesianProbalities <- as.data.frame(countTable/rowSums(countTable))
  # category_mapping <- bayesianProbalities[bayesianProbalities$TARGET == 1, c("CATEGORIES","Freq")]
  wholeInOutdfdf <- data.frame(CATEGORIES=column,ORDER=1:length(column))
  # Beware that merge deorder a data frame : we have to reorder afterwards
  wholeInOutdfdf <- merge(wholeInOutdfdf,category_mapping,by="CATEGORIES",all.x=T)
  wholeInOutdfdf <- wholeInOutdfdf[order(wholeInOutdfdf$ORDER),]
  return(wholeInOutdfdf$Freq)
}

quantilized.train.test.dataframe.likelihood <- function(input, output, trainindex, keep.na = TRUE){
  data.frame.likelihood <- apply(X=input,MARGIN = 2,FUN = function(x){column.train.test.likelihood(x,output, trainindex, keep.na)})
  return(data.frame.likelihood)
}

na.likelihood <- function(column, output){
  na_index <- is.na(column)
  column[na_index] <- "-1"
  column[!na_index] <- "1"
  
  InOutdf <- data.frame(CATEGORIES=as.factor(column),TARGET=as.numeric(output))
  # InOutdf$CATEGORIES <- as.factor(InOutdf$CATEGORIES)
  countYoneCategory <- aggregate(InOutdf$TARGET,by=list(InOutdf$CATEGORIES),sum)
  countAllCategory <- aggregate(InOutdf$TARGET,by=list(InOutdf$CATEGORIES),length)
  colnames(countYoneCategory) <- c("CATEGORIES","ONE")
  colnames(countAllCategory) <- c("CATEGORIES","COUNT")
  countTable <- table(InOutdf)
  bayesianProbalities <- as.data.frame(countTable/rowSums(countTable))
  category_mapping <- bayesianProbalities[bayesianProbalities$TARGET == 1, c("CATEGORIES","Freq")]
  wholeInOutdfdf <- data.frame(CATEGORIES=as.vector(column),ORDER=1:length(column))
  # Beware that merge deorder a data frame : we have to reorder afterwards
  wholeInOutdfdf <- merge(wholeInOutdfdf,category_mapping,by="CATEGORIES",all.x=T)
  wholeInOutdfdf <- wholeInOutdfdf[order(wholeInOutdfdf$ORDER),]
  return(wholeInOutdfdf$Freq)
}


XgbCvNFoldFineTuning <- function(trainX, trainY, testX, testY, nbFold){
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
  startFrom <- 10
  biggestAUC <- -Inf
  presults <- data.table( Iteration = integer(), eta = numeric(), depth = integer(), auc=numeric(), nround = integer())
  for (depth in c(5,10,15,20)) { 
    for (eta in c(0.01,0.05,0.1,0.2)){
      if(i >= startFrom){
        #                 print("Testing")
        #                 print("nbFold")
        #                 print(nbFold)
        #                 print("depth") 
        #                 print(depth) 
        #                 print("eta")
        #                 print(eta)
        #                 print("min_child_weight")
        #                 print(min_child_weight)
        #                 print("subsample")
        #                 print(subsample)
        #                 print("colsample_by_tree")
        #                 print(colsample_by_tree)
        # #                 print("gamma")
        #                 print("booster")
        #                 print(booster)
        
        # param <- list("objective" = "binary:logistic",    # multiclass classification 
        #               "eval_metric" = "auc",    # evaluation metric 
        #               "nthread" = 7,   # number of threads to be used 
        #               "max_depth" = depth,    # maximum depth of tree 
        #               "eta" = eta,    # step size shrinkage 
        #               # "gamma" = gamma,    # minimum loss reduction 
        #               "subsample" = subsample,    # part of data instances to grow tree 
        #               "colsample_bytree" = colsample_by_tree,  # subsample ratio of columns when constructing each tree 
        #               "min_child_weight" = min_child_weight,  # minimum sum of instance weight needed in a child 
        #               "booster" = booster
        # )
        print(paste0("testing depth",depth))
        param <- list(objective = "binary:logistic", 
                      eval_metric = "auc",
                      booster = "gbtree", 
                      eta = eta,
                      subsample = 0.8,
                      colsample_bytree = 0.8,
                      min_child_weight = 0,
                      max_depth = depth)
        
        nround.cv = 350
        set.seed(120)
        bst.cv <- xgb.cv(param=param, data=trainX, label=trainY, 
                         nfold=nbFold, nrounds=nround.cv,verbose=TRUE,print_every_n = 101) 
        
        cv_nrounds_min <- which.max(bst.cv[, test.auc.mean])
        cv_test_auc_mean <-  bst.cv[cv_nrounds_min,test.auc.mean]
        
        
        # param <- list(objective = "binary:logistic", 
        #               eval_metric = "auc",
        #               booster = booster, 
        #               eta = eta,
        #               subsample = subsample,
        #               colsample_bytree = colsample_by_tree,
        #               min_child_weight = min_child_weight,
        #               max_depth = depth)
        # 
        
        print("best n rounds")
        print(cv_nrounds_min)
        cat(Sys.time())
        cat("XGBoost\n")
        
        set.seed(120)
        dtrain  <- xgb.DMatrix(trainX, label = trainY)
        m2 <- xgb.train(data = dtrain, 
                        # param, nrounds = 500,
                        # param, nrounds = 400,
                        # param, nrounds = 305,
                        param, nrounds = cv_nrounds_min)
        # watchlist = list(train = dtrain),
        # print_every_n = 101)
        
        
        # Predict
        out <- predict(m2, testX)
        
        
        sub <- data.frame( pred = out, outcome = as.numeric(testY))
        
        auc_jn <- auc(sub[,'outcome'],sub[,'pred'])
        
        rowresults <- data.table( Iteration = i, eta = eta, depth = depth, auc=auc_jn, nround = cv_nrounds_min)
        
        presults <- rbind(presults, rowresults)
        
        saveRDS(presults,file=paste0("C://My_Kaggle_Challenges_Data/BOSCH/tresbestCVparameters.rds"))
        if ( auc_jn > biggestAUC) {
          biggestAUC <- auc_jn
          print("Biggest AUC")
          print(biggestAUC)
          print(paste(depth,cv_nrounds_min))
          # print(paste(depth,cv_nrounds_min,eta,subsample,booster,min_child_weight,colsample_by_tree,biggestAUC))
          my_best_depth <- depth
          my_best_rounds <- cv_nrounds_min
          my_best_eta <- eta
          # # my_best_gamma <- gamma
          # my_best_subsample <- subsample
          # my_best_min_child_weight <- min_child_weight
          # my_best_colsample_by_tree <- colsample_by_tree
          # my_best_booster <- booster
          tunedParameters <- list()
          tunedParameters$depth <- my_best_depth
          tunedParameters$rounds <- my_best_rounds
          tunedParameters$eta <- my_best_eta
          # # tunedParameters$gamma <- my_best_gamma
          # tunedParameters$subsample <-  my_best_subsample
          # tunedParameters$booster <-  my_best_booster
          # tunedParameters$min_child_weight <- my_best_min_child_weight
          # tunedParameters$colsample_by_tree  <- my_best_colsample_by_tree 
          # tunedParameters$booster <- my_best_booster
          
        } else {
          print(paste0("Running iteration : ",i))
          
          print("over")
          print(4*4)
        }
      } 
      i <- i+1
    }
  } 
  #       }
  #     }
  #   }
  # }
  # }
  tunedParameters <- list()
  tunedParameters$depth <- my_best_depth
  tunedParameters$rounds <- my_best_rounds
  tunedParameters$eta <- my_best_eta
  # # tunedParameters$gamma <- my_best_gamma
  # tunedParameters$subsample <-  my_best_subsample
  # tunedParameters$booster <-  my_best_booster
  # tunedParameters$min_child_weight <- my_best_min_child_weight
  # tunedParameters$colsample_by_tree  <- my_best_colsample_by_tree 
  # tunedParameters$booster <- my_best_booster
  return(tunedParameters)
}


XgbCvNFoldFineTuningMat <- function(trainX, trainY, testX, testY, nbFold){
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
  startFrom <- 1
  biggestmattcoef <- -Inf
  presults <- data.table( Iteration = integer(), eta = numeric(), depth = integer(), mattcoef = numeric(),auc=numeric(), nround = integer())
  for (booster in c("gbtree","gblinear")){
    for (depth in c(5,10,15,20)) { 
      for (eta in c(0.01,0.05,0.1,0.2)){
        if(i >= startFrom){
          #                 print("Testing")
          #                 print("nbFold")
          #                 print(nbFold)
          #                 print("depth") 
          #                 print(depth) 
          #                 print("eta")
          #                 print(eta)
          #                 print("min_child_weight")
          #                 print(min_child_weight)
          #                 print("subsample")
          #                 print(subsample)
          #                 print("colsample_by_tree")
          #                 print(colsample_by_tree)
          # #                 print("gamma")
          #                 print("booster")
          #                 print(booster)
          
          # param <- list("objective" = "binary:logistic",    # multiclass classification 
          #               "eval_metric" = "auc",    # evaluation metric 
          #               "nthread" = 7,   # number of threads to be used 
          #               "max_depth" = depth,    # maximum depth of tree 
          #               "eta" = eta,    # step size shrinkage 
          #               # "gamma" = gamma,    # minimum loss reduction 
          #               "subsample" = subsample,    # part of data instances to grow tree 
          #               "colsample_bytree" = colsample_by_tree,  # subsample ratio of columns when constructing each tree 
          #               "min_child_weight" = min_child_weight,  # minimum sum of instance weight needed in a child 
          #               "booster" = booster
          # )
          
#           evalerrorMat <- function(preds, dtrain) {
#             labels <- getinfo(dtrain, "label")
#             err <- computeErrorMatthew(labels, preds)
#             return(list(metric = "error", value = err))
#           }
#           
          print(paste0("testing depth",depth))
          param <- list(objective = "binary:logistic", 
                        # eval_metric = evalerrorMat,
                        eval_metric = "auc",
                        booster = booster, 
                        eta = eta,
                        subsample = 0.8,
                        colsample_bytree = 0.8,
                        min_child_weight = 0,
                        max_depth = depth)
          
          nround.cv = 10
          set.seed(120)
          bst.cv <- xgb.cv(param=param, data=trainX, label=trainY, 
                           nfold=nbFold, nrounds=nround.cv,verbose=TRUE,print_every_n = 101) 
          
          cv_nrounds_min <- which.max(bst.cv[, test.auc.mean])
          cv_test_auc_mean <-  bst.cv[cv_nrounds_min,test.auc.mean]
          
          

          
          print("best n rounds")
          print(cv_nrounds_min)
          cat(Sys.time())
          cat("XGBoost\n")
          
          set.seed(120)
          dtrain  <- xgb.DMatrix(trainX, label = trainY)
          m2 <- xgb.train(data = dtrain, 
                          param, nrounds = cv_nrounds_min)

          
          pred <- predict(m2, testX)
          
          
          sub <- data.frame( pred = pred, outcome = as.numeric(testY))
          library("glmnet")
          auc_jn <- auc(sub[,'outcome'],sub[,'pred'])
          

          mattcoef <- computeErrorMatthew(testY,pred)
          rowresults <- data.table( Iteration = i, eta = eta, depth = depth, mattcoef = mattcoef, auc=auc_jn, nround = cv_nrounds_min)
          presults <- rbind(presults, rowresults)
          
          saveRDS(presults,file=paste0("C://My_Kaggle_Challenges_Data/BOSCH/matbestCVparameters.rds"))
          if ( mattcoef$maxmat > biggestmattcoef) {
            biggestmattcoef <- mattcoef$maxmat
            print("Biggest Matt Coeff")
            print(biggestmattcoef)
            print(paste(depth,cv_nrounds_min,eta,booster))
            # print(paste(depth,cv_nrounds_min,eta,subsample,booster,min_child_weight,colsample_by_tree,biggestAUC))
            my_best_depth <- depth
            my_best_rounds <- cv_nrounds_min
            my_best_eta <- eta
            my_best_booster <- booster
            # # my_best_gamma <- gamma
            # my_best_subsample <- subsample
            # my_best_min_child_weight <- min_child_weight
            # my_best_colsample_by_tree <- colsample_by_tree

            tunedParameters <- list()
            tunedParameters$depth <- my_best_depth
            tunedParameters$rounds <- my_best_rounds
            tunedParameters$eta <- my_best_eta
            tunedParameters$booster <-  my_best_booster
            # # tunedParameters$gamma <- my_best_gamma
            # tunedParameters$subsample <-  my_best_subsample
            # 
            # tunedParameters$min_child_weight <- my_best_min_child_weight
            # tunedParameters$colsample_by_tree  <- my_best_colsample_by_tree 
            # tunedParameters$booster <- my_best_booster
            
          } else {
            print(paste0("Running iteration : ",i))
            
            print("over")
            print(4*4*2)
          }
        } 
        i <- i+1
      }
    } 
  }
  #       }
  #     }
  #   }
  # }
  # }
  tunedParameters <- list()
  tunedParameters$depth <- my_best_depth
  tunedParameters$rounds <- my_best_rounds
  tunedParameters$eta <- my_best_eta
  # # tunedParameters$gamma <- my_best_gamma
  # tunedParameters$subsample <-  my_best_subsample
  # tunedParameters$booster <-  my_best_booster
  # tunedParameters$min_child_weight <- my_best_min_child_weight
  # tunedParameters$colsample_by_tree  <- my_best_colsample_by_tree 
  # tunedParameters$booster <- my_best_booster
  return(tunedParameters)
}
