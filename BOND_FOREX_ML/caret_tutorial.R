### Caret tutorial

library(C50)
data(churn)
?churnTrain
colnames(churnTrain)

head(churnTrain)
summary(churnTrain)


library(ggplot2)
g <- ggplot(data=churnTrain, aes(x=total_day_minutes, y=number_customer_service_calls)) + 
  geom_point(aes(color=churn), position="jitter", alpha=0.5) + 
  facet_wrap(~churn) +
  theme_bw()

print(g)


library(caret)
index <- which(!sapply(churnTrain, is.numeric))
churn.nums <- churnTrain[,-index]
churn.corr <- cor(churn.nums)
(index <- findCorrelation(churn.corr, verbose=FALSE))
colnames(churn.nums[,index])

churn.nums.clean <- churn.nums[,-index]

churn.pca <- princomp(churn.nums.clean, cor= TRUE)
summary(churn.pca)
loadings(churn.pca)
plot(churn.pca, type="l")
# par(mfrow=c(1,2))
# biplot(churn.pca, cex=c(.5,.75), col=c(0,2))
# biplot(churn.pca, choices=2:3, cex=c(.5,.75), col=c(0,2))
# par(mfrow=c(1,1))

set.seed(123)
tree_cart <- train(churn~., data=churnTrain, method="rpart") # that creates the tree
library(partykit) # we need this package to make the nice plot one line below
plot(as.party(tree_cart$finalModel), tp_args=list(beside=TRUE), gp=gpar(cex=0.7, las=0))



cvCtrl <- trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
set.seed(123)
tree_cart_cv <- train(churn~., data=churnTrain, method="rpart", trControl=cvCtrl, metric="ROC") # that creates the tree
#tree_cart_cv
#tree_cart_cv$finalModel
plot(as.party(tree_cart_cv$finalModel), tp_args=list(beside=FALSE), gp=gpar(cex=0.7, las=0))

# tree_rpart <- train(churn~., data=churnTrain, method="rpart", trControl=trainControl(method="none"), tuneLength=1) 
# tree_rpart$finalModel
#tree_rpart <- rpart(churn~., data=churnTrain, method="class")
#plot(as.party(tree_rpart), tp_args=list(beside=FALSE), gp=gpar(cex=0.7, las=0))


predict_train <- predict(tree_cart_cv, churnTrain)
confusionMatrix(predict_train, churnTrain$churn)
# predict_train_rpart <- predict(tree_rpart, churnTrain, type="class") #predicts the unprunded rpart
# confusionMatrix(predict_train_rpart, churnTrain$churn)

predict_test <- predict(tree_cart_cv, churnTest)
confusionMatrix(predict_test, churnTest$churn)


predict_probs <- predict(tree_cart_cv, churnTrain, type="prob")
tail(predict_probs)

# library(pROC)
# auc(churnTrain$churn, predict_probs$no)

library(ROCR)
preds_rocr <- prediction(predict_probs$yes, churnTrain$churn)
auc <- round(performance(preds_rocr, "auc")@y.values[[1]],4)
plot(performance(preds_rocr, "tpr", "fpr"), colorize=TRUE, main="ROC curve for class 'yes'")
text(x=.4, y=0.4, labels=paste("AUC value: ",auc))





library(RWeka)
# tree_j48 <- J48(churn~., data=churnTrain)
# evaluate_Weka_classifier(tree_j48, numFolds = 10, complexity = FALSE, 
#     seed = 1, class = TRUE)

set.seed(123)
tree_j48_cv <- train(churn~., data=churnTrain, method="J48", trControl=cvCtrl) # that creates the tree
tree_j48_cv
(tree_j48_cv$finalModel)
predict_probs_j48 <- predict(tree_j48_cv, churnTrain, type="prob")

## C5.0
set.seed(123)
tree_c5_cv <- train(churn~., data=churnTrain, method="C5.0Tree", trControl=cvCtrl) # that creates the tree
#tree_c5_cv
#summary(tree_c5_cv$finalModel)
predict_probs_c5 <- predict(tree_c5_cv, churnTrain, type="prob")


## SVM
library(kernlab)
set.seed(123)
#NOTE: Cannot get SVM running with caret for whatever reason....
#svmTune <- ksvm(Species~., data=train,prob.model=TRUE)
# svmTune <- train(churn~., data=churnTrain, method="svmRadial", trControl=cvCtrl) # that creates the tree
# predict_svm <- predict(svmTune, as.data.frame(test)[,-5], type="prob")

# kNN
knnFit <- train(churn~., data=churnTrain, method = "knn", trControl = cvCtrl)
predict_knn <- predict(knnFit, churnTrain, type="prob")

#compare different models
#the problem seems to be how CARET treats factor variables. It blows the colums up, e.g. "stateAL"
modelObjects <- list(cart=tree_cart_cv,
                     j48=tree_j48_cv,
                     c50=tree_c5_cv,
                     knn=knnFit)
#predict(modelObjects, churnTrain)

#sapply(modelObjects, function(x) x$method)
x <- data.frame(ROC=sort(sapply(modelObjects, function(x) max(x$results$ROC)), decreasing=TRUE))
x
# We observe that C50 and J48 perform similarly, while kNN and CART are significantly worse.

# We can also perform a resampling of the models to get a feeling for stability

# variances in resamples.
cvValues <- resamples(list(CART=tree_cart_cv, C50=tree_c5_cv, J48=tree_j48_cv, kNN=knnFit))
summary(cvValues)
dotplot(cvValues)
