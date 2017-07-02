## Draft features selection with Boruta paper
library("Boruta")

run.name <- "feature-1"
set.seed(1)
## Set up artificial test data for our analysis
n.var <- 20
n.obs <- 200
x <- data.frame(V=matrix(rnorm(n.var*n.obs), n.obs, n.var))


## Utility function to make plots of Boruta test results
make.plots <- function(b, num,
                       true.var = NA,
                       main = paste("Boruta feature selection for test", num)) {
  write.text <- function(b, true.var) {
    if ( !is.na(true.var) ) {
      text(1, max(attStats(b)$meanZ), pos = 4,
           labels = paste("True vars are V.1-V.",
                          true.var, sep = ""))        
    }
  }
  plot(b, main = main, las = 3, xlab = "")
  write.text(b, true.var)
  png(paste(run.name, num, "png", sep = "."), width = 8, height = 8,
      units = "cm", res = 300, pointsize = 4)
  plot(b, main = main, lwd = 0.5, las = 3, xlab = "")
  write.text(b, true.var)
  dev.off()
}


## 1. Simple test of single variable
y.1 <- factor( ifelse( x$V.1 >= 0, 'A', 'B' ) )

b.1 <- Boruta(x, y.1, doTrace = 2)
make.plots(b.1, 1)



## 2. Simple test of linear combination
n.dep <- floor(n.var/5)
print(n.dep)

m <- diag(n.dep:1)

y.2 <- ifelse( rowSums(as.matrix(x[, 1:n.dep]) %*% m) >= 0, "A", "B" )
y.2 <- factor(y.2)

b.2 <- Boruta(x, y.2, doTrace = 2)
make.plots(b.2, 2, n.dep)


## 3. Simple test of less-linear combination
y.3 <- factor(rowSums(x[, 1:n.dep] >= 0))
print(summary(y.3))
b.3 <- Boruta(x, y.3, doTrace = 2)
print(b.3)
make.plots(b.3, 3, n.dep)



# Using the caret package
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


# Rank by importance
# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


# Recursive feature elimination : RFE
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
