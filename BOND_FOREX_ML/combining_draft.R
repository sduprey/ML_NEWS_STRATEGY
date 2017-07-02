# Combining draft
regressors <- c("y1", "y2", "y3", "y4")

vec <- c(T, F, T, F)
paste(regressors[vec])


paste(regressors[vec], collapse=" + ")
paste(c("y ~ 1", regressors[vec]), collapse=" + ")

as.formula(paste(c("y ~ 1", regressors[vec]), collapse=" + "))