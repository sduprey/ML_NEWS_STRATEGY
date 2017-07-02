####### Plotting the trading weights position

### Launching all pair spread computations
### Trying to forecast the spread between ?/? bonds futures
# library("SIT")
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
require(rpart)
require(randomForest)
# require(rpart.plot)
# require(rattle)
# install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
# require(caret)
require(xgboost)

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

outputDataPathMonth <- paste(outputDataPath,"Month_2007/",sep="")


## Plotting our weight trading pattern

grid <- seq(-2,2,0.1)
small_grid <- seq(0,0.5,0.1)
leverage_threshold <- 0.5
epsilon <- 0.2
grid_plus_one_up <- 1+small_grid
grid_plus_one_down<- -1-small_grid
grid_up <- small_grid
grid_down <- -small_grid


big_lines_df <- data.frame(origin=rep(0,length(grid)) ,grid=grid, x = grid,y_long = 1-grid,y_short = -1-grid)
small_lines <- data.frame(origin=rep(0,length(small_grid)),
                          grid_plus_one_up=grid_plus_one_up,
                          grid_plus_one_down=grid_plus_one_down,
                          grid_up=grid_up,
                          grid_down=grid_down)
points_labels <- data.frame(x=c(0-epsilon,0+epsilon,-1-epsilon,1+epsilon,0-epsilon,                    0-epsilon,                 -1-leverage_threshold-epsilon,1+leverage_threshold+epsilon),
                     y=c(-1-epsilon,1+epsilon,0-epsilon,0+epsilon,-1-leverage_threshold-epsilon,1+leverage_threshold+epsilon,0-epsilon,                  0-epsilon),
                     my_labels=c("-1","1","-1","1","-1-T","1+T","-1-T","1+T"))
points <- data.frame(x=c(0,0,-1,1,0,                    0,                 -1-leverage_threshold,1+leverage_threshold),
                            y=c(-1,1,0,0,-1-leverage_threshold,1+leverage_threshold,0,                  0),
                            my_labels=c("-1","1","-1","1","-1-T","1+T","-1-T","1+T"))

g <- ggplot() + 
  geom_line(data=big_lines_df, aes(x =x, y = y_long)) + 
  geom_line(data=big_lines_df,aes(x =x, y = y_short))+
  geom_line(data=big_lines_df,aes(x =origin, y = grid))+
  geom_line(data=big_lines_df,aes(x =grid, y = origin))+
  geom_line(data=small_lines,aes(x =origin, y = grid_plus_one_up),size=2)+
  geom_line(data=small_lines,aes(x =origin, y = grid_plus_one_down),size=2)+
  geom_line(data=small_lines,aes(x = grid_plus_one_up, y =origin),size=2)+
  geom_line(data=small_lines,aes(x = grid_plus_one_down, y =origin),size=2)+
  geom_line(data=small_lines,aes(x =origin, y = grid_down),size=2)+
  geom_line(data=small_lines,aes(x =origin, y = grid_up),size=2)+
  geom_line(data=small_lines,aes(x = grid_down, y =origin),size=2)+
  geom_line(data=small_lines,aes(x = grid_up, y =origin),size=2)+
  geom_line(data=small_lines,aes(x = (1+grid_up), y =(-grid_up)),size=2)+
  geom_line(data=small_lines,aes(x = (-1+grid_down), y =(-grid_down)),size=2)+
  geom_line(data=small_lines,aes(x = (-grid_up), y =(1+grid_up)),size=2)+
  geom_line(data=small_lines,aes(x = (-grid_down), y =(-1+grid_down)),size=2)+
  geom_text(data=points_labels, aes(x=x, y=y, label=my_labels))+
  geom_point(data=points, aes(x=x, y=y),size=5)+
  scale_y_continuous("w2",limits = c(-2,2))+
  scale_x_continuous("w1",limits = c(-2,2))
print(g)  




ExportPlot(g,outputDataPath,"my_leveraged_weights")




# g <- ggplot() + 
#   geom_line(data=small_lines,aes(x =origin, y = grid_plus_one_up),size=1)+
#   geom_line(data=small_lines,aes(x =origin, y = grid_plus_one_down),size=1)+
#   geom_line(data=small_lines,aes(x = grid_plus_one_up, y =origin),size=1)+
#   geom_line(data=small_lines,aes(x = grid_plus_one_down, y =origin),size=1)+
#   geom_line(data=small_lines,aes(x =origin, y = grid_down),size=1)+
#   geom_line(data=small_lines,aes(x =origin, y = grid_up),size=1)+
#   geom_line(data=small_lines,aes(x = grid_down, y =origin),size=1)+
#   geom_line(data=small_lines,aes(x = grid_up, y =origin),size=1)+
#   geom_line(data=small_lines,aes(x = (1+grid_up), y =(-grid_up)),size=1)+
#   geom_line(data=small_lines,aes(x = (-1+grid_down), y =(-grid_down)),size=1)+
#   geom_line(data=small_lines,aes(x = (-grid_up), y =(1+grid_up)),size=1)+
#   geom_line(data=small_lines,aes(x = (-grid_down), y =(-1+grid_down)),size=1)+
#   
#   scale_y_continuous("w2",limits = c(-2,2))+
#   scale_x_continuous("w1",limits = c(-2,2))
# 
# print(g)  

