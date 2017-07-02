# very basic test 
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
nba$Name <- with(nba, reorder(Name, PTS))
library(ggplot2)
nba.m <- melt(nba,)
nba.m <- ddply(nba.m, .(variable), transform,
                 +     rescale = rescale(value))