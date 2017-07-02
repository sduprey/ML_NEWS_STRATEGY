############## collection of functions to help us plot 
library("lubridate")
library("ggplot2")
library("scales")
library("ggthemes")
library("grid")
require(plyr)


scale_dimension.custom_expand <- function(scale, expand = ggplot2:::scale_expand(scale)) {
  expand_range(ggplot2:::scale_limits(scale), expand[[1]], expand[[2]])
}

scale_y_continuous <- function(...) {
  s <- ggplot2::scale_y_continuous(...)
  class(s) <- c('custom_expand', class(s))
  s
}


##### my function to export my plot to a file
ExportPlot <- function(gplot, outputDataPath, filename, width=10, height=6) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  #ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  #postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  #print(gplot)
  #dev.off()
  png(file = paste(outputDataPath, filename, '.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}


#### Plotting them all together
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


SaveDataFrame <- function(dataframe, outputDataPath, filename) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  #ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  #postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  #print(gplot)
  #dev.off()
  saveRDS(dataframe, file=paste(outputDataPath, filename, '.rds', sep=""))
}




