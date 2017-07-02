##### Markdown reporting
source("./RCode/RP_Plotting_Utils.R")
source("./RCode/RP_Macro_Monthly_Utils.R")
source("./RCode/RP_Spread_Utils.R")

rmarkdown::render("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS.R")
rmarkdown::render("./RCode/WEEK/NAR-269_MARKDOWN_2007_WEEK_ALL_PAIRS.R")
rmarkdown::render("./RCode/DAY/NAR-269_MARKDOWN_2007_DAY_ALL_PAIRS.R")

