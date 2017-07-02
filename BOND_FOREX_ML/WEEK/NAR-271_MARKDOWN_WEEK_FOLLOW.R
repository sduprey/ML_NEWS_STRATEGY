##### Steps to follow to output strategy reports
# First we generate the computations results for all pairs

source("./RCode/WEEK/NAR-269_CUM_US_WW_HORIZON_ALL_COMPUTATIONS_2007_WEEK.R")
# Dissecting all computations
source("./RCode/WEEK/NAR-269_MARKDOWN_CUM_US_WW_HORIZON_ALL_COMPUTATIONS_2007_WEEK.R")
# Aggregating all the per pair best results
source("./RCode/WEEK/NAR-269_MARKDOWN_2007_WEEK_ALL_PAIRS.R")
# Putting together every thing
source("./RCode/WEEK/NAR-269_PUTTING_ALL_TOGETHER_2007_WEEK_ALL_PAIRS.R")
# Running the variable importance computation scripts
source("./RCode/WEEK/NAR-269_MARKDOWN_2007_WEEK_ALL_PAIRS_VARIABLE_IMPORTANCE.R")
# Running the unidirectionnality quality metrics computing script
source("./RCode/WEEK/NAR-269_MARKDOWN_2007_WEEK_ALL_PAIRS_UNIDIRECTION_REPORTING.R")

## # Tree visualization also
###### source("./RCode/WEEK/NAR-269_MARKDOWN_2007_WEEK_ALL_PAIRS_TREE_VISU.R")
###### source("./RCode/WEEK/NAR-269_MARKDOWN_2007_WEEK_ALL_PAIRS_CALIBRATION.R")

# Outputing the strategy report
source("./RCode/WEEK/NAR-271_STRATEGY_SUMMARIZING_2007_WEEK_ALL_PAIRS.R")
# Outputing the strategy report
source("./RCode/WEEK/NAR-271_STRATEGY_SUMMARIZING_2007_WEEK_ALL_PAIRS.Rmd")




