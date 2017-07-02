##### Steps to follow to output strategy reports
# First we generate the computations results for all pairs

source("./RCode/MONTH/NAR-269_CUM_US_WW_HORIZON_ALL_COMPUTATIONS_2007_MONTH.R")
# Dissecting all computations
source("./RCode/MONTH/NAR-269_MARKDOWN_CUM_US_WW_HORIZON_ALL_COMPUTATIONS_2007_MONTH.R")
# Aggregating all the per pair best results
source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS.R")
# Putting together every thing
# source("./RCode/MONTH/NAR-269_PUTTING_ALL_TOGETHER_2007_MONTH_ALL_PAIRS.R")
source("./RCode/MONTH/NAR-269_PUTTING_ALL_TOGETHER_2007_MONTH_ALL_PAIRS_TOMORROW.R")
# Running the variable importance computation scripts
source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS_VARIABLE_IMPORTANCE.R")
# Running the unidirectionnality quality metrics computing script
source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS_UNIDIRECTION_REPORTING.R")

## # Tree visualization also
###### source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS_TREE_VISU.R")
###### source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS_CALIBRATION.R")

# Outputing the strategy report
# source("./RCode/MONTH/NAR-271_STRATEGY_SUMMARIZING_2007_MONTH_ALL_PAIRS.R")
source("./RCode/MONTH/NAR-271_STRATEGY_SUMMARIZING_2007_MONTH_ALL_PAIRS_TOMORROW.R")
# Outputing the strategy report
source("./RCode/MONTH/NAR-271_STRATEGY_SUMMARIZING_2007_MONTH_ALL_PAIRS.Rmd")



##### For tomorrow strategy
### the beginning keeps the same
source("./RCode/MONTH/NAR-269_CUM_US_WW_HORIZON_ALL_COMPUTATIONS_2007_MONTH.R")
# Dissecting all computations
source("./RCode/MONTH/NAR-269_MARKDOWN_CUM_US_WW_HORIZON_ALL_COMPUTATIONS_2007_MONTH.R")
# Aggregating all the per pair best results
source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS.R")
# Putting together every thing
# source("./RCode/MONTH/NAR-269_PUTTING_ALL_TOGETHER_2007_MONTH_ALL_PAIRS.R")
source("./RCode/MONTH/NAR-269_PUTTING_ALL_TOGETHER_2007_MONTH_ALL_PAIRS_TOMORROW.R")



# Running the variable importance computation scripts
source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS_VARIABLE_IMPORTANCE.R")
# Running the unidirectionnality quality metrics computing script
source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS_UNIDIRECTION_REPORTING.R")

## # Tree visualization also
###### source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS_TREE_VISU.R")
###### source("./RCode/MONTH/NAR-269_MARKDOWN_2007_MONTH_ALL_PAIRS_CALIBRATION.R")

# Outputing the strategy report
source("./RCode/MONTH/NAR-271_STRATEGY_SUMMARIZING_2007_MONTH_ALL_PAIRS.R")
# Outputing the strategy report
source("./RCode/MONTH/NAR-271_STRATEGY_SUMMARIZING_2007_MONTH_ALL_PAIRS.Rmd")