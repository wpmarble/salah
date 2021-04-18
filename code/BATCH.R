

## Batch file that runs all analyses (w/ the exception of Stata files)
# setwd("/path/to/replication_archive")

# In code/hate_crimes/02_synth_analysis.R, code/twitter/01twitter_synth.R, and
# code/survey_experiment/02main_figure.R, look for flag "save_bw" that will save
# grayscale versions of figures to a directory outside the replication archive
# directory. To avoid errors, set save_bw <- FALSE or change directory that these
# figs are saved to.

dir.create("figs")
dir.create("tables")

# Hate Crime Analysis -----------------------------------------------------

# "code/hate_crimes/01_merge.do" # run in Stata
source("code/hate_crimes/02_synth_analysis.R")
source("code/hate_crimes/03_synth_analysis_mane.R")
# "code/hate_crimes/04_DiD_placebo.do" # run in Stata
# "code/hate_crimes/05_crime_placebo_clean_merge.R" # requires downloading raw data, see README
source("code/hate_crimes/06_crime_placebo_analysis.R")



# Twitter Analysis --------------------------------------------------------


source("code/twitter/01twitter_synth.R")
source("code/twitter/02twitter_synth_mane.R")
source("code/twitter/03twitter_twfe_placebo.R")



# Survey Experiment Analysis ----------------------------------------------


source("code/survey_experiment/01cleaning.R")
source("code/survey_experiment/02main_figure.R")
source("code/survey_experiment/03analysis_ate.R")
source("code/survey_experiment/04analysis_amce_success.R")
source("code/survey_experiment/05analysis_amce_rlgn.R")
source("code/survey_experiment/06averages.R")
source("code/survey_experiment/07games.R")
source("code/survey_experiment/08balance_table.R")



# Miscellaneous -----------------------------------------------------------

source("code/misc/liverpool_echo.R")
source("code/misc/yougov.R")
