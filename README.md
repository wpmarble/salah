# Replication Archive for "Can Exposure to Celebrities Reduce Prejudice?"

This archive contains data and code necessary to replicate the results in "Can Celebrities Reduce Prejudice? The Effect of Mohamed Salah on Islamophobic Behaviors and Attitudes," by Ala' Alrababa'h, William Marble, Salma Mousa, and Alexandra Siegel. 

This replication archive is available both on the _APSR_ dataverse and on Github. To download this archive from Github, run the following command in Terminal:
```
git clone https://github.com/wpmarble/salah.git
```

Begin by downloading the whole replication archive from Dataverse or Github, unzipping it if necessary, and setting your working directory in R/Stata to the base folder in the replication archive.

All paths in the scripts are relative to this base directory. All the code for reproducinig the results (including tables and figures) in the main text and appendix is in the `code` subfolder. Almost all of the code is in R, with the exception of two Stata do-files in the hate crime analysis section.

The code has 4 parts:
  1. hate crime analysis
  2. twitter analysis
  3. survey experiment analysis
  4. miscellaneous

The rest of the README describes the code files included in each part as well as the necessary raw data in the `data` subdirectory. All other files in the `data` subdirectory are created by one of  the code files below. Additionally, all files in the `figs` and `tables` subdirectories are created by the code files below.






## Hate Crime Analysis

The data we use here were collected using public information requests to individual police forces. We have provided the standardized data from the responses to those requests, which includes the monthly counts of hate crimes at the police force level.

This replication archiive *does not* contain the raw datasets for other types of crimes (used to produce Figure 3) because they are very large. See comments inside `code/hate_crimes/05_crime_placebo_clean_merge.R` for straightforward instructions on how to download them for yourself.  We have, however, included the output of the cleaning file that uses them in the replication archive, so you can run the placebo crime analysis without downloading the raw data.


### Raw Data
- `data/hate_crimes/police_reports/*` (directory with a file for each police force)
- `data/hate_crimes/police-area-population.csv`
- `data/hate_crimes/cleaned_all_crime.rds`


### Code Files
- `code/hate_crimes/01_merge.do`
	- merges all the individual hate crime files together, make sure there's no gaps in the time series, create variables, etc. 

- `code/hate_crimes/02_synth_analysis.R`
	- conducts the main synthetic control analysis
	- outputs Figure 2 and additional appendix figs

- `code/hate_crimes/03_synth_analysis_mane.R`
	- conducts the hate crime analysis using Sadio Mané's signing as the treatment
	- outputs figures in the appendix

- `code/hate_crimes/04_DiD_placebo.do`
	- conducts the two-way fixed effects analysis for hate crimes (with randomization inference) that is reported in the appendix

- `code/hate_crimes/05_crime_placebo_clean_merge.R`
	- cleans the data on other crime types

- `code/hate_crimes/06_crime_placebo_analysis.R`
	- runs the synthetic control method for all other types of crime
	- makes Figure 3


## Twitter Analysis

Due to the terms of service of the Twitter API, we cannot provide the raw tweet data. We have provided data on the classification of each tweet (relevant to Muslims vs. not and anti-Muslim or not), along with the tweet ID's. Using the tweet ID's, you can download each tweet used in this analysis using the Twitter API. (Tweets that have been deleted since we first downloaded them will not be available anymore.)

### Raw Data
- `data/classified_fan_tweets.RData`

### Code Files
- `code/twitter/01twitter_synth.R`
	- conducts the main synthetic control analysis for tweets
	- outputs Figure 4 and additional appendix figs

- `code/twitter/02twitter_synth_mane.R`
	- conducts the twitter analysis using Sadio Mané's signing as the treatment
	- outputs figures in the appendix

- `code/twitter/03twitter_twfe_placebo.R`
	- conducts the two-way fixed effects analysis for tweets (with randomization inference) that is reported in the appendix



## Survey Experiment

The questionnaire for the survey experiment is provided as code/survey_experiment/Salah_Survey_v20.docx. We have also included the raw Qualtrics file, except for any personally identifiable information (such as IP address). 

### Raw Data
- `data/salah_survey.csv`
- `data/survey_games.csv`

### Code Files
- `code/survey_experiment/01cleaning.R`
	- Imports csv from qualtrics (under legacy qualtrics downloads)
	- Cleans the data, creates treatments, and creates weights, creates principal components, and control variables/variables for heterogeneous treatments
	- calls `99pca_fun.R`
	- Outputs `survey_clean.rds`

- `code/survey_experiment/02main_figure.R`
	- Inputs `survey_clean.rds`
	- Outputs the main figure in the survey experiment section of the paper (Figure 5)

- `code/survey_experiment/03analysis_ate.R`
	- Inputs `survey_clean.rds`
	- Runs the treatment check and the ATE regressions reported in the appendix
	- Outputs regression tables and figures for ATE that are shown in the appendix

- `code/survey_experiment/04analysis_amce_success.R`
	- Inputs `survey_clean.rds`
	- Conducts regressions and outputs tables and figures for AMCE for the success/failure treatments that are shown in the appendix

- `code/survey_experiment/05analysis_amce_rlgn.R`
	- Inputs `survey_clean.rds`
	- Conducts regressions and outputs tables and figures for AMCE for the religion/character treatments that are shown in the appendix

- `code/survey_experiment/06averages.R`
	- Inputs `survey_clean.rds`
	- Produces bar charts for the outcomes by treatment groups 

- `code/survey_experiment/07games.R`
	- Tests how survey outcomes changed around Liverpool victories and Salah goals

- `code/survey_experiment/08balance_table.R`
	- Produces the balance table in the appendix

- `code/survey_experiment/99pca_fun.R`
	- utility functions for conducting PCA


## Miscellaneous

Two miscellaneous analyses: Figure 1, plotting trends in attitudes towards Islam over time; and Figure A-7 in the appendix, showing media coverage of Salah and Mané.

### Raw Data
- `data/yougov.csv`
- `data/liverpool_echo.rds`

### Code Files

- `data/misc/liverpool_echo.R`
	- reads in `liverpool_echo.rds` and creates Figure A-7

- `data/misc/yougov.R`
	- reads in `yougov.csv` and creates Figure 1


## Software Version Information

The entire replication archive was last run on a MacBook Pro running MacOS 11.2.3 (Big Sur) using R version 4.0.5 and Stata 15. The Stata files require the user-written packages `reghdfe` and `outreg2` which can be downloaded from SSC. A list of all R packages called via `require()` or `library()` is below:

- dplyr 1.0.5
- estimatr 0.30.2
- ggplot2 3.3.3
- ggthemes 4.2.4
- gsynth 1.0.9
- lubridate 1.7.10
- panelView 1.1.2
- psych 2.1.3
- stats 4.0.5
- survey 4.0
- texreg 1.37.5
- tidyr 1.1.3
- tidyverse 1.3.0
- tools 4.0.5

